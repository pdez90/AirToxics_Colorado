# ==============================================================
# 26  Hotspot-rotated wind Source probability profiles
# Auto-split from Suncor.Rmd  (section 26 of 40)
# ==============================================================

#Hotspot-rotated wind Source probability profiles

# ============================================================
# STANDALONE: Weighted + Smoothed source probability maps (6 pollutants)
# + TRI overlay + highlight key facilities + contextual points
# - Reads mobile data from mobile_wswd.RData (expects object: out OR df)
# - Uses exceedances >= p99 per pollutant to generate weighted upwind rays (wd FROM)
# - Ray-density “source probability” surface (gridded + Gaussian-smoothed)
# - Overlays TRI facilities filtered to bbox
# - Highlights + labels:
#     Suncor (red)
#     Sinclair (red)
#     Phillips 66 (red)
#     WWTF1 (green)
#     WWTF2 (green)
#     Woodshop (purple)
#     Refuel sites (same blue color; shortened labels to "Refuel")
# - Saves 6 individual PNGs + one combined 2x3 PNG
# - UPDATED: top Refuel labels re-positioned so they do not overlap
# - UPDATED: added WWTF2 and renamed original WWTP to WWTF1
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(readr)
  library(ggplot2)
  library(ggspatial)
  library(patchwork)
  library(scales)
  library(terra)
  library(stringr)
  library(tibble)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
in_rdata <- "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData"
tri_csv  <- "/Users/priyanka/Downloads/Suncor/TRI.csv"

out_dir  <- "/Users/priyanka/Downloads/Suncor/sourceprob_maps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# basemap
tile_type <- "cartolight"
tile_zoom <- 11
dpi_out   <- 450
surface_alpha <- 0.75
tri_alpha     <- 0.35

# projections
crs_ll <- 4326
crs_m  <- 32613  # UTM 13N

# ray settings
ray_len_m   <- 15000
ray_step_m  <- 150

# gridding + smoothing
grid_res_m    <- 250
sigma_m       <- 900
kernel_radius <- 3

# wind QC
min_ws <- 1.0

# pollutants
pollutants <- c(
  Benzene_ppb            = "benzene",
  Toluene_ppb            = "toluene",
  Trimethylbenzene_ppb   = "trimethylbenzene",
  Xylene_ppb             = "xylene",
  Hydrogen_Sulfide_ppb   = "h2s",
  Hydrogen_Cyanide_ppb   = "hcn"
)

# ----------------------------
# HIGHLIGHTED / CONTEXT POINTS
# ----------------------------
# facility_type:
#   - "tri_match": try TRI-name match first, then fall back to provided coordinates
#   - "manual": always use provided coordinates
key_facilities <- tibble::tibble(
  label = c(
    "Suncor",
    "Sinclair",
    "Phillips 66",
    "WWTF1",
    "WWTF2",
    "Woodshop",
    "Refuel",
    "Refuel",
    "Refuel",
    "Refuel"
  ),
  facility_type = c(
    "tri_match", "tri_match", "tri_match",
    "manual", "manual", "manual", "manual", "manual", "manual", "manual"
  ),
  tri_name_pattern = c(
    "SUNCOR ENERGY COMMERCE CITY REFINERY",
    "SINCLAIR DENVER PRODUCTS TERMINAL",
    "PHILLIPS 66 CO DENVER TERMINAL",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_
  ),
  lat = c(
    39.803333,
    39.8724,
    39.79668,
    39.80822838231637,   # WWTF1
    39.87304794998779,   # WWTF2
    39.791382444842746,
    39.79935581470166,   # 5640 Central Park Blvd
    39.783338577716854,  # 4750 Kipling St
    39.886063120868805,  # 12241 E 104th Ave
    39.88659578717162    # 8991 E 104th Ave
  ),
  lon = c(
    -104.945556,
    -104.8861,
    -104.94236,
    -104.9553246877205,  # WWTF1
    -104.91204700295945, # WWTF2
    -104.94754520948433,
    -104.88376424570843,
    -105.10918240337975,
    -104.84531380337543,
    -104.88371420337545
  ),
  point_fill = c(
    "red", "red", "red",
    "green3", "green3",
    "purple3",
    "dodgerblue3", "dodgerblue3", "dodgerblue3", "dodgerblue3"
  ),
  point_color = c(
    "white", "white", "white",
    "white", "white",
    "white",
    "white", "white", "white", "white"
  ),
  text_color = c(
    "red", "red", "red",
    "green4", "green4",
    "purple4",
    "dodgerblue4", "dodgerblue4", "dodgerblue4", "dodgerblue4"
  ),
  point_size = c(
    3.8, 3.8, 3.8,
    4.2, 4.2,
    3.8,
    3.3, 3.3, 3.3, 3.3
  ),
  # label nudges in meters, tuned to reduce overlap
  nudge_x = c(
     350,   250,  -350,
    -850,   700,
    -900,
    -350,   # Refuel (Central Park Blvd)
    -1100,  # Refuel (Kipling)
     900,   # Refuel (12241 E 104th Ave)
     250    # Refuel (8991 E 104th Ave)
  ),
  nudge_y = c(
     650,   650,  -650,
     850,   850,
    -900,
    -450,   # Refuel (Central Park Blvd)
    -850,   # Refuel (Kipling)
     750,   # Refuel (12241 E 104th Ave)
    1100    # Refuel (8991 E 104th Ave)
  )
)

# ----------------------------
# 0) Load mobile data
# ----------------------------
stopifnot(file.exists(in_rdata))
load(in_rdata)

if (exists("out")) {
  df <- out
} else if (exists("df")) {
  df <- df
} else {
  stop("mobile_wswd.RData must contain an object named 'out' or 'df'.")
}

req_cols <- c("Longitude", "Latitude", "date", "ws", "wd")
missing_req <- setdiff(req_cols, names(df))
if (length(missing_req) > 0) {
  stop("Missing required columns in mobile df: ", paste(missing_req, collapse = ", "))
}

pollutants <- pollutants[names(pollutants) %in% names(df)]
stopifnot(length(pollutants) > 0)

# ----------------------------
# 1) Clean / QC
# ----------------------------
df <- df %>%
  dplyr::mutate(
    Longitude = suppressWarnings(as.numeric(.data$Longitude)),
    Latitude  = suppressWarnings(as.numeric(.data$Latitude)),
    ws        = suppressWarnings(as.numeric(.data$ws)),
    wd        = suppressWarnings(as.numeric(.data$wd))
  ) %>%
  dplyr::filter(
    is.finite(.data$Longitude), is.finite(.data$Latitude),
    is.finite(.data$ws), .data$ws >= min_ws,
    is.finite(.data$wd), .data$wd >= 0, .data$wd <= 360
  )

if (nrow(df) == 0) stop("After QC filtering, df has 0 rows. Check ws/wd/coords.")

df_sf_m <- sf::st_as_sf(df, coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
  sf::st_transform(crs_m)

bb_m <- sf::st_bbox(df_sf_m)
xlim_m <- c(as.numeric(bb_m["xmin"]), as.numeric(bb_m["xmax"]))
ylim_m <- c(as.numeric(bb_m["ymin"]), as.numeric(bb_m["ymax"]))

pad <- 1500
xlim_m <- xlim_m + c(-pad, pad)
ylim_m <- ylim_m + c(-pad, pad)

# ----------------------------
# 2) TRI overlay + key facilities
# ----------------------------
tri_sf <- NULL
key_sf <- NULL

bb_poly <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = xlim_m[1], xmax = xlim_m[2],
  ymin = ylim_m[1], ymax = ylim_m[2]
), crs = sf::st_crs(crs_m)))

if (file.exists(tri_csv)) {

  tri_raw <- suppressWarnings(readr::read_csv(tri_csv, show_col_types = FALSE))
  nm <- names(tri_raw)

  col_name <- dplyr::coalesce(
    nm[match("TRI Facility Name", nm)],
    nm[match("TRI.Facility.Name", nm)],
    nm[str_detect(nm, regex("facility.*name", ignore_case = TRUE))][1]
  )
  col_id <- dplyr::coalesce(
    nm[match("TRI Facility ID", nm)],
    nm[match("TRI.Facility.ID", nm)],
    nm[str_detect(nm, regex("facility.*id", ignore_case = TRUE))][1]
  )
  col_lat <- dplyr::coalesce(
    nm[match("Latitude", nm)],
    nm[str_detect(nm, regex("^lat", ignore_case = TRUE))][1]
  )
  col_lon <- dplyr::coalesce(
    nm[match("Longitude", nm)],
    nm[str_detect(nm, regex("^lon", ignore_case = TRUE))][1]
  )

  if (all(!is.na(c(col_lat, col_lon)))) {

    tri_sf <- tri_raw %>%
      dplyr::mutate(
        Latitude  = suppressWarnings(as.numeric(.data[[col_lat]])),
        Longitude = suppressWarnings(as.numeric(.data[[col_lon]])),
        TRI_Facility_Name = if (!is.na(col_name)) as.character(.data[[col_name]]) else NA_character_,
        TRI_Facility_ID   = if (!is.na(col_id))   as.character(.data[[col_id]])   else NA_character_
      ) %>%
      dplyr::filter(is.finite(.data$Latitude), is.finite(.data$Longitude)) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
      sf::st_transform(crs_m)

    tri_sf <- tri_sf[sf::st_intersects(tri_sf, bb_poly, sparse = FALSE), ]
    if (nrow(tri_sf) == 0) tri_sf <- NULL
  }
}

matched_tri_sf <- NULL
if (!is.null(tri_sf) && "TRI_Facility_Name" %in% names(tri_sf)) {
  matched_tri_sf <- lapply(seq_len(nrow(key_facilities)), function(i) {
    if (key_facilities$facility_type[i] != "tri_match") return(NULL)

    pat <- key_facilities$tri_name_pattern[i]
    if (is.na(pat)) return(NULL)

    hit <- tri_sf %>%
      dplyr::filter(!is.na(.data$TRI_Facility_Name)) %>%
      dplyr::filter(stringr::str_to_upper(.data$TRI_Facility_Name) ==
                      stringr::str_to_upper(pat))

    if (nrow(hit) > 0) {
      hit[1, ] %>%
        dplyr::mutate(
          key_label   = key_facilities$label[i],
          point_fill  = key_facilities$point_fill[i],
          point_color = key_facilities$point_color[i],
          text_color  = key_facilities$text_color[i],
          point_size  = key_facilities$point_size[i],
          nudge_x     = key_facilities$nudge_x[i],
          nudge_y     = key_facilities$nudge_y[i]
        )
    } else {
      NULL
    }
  }) %>% dplyr::bind_rows()
}

matched_labels <- if (!is.null(matched_tri_sf) && nrow(matched_tri_sf) > 0) {
  unique(matched_tri_sf$key_label)
} else {
  character(0)
}

fallback_sf <- key_facilities %>%
  dplyr::filter(
    facility_type == "manual" | !label %in% matched_labels
  ) %>%
  dplyr::mutate(
    Latitude  = .data$lat,
    Longitude = .data$lon
  ) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
  sf::st_transform(crs_m) %>%
  dplyr::mutate(
    key_label   = .data$label,
    point_fill  = .data$point_fill,
    point_color = .data$point_color,
    text_color  = .data$text_color,
    point_size  = .data$point_size,
    nudge_x     = .data$nudge_x,
    nudge_y     = .data$nudge_y
  )

fallback_sf <- fallback_sf[sf::st_intersects(fallback_sf, bb_poly, sparse = FALSE), ]

if (!is.null(matched_tri_sf) && nrow(matched_tri_sf) > 0) {
  key_sf <- dplyr::bind_rows(matched_tri_sf, fallback_sf)
} else if (nrow(fallback_sf) > 0) {
  key_sf <- fallback_sf
} else {
  key_sf <- NULL
}

# ----------------------------
# 3) Helpers
# ----------------------------
bearing_to_dxdy <- function(bearing_deg, L) {
  b <- bearing_deg * pi / 180
  dx <- sin(b) * L
  dy <- cos(b) * L
  list(dx = dx, dy = dy)
}

make_template <- function(xlim, ylim, res_m, crs_obj) {
  terra::rast(
    xmin = xlim[1], xmax = xlim[2],
    ymin = ylim[1], ymax = ylim[2],
    resolution = res_m,
    crs = crs_obj
  )
}

gaussian_kernel <- function(sigma_m, res_m, radius_sigma = 3) {
  sigma_cells <- sigma_m / res_m
  rad_cells <- max(1, ceiling(radius_sigma * sigma_cells))
  xs <- seq(-rad_cells, rad_cells)
  ys <- seq(-rad_cells, rad_cells)
  K <- outer(xs, ys, function(i, j) exp(-0.5 * ((i^2 + j^2) / (sigma_cells^2))))
  K <- K / sum(K)
  K
}

make_sourceprob_surface <- function(pol_col, pol_key) {

  pol_vals <- suppressWarnings(as.numeric(df[[pol_col]]))
  pol_vals <- pol_vals[is.finite(pol_vals)]
  if (length(pol_vals) < 50) {
    message("[Skip] ", pol_col, ": too few finite values.")
    return(NULL)
  }

  p99 <- as.numeric(stats::quantile(pol_vals, 0.99, na.rm = TRUE))

  use_sf <- df_sf_m %>%
    dplyr::mutate(pol = suppressWarnings(as.numeric(.data[[pol_col]]))) %>%
    dplyr::filter(is.finite(.data$pol), .data$pol >= p99)

  if (nrow(use_sf) < 10) {
    message("[Skip] ", pol_col, ": <10 exceedances above p99.")
    return(NULL)
  }

  use_sf <- use_sf %>%
    dplyr::mutate(w = pmin(.data$pol / p99, 5))

  xy <- sf::st_coordinates(use_sf)
  x0 <- xy[, 1]
  y0 <- xy[, 2]

  steps <- seq(0, ray_len_m, by = ray_step_m)
  n_steps <- length(steps)
  n_rays <- nrow(use_sf)

  bearing <- use_sf$wd
  dd <- bearing_to_dxdy(bearing, 1)
  ux <- dd$dx
  uy <- dd$dy

  X <- rep(x0, each = n_steps) + rep(ux, each = n_steps) * rep(steps, times = n_rays)
  Y <- rep(y0, each = n_steps) + rep(uy, each = n_steps) * rep(steps, times = n_rays)

  dist_decay <- exp(-rep(steps, times = n_rays) / 12000)
  W <- rep(use_sf$w, each = n_steps) * dist_decay

  pts <- data.frame(x = X, y = Y, w = W)
  pts <- pts[is.finite(pts$x) & is.finite(pts$y) & is.finite(pts$w), ]
  if (nrow(pts) == 0) return(NULL)

  r0 <- make_template(xlim_m, ylim_m, grid_res_m, sf::st_crs(crs_m)$wkt)
  v  <- terra::vect(pts, geom = c("x", "y"), crs = sf::st_crs(crs_m)$wkt)

  r_sum <- terra::rasterize(v, r0, field = "w", fun = "sum", background = 0)

  K <- gaussian_kernel(sigma_m, grid_res_m, kernel_radius)
  r_sm <- terra::focal(r_sum, w = K, fun = "sum", na.policy = "omit", fillvalue = 0)

  mx <- terra::global(r_sm, fun = "max", na.rm = TRUE)[1, 1]
  if (!is.finite(mx) || mx <= 0) return(NULL)
  r_prob <- r_sm / mx

  df_r <- as.data.frame(r_prob, xy = TRUE, na.rm = FALSE)
  names(df_r) <- c("x", "y", "prob")

  list(
    pol_col = pol_col,
    pol_key = pol_key,
    p99 = p99,
    n_exc = nrow(use_sf),
    r_df = df_r
  )
}

plot_sourceprob <- function(obj) {

  df_r <- obj$r_df

  q <- stats::quantile(df_r$prob[df_r$prob > 0], probs = c(0.02, 0.98), na.rm = TRUE)
  lims <- as.numeric(q)
  if (!all(is.finite(lims)) || lims[1] == lims[2]) {
    lims <- c(0, max(df_r$prob, na.rm = TRUE))
  }

  title_txt <- paste0(gsub("_ppb", "", obj$pol_col), " source probability")
  subtitle_txt <- paste0(
    "Upwind projection of p99 events | p99=", signif(obj$p99, 4),
    " | n exceed=", obj$n_exc,
    " | rays=", ray_len_m / 1000, " km | smooth σ=", sigma_m, " m"
  )

  p <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +

    ggplot2::geom_raster(
      data = df_r,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$prob),
      alpha = surface_alpha
    ) +

    { if (!is.null(tri_sf)) ggplot2::geom_sf(
        data = tri_sf,
        shape = 21, size = 1.9, stroke = 0.2,
        fill = "white", color = "black", alpha = tri_alpha
      ) else NULL } +

    { if (!is.null(key_sf)) ggplot2::geom_sf(
        data = key_sf,
        ggplot2::aes(size = .data$point_size),
        shape = 21, stroke = 0.7,
        fill = key_sf$point_fill,
        color = key_sf$point_color,
        alpha = 0.98,
        show.legend = FALSE
      ) else NULL } +

    { if (!is.null(key_sf) && nrow(key_sf) > 0) {
        label_layers <- lapply(seq_len(nrow(key_sf)), function(i) {
          ggplot2::geom_sf_text(
            data = key_sf[i, ],
            ggplot2::aes(label = .data$key_label),
            color = key_sf$text_color[i],
            size = 3.2,
            fontface = "bold",
            nudge_x = key_sf$nudge_x[i],
            nudge_y = key_sf$nudge_y[i],
            check_overlap = FALSE
          )
        })
        label_layers
      } else NULL } +

    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      limits = lims,
      oob = scales::squish,
      name = "Relative\nprobability"
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(crs_m),
      xlim = xlim_m, ylim = ylim_m,
      expand = FALSE
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL, y = NULL
    )

  p
}

# ----------------------------
# 4) Run all pollutants -> save maps
# ----------------------------
plots <- list()

for (pol_col in names(pollutants)) {
  pol_key <- pollutants[[pol_col]]
  message("\n--- ", pol_col, " ---")

  obj <- make_sourceprob_surface(pol_col, pol_key)
  if (is.null(obj)) next

  p <- plot_sourceprob(obj)

  out_png <- file.path(out_dir, paste0("sourceprob_", pol_key, "_p99_weighted_smoothed_TRI_KEY_CONTEXT_refuel_adjusted.png"))
  ggplot2::ggsave(out_png, plot = p, width = 8.7, height = 7.5, units = "in", dpi = dpi_out, bg = "white")
  message("[Saved] ", out_png)

  plots[[pol_key]] <- p
}

plots_keep <- Filter(Negate(is.null), plots)
stopifnot(length(plots_keep) > 0)

# ----------------------------
# 5) Combined panel
# ----------------------------
order_keys <- c("benzene", "toluene", "trimethylbenzene", "xylene", "h2s", "hcn")
plots_ordered <- plots_keep[intersect(order_keys, names(plots_keep))]

panel <- patchwork::wrap_plots(plots_ordered, ncol = 2) +
  patchwork::plot_annotation(
    title = "Weighted, smoothed source probability maps (upwind rays from p99 events) with TRI overlay and contextual source locations",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
  )

out_panel <- file.path(out_dir, "FIG_sourceprob_6panel_p99_weighted_smoothed_TRI_KEY_CONTEXT_refuel_adjusted.png")
ggplot2::ggsave(out_panel, plot = panel, width = 16, height = 20, units = "in", dpi = dpi_out, bg = "white")
message("\n[Saved] ", out_panel)
message("DONE. Outputs in: ", out_dir)
