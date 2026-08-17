# ==============================================================
# 27  Hotspot sensitivity sensitivity
# Auto-split from Suncor.Rmd  (section 27 of 40)
# ==============================================================

#Hotspot sensitivity sensitivity

# ============================================================
# POWERFUL DIAGNOSTICS for mobile source-probability maps (NO time stratification)
# Implements:
#   (1) Sensitivity checks: ray length + smoothing sigma grid
#   (2) Compare event thresholds: p99 vs p95
#   (3) Effort-normalized surfaces: event / opportunity (all points)
#   (4) Ratio-based discrimination: (Toluene/Benzene, TMB/Benzene) + “fresh vs aged” bins
#
# UPDATED:
#   - Adds WWTF1 (green), WWTF2 (green), Woodshop (purple), and Refuel sites (blue)
#   - Refuel labels shortened to "Refuel"
#   - Top Refuel labels separated to avoid overlap
#   - Keeps subtitle and axis readability fixes
#   - Renamed old WWTP to WWTF1 and added WWTF2
#   - FIXED patchwork theming so no "&" error
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
  library(tidyr)
  library(tibble)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
in_rdata <- "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData"
tri_csv  <- "/Users/priyanka/Downloads/Suncor/TRI.csv"
out_dir  <- "/Users/priyanka/Downloads/Suncor/sourceprob_diagnostics"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Basemap
tile_type <- "cartolight"
tile_zoom <- 11
dpi_out   <- 450
surface_alpha <- 0.75

# Projections
crs_ll <- 4326
crs_m  <- 32613 # UTM 13N

# Wind QC
min_ws <- 1.0

# Ray discretization
ray_step_m  <- 150

# (1) Sensitivity grids
ray_len_grid_m <- c(5000, 10000, 15000, 20000)
sigma_grid_m   <- c(500, 900, 1200, 1800)

# Grid res
grid_res_m <- 250

# Kernel radius
kernel_radius <- 3

# (2) Event thresholds
event_probs <- c(p95 = 0.95, p99 = 0.99)

# (3) Distance decay
use_distance_decay <- TRUE
decay_scale_m <- 12000

# (4) Ratio diagnostics
ratio_defs <- list(
  TB  = list(num = "Toluene_ppb",          den = "Benzene_ppb", name = "Toluene/Benzene"),
  TMB = list(num = "Trimethylbenzene_ppb", den = "Benzene_ppb", name = "Trimethylbenzene/Benzene")
)
ratio_fresh_q <- 0.80
ratio_aged_q  <- 0.20

pollutants <- c(
  Benzene_ppb            = "benzene",
  Toluene_ppb            = "toluene",
  Trimethylbenzene_ppb   = "trimethylbenzene",
  Xylene_ppb             = "xylene",
  Hydrogen_Sulfide_ppb   = "h2s",
  Hydrogen_Cyanide_ppb   = "hcn"
)

# ----------------------------
# Context points / facilities
# ----------------------------
context_points <- tibble::tibble(
  key = c(
    "Suncor", "Sinclair", "Phillips 66", "WWTF1", "WWTF2", "Woodshop",
    "Refuel", "Refuel", "Refuel", "Refuel"
  ),
  Latitude = c(
    39.803333,
    39.8724,
    39.79668,
    39.80822838231637,   # WWTF1 (old WWTP)
    39.87304794998779,   # WWTF2
    39.791382444842746,
    39.79935581470166,
    39.783338577716854,
    39.886063120868805,
    39.88659578717162
  ),
  Longitude = c(
    -104.945556,
    -104.8861,
    -104.94236,
    -104.9553246877205,   # WWTF1
    -104.91204700295945,  # WWTF2
    -104.94754520948433,
    -104.88376424570843,
    -105.10918240337975,
    -104.84531380337543,
    -104.88371420337545
  ),
  point_fill  = c(
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
    3.2, 3.2, 3.2,
    3.8, 3.8,
    3.2,
    3.0, 3.0, 3.0, 3.0
  ),
  nudge_x = c(
     350,   250,  -350,
    -850,   700,
    -900,
    -350,  -1100,   900,   250
  ),
  nudge_y = c(
     650,   650,  -650,
     850,   850,
    -900,
    -450,  -850,   750,  1100
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

req_cols <- c("Longitude", "Latitude", "ws", "wd")
missing_req <- setdiff(req_cols, names(df))
if (length(missing_req) > 0) stop("Missing required columns in mobile df: ", paste(missing_req, collapse = ", "))

pollutants <- pollutants[names(pollutants) %in% names(df)]
stopifnot(length(pollutants) > 0)

# ----------------------------
# 1) QC + SF (meters) + bbox
# ----------------------------
df <- df %>%
  mutate(
    Longitude = suppressWarnings(as.numeric(.data$Longitude)),
    Latitude  = suppressWarnings(as.numeric(.data$Latitude)),
    ws        = suppressWarnings(as.numeric(.data$ws)),
    wd        = suppressWarnings(as.numeric(.data$wd))
  ) %>%
  filter(
    is.finite(.data$Longitude), is.finite(.data$Latitude),
    is.finite(.data$ws), .data$ws >= min_ws,
    is.finite(.data$wd), .data$wd >= 0, .data$wd <= 360
  )
if (nrow(df) == 0) stop("After QC filtering, df has 0 rows.")

df_sf_m <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
  st_transform(crs_m)

bb_m <- st_bbox(df_sf_m)
xlim_m <- c(as.numeric(bb_m["xmin"]), as.numeric(bb_m["xmax"]))
ylim_m <- c(as.numeric(bb_m["ymin"]), as.numeric(bb_m["ymax"]))
pad <- 1500
xlim_m <- xlim_m + c(-pad, pad)
ylim_m <- ylim_m + c(-pad, pad)

# ----------------------------
# TRI overlay + context points
# ----------------------------
tri_sf <- NULL
if (file.exists(tri_csv)) {
  tri_raw <- suppressWarnings(readr::read_csv(tri_csv, show_col_types = FALSE))
  if (all(c("Latitude", "Longitude") %in% names(tri_raw))) {
    tri_sf <- tri_raw %>%
      mutate(
        Latitude  = suppressWarnings(as.numeric(.data$Latitude)),
        Longitude = suppressWarnings(as.numeric(.data$Longitude))
      ) %>%
      filter(is.finite(.data$Latitude), is.finite(.data$Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
      st_transform(crs_m)

    bb_poly <- st_as_sfc(st_bbox(c(
      xmin = xlim_m[1], xmax = xlim_m[2],
      ymin = ylim_m[1], ymax = ylim_m[2]
    ), crs = st_crs(crs_m)))

    tri_sf <- tri_sf[st_intersects(tri_sf, bb_poly, sparse = FALSE), ]
    if (nrow(tri_sf) == 0) tri_sf <- NULL
  }
}

key_sf <- context_points %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
  st_transform(crs_m)

# ----------------------------
# Helpers
# ----------------------------
bearing_to_dxdy <- function(bearing_deg, L) {
  b <- bearing_deg * pi / 180
  list(dx = sin(b) * L, dy = cos(b) * L)
}

make_template <- function(xlim, ylim, res_m, crs_wkt) {
  terra::rast(
    xmin = xlim[1], xmax = xlim[2],
    ymin = ylim[1], ymax = ylim[2],
    resolution = res_m,
    crs = crs_wkt
  )
}

gaussian_kernel <- function(sigma_m, res_m, radius_sigma = 3) {
  sigma_cells <- sigma_m / res_m
  rad_cells <- max(1, ceiling(radius_sigma * sigma_cells))
  xs <- seq(-rad_cells, rad_cells)
  ys <- seq(-rad_cells, rad_cells)
  K <- outer(xs, ys, function(i, j) exp(-0.5 * ((i^2 + j^2) / (sigma_cells^2))))
  K / sum(K)
}

build_ray_surface <- function(receptors_sf_m,
                              weights,
                              ray_len_m,
                              sigma_m,
                              grid_res_m,
                              ray_step_m,
                              xlim_m, ylim_m,
                              crs_wkt,
                              use_distance_decay = TRUE,
                              decay_scale_m = 12000) {

  stopifnot(nrow(receptors_sf_m) == length(weights))

  xy <- st_coordinates(receptors_sf_m)
  x0 <- xy[, 1]; y0 <- xy[, 2]
  bearing <- receptors_sf_m$wd

  steps <- seq(0, ray_len_m, by = ray_step_m)
  n_steps <- length(steps)
  n_rays  <- nrow(receptors_sf_m)

  uv <- bearing_to_dxdy(bearing, 1)
  ux <- uv$dx; uy <- uv$dy

  X <- rep(x0, each = n_steps) + rep(ux, each = n_steps) * rep(steps, times = n_rays)
  Y <- rep(y0, each = n_steps) + rep(uy, each = n_steps) * rep(steps, times = n_rays)

  dist_decay <- if (use_distance_decay) exp(-rep(steps, times = n_rays) / decay_scale_m) else rep(1, n_steps * n_rays)
  W <- rep(weights, each = n_steps) * dist_decay

  pts <- data.frame(x = X, y = Y, w = W)
  pts <- pts[is.finite(pts$x) & is.finite(pts$y) & is.finite(pts$w), ]
  if (nrow(pts) == 0) return(NULL)

  r0 <- make_template(xlim_m, ylim_m, grid_res_m, crs_wkt)
  v  <- terra::vect(pts, geom = c("x", "y"), crs = crs_wkt)

  r_sum <- terra::rasterize(v, r0, field = "w", fun = "sum", background = 0)

  K <- gaussian_kernel(sigma_m, grid_res_m, kernel_radius)
  r_sm <- terra::focal(r_sum, w = K, fun = "sum", na.policy = "omit", fillvalue = 0)

  mx <- terra::global(r_sm, fun = "max", na.rm = TRUE)[1, 1]
  if (!is.finite(mx) || mx <= 0) return(NULL)

  r_prob <- r_sm / mx
  df_r <- as.data.frame(r_prob, xy = TRUE, na.rm = FALSE)
  names(df_r) <- c("x", "y", "prob")
  df_r
}

plot_surface <- function(df_r, title, subtitle,
                         tri_sf = NULL, key_sf = NULL,
                         show_receptors_sf = NULL,
                         alpha_surface = 0.75,
                         title_size = 12,
                         subtitle_size = 9) {

  q <- stats::quantile(df_r$prob[df_r$prob > 0], probs = c(0.02, 0.98), na.rm = TRUE)
  lims <- as.numeric(q)
  if (!all(is.finite(lims)) || lims[1] == lims[2]) lims <- c(0, max(df_r$prob, na.rm = TRUE))

  p <- ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
    geom_raster(
      data = df_r,
      aes(x = .data$x, y = .data$y, fill = .data$prob),
      alpha = alpha_surface
    ) +
    { if (!is.null(show_receptors_sf)) geom_sf(
        data = show_receptors_sf, color = "white", fill = "white",
        alpha = 0.55, size = 1.2
      ) else NULL } +
    { if (!is.null(tri_sf)) geom_sf(
        data = tri_sf, shape = 21, size = 2.0, stroke = 0.25,
        fill = "white", color = "black", alpha = 0.45
      ) else NULL } +
    { if (!is.null(key_sf)) geom_sf(
        data = key_sf,
        aes(size = .data$point_size),
        shape = 21, stroke = 0.5,
        fill = key_sf$point_fill,
        color = key_sf$point_color,
        alpha = 0.95,
        show.legend = FALSE
      ) else NULL } +
    { if (!is.null(key_sf) && nrow(key_sf) > 0) {
        label_layers <- lapply(seq_len(nrow(key_sf)), function(i) {
          geom_sf_text(
            data = key_sf[i, ],
            aes(label = .data$key),
            color = key_sf$text_color[i],
            size = 3.6,
            fontface = "bold",
            nudge_x = key_sf$nudge_x[i],
            nudge_y = key_sf$nudge_y[i],
            check_overlap = FALSE
          )
        })
        label_layers
      } else NULL } +
    scale_size_identity() +
    scale_fill_viridis_c(
      option = "C",
      limits = lims,
      oob = scales::squish,
      name = "Relative\nprobability"
    ) +
    coord_sf(crs = st_crs(crs_m), xlim = xlim_m, ylim = ylim_m, expand = FALSE) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = title_size, margin = margin(b = 4)),
      plot.subtitle = element_text(size = subtitle_size, lineheight = 0.95, margin = margin(b = 6)),
      plot.margin = margin(t = 10, r = 6, b = 6, l = 6, unit = "pt")
    ) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL)

  p
}

save_plot <- function(p, filename, w = 8.5, h = 7.3, dpi = dpi_out) {
  ggsave(filename, plot = p, width = w, height = h, units = "in", dpi = dpi, bg = "white")
  message("[Saved] ", filename)
}

crs_wkt <- st_crs(crs_m)$wkt

# ============================================================
# (2) EVENT THRESHOLDS: p99 vs p95
# ============================================================
message("\n========== (2) Event thresholds: p95 vs p99 ==========")

plots_thresh <- list()
log_thresh <- list()

for (pol_col in names(pollutants)) {

  pol_key <- pollutants[[pol_col]]
  vals <- suppressWarnings(as.numeric(df[[pol_col]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) < 50) next

  for (nm in names(event_probs)) {

    p_event <- event_probs[[nm]]
    thr <- as.numeric(stats::quantile(vals, p_event, na.rm = TRUE))

    use_sf <- df_sf_m %>%
      mutate(pol = suppressWarnings(as.numeric(.data[[pol_col]]))) %>%
      filter(is.finite(.data$pol), .data$pol >= thr)

    if (nrow(use_sf) < 10) next
    w <- pmin(use_sf$pol / thr, 5)

    ray_len_m <- 15000
    sigma_m   <- 900

    df_r <- build_ray_surface(
      use_sf, w,
      ray_len_m = ray_len_m,
      sigma_m = sigma_m,
      grid_res_m = grid_res_m,
      ray_step_m = ray_step_m,
      xlim_m = xlim_m, ylim_m = ylim_m,
      crs_wkt = crs_wkt,
      use_distance_decay = use_distance_decay,
      decay_scale_m = decay_scale_m
    )
    if (is.null(df_r)) next

    title <- paste0(gsub("_ppb", "", pol_col), " source probability (", nm, " events)")
    subtitle <- paste0("Events ≥", nm, " (thr=", signif(thr, 4), " ppb); n=", nrow(use_sf),
                       "; ray=", ray_len_m / 1000, " km; σ=", sigma_m, " m")

    p <- plot_surface(df_r, title, subtitle, tri_sf = tri_sf, key_sf = key_sf,
                      alpha_surface = surface_alpha, subtitle_size = 9)

    out_png <- file.path(out_dir, paste0("A_thresh_", pol_key, "_", nm, "_ray15km_sigma900.png"))
    save_plot(p, out_png)

    plots_thresh[[paste(pol_key, nm, sep = "_")]] <- p
    log_thresh[[paste(pol_key, nm, sep = "_")]] <- data.frame(
      pollutant = pol_key, pollutant_col = pol_col,
      event = nm, prob = p_event, threshold_ppb = thr,
      n_events = nrow(use_sf),
      ray_len_m = ray_len_m, sigma_m = sigma_m,
      stringsAsFactors = FALSE
    )
  }
}
readr::write_csv(bind_rows(log_thresh), file.path(out_dir, "A_thresh_log.csv"))

# ============================================================
# (1) SENSITIVITY GRID: ray length x sigma (B panel)
# ============================================================
message("\n========== (1) Sensitivity grid: ray length x sigma ==========")

pol_focus <- "Benzene_ppb"
stopifnot(pol_focus %in% names(df))

vals <- suppressWarnings(as.numeric(df[[pol_focus]]))
vals <- vals[is.finite(vals)]
thr99 <- as.numeric(stats::quantile(vals, 0.99, na.rm = TRUE))

use_sf <- df_sf_m %>%
  mutate(pol = suppressWarnings(as.numeric(.data[[pol_focus]]))) %>%
  filter(is.finite(.data$pol), .data$pol >= thr99)

stopifnot(nrow(use_sf) >= 10)
w <- pmin(use_sf$pol / thr99, 5)

sens_plots <- list()
sens_log <- list()

for (ray_len_m in ray_len_grid_m) {
  for (sigma_m in sigma_grid_m) {

    df_r <- build_ray_surface(
      use_sf, w,
      ray_len_m = ray_len_m,
      sigma_m = sigma_m,
      grid_res_m = grid_res_m,
      ray_step_m = ray_step_m,
      xlim_m = xlim_m, ylim_m = ylim_m,
      crs_wkt = crs_wkt,
      use_distance_decay = use_distance_decay,
      decay_scale_m = decay_scale_m
    )
    if (is.null(df_r)) next

    title <- paste0(gsub("_ppb", "", pol_focus), " sensitivity (p99 events)")
    subtitle <- paste0("ray=", ray_len_m / 1000, " km; σ=", sigma_m, " m; n=", nrow(use_sf),
                       "; thr(p99)=", signif(thr99, 4), " ppb")

    p <- plot_surface(df_r, title, subtitle, tri_sf = tri_sf, key_sf = key_sf,
                      alpha_surface = surface_alpha, subtitle_size = 9)

    key <- paste0("ray", ray_len_m / 1000, "km_sigma", sigma_m)
    out_png <- file.path(out_dir, paste0("B_sensitivity_", key, "_", tolower(gsub("_ppb", "", pol_focus)), ".png"))
    save_plot(p, out_png, w = 7.8, h = 6.8)

    sens_plots[[key]] <- p
    sens_log[[key]] <- data.frame(
      pollutant_col = pol_focus,
      thr99 = thr99,
      n_events = nrow(use_sf),
      ray_len_m = ray_len_m,
      sigma_m = sigma_m,
      grid_res_m = grid_res_m,
      ray_step_m = ray_step_m,
      stringsAsFactors = FALSE
    )
  }
}
readr::write_csv(bind_rows(sens_log), file.path(out_dir, "B_sensitivity_log.csv"))

sens_plots_fixed <- lapply(sens_plots, function(p) {
  p + theme(
    axis.text.x  = element_text(size = 13, angle = 45, hjust = 1, vjust = 1, color = "black"),
    axis.text.y  = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.margin  = margin(t = 8, r = 8, b = 34, l = 8, unit = "pt")
  )
})

sens_panel_fixed <- patchwork::wrap_plots(sens_plots_fixed, ncol = length(sigma_grid_m)) +
  patchwork::plot_annotation(
    title = paste0("Sensitivity: ", gsub("_ppb", "", pol_focus), " (p99) — ray length × smoothing σ"),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

out_panel <- file.path(out_dir, paste0("B_sensitivity_PANEL_", tolower(gsub("_ppb", "", pol_focus)), ".png"))
ggsave(
  out_panel,
  plot   = sens_panel_fixed,
  width  = 30,
  height = 18,
  units  = "in",
  dpi    = dpi_out,
  bg     = "white",
  limitsize = FALSE
)
message("[Saved] ", out_panel)

# ============================================================
# (3) EFFORT-NORMALIZED: Event surface / Opportunity surface (C panel)
# ============================================================
message("\n========== (3) Effort-normalized surfaces: event / opportunity ==========")

opp_sf <- df_sf_m
opp_w  <- rep(1, nrow(opp_sf))

ray_len_m <- 15000
sigma_m   <- 900

opp_df_r <- build_ray_surface(
  opp_sf, opp_w,
  ray_len_m = ray_len_m,
  sigma_m = sigma_m,
  grid_res_m = grid_res_m,
  ray_step_m = ray_step_m,
  xlim_m = xlim_m, ylim_m = ylim_m,
  crs_wkt = crs_wkt,
  use_distance_decay = use_distance_decay,
  decay_scale_m = decay_scale_m
)
stopifnot(!is.null(opp_df_r))

opp_tbl <- opp_df_r %>% rename(opp = prob)

plots_norm <- list()
log_norm <- list()

for (pol_col in names(pollutants)) {

  pol_key <- pollutants[[pol_col]]
  vals <- suppressWarnings(as.numeric(df[[pol_col]]))
  vals <- vals[is.finite(vals)]
  if (length(vals) < 50) next

  thr <- as.numeric(stats::quantile(vals, 0.99, na.rm = TRUE))

  ev_sf <- df_sf_m %>%
    mutate(pol = suppressWarnings(as.numeric(.data[[pol_col]]))) %>%
    filter(is.finite(.data$pol), .data$pol >= thr)

  if (nrow(ev_sf) < 10) next
  w <- pmin(ev_sf$pol / thr, 5)

  ev_df_r <- build_ray_surface(
    ev_sf, w,
    ray_len_m = ray_len_m,
    sigma_m = sigma_m,
    grid_res_m = grid_res_m,
    ray_step_m = ray_step_m,
    xlim_m = xlim_m, ylim_m = ylim_m,
    crs_wkt = crs_wkt,
    use_distance_decay = use_distance_decay,
    decay_scale_m = decay_scale_m
  )
  if (is.null(ev_df_r)) next

  ev_tbl <- ev_df_r %>% rename(ev = prob)

  norm_tbl <- left_join(ev_tbl, opp_tbl, by = c("x", "y")) %>%
    mutate(
      opp = if_else(is.na(.data$opp), 0, .data$opp),
      ratio = if_else(.data$opp > 0, .data$ev / .data$opp, NA_real_)
    )

  rmax <- stats::quantile(norm_tbl$ratio, 0.99, na.rm = TRUE)
  if (!is.finite(rmax) || rmax <= 0) next
  norm_tbl <- norm_tbl %>% mutate(prob = pmin(.data$ratio / rmax, 1))

  title <- paste0(gsub("_ppb", "", pol_col), " effort-normalized source probability")
  subtitle <- paste0("Event/Opportunity; res=", grid_res_m, " m; ray=", ray_len_m / 1000,
                     " km; σ=", sigma_m, " m; p99 thr=", signif(thr, 4), " ppb")

  p <- plot_surface(norm_tbl %>% select(x, y, prob), title, subtitle,
                    tri_sf = tri_sf, key_sf = key_sf,
                    alpha_surface = surface_alpha, subtitle_size = 9)

  out_png <- file.path(out_dir, paste0("C_effortnorm_", pol_key, "_p99_ray15km_sigma900.png"))
  save_plot(p, out_png)

  plots_norm[[pol_key]] <- p
  log_norm[[pol_key]] <- data.frame(
    pollutant = pol_key, pollutant_col = pol_col,
    p99_thr = thr, n_events = nrow(ev_sf),
    ray_len_m = ray_len_m, sigma_m = sigma_m,
    stringsAsFactors = FALSE
  )
}
readr::write_csv(bind_rows(log_norm), file.path(out_dir, "C_effortnorm_log.csv"))

panel_norm <- patchwork::wrap_plots(plots_norm, ncol = 2) +
  patchwork::plot_annotation(
    title = "Effort-normalized source probability (event/opportunity), p99 events",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

out_panel_norm <- file.path(out_dir, "C_effortnorm_PANEL.png")
ggsave(out_panel_norm, plot = panel_norm, width = 16, height = 20, units = "in", dpi = dpi_out, bg = "white")
message("[Saved] ", out_panel_norm)

# ============================================================
# (4) RATIO-BASED DISCRIMINATION (D panel)
# ============================================================
message("\n========== (4) Ratio-based fresh vs aged surfaces ==========")

plots_ratio <- list()
ratio_log <- list()

for (rnm in names(ratio_defs)) {

  rd <- ratio_defs[[rnm]]
  if (!(rd$num %in% names(df)) || !(rd$den %in% names(df))) {
    message("[Skip ratio] missing columns for ", rnm)
    next
  }

  num <- suppressWarnings(as.numeric(df[[rd$num]]))
  den <- suppressWarnings(as.numeric(df[[rd$den]]))

  ratio <- num / den
  ok <- is.finite(ratio) & is.finite(num) & is.finite(den) & den > 0
  if (sum(ok) < 100) {
    message("[Skip ratio] too few valid ratios for ", rnm)
    next
  }

  q_hi <- as.numeric(stats::quantile(ratio[ok], ratio_fresh_q, na.rm = TRUE))
  q_lo <- as.numeric(stats::quantile(ratio[ok], ratio_aged_q,  na.rm = TRUE))

  df_ratio <- df %>%
    mutate(.ratio = suppressWarnings(as.numeric(.data[[rd$num]])) / suppressWarnings(as.numeric(.data[[rd$den]]))) %>%
    filter(is.finite(.data$.ratio),
           is.finite(.data$Longitude), is.finite(.data$Latitude),
           is.finite(.data$ws), .data$ws >= min_ws,
           is.finite(.data$wd), .data$wd >= 0, .data$wd <= 360,
           suppressWarnings(as.numeric(.data[[rd$den]])) > 0)

  df_fresh <- df_ratio %>% filter(.data$.ratio >= q_hi)
  df_aged  <- df_ratio %>% filter(.data$.ratio <= q_lo)

  pol_anchor <- "Benzene_ppb"
  if (!(pol_anchor %in% names(df))) {
    message("[Skip ratio] missing anchor pollutant: ", pol_anchor)
    next
  }

  run_subset <- function(dsub, label) {

    vals <- suppressWarnings(as.numeric(dsub[[pol_anchor]]))
    vals <- vals[is.finite(vals)]
    if (length(vals) < 50) return(NULL)

    thr <- as.numeric(stats::quantile(vals, 0.99, na.rm = TRUE))

    sub_sf <- st_as_sf(dsub, coords = c("Longitude", "Latitude"), crs = crs_ll, remove = FALSE) %>%
      st_transform(crs_m) %>%
      mutate(pol = suppressWarnings(as.numeric(.data[[pol_anchor]]))) %>%
      filter(is.finite(.data$pol), .data$pol >= thr)

    if (nrow(sub_sf) < 10) return(NULL)

    w <- pmin(sub_sf$pol / thr, 5)

    df_r <- build_ray_surface(
      sub_sf, w,
      ray_len_m = 15000,
      sigma_m = 900,
      grid_res_m = grid_res_m,
      ray_step_m = ray_step_m,
      xlim_m = xlim_m, ylim_m = ylim_m,
      crs_wkt = crs_wkt,
      use_distance_decay = use_distance_decay,
      decay_scale_m = decay_scale_m
    )
    if (is.null(df_r)) return(NULL)

    title <- paste0(rd$name, " — ", label, " subset (Benzene p99 events)")
    subtitle <- paste0(
      "Aged ≤", signif(q_lo, 3), "; Fresh ≥", signif(q_hi, 3), "\n",
      "Benz p99 thr=", signif(thr, 4), " ppb; n=", nrow(sub_sf), "; ray=15 km; σ=900 m"
    )

    plot_surface(df_r, title, subtitle,
                 tri_sf = tri_sf, key_sf = key_sf,
                 alpha_surface = surface_alpha,
                 title_size = 11,
                 subtitle_size = 8.2)
  }

  p_fresh <- run_subset(df_fresh, "FRESH (high ratio)")
  p_aged  <- run_subset(df_aged,  "AGED (low ratio)")

  if (!is.null(p_fresh)) {
    out_png <- file.path(out_dir, paste0("D_ratio_", rnm, "_FRESH_benz_p99.png"))
    save_plot(p_fresh, out_png)
    plots_ratio[[paste0(rnm, "_fresh")]] <- p_fresh
  }
  if (!is.null(p_aged)) {
    out_png <- file.path(out_dir, paste0("D_ratio_", rnm, "_AGED_benz_p99.png"))
    save_plot(p_aged, out_png)
    plots_ratio[[paste0(rnm, "_aged")]] <- p_aged
  }

  ratio_log[[rnm]] <- data.frame(
    ratio = rnm,
    ratio_name = rd$name,
    q_fresh = ratio_fresh_q, q_aged = ratio_aged_q,
    q_hi = q_hi, q_lo = q_lo,
    n_valid_ratio = sum(ok),
    stringsAsFactors = FALSE
  )
}

if (length(ratio_log) > 0) {
  readr::write_csv(bind_rows(ratio_log), file.path(out_dir, "D_ratio_log.csv"))
}

if (length(plots_ratio) > 0) {
  plots_ratio_fixed <- lapply(plots_ratio, function(p) {
    p + theme(plot.margin = margin(t = 12, r = 8, b = 8, l = 8, unit = "pt"))
  })

  panel_ratio_fixed <- patchwork::wrap_plots(plots_ratio_fixed, ncol = 2) +
    patchwork::plot_annotation(
      title = "Ratio-based source probability (fresh vs aged subsets; Benzene p99 events)",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )

  out_panel_ratio <- file.path(out_dir, "D_ratio_PANEL.png")
  ggsave(out_panel_ratio, plot = panel_ratio_fixed,
         width = 18, height = 16, units = "in", dpi = dpi_out, bg = "white")
  message("[Saved] ", out_panel_ratio)
}

message("\nDONE. All diagnostics written to: ", out_dir)


# Wind rotated Facility plumes (All events)

# ============================================================
# FACILITY ATTRIBUTION DIAGNOSTICS (Steps 1–6) — per pollutant
# - Standalone: reads mobile_wswd.RData (expects object out OR df)
# - Builds ONE facility × mobile-point table with distance + wind alignment
# - Runs Steps 1–6 for EACH pollutant:
#     1) Define downwind (alignment<thresh)
#     2) Restrict by distance (max_dist_m)
#     3) Summarize upwind vs downwind medians
#     4) Plot alignment vs concentration (log scale)
#     5) Plot distance decay (downwind only; log-log)
#     6) Regression: log(conc+eps) ~ facility + downwind + log(distance) + facility:downwind
# - Saves: per-pollutant summary CSV + 2 PNGs + model coef CSV
# - NO time stratification (uses all points)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(geosphere)  # distGeo(), bearing()
  library(ggplot2)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
in_rdata <- "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData"
out_dir  <- "/Users/priyanka/Downloads/Suncor/facility_attribution"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Facilities to test
facilities <- data.frame(
  name = c("Suncor","Sinclair","Phillips66"),
  lat  = c(39.803333, 39.8724, 39.79668),
  lon  = c(-104.945556, -104.8861, -104.94236),
  stringsAsFactors = FALSE
)

# Pollutants to run (columns in df). Keeps only those that exist.
pollutants <- c(
  Benzene_ppb            = "benzene",
  Toluene_ppb            = "toluene",
  Trimethylbenzene_ppb   = "trimethylbenzene",
  Xylene_ppb             = "xylene",
  Hydrogen_Sulfide_ppb   = "h2s",
  Hydrogen_Cyanide_ppb   = "hcn"
)

# Diagnostics knobs
downwind_thresh_deg <- 30     # Step 1
max_dist_m          <- 5000   # Step 2 (recommended <= 5 km)
min_ws              <- 0.5    # optional wind QC (set 0 to keep all)
plot_sample_n       <- 200000 # for scatter plots ONLY (keeps compute on full data)
eps_log             <- 0.01   # added before log() in plots/models

# ----------------------------
# 0) Read mobile data (standalone)
# ----------------------------
stopifnot(file.exists(in_rdata))
load(in_rdata)

if (exists("out")) {
  df0 <- out
  rm(out)
} else if (exists("df")) {
  df0 <- df
} else {
  stop("mobile_wswd.RData must contain an object named 'out' or 'df'.")
}

# ----------------------------
# 1) Build ONE filtered mobile dataset (used everywhere)
# ----------------------------
req_base <- c("Longitude","Latitude","ws","wd")
stopifnot(all(req_base %in% names(df0)))

# keep only pollutants that exist
pollutants <- pollutants[names(pollutants) %in% names(df0)]
stopifnot(length(pollutants) > 0)

mobile_df <- df0 %>%
  dplyr::mutate(
    Longitude = suppressWarnings(as.numeric(.data$Longitude)),
    Latitude  = suppressWarnings(as.numeric(.data$Latitude)),
    ws        = suppressWarnings(as.numeric(.data$ws)),
    wd        = suppressWarnings(as.numeric(.data$wd))
  )

# add pollutant numeric columns
for (pc in names(pollutants)) {
  mobile_df[[pc]] <- suppressWarnings(as.numeric(mobile_df[[pc]]))
}

# QC: coords + wind
mobile_df <- mobile_df %>%
  dplyr::filter(
    is.finite(.data$Longitude), is.finite(.data$Latitude),
    is.finite(.data$wd), .data$wd >= 0, .data$wd <= 360,
    is.finite(.data$ws),
    .data$ws >= min_ws
  )

stopifnot(nrow(mobile_df) > 0)

receptors_ll <- as.matrix(mobile_df[, c("Longitude","Latitude")])

# ----------------------------
# Helpers
# ----------------------------
angle_diff_deg <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

compute_facility_metrics <- function(fac_row, mobile_df, receptors_ll) {

  fac_ll <- c(fac_row$lon, fac_row$lat)

  # distance (m)
  d_m <- geosphere::distGeo(receptors_ll, fac_ll)

  # bearing FROM facility TO receptor (0=N, clockwise)
  b_deg <- geosphere::bearing(fac_ll, receptors_ll)
  b_deg <- (b_deg + 360) %% 360

  # wind direction is meteorological FROM (deg)
  align_deg <- angle_diff_deg(mobile_df$wd, b_deg)

  # base table (pollutants appended later by left join in memory via cbind)
  out <- data.frame(
    facility = fac_row$name,
    fac_lon  = fac_row$lon,
    fac_lat  = fac_row$lat,
    lon      = mobile_df$Longitude,
    lat      = mobile_df$Latitude,
    ws       = mobile_df$ws,
    wd       = mobile_df$wd,
    distance_m          = d_m,
    bearing_fac2rec_deg = b_deg,
    wind_alignment_deg  = align_deg,
    stringsAsFactors = FALSE
  )

  # append pollutants (same row count; safe)
  out <- cbind(out, as.data.frame(mobile_df)[, names(pollutants), drop = FALSE])
  out
}

# ----------------------------
# 2) Build fac_data ONCE (mobile points × facilities)
# ----------------------------
fac_data <- dplyr::bind_rows(
  lapply(seq_len(nrow(facilities)), function(i) {
    compute_facility_metrics(facilities[i, ], mobile_df, receptors_ll)
  })
)

message("Built fac_data: ", nrow(fac_data), " rows (",
        nrow(mobile_df), " mobile points × ", nrow(facilities), " facilities).")

# Add common flags used in every pollutant run
fac_data <- fac_data %>%
  dplyr::mutate(
    downwind = .data$wind_alignment_deg < downwind_thresh_deg
  )

# Utility: safe model coefficient table without extra packages
coef_table <- function(fit) {
  sm <- summary(fit)
  co <- as.data.frame(sm$coefficients)
  co$term <- rownames(co)
  rownames(co) <- NULL
  names(co) <- c("estimate","std_error","t_value","p_value","term")

  # confint can fail for huge models; guard it
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
  if (!is.null(ci)) {
    ci <- as.data.frame(ci)
    ci$term <- rownames(ci)
    rownames(ci) <- NULL
    names(ci) <- c("conf_low","conf_high","term")
    co <- dplyr::left_join(co, ci, by = "term")
  } else {
    co$conf_low <- NA_real_
    co$conf_high <- NA_real_
  }
  co
}

# ----------------------------
# 3) Run Steps 1–6 per pollutant
# ----------------------------
all_summaries <- list()

for (pol_col in names(pollutants)) {

  pol_key <- pollutants[[pol_col]]
  message("\n============================")
  message("Pollutant: ", pol_col, " (", pol_key, ")")

  # keep only finite values for this pollutant
  dat <- fac_data %>%
    dplyr::mutate(conc = .data[[pol_col]]) %>%
    dplyr::filter(is.finite(.data$conc))

  if (nrow(dat) == 0) {
    message("[Skip] no finite values for ", pol_col)
    next
  }

  # Step 2: distance restriction
  dat <- dat %>% dplyr::filter(.data$distance_m <= max_dist_m)
  if (nrow(dat) == 0) {
    message("[Skip] all rows removed by max_dist_m for ", pol_col)
    next
  }

  # Step 3: upwind vs downwind summaries by facility
  summ <- dat %>%
    dplyr::group_by(.data$facility, .data$downwind) %>%
    dplyr::summarise(
      n = dplyr::n(),
      conc_med = stats::median(.data$conc, na.rm = TRUE),
      conc_p90 = stats::quantile(.data$conc, 0.90, na.rm = TRUE),
      dist_med_m = stats::median(.data$distance_m, na.rm = TRUE),
      align_med_deg = stats::median(.data$wind_alignment_deg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pollutant_col = pol_col,
      pollutant_key = pol_key,
      downwind_thresh_deg = downwind_thresh_deg,
      max_dist_m = max_dist_m
    )

  out_csv <- file.path(out_dir, paste0("summary_upwind_downwind_", pol_key, ".csv"))
  utils::write.csv(summ, out_csv, row.names = FALSE)
  message("[Saved] ", out_csv)
  all_summaries[[pol_key]] <- summ

  # Data for plotting: sample only for speed/size
  plot_dat <- dat
  if (nrow(plot_dat) > plot_sample_n) {
    set.seed(1)
    plot_dat <- dplyr::slice_sample(plot_dat, n = plot_sample_n)
  }

  # Step 4: alignment vs concentration (log y)
  p_align <- ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(x = .data$wind_alignment_deg, y = .data$conc, color = .data$facility)
  ) +
    ggplot2::geom_point(alpha = 0.12, size = 0.6) +
    ggplot2::geom_smooth(se = FALSE, linewidth = 0.9) +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::labs(
      title = paste0(pol_col, " — facility downwind signal"),
      subtitle = paste0("Distance ≤ ", max_dist_m, " m; downwind < ", downwind_thresh_deg,
                        "° (used in summaries/models). y is log10."),
      x = "Wind alignment (°; 0° = perfectly downwind of facility)",
      y = paste0(pol_col, " (ppb; log scale)")
    ) +
    ggplot2::theme_bw(base_size = 12)

  out_png1 <- file.path(out_dir, paste0("plot_alignment_", pol_key, ".png"))
  ggplot2::ggsave(out_png1, p_align, width = 9.5, height = 6.5, units = "in", dpi = 350, bg = "white")
  message("[Saved] ", out_png1)

  # Step 5: distance decay (downwind only; log-log)
  p_dist <- ggplot2::ggplot(
    plot_dat %>% dplyr::filter(.data$downwind),
    ggplot2::aes(x = .data$distance_m, y = .data$conc, color = .data$facility)
  ) +
    ggplot2::geom_point(alpha = 0.12, size = 0.6) +
    ggplot2::geom_smooth(se = FALSE, linewidth = 0.9) +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::labs(
      title = paste0(pol_col, " — distance decay (downwind only)"),
      subtitle = paste0("Downwind defined as alignment < ", downwind_thresh_deg,
                        "°; distance ≤ ", max_dist_m, " m. x,y are log10."),
      x = "Distance to facility (m; log scale)",
      y = paste0(pol_col, " (ppb; log scale)")
    ) +
    ggplot2::theme_bw(base_size = 12)

  out_png2 <- file.path(out_dir, paste0("plot_distance_decay_", pol_key, ".png"))
  ggplot2::ggsave(out_png2, p_dist, width = 9.5, height = 6.5, units = "in", dpi = 350, bg = "white")
  message("[Saved] ", out_png2)

  # Step 6: regression (with facility-specific downwind effect)
  # log(conc+eps) ~ facility + downwind + log(distance) + facility:downwind
  # (This asks: does the downwind boost differ by facility?)
  dat_m <- dat %>%
    dplyr::mutate(
      y = log(.data$conc + eps_log),
      logdist = log(.data$distance_m + 1)
    )

  fit <- tryCatch(
    stats::lm(y ~ facility + downwind + logdist + facility:downwind, data = dat_m),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    message("[Model failed] ", pol_col, ": ", fit$message)
  } else {
    co <- coef_table(fit) %>%
      dplyr::mutate(
        pollutant_col = pol_col,
        pollutant_key = pol_key,
        n_used = nrow(dat_m),
        eps_log = eps_log,
        downwind_thresh_deg = downwind_thresh_deg,
        max_dist_m = max_dist_m
      )

    out_coef <- file.path(out_dir, paste0("model_coefs_", pol_key, ".csv"))
    utils::write.csv(co, out_coef, row.names = FALSE)
    message("[Saved] ", out_coef)
  }
}

# Optional: write a single combined summary file
if (length(all_summaries) > 0) {
  summary_all <- dplyr::bind_rows(all_summaries)
  out_all <- file.path(out_dir, "summary_upwind_downwind_ALLPOLLUTANTS.csv")
  utils::write.csv(summary_all, out_all, row.names = FALSE)
  message("\n[Saved] ", out_all)
}

message("\nDONE. Outputs in: ", out_dir)


# Wind rotated Facility plumes (Top 5 percentile)

# ============================================================
# STANDALONE (FIXED v5): Facility–wind directional patterns + directional quantile qGAMs
# - NO time stratification
# - qgam 2.0.0 compatible (no knots=; no formula=)
# - Uses non-cyclic smooth for align_deg (0–180 is NOT circular)
#
# OUTPUTS per pollutant_key in out_dir:
# - A_directional_quantiles_<key>.png
# - B_distance_decay_quantiles_<key>.png
# - TABLE_downwind_summary_<key>.csv
# - SUMMARY_enhancement_<key>.csv
# PLUS:
# - ALL_SUMMARIES_enhancement.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(geosphere)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(mgcv)   # provides s()
  library(qgam)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
in_rdata <- "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData"
out_dir  <- "/Users/priyanka/Downloads/Suncor/facility_directional_patterns"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

exclude_site <- "Goodrich Corporation (Collins Aerospace)"  # set NULL to skip
min_ws <- 1.0
max_dist_m <- 5000
downwind_thresh_deg <- 30

p_hi <- 0.95
taus <- c(0.5, 0.75, 0.9, 0.95)

k_align <- 20   # slightly lower than 24 is often more stable
k_ldist <- 10
k_ws    <- 10

eps_ppb <- 0.01
dpi_out <- 350
axis_angle <- 35
set.seed(1)

facilities <- data.frame(
  name = c("Suncor","Sinclair","Phillips66"),
  lat  = c(39.803333, 39.8724, 39.79668),
  lon  = c(-104.945556, -104.8861, -104.94236),
  stringsAsFactors = FALSE
)

# ----------------------------
# 0) Read mobile data
# ----------------------------
stopifnot(file.exists(in_rdata))
load(in_rdata)

if (exists("out")) {
  df_raw <- out
} else if (exists("df")) {
  df_raw <- df
} else {
  stop("mobile_wswd.RData must contain an object named 'out' or 'df'.")
}

# pollutants you want
pollutants <- c(
  Benzene_ppb            = "benzene",
  Toluene_ppb            = "toluene",
  Trimethylbenzene_ppb   = "trimethylbenzene",
  Xylene_ppb             = "xylene",
  Hydrogen_Sulfide_ppb   = "h2s",
  Hydrogen_Cyanide_ppb   = "hcn"
)
pollutants <- pollutants[names(pollutants) %in% names(df_raw)]
stopifnot(length(pollutants) > 0)

# ----------------------------
# 1) ONE consistent filtered mobile dataset
# ----------------------------
req_cols <- c("Longitude","Latitude","ws","wd", names(pollutants))
stopifnot(all(req_cols %in% names(df_raw)))

mobile_df <- df_raw %>%
  dplyr::mutate(
    Longitude = suppressWarnings(as.numeric(.data$Longitude)),
    Latitude  = suppressWarnings(as.numeric(.data$Latitude)),
    ws        = suppressWarnings(as.numeric(.data$ws)),
    wd        = suppressWarnings(as.numeric(.data$wd))
  ) %>%
  { if (!is.null(exclude_site) && "Site" %in% names(df_raw)) dplyr::filter(., .data$Site != exclude_site) else . } %>%
  dplyr::filter(
    is.finite(.data$Longitude), is.finite(.data$Latitude),
    is.finite(.data$ws), .data$ws >= min_ws,
    is.finite(.data$wd), .data$wd >= 0, .data$wd <= 360
  )

stopifnot(nrow(mobile_df) > 0)
receptors_ll <- as.matrix(mobile_df[, c("Longitude","Latitude")])

angle_diff_deg <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

compute_facility_metrics <- function(fac_row, mobile_df, receptors_ll) {
  fac_ll <- c(fac_row$lon, fac_row$lat)

  d_m <- geosphere::distGeo(receptors_ll, fac_ll)
  b_deg <- geosphere::bearing(fac_ll, receptors_ll)
  b_deg <- (b_deg + 360) %% 360
  align_deg <- angle_diff_deg(mobile_df$wd, b_deg)

  data.frame(
    facility = fac_row$name,
    ws       = mobile_df$ws,
    wd       = mobile_df$wd,
    distance_m          = d_m,
    bearing_fac2rec_deg = b_deg,
    wind_alignment_deg  = align_deg,
    stringsAsFactors = FALSE
  )
}

fac_core <- dplyr::bind_rows(
  lapply(seq_len(nrow(facilities)), function(i) {
    compute_facility_metrics(facilities[i, ], mobile_df, receptors_ll)
  })
)

message("Built fac_core: ", nrow(fac_core), " rows (",
        nrow(mobile_df), " mobile points × ", nrow(facilities), " facilities).")

# prediction grids
make_pred_grid_align <- function(dat_use, n_align = 181) {
  tidyr::expand_grid(
    facility = levels(dat_use$facility),
    align_deg = seq(0, 180, length.out = n_align)
  ) %>%
    dplyr::mutate(
      logdist  = stats::median(dat_use$logdist, na.rm = TRUE),
      ws       = stats::median(dat_use$ws, na.rm = TRUE),
      downwind = factor(TRUE, levels = levels(dat_use$downwind)),
      facility = factor(.data$facility, levels = levels(dat_use$facility))
    )
}

make_pred_grid_dist <- function(dat_use, n_dist = 120) {
  d_min <- max(50, stats::quantile(dat_use$distance_m, 0.05, na.rm = TRUE))
  d_max <- stats::quantile(dat_use$distance_m, 0.95, na.rm = TRUE)
  d_seq <- exp(seq(log(d_min), log(d_max), length.out = n_dist))

  tidyr::expand_grid(
    facility = levels(dat_use$facility),
    dist_m = d_seq
  ) %>%
    dplyr::mutate(
      align_deg = 0,
      logdist   = log10(.data$dist_m + 1),
      ws        = stats::median(dat_use$ws, na.rm = TRUE),
      downwind  = factor(TRUE, levels = levels(dat_use$downwind)),
      facility  = factor(.data$facility, levels = levels(dat_use$facility))
    )
}

# ----------------------------
# Steps 1–6 for ONE pollutant
# ----------------------------
run_steps_for_pollutant <- function(pol_col, pol_key) {

  message("\n==============================")
  message("Pollutant: ", pol_col)
  message("==============================")

  pol_vec <- suppressWarnings(as.numeric(mobile_df[[pol_col]]))
  pol_vec <- ifelse(is.finite(pol_vec), pol_vec, NA_real_)

  dat <- fac_core %>%
    dplyr::mutate(pol = rep(pol_vec, times = nrow(facilities))) %>%
    dplyr::filter(is.finite(.data$pol)) %>%
    dplyr::filter(.data$distance_m <= max_dist_m) %>%
    dplyr::mutate(downwind = .data$wind_alignment_deg <= downwind_thresh_deg)

  if (nrow(dat) == 0) {
    message("[Skip] No usable rows after QC+distance for ", pol_col)
    return(NULL)
  }

  # STEP 4: downwind summary
  tab <- dat %>%
    dplyr::group_by(.data$facility, .data$downwind) %>%
    dplyr::summarise(
      n = dplyr::n(),
      conc_med = stats::median(.data$pol, na.rm = TRUE),
      conc_p90 = stats::quantile(.data$pol, 0.90, na.rm = TRUE),
      dist_med_m = stats::median(.data$distance_m, na.rm = TRUE),
      align_med_deg = stats::median(.data$wind_alignment_deg, na.rm = TRUE),
      pollutant_col = pol_col,
      pollutant_key = pol_key,
      downwind_thresh_deg = downwind_thresh_deg,
      max_dist_m = max_dist_m,
      .groups = "drop"
    )

  out_tab <- file.path(out_dir, paste0("TABLE_downwind_summary_", pol_key, ".csv"))
  utils::write.csv(tab, out_tab, row.names = FALSE)
  message("[Saved] ", out_tab)

  # STEP 5: high events for modeling
  thr_hi <- as.numeric(stats::quantile(dat$pol, p_hi, na.rm = TRUE))

  dat_use <- dat %>%
    dplyr::filter(.data$pol >= thr_hi) %>%
    dplyr::mutate(
      align_deg = pmin(pmax(.data$wind_alignment_deg, 0), 180),
      logdist   = log10(.data$distance_m + 1),
      y         = log10(pmax(.data$pol + eps_ppb, eps_ppb)),
      facility  = factor(.data$facility, levels = facilities$name),
      downwind  = factor(.data$downwind, levels = c(FALSE, TRUE))
    ) %>%
    dplyr::filter(is.finite(.data$y), is.finite(.data$logdist), is.finite(.data$ws))

  message("Using HIGH events only: n = ", nrow(dat_use), " | thr(top 5%) = ", signif(thr_hi, 4))

  if (nrow(dat_use) < 500) {
    message("[Skip] Too few HIGH events for stable qGAM fits: ", nrow(dat_use))
    return(list(pollutant_col = pol_col, pollutant_key = pol_key, thr_hi = thr_hi, tab = tab, enh = NULL))
  }

  dat_use_df <- as.data.frame(dat_use)

  # NOTE: non-cyclic spline for align_deg (tp), because align_deg is 0..180 (not circular)
  form <- y ~
    s(align_deg, bs = "tp", k = k_align) +
    s(logdist,  bs = "tp", k = k_ldist) +
    s(ws,       bs = "tp", k = k_ws) +
    facility * downwind

  fits <- purrr::map(taus, function(tau) {
    qgam::qgam(form, data = dat_use_df, qu = tau)
  })

  # Plot A: alignment curves
  gridA <- make_pred_grid_align(dat_use)
  predA <- purrr::map2_dfr(fits, taus, function(mod, tau) {
    g <- as.data.frame(gridA)
    g$yhat <- stats::predict(mod, newdata = g)
    g$tau <- tau
    g
  }) %>%
    dplyr::mutate(conc_hat = pmax(10^yhat - eps_ppb, 0))

  pA <- ggplot2::ggplot(predA, ggplot2::aes(x = .data$align_deg, y = .data$conc_hat, color = .data$facility)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~tau, scales = "free_y") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = axis_angle, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0(pol_col, " — directional quantiles (predicted)"),
      subtitle = paste0("Top 5% events (>= ", signif(thr_hi, 3), " ppb). Downwind: alignment ≤ ",
                        downwind_thresh_deg, "° within ", max_dist_m/1000, " km."),
      x = "Wind alignment to facility (degrees; 0°=perfectly downwind)",
      y = "Predicted concentration (ppb)",
      color = "Facility"
    )

  outA <- file.path(out_dir, paste0("A_directional_quantiles_", pol_key, ".png"))
  ggplot2::ggsave(outA, pA, width = 11, height = 6.5, dpi = dpi_out, bg = "white")
  message("[Saved] ", outA)

  # Plot B: distance decay (align fixed at 0)
  gridB <- make_pred_grid_dist(dat_use)
  predB <- purrr::map2_dfr(fits, taus, function(mod, tau) {
    g <- as.data.frame(gridB)
    g$yhat <- stats::predict(mod, newdata = g)
    g$tau <- tau
    g
  }) %>%
    dplyr::mutate(conc_hat = pmax(10^yhat - eps_ppb, 0))

  pB <- ggplot2::ggplot(predB, ggplot2::aes(x = .data$dist_m, y = .data$conc_hat, color = .data$facility)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_log10() +
    ggplot2::facet_wrap(~tau, scales = "free_y") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = axis_angle, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0(pol_col, " — distance decay (predicted; downwind)"),
      subtitle = "Alignment fixed at 0°; ws fixed at median; x-axis log scale.",
      x = "Distance to facility (m; log scale)",
      y = "Predicted concentration (ppb)",
      color = "Facility"
    )

  outB <- file.path(out_dir, paste0("B_distance_decay_quantiles_", pol_key, ".png"))
  ggplot2::ggsave(outB, pB, width = 11, height = 6.5, dpi = dpi_out, bg = "white")
  message("[Saved] ", outB)

  # STEP 6: enhancement (align 0 vs 90) at median distance/ws
  enh_grid <- tidyr::expand_grid(
    facility = levels(dat_use$facility),
    align_deg = c(0, 90)
  ) %>%
    dplyr::mutate(
      logdist  = stats::median(dat_use$logdist, na.rm = TRUE),
      ws       = stats::median(dat_use$ws, na.rm = TRUE),
      downwind = factor(TRUE, levels = levels(dat_use$downwind)),
      facility = factor(.data$facility, levels = levels(dat_use$facility))
    )

  enh <- purrr::map2_dfr(fits, taus, function(mod, tau) {
    g <- as.data.frame(enh_grid)
    g$yhat <- stats::predict(mod, newdata = g)
    g$tau <- tau
    g
  }) %>%
    dplyr::mutate(conc_hat = pmax(10^yhat - eps_ppb, 0)) %>%
    dplyr::select(.data$facility, .data$tau, .data$align_deg, .data$conc_hat) %>%
    tidyr::pivot_wider(names_from = .data$align_deg, values_from = .data$conc_hat, names_prefix = "align_") %>%
    dplyr::mutate(
      ratio_0_vs_90 = .data$align_0 / .data$align_90,
      diff_0_minus_90 = .data$align_0 - .data$align_90,
      pollutant_col = pol_col,
      pollutant_key = pol_key,
      n_used = nrow(dat_use),
      eps_log = eps_ppb,
      downwind_thresh_deg = downwind_thresh_deg,
      max_dist_m = max_dist_m
    ) %>%
    dplyr::arrange(.data$tau, dplyr::desc(.data$ratio_0_vs_90))

  outS <- file.path(out_dir, paste0("SUMMARY_enhancement_", pol_key, ".csv"))
  utils::write.csv(enh, outS, row.names = FALSE)
  message("[Saved] ", outS)

  list(pollutant_col = pol_col, pollutant_key = pol_key, thr_hi = thr_hi, tab = tab, enh = enh)
}

# ----------------------------
# Run all pollutants
# ----------------------------
all_results <- list()
for (pol_col in names(pollutants)) {
  pol_key <- pollutants[[pol_col]]
  all_results[[pol_key]] <- run_steps_for_pollutant(pol_col, pol_key)
}

enh_all <- dplyr::bind_rows(lapply(all_results, function(x) if (!is.null(x)) x$enh else NULL))
if (nrow(enh_all) > 0) {
  out_all <- file.path(out_dir, "ALL_SUMMARIES_enhancement.csv")
  utils::write.csv(enh_all, out_all, row.names = FALSE)
  message("\n[Saved] ", out_all)
}

message("\nDONE. Outputs in: ", out_dir)
