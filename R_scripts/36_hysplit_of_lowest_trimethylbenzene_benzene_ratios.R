# ==============================================================
# 36  Hysplit of lowest trimethylbenzene/benzene ratios
# Auto-split from Suncor.Rmd  (section 36 of 40)
# ==============================================================

#Hysplit of lowest trimethylbenzene/benzene ratios

# ============================================================
# CLEAN + ROBUST: HYSPLIT backward trajectories for BOTTOM 20% TMB/Benzene
# UPDATED TO PLOT EACH TRAJECTORY SEPARATELY
# FIXED OBJECT LOADING: prefers `out` from mobile_wswd.RData
#
# Outputs:
#   1) traj_low_TMBbyBenz_bottom20pct.RData
#   2) traj_low_TMBbyBenz_bottom20pct_map.html
#   3) traj_low_TMBbyBenz_bottom20pct_map.jpeg
#   4) traj_low_TMBbyBenz_bottom20pct_gg_paths.jpeg
#   5) traj_low_TMBbyBenz_bottom20pct_gg_points.jpeg
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(splitr)
  library(htmlwidgets)
  library(ggplot2)
  library(webshot2)
  library(ggspatial)
  library(sf)
  library(scales)
  library(leaflet)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
in_rdata <- "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData"

out_dir  <- "/Users/priyanka/Downloads/Suncor"
out_rdata <- file.path(out_dir, "traj_low_TMBbyBenz_bottom20pct.RData")
out_html  <- file.path(out_dir, "traj_low_TMBbyBenz_bottom20pct_map.html")
out_jpeg_leaflet <- file.path(out_dir, "traj_low_TMBbyBenz_bottom20pct_map.jpeg")
out_jpeg_gg_paths  <- file.path(out_dir, "traj_low_TMBbyBenz_bottom20pct_gg_paths.jpeg")
out_jpeg_gg_points <- file.path(out_dir, "traj_low_TMBbyBenz_bottom20pct_gg_points.jpeg")

exclude_site <- "Goodrich Corporation (Collins Aerospace)"

# trajectory config
met_type   <- "reanalysis"
direction  <- "backward"
duration_h <- 24
height_m   <- 30

# selection / speed
q_low <- 0.20
N_max <- 60
set.seed(1)

# leaflet screenshot settings
vwidth   <- 2200
vheight  <- 1600
zoom_img <- 2.5

# ggplot zoom padding (degrees)
lon_pad <- 1.0
lat_pad <- 1.0

# basemap style for ggplot
tile_type <- "cartolight"

# ----------------------------
# 0) Load cleanly
# ----------------------------
stopifnot(file.exists(in_rdata))

# remove stale objects from current session
if (exists("df", inherits = FALSE))  rm(df)
if (exists("out", inherits = FALSE)) rm(out)

load(in_rdata)

# Prefer `out`, then `df`
if (exists("out", inherits = FALSE) && is.data.frame(out)) {
  df <- out
} else if (exists("df", inherits = FALSE) && is.data.frame(df)) {
  df <- df
} else {
  stop("mobile_wswd.RData must contain a data.frame named 'out' or 'df'.")
}

req_cols <- c("Site", "date", "Latitude", "Longitude", "Trimethylbenzene_ppb", "Benzene_ppb")
missing_cols <- setdiff(req_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    "Loaded object is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    "\nAvailable columns are:\n",
    paste(names(df), collapse = ", ")
  )
}

message("Loaded object with ", nrow(df), " rows and ", ncol(df), " columns.")

# ----------------------------
# 1) Filter + compute ratio + bottom 20%
# ----------------------------
hs_df <- df %>%
  dplyr::filter(
    .data$Site != exclude_site,
    is.finite(.data$Trimethylbenzene_ppb),
    is.finite(.data$Benzene_ppb),
    .data$Benzene_ppb > 0,
    is.finite(.data$Longitude),
    is.finite(.data$Latitude),
    !is.na(.data$date)
  ) %>%
  dplyr::mutate(
    tmb_by_benz = .data$Trimethylbenzene_ppb / .data$Benzene_ppb
  ) %>%
  dplyr::filter(is.finite(.data$tmb_by_benz))

if (nrow(hs_df) == 0) {
  stop("After filtering, hs_df has 0 rows. This likely means too many NA values in TMB/Benzene.")
}

thr_low <- as.numeric(stats::quantile(hs_df$tmb_by_benz, probs = q_low, na.rm = TRUE))

hs_low <- hs_df %>%
  dplyr::filter(.data$tmb_by_benz <= thr_low)

message("Bottom ", q_low * 100, "% threshold (TMB/Benz): ", signif(thr_low, 4))
message("Rows in bottom ", q_low * 100, "%: ", nrow(hs_low))

if (nrow(hs_low) == 0) stop("Bottom selection returned 0 rows.")

# ----------------------------
# 2) Convert time to UTC day + hour for splitr
# ----------------------------
hs_low <- hs_low %>%
  dplyr::mutate(
    date_posix = as.POSIXct(.data$date),
    date_local = lubridate::force_tz(.data$date_posix, tzone = "America/Denver"),
    date_utc   = lubridate::with_tz(.data$date_local, tzone = "UTC"),
    day_utc    = as.Date(.data$date_utc),
    hour_utc   = lubridate::hour(.data$date_utc)
  )

# ----------------------------
# 3) Thin to 1 obs per UTC hour, then sample up to N_max receptor-times
# ----------------------------
hs_low_thin <- hs_low %>%
  dplyr::arrange(.data$date_utc) %>%
  dplyr::distinct(.data$day_utc, .data$hour_utc, .keep_all = TRUE)

N <- min(N_max, nrow(hs_low_thin))
hs_low_run <- hs_low_thin %>%
  dplyr::slice_sample(n = N)

message("Running trajectories for N = ", nrow(hs_low_run), " receptor-times...")

# ----------------------------
# 4) Run HYSPLIT backward trajectories
# ----------------------------
options(timeout = 1000000)

traj_list <- vector("list", nrow(hs_low_run))

for (i in seq_len(nrow(hs_low_run))) {
  message("Trajectory ", i, " / ", nrow(hs_low_run))

  tr_i <- splitr::hysplit_trajectory(
    lat         = hs_low_run$Latitude[i],
    lon         = hs_low_run$Longitude[i],
    height      = height_m,
    duration    = duration_h,
    met_type    = met_type,
    direction   = direction,
    days        = hs_low_run$day_utc[i],
    daily_hours = hs_low_run$hour_utc[i]
  )

  tr_i <- tr_i %>%
    dplyr::mutate(
      traj_id = i,
      receptor_lat = hs_low_run$Latitude[i],
      receptor_lon = hs_low_run$Longitude[i],
      receptor_time_utc = hs_low_run$date_utc[i]
    )

  traj_list[[i]] <- tr_i
}

traj_df <- dplyr::bind_rows(traj_list) %>%
  tidyr::drop_na(.data$lat, .data$lon)

stopifnot(nrow(traj_df) > 0)

# ----------------------------
# 4b) Robust ordering within each trajectory
# ----------------------------
order_col <- intersect(
  c("hour_along", "traj_hour", "forecast_hour", "time_along", "step"),
  names(traj_df)
)

if (length(order_col) > 0) {
  traj_df <- traj_df %>%
    dplyr::arrange(.data$traj_id, .data[[order_col[1]]])
} else if (all(c("year", "month", "day", "hour") %in% names(traj_df))) {
  traj_df <- traj_df %>%
    dplyr::mutate(
      traj_time = as.POSIXct(
        sprintf("%04d-%02d-%02d %02d:00:00", .data$year, .data$month, .data$day, .data$hour),
        tz = "UTC"
      )
    ) %>%
    dplyr::arrange(.data$traj_id, .data$traj_time)
} else {
  traj_df <- traj_df %>%
    dplyr::group_by(.data$traj_id) %>%
    dplyr::mutate(row_in_traj = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$traj_id, .data$row_in_traj)
}

save(traj_df, hs_low_run, thr_low, q_low, file = out_rdata)
message("[Saved] ", out_rdata)

# ----------------------------
# 5) Interactive leaflet map
# ----------------------------
pal <- scales::hue_pal()(length(unique(traj_df$traj_id)))
traj_ids <- sort(unique(traj_df$traj_id))
pal_df <- data.frame(traj_id = traj_ids, col = pal)

traj_df <- traj_df %>%
  dplyr::left_join(pal_df, by = "traj_id")

m <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron")

for (i in traj_ids) {
  tr_i <- traj_df %>% dplyr::filter(.data$traj_id == i)

  m <- m %>%
    leaflet::addPolylines(
      lng = tr_i$lon,
      lat = tr_i$lat,
      color = tr_i$col[1],
      weight = 2,
      opacity = 0.7,
      group = paste0("traj_", i)
    )
}

m <- m %>%
  leaflet::addCircleMarkers(
    lng = hs_low_run$Longitude,
    lat = hs_low_run$Latitude,
    radius = 4,
    stroke = TRUE,
    weight = 1,
    color = "black",
    fillColor = "yellow",
    fillOpacity = 0.9,
    popup = paste0(
      "Trajectory ID: ", seq_len(nrow(hs_low_run)),
      "<br>UTC time: ", hs_low_run$date_utc
    )
  )

# selfcontained=TRUE needs pandoc, which is absent from the Rscript PATH
# (RStudio bundles its own copy — that's why this worked interactively).
# A non-selfcontained widget saves without pandoc and webshot2 renders it
# identically; use selfcontained only if pandoc is actually findable.
has_pandoc <- nzchar(Sys.which("pandoc")) || nzchar(Sys.getenv("RSTUDIO_PANDOC"))
htmlwidgets::saveWidget(m, out_html, selfcontained = has_pandoc)
message("[Saved] ", out_html)

webshot2::webshot(
  url     = out_html,
  file    = out_jpeg_leaflet,
  vwidth  = vwidth,
  vheight = vheight,
  zoom    = zoom_img
)
message("[Saved] ", out_jpeg_leaflet)

# ----------------------------
# 6) Static plots with basemap
# ----------------------------
lon_lim <- range(traj_df$lon, na.rm = TRUE) + c(-lon_pad, lon_pad)
lat_lim <- range(traj_df$lat, na.rm = TRUE) + c(-lat_pad, lat_pad)

lon_lim[1] <- max(lon_lim[1], -180)
lon_lim[2] <- min(lon_lim[2], 180)
lat_lim[1] <- max(lat_lim[1], -90)
lat_lim[2] <- min(lat_lim[2], 90)

span_lon <- diff(lon_lim)
span_lat <- diff(lat_lim)
span_max <- max(span_lon, span_lat)

tile_zoom <- dplyr::case_when(
  span_max >= 40 ~ 3L,
  span_max >= 20 ~ 4L,
  span_max >= 10 ~ 5L,
  span_max >=  6 ~ 6L,
  span_max >=  3 ~ 7L,
  span_max >=  1.5 ~ 8L,
  TRUE ~ 9L
)

# PATH VERSION
p_paths <- ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
  ggplot2::geom_path(
    data = traj_df,
    ggplot2::aes(
      x = .data$lon,
      y = .data$lat,
      group = .data$traj_id,
      color = factor(.data$traj_id)
    ),
    linewidth = 0.5,
    alpha = 0.65,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    data = hs_low_run,
    ggplot2::aes(x = .data$Longitude, y = .data$Latitude),
    shape = 21,
    fill = "yellow",
    color = "black",
    size = 2.2,
    stroke = 0.4
  ) +
  ggplot2::coord_sf(
    crs = 4326,
    xlim = lon_lim,
    ylim = lat_lim,
    expand = FALSE
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid = ggplot2::element_blank()) +
  ggplot2::labs(
    title = paste0("Backward HYSPLIT trajectories for bottom ", q_low * 100, "% TMB/Benzene ratios"),
    subtitle = paste0(
      "Each trajectory plotted separately; receptors=", nrow(hs_low_run),
      "; duration=", duration_h, " h; height=", height_m, " m AGL"
    ),
    x = "Longitude",
    y = "Latitude"
  )

ggplot2::ggsave(
  filename = out_jpeg_gg_paths,
  plot = p_paths,
  width = 9.5,
  height = 7.5,
  units = "in",
  dpi = 600,
  bg = "white"
)
message("[Saved] ", out_jpeg_gg_paths)

# POINT VERSION
p_points <- ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
  ggplot2::geom_point(
    data = traj_df,
    ggplot2::aes(
      x = .data$lon,
      y = .data$lat,
      color = factor(.data$traj_id)
    ),
    size = 0.7,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    data = hs_low_run,
    ggplot2::aes(x = .data$Longitude, y = .data$Latitude),
    shape = 21,
    fill = "yellow",
    color = "black",
    size = 2.2,
    stroke = 0.4
  ) +
  ggplot2::coord_sf(
    crs = 4326,
    xlim = lon_lim,
    ylim = lat_lim,
    expand = FALSE
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid = ggplot2::element_blank()) +
  ggplot2::labs(
    title = paste0("Backward HYSPLIT trajectory points for bottom ", q_low * 100, "% TMB/Benzene ratios"),
    subtitle = "Diagnostic scatter plot of trajectory points (no line connections)",
    x = "Longitude",
    y = "Latitude"
  )

ggplot2::ggsave(
  filename = out_jpeg_gg_points,
  plot = p_points,
  width = 9.5,
  height = 7.5,
  units = "in",
  dpi = 600,
  bg = "white"
)
message("[Saved] ", out_jpeg_gg_points)

message(
  "\nDONE.\nOutputs:\n  ",
  out_rdata, "\n  ",
  out_html, "\n  ",
  out_jpeg_leaflet, "\n  ",
  out_jpeg_gg_paths, "\n  ",
  out_jpeg_gg_points
)
