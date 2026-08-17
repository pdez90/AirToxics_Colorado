# ==============================================================
# 37  Creating GIF Claude Figure S1.1
# Auto-split from Suncor.Rmd  (section 37 of 40)
# ==============================================================

#Creating GIF Claude Figure S1.1

# ============================================================
# ggspatial UPDATED (fixed bbox for ALL runs + cleaner basemap + less blur)
# - Uses ONE bbox computed from ALL out_merge points (so every frame matches)
# - Uses a simpler basemap: "cartolight" (clean, low clutter)
# - Reduces blur by (1) setting an explicit tile zoom, and (2) using ragg::agg_png
#   (tiles are still raster—higher zoom = sharper)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(ggspatial)
  library(geosphere)
  library(purrr)
  library(magick)
  library(cowplot)
  library(ragg)     # <- for sharper PNG output
})

load("/Users/priyanka/Downloads/Suncor/mobile_corrected.RData")
if (!exists("out_merge") && exists("out_sf")) out_merge <- out_sf
stopifnot(exists("out_merge"))

# Ensure day + Run label
if (!"day" %in% names(out_merge)) out_merge$day <- as.Date(out_merge$date)
out_merge$Run <- paste0(format(as.Date(out_merge$day), "%d %B %Y"), " ", out_merge$Site)

# ----------------------------
# 1) ONE bbox for ALL runs (same extent for every frame)
# ----------------------------
pad <- 0.01  # degrees; tighten/loosen as needed (0.005 tighter, 0.02 wider)

xr <- range(out_merge$Longitude, na.rm = TRUE)
yr <- range(out_merge$Latitude,  na.rm = TRUE)
if (diff(xr) == 0) xr <- xr + c(-pad, pad)
if (diff(yr) == 0) yr <- yr + c(-pad, pad)

bb_global <- c(
  xmin = xr[1] - pad, xmax = xr[2] + pad,
  ymin = yr[1] - pad, ymax = yr[2] + pad
)

# Pick a tile zoom (higher = sharper but more tile downloads)
# For your Denver-area bbox, 12–14 is usually good. Try 13 first.
tile_zoom <- 13

# Cleaner basemap type
tile_type <- "cartolight"  # other clean options: "osm", "stamenbw", "stamentoner"

# ----------------------------
# 2) Run stats (same as your function)
# ----------------------------
calculate_run_stats <- function(data) {
  if (nrow(data) < 2) {
    return(data.frame(total_distance_km=0, duration_hours=0, avg_speed_kmh=0,
                      start_time=NA, end_time=NA, total_points=nrow(data)))
  }
  data <- data[order(data$date), ]

  distances_m <- vapply(1:(nrow(data) - 1), function(i) {
    distHaversine(c(data$Longitude[i], data$Latitude[i]),
                  c(data$Longitude[i+1], data$Latitude[i+1]))
  }, numeric(1))

  total_distance_km <- sum(distances_m, na.rm = TRUE) / 1000
  start_time <- min(data$date, na.rm = TRUE)
  end_time   <- max(data$date, na.rm = TRUE)
  duration_hours <- as.numeric(difftime(end_time, start_time, units = "hours"))
  avg_speed_kmh  <- if (duration_hours > 0) total_distance_km / duration_hours else 0

  data.frame(
    total_distance_km = round(total_distance_km, 2),
    duration_hours    = round(duration_hours, 2),
    avg_speed_kmh     = round(avg_speed_kmh, 1),
    start_time        = as.character(start_time),
    end_time          = as.character(end_time),
    total_points      = nrow(data)
  )
}

# ----------------------------
# 3) Route map per run (same bbox each time)
# ----------------------------
route_map <- function(Run) {
  current_data <- out_merge[out_merge$Run == Run, ]
  if (nrow(current_data) == 0) return(NULL)

  run_stats <- calculate_run_stats(current_data)

  p <- ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
    coord_sf(
      crs = 4326,
      xlim = c(bb_global["xmin"], bb_global["xmax"]),
      ylim = c(bb_global["ymin"], bb_global["ymax"]),
      expand = FALSE
    ) +
    # draw path + points (usually looks cleaner than points-only)
    geom_path(
      data = current_data[order(current_data$date), ],
      aes(x = Longitude, y = Latitude),
      linewidth = 1.2, color = "black"
    ) +
    geom_point(
      data = current_data,
      aes(x = Longitude, y = Latitude),
      size = 1.5, color = "black", alpha = 0.8
    ) +
    theme_bw() +
    theme(
      plot.margin = margin(10, 10, 10, 10, unit = "pt"),
      axis.title = element_text(size = 12),
      axis.text  = element_text(size = 10)
    ) +
    labs(
      title = paste("Mobile Laboratory Route:", Run),
      subtitle = paste0("Duration: ", run_stats$duration_hours, " hrs | Speed: ",
                        run_stats$avg_speed_kmh, " km/h")
    )

  out_path <- paste0(
    "/Users/priyanka/Downloads/Suncor/GIF_route/individual_route_",
    gsub("[^A-Za-z0-9]", "_", Run), ".png"
  )

  # ragg device -> sharper text/lines; tiles still raster but this helps overall
  ragg::agg_png(out_path, width = 10, height = 8, units = "in", res = 300, background = "white")
  print(p)
  dev.off()

  invisible(out_path)
}

# Make all frames
runs <- unique(out_merge$Run)
purrr::walk(runs, route_map)

# ----------------------------
# 4) GIF (same as your approach)
# ----------------------------
quick_reverse_time_sort <- function(file_paths) {
  file_paths[order(file.mtime(file_paths), decreasing = FALSE)]
}

png_files_sorted <- list.files(
  "/Users/priyanka/Downloads/Suncor/GIF_route/",
  pattern = "\\.png$", full.names = TRUE
) |> quick_reverse_time_sort()

png_files_sorted %>%
  purrr::map(image_read) %>%
  image_join() %>%
  image_scale("1000x800!") %>%
  image_quantize(max = 128, colorspace = "RGB") %>%
  image_animate(fps = 2, loop = 1, optimize = TRUE) %>%
  image_write("/Users/priyanka/Downloads/Suncor/routes_runs_optimized.gif")

# Combine static + gif (unchanged)
static_plot <- ggdraw() + draw_image("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor_Terminal_Route.jpeg")
gif_plot    <- ggdraw() + draw_image("/Users/priyanka/Downloads/Suncor/routes_runs_optimized.gif")
combined_plot <- plot_grid(static_plot, gif_plot, ncol = 1)
ggsave("/Users/priyanka/Downloads/Suncor/combined_gif.png", combined_plot, width = 12, height = 4)
