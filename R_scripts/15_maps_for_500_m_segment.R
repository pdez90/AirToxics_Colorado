# ==============================================================
# 15  Maps for 500 m segment
# Auto-split from Suncor.Rmd  (section 15 of 40)
# ==============================================================

#Maps for 500 m segment

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(scales)
  library(ragg)
  library(magick)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
out_dir   <- "/Users/priyanka/Downloads/Suncor/segment500_maps_points"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

tile_type <- "cartolight"
tile_zoom <- 12
dpi_out   <- 400

# variables to map (must exist in seg_wide_sf)
v_benz <- "bgcorr_Benzene_median_of_daily_medians"
v_tol  <- "bgcorr_Toluene_median_of_daily_medians"
v_tmb  <- "bgcorr_Trimethylbenzene_median_of_daily_medians"
v_xyl  <- "bgcorr_Xylene_median_of_daily_medians"
v_h2s  <- "bgcorr_H2S_median_of_daily_medians"
v_hcn  <- "bgcorr_HCN_median_of_daily_medians"

stopifnot(exists("seg_wide_sf"))
stopifnot(all(c(v_benz, v_tol, v_tmb, v_xyl, v_h2s, v_hcn) %in% names(seg_wide_sf)))

# ----------------------------
# 0) Build plotting df in TRUE lon/lat (EPSG:4326), from geometry
#     (DO NOT trust Lon_grid/Lat_grid columns)
# ----------------------------
seg_ll <- seg_wide_sf |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  sf::st_transform(4326)

cent <- sf::st_centroid(sf::st_geometry(seg_ll))
xy   <- sf::st_coordinates(cent)

plot_df <- seg_ll |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    Lon = as.numeric(xy[,1]),
    Lat = as.numeric(xy[,2])
  ) |>
  dplyr::filter(is.finite(Lon), is.finite(Lat))

# sanity check: lon/lat should look like Denver-ish, not 0.0009
if (median(plot_df$Lon, na.rm = TRUE) > -10) {
  stop("Lon/Lat do not look like degrees (expected around -105, 39.x). Check CRS/geometry.")
}

# shared bbox for consistent framing
bb <- sf::st_bbox(sf::st_as_sf(plot_df, coords = c("Lon","Lat"), crs = 4326))
pad_x <- max(as.numeric(bb["xmax"] - bb["xmin"]) * 0.06, 0.01)
pad_y <- max(as.numeric(bb["ymax"] - bb["ymin"]) * 0.06, 0.01)
xlim_use <- c(as.numeric(bb["xmin"]) - pad_x, as.numeric(bb["xmax"]) + pad_x)
ylim_use <- c(as.numeric(bb["ymin"]) - pad_y, as.numeric(bb["ymax"]) + pad_y)

# ----------------------------
# 1) Build ONE map (points on basemap) + save via ggsave (ragg)
# ----------------------------
build_point_map <- function(varname, title_txt) {
  stopifnot(varname %in% names(plot_df))

  dfv <- plot_df |>
    dplyr::mutate(val = suppressWarnings(as.numeric(.data[[varname]]))) |>
    dplyr::filter(is.finite(val))

  if (nrow(dfv) == 0) {
    warning("No finite values for: ", varname)
    return(NULL)
  }

  # robust limits (avoid a single outlier crushing the palette)
  lims <- as.numeric(stats::quantile(dfv$val, probs = c(0.02, 0.98), na.rm = TRUE))
  if (!all(is.finite(lims)) || lims[1] == lims[2]) lims <- range(dfv$val, na.rm = TRUE)

  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
    ggplot2::geom_point(
      data = dfv,
      ggplot2::aes(x = Lon, y = Lat, color = val),
      size = 1.2, alpha = 0.95
    ) +
    ggplot2::coord_sf(crs = 4326, xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
    ggplot2::scale_color_viridis_c(
      option = "plasma",
      limits = lims,
      oob = scales::squish,
      name = "ppb"
    ) +
    ggplot2::labs(title = title_txt, x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )
}

save_one <- function(varname, title_txt, filename) {
  p <- build_point_map(varname, title_txt)
  if (is.null(p)) return(invisible(NULL))

  out_path <- file.path(out_dir, filename)

  ggplot2::ggsave(
    filename = out_path,
    plot     = p,
    width    = 7.2,
    height   = 6.0,
    units    = "in",
    dpi      = dpi_out,
    device   = ragg::agg_jpeg,
    background = "white"
  )

  message("[Saved] ", out_path)
  invisible(out_path)
}

# ----------------------------
# 2) Save SIX maps (explicit calls; no lists required)
# ----------------------------
f1 <- save_one(v_benz, "Benzene (bg-corrected) — median of daily medians",
               "map_bgcorr_Benzene_medianDailyMedians.jpg")
f2 <- save_one(v_tol,  "Toluene (bg-corrected) — median of daily medians",
               "map_bgcorr_Toluene_medianDailyMedians.jpg")
f3 <- save_one(v_tmb,  "Trimethylbenzene (bg-corrected) — median of daily medians",
               "map_bgcorr_Trimethylbenzene_medianDailyMedians.jpg")
f4 <- save_one(v_xyl,  "Xylene (bg-corrected) — median of daily medians",
               "map_bgcorr_Xylene_medianDailyMedians.jpg")
f5 <- save_one(v_h2s,  "H2S (bg-corrected) — median of daily medians",
               "map_bgcorr_H2S_medianDailyMedians.jpg")
f6 <- save_one(v_hcn,  "HCN (bg-corrected) — median of daily medians",
               "map_bgcorr_HCN_medianDailyMedians.jpg")

# ----------------------------
# 3) Combine into 3×2 panel with magick (keeps basemaps)
# ----------------------------
img_files <- c(f1, f2, f3, f4, f5, f6)
img_files <- img_files[!is.na(img_files)]
stopifnot(length(img_files) == 6)
stopifnot(all(file.exists(img_files)))

imgs <- lapply(img_files, magick::image_read)

# standardize widths so grid aligns
target_w <- 2400
imgs <- lapply(imgs, function(im) magick::image_resize(im, paste0(target_w, "x")))

row1 <- magick::image_append(c(imgs[[1]], imgs[[2]]), stack = FALSE)
row2 <- magick::image_append(c(imgs[[3]], imgs[[4]]), stack = FALSE)
row3 <- magick::image_append(c(imgs[[5]], imgs[[6]]), stack = FALSE)
panel6 <- magick::image_append(c(row1, row2, row3), stack = TRUE)

out_png <- file.path(out_dir, "FIG_segment500_bgcorr_medianDailyMedians_6panel.png")
out_jpg <- file.path(out_dir, "FIG_segment500_bgcorr_medianDailyMedians_6panel.jpg")

magick::image_write(panel6, path = out_png, format = "png")
magick::image_write(panel6, path = out_jpg, format = "jpeg", quality = 98)

message("[Saved] ", out_png)
message("[Saved] ", out_jpg)
