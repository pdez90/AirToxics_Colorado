# ==============================================================
# 16  500 m plotting ratios
# Auto-split from Suncor.Rmd  (section 16 of 40)
# ==============================================================

#500 m plotting ratios

# ============================================================
# SEGMENT-LEVEL SOURCE DIAGNOSTIC RATIO MAPS (POINTS ON BASEMAP)
# - No geom_sf (avoids grob/fontsize errors)
# - Uses ggspatial basemap tiles + geom_point
# - Saves 4 individual high-res JPEGs (400 dpi)
# - Also saves one combined 2×2 panel figure (JPEG + PNG)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(scales)
  library(magick)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
stopifnot(exists("seg_wide_sf"))

out_dir <- "/Users/priyanka/Downloads/Suncor/segment500_ratio_maps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

tile_type <- "cartolight"
tile_zoom <- 12
dpi_out   <- 400

# ----------------------------
# 0) Prep lon/lat plotting DF (NO GEOM)
# ----------------------------
seg_ll <- sf::st_transform(seg_wide_sf, 4326) |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  sf::st_make_valid()

# prefer existing Lon_grid/Lat_grid; otherwise compute centroids
have_lonlat <- all(c("Lon_grid","Lat_grid") %in% names(seg_ll))
if (!have_lonlat) {
  cent <- sf::st_centroid(sf::st_geometry(seg_ll))
  xy   <- sf::st_coordinates(cent)
  seg_ll$Lon_grid <- xy[, 1]
  seg_ll$Lat_grid <- xy[, 2]
}

plot_df <- seg_ll |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    Lon_grid = as.numeric(Lon_grid),
    Lat_grid = as.numeric(Lat_grid)
  ) |>
  dplyr::filter(is.finite(Lon_grid), is.finite(Lat_grid))

# ----------------------------
# 1) Define columns (must exist)
# ----------------------------
v_benz <- "bgcorr_Benzene_median_of_daily_medians"
v_tol  <- "bgcorr_Toluene_median_of_daily_medians"
v_tmb  <- "bgcorr_Trimethylbenzene_median_of_daily_medians"
v_xyl  <- "bgcorr_Xylene_median_of_daily_medians"
v_h2s  <- "bgcorr_H2S_median_of_daily_medians"
v_hcn  <- "bgcorr_HCN_median_of_daily_medians"

need <- c(v_benz, v_tol, v_tmb, v_xyl, v_h2s, v_hcn)
stopifnot(all(need %in% names(plot_df)))

# ----------------------------
# 2) Safe ratio helper (avoid div-by-0 and crazy infinities)
# ----------------------------
safe_ratio <- function(num, den) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  out <- rep(NA_real_, length(num))
  ok  <- is.finite(num) & is.finite(den) & den > 0
  out[ok] <- num[ok] / den[ok]
  out
}

# ----------------------------
# 3) Compute ratios in plot_df
# ----------------------------
plot_df <- plot_df |>
  dplyr::mutate(
    ratio_T_B   = safe_ratio(.data[[v_tol]],  .data[[v_benz]]),
    ratio_X_B   = safe_ratio(.data[[v_xyl]],  .data[[v_benz]]),
    ratio_TMB_B = safe_ratio(.data[[v_tmb]],  .data[[v_benz]]),
    ratio_H2S_B = safe_ratio(.data[[v_h2s]],  .data[[v_benz]]),
    ratio_HCN_B = safe_ratio(.data[[v_hcn]],  .data[[v_benz]])
  )

# ----------------------------
# 4) Shared bbox for consistent framing
# ----------------------------
bb <- sf::st_bbox(sf::st_as_sf(plot_df, coords = c("Lon_grid","Lat_grid"), crs = 4326))
pad_x <- max(as.numeric(bb["xmax"] - bb["xmin"]) * 0.08, 0.01)
pad_y <- max(as.numeric(bb["ymax"] - bb["ymin"]) * 0.08, 0.01)

xlim_use <- c(as.numeric(bb["xmin"]) - pad_x, as.numeric(bb["xmax"]) + pad_x)
ylim_use <- c(as.numeric(bb["ymin"]) - pad_y, as.numeric(bb["ymax"]) + pad_y)

# ----------------------------
# 5) Build + save ONE ratio map (ggsave, 400 dpi)
# ----------------------------
save_ratio_map <- function(varname, title_txt, out_file) {

  dfv <- plot_df |>
    dplyr::mutate(val = suppressWarnings(as.numeric(.data[[varname]]))) |>
    dplyr::filter(is.finite(val))

  if (nrow(dfv) == 0) {
    message("[Skip] ", varname, " (no finite values)")
    return(invisible(NULL))
  }

  # robust limits (avoid one outlier dominating)
  lims <- as.numeric(stats::quantile(dfv$val, probs = c(0.02, 0.98), na.rm = TRUE))
  if (!all(is.finite(lims)) || lims[1] >= lims[2]) lims <- range(dfv$val, na.rm = TRUE)

  p <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
    ggplot2::geom_point(
      data = dfv,
      ggplot2::aes(x = Lon_grid, y = Lat_grid, color = val),
      size = 1.4, alpha = 0.95
    ) +
    ggplot2::coord_sf(crs = 4326, xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
    ggplot2::scale_color_viridis_c(
      option = "C",
      limits = lims,
      oob = scales::squish,
      name = "ratio"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = title_txt, x = NULL, y = NULL)

  ggplot2::ggsave(
    filename = file.path(out_dir, out_file),
    plot = p,
    width = 7.2, height = 5.6, units = "in",
    dpi = dpi_out,
    bg = "white"
  )

  message("[Saved] ", file.path(out_dir, out_file))
  invisible(file.path(out_dir, out_file))
}

# ----------------------------
# 6) Save 4 ratio maps (no lists, no loops required)
# ----------------------------
f1 <- save_ratio_map("ratio_T_B",   "Toluene/Benzene (bg-corrected) — median-of-daily-medians ratio",
                     "ratio_TB.jpg")
f2 <- save_ratio_map("ratio_X_B",   "Xylene/Benzene (bg-corrected) — median-of-daily-medians ratio",
                     "ratio_XB.jpg")
f3 <- save_ratio_map("ratio_TMB_B", "Trimethylbenzene/Benzene (bg-corrected) — median-of-daily-medians ratio",
                     "ratio_TMBB.jpg")
f4 <- save_ratio_map("ratio_H2S_B", "H2S/Benzene (bg-corrected) — median-of-daily-medians ratio",
                     "ratio_H2SB.jpg")

# If you also want HCN/B as a 5th map, uncomment:
# f5 <- save_ratio_map("ratio_HCN_B", "HCN/Benzene (bg-corrected) — median-of-daily-medians ratio",
#                      "ratio_HCNB.jpg")

# ----------------------------
# 7) Combine into ONE 2×2 panel (magick is robust)
# ----------------------------
img_files <- c(
  file.path(out_dir, "ratio_TB.jpg"),
  file.path(out_dir, "ratio_XB.jpg"),
  file.path(out_dir, "ratio_TMBB.jpg"),
  file.path(out_dir, "ratio_H2SB.jpg")
)
stopifnot(all(file.exists(img_files)))

imgs <- lapply(img_files, magick::image_read)

# standardize width so panels align
target_w <- 2200
imgs <- lapply(imgs, function(im) magick::image_resize(im, paste0(target_w, "x")))

row1 <- magick::image_append(c(imgs[[1]], imgs[[2]]), stack = FALSE)
row2 <- magick::image_append(c(imgs[[3]], imgs[[4]]), stack = FALSE)
panel4 <- magick::image_append(c(row1, row2), stack = TRUE)

out_panel_png <- file.path(out_dir, "FIG_segment500_ratio_maps_4panel.png")
out_panel_jpg <- file.path(out_dir, "FIG_segment500_ratio_maps_4panel.jpg")

magick::image_write(panel4, path = out_panel_png, format = "png")
magick::image_write(panel4, path = out_panel_jpg, format = "jpeg", quality = 98)

message("[Saved] ", out_panel_png)
message("[Saved] ", out_panel_jpg)
