# ==============================================================
# 19  Plot maps census blocks
# Auto-split from Suncor.Rmd  (section 19 of 40)
# ==============================================================

#Plot maps census blocks

# ============================================================
# MAPS + SCATTERPLOTS (POLYGONS): AirToxScreen vs Mobile (scaled; MEDIAN-OF-DAILY-MEDIANS)
# - Plots census block POLYGONS (not dots) + basemap tiles
# - ALSO makes 3 scatterplots (AirToxScreen ppb vs Mobile scaled ppb)
# - UPDATE (per your request):
#     * MAPS use ROBUST limits (2–98% quantiles) computed across AirTox + Mobile (per pollutant)
#     * Scatterplots stay EXACTLY as-is (still use the same limits object; now that object is robust)
# - OPTIONAL: log10 color scale on maps (USE_LOG_FILL)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(scales)
  library(patchwork)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
out_dir <- "/Users/priyanka/Downloads/Suncor/FinalFig/block_maps_airtox_vs_mobile_scaled_polygons_medofdailymed_ROBUST"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dpi_out   <- 450
tile_type <- "cartolight"
tile_zoom <- 11

# Map fill scaling:
USE_LOG_FILL <- TRUE      # <-- set FALSE if you want linear colors
LOG_EPS      <- 1e-6      # floor for log scale (avoids log(0))

stopifnot(exists("block_sf_overlap"))

# ----------------------------
# 0) Column names (Mobile = MEDIAN-OF-DAILY-MEDIANS SCALED)
# ----------------------------
mob_benz <- "sBenzene_med_of_daily_med_scaled"
mob_tol  <- "sToluene_med_of_daily_med_scaled"
mob_xyl  <- "sXylene_med_of_daily_med_scaled"

air_benz <- "benzene_ppb"
air_tol  <- "toluene_ppb"
air_xyl  <- "xylene_ppb"

need_cols <- c(air_benz, air_tol, air_xyl, mob_benz, mob_tol, mob_xyl)
missing_cols <- setdiff(need_cols, names(block_sf_overlap))
if (length(missing_cols) > 0) {
  stop("Missing required columns in block_sf_overlap: ", paste(missing_cols, collapse = ", "))
}

# ----------------------------
# 1) Prep sf + bbox (shared across all maps)
# ----------------------------
blk_ll <- block_sf_overlap %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

bb <- sf::st_bbox(blk_ll)
xmin <- as.numeric(bb["xmin"]); xmax <- as.numeric(bb["xmax"])
ymin <- as.numeric(bb["ymin"]); ymax <- as.numeric(bb["ymax"])

# expand bbox a bit for tiles + aesthetics
pad_x <- max((xmax - xmin) * 0.12, 0.03)
pad_y <- max((ymax - ymin) * 0.12, 0.03)

# enforce minimum span to avoid "bounding box is too small" warnings
min_span <- 0.18  # degrees
xmid <- (xmin + xmax) / 2
ymid <- (ymin + ymax) / 2
xspan <- max((xmax - xmin) + 2 * pad_x, min_span)
yspan <- max((ymax - ymin) + 2 * pad_y, min_span)

xlim_use <- c(xmid - xspan / 2, xmid + xspan / 2)
ylim_use <- c(ymid - yspan / 2, ymid + yspan / 2)

print(range(xlim_use))
print(range(ylim_use))

# ----------------------------
# 2) Helpers: robust limits + safe numeric
# ----------------------------
as_num <- function(x) suppressWarnings(as.numeric(x))

robust_lims <- function(x, p = c(0.02, 0.98)) {
  x <- as_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(c(NA_real_, NA_real_))
  q <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE))
  if (!all(is.finite(q)) || q[1] == q[2]) q <- range(x, na.rm = TRUE)
  q
}

# Robust limits across AirTox + Mobile scaled (per pollutant pair)
pair_lims_robust <- function(df, air_col, mob_col, p = c(0.02, 0.98)) {
  v <- c(as_num(df[[air_col]]), as_num(df[[mob_col]]))
  v <- v[is.finite(v)]
  if (!length(v)) return(c(0, 1))
  rng <- robust_lims(v, p = p)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    rng <- range(v, na.rm = TRUE)
    if (diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
  }
  rng
}

# For log scale: ensure lower limit is > 0
safe_log_lims <- function(lims, eps = LOG_EPS) {
  lims2 <- lims
  if (!all(is.finite(lims2))) return(lims2)
  lims2[1] <- max(lims2[1], eps)
  if (lims2[2] <= lims2[1]) lims2[2] <- lims2[1] * 10
  lims2
}

# ----------------------------
# 3) Helper: polygon map with basemap
#     + optional log10 fill
#     + uses ROBUST limits (passed in as lims)
# ----------------------------
make_poly_map <- function(sf_obj, varname, title_txt, lims, legend_name = "ppb",
                          use_log = TRUE, eps = LOG_EPS) {
  stopifnot(inherits(sf_obj, "sf"))
  stopifnot(varname %in% names(sf_obj))

  sfp <- sf_obj %>%
    mutate(val = as_num(.data[[varname]])) %>%
    filter(is.finite(val))

  if (nrow(sfp) == 0) {
    message("[Skip] ", title_txt, " (no finite values)")
    return(ggplot() + theme_void() + labs(title = paste0(title_txt, " (no data)")))
  }

  lims_use <- lims
  if (use_log) {
    sfp <- sfp %>% mutate(val = pmax(val, eps))
    lims_use <- safe_log_lims(lims_use, eps = eps)
  }

  p <- ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom) +
    geom_sf(data = sfp, aes(fill = val), color = NA) +
    coord_sf(crs = 4326, xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 12)
    ) +
    labs(title = title_txt, x = NULL, y = NULL)

  if (use_log) {
    p <- p +
      scale_fill_viridis_c(
        option = "C",
        trans = "log10",
        limits = lims_use,
        oob = scales::squish,
        name = legend_name,
        breaks = scales::log_breaks(n = 5)
      )
  } else {
    p <- p +
      scale_fill_viridis_c(
        option = "C",
        limits = lims_use,
        oob = scales::squish,
        name = legend_name
      )
  }

  p
}

# ----------------------------
# 4) Scatterplot helper (UNCHANGED)
# ----------------------------
USE_LOG_AXES <- FALSE  # keep as you had it

make_scatter <- function(df, air_col, mob_col, title_txt, lims, xlab, ylab,
                         use_log_axes = FALSE, eps = LOG_EPS) {
  d <- df %>%
    st_drop_geometry() %>%
    transmute(
      air = as_num(.data[[air_col]]),
      mob = as_num(.data[[mob_col]])
    ) %>%
    filter(is.finite(air), is.finite(mob))

  if (nrow(d) == 0) {
    message("[Skip] ", title_txt, " (no finite values)")
    return(ggplot() + theme_void() + labs(title = paste0(title_txt, " (no data)")))
  }

  R <- suppressWarnings(cor(d$air, d$mob, use = "complete.obs"))
  RMSE <- sqrt(mean((d$mob - d$air)^2, na.rm = TRUE))
  ann <- sprintf("n = %d\nR = %.2f\nRMSE = %.2f", nrow(d), R, RMSE)

  lims_use <- lims
  if (use_log_axes) {
    d <- d %>% mutate(air = pmax(air, eps), mob = pmax(mob, eps))
    lims_use <- safe_log_lims(lims_use, eps = eps)
  }

  p <- ggplot(d, aes(x = air, y = mob)) +
    geom_point(alpha = 0.35, size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.8) +
    coord_equal(xlim = lims_use, ylim = lims_use, expand = FALSE) +
    labs(title = title_txt, x = xlab, y = ylab) +
    theme_bw(base_size = 12) +
    ggplot2::annotate(
      "text",
      x = lims_use[1] + 0.98 * diff(lims_use),
      y = lims_use[1] + 0.08 * diff(lims_use),
      label = ann, hjust = 1, vjust = 0, size = 4
    )

  if (use_log_axes) {
    p <- p +
      scale_x_log10(labels = scales::number_format(accuracy = 0.01)) +
      scale_y_log10(labels = scales::number_format(accuracy = 0.01))
  } else {
    p <- p +
      scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
  }

  p
}

# ----------------------------
# 5) Pollutant-specific overlap subsets
#     Require AirTox AND Mobile scaled for that pollutant
# ----------------------------
sf_benz <- blk_ll %>%
  filter(is.finite(as_num(.data[[air_benz]])),
         is.finite(as_num(.data[[mob_benz]])))

sf_tol <- blk_ll %>%
  filter(is.finite(as_num(.data[[air_tol]])),
         is.finite(as_num(.data[[mob_tol]])))

sf_xyl <- blk_ll %>%
  filter(is.finite(as_num(.data[[air_xyl]])),
         is.finite(as_num(.data[[mob_xyl]])))

# ----------------------------
# 6) ROBUST matched limits per pollutant pair
#     -> Used for BOTH maps and scatterplots (same units/limits)
# ----------------------------
benz_lims <- pair_lims_robust(sf_benz, air_benz, mob_benz, p = c(0.02, 0.98))
tol_lims  <- pair_lims_robust(sf_tol,  air_tol,  mob_tol,  p = c(0.02, 0.98))
xyl_lims  <- pair_lims_robust(sf_xyl,  air_xyl,  mob_xyl,  p = c(0.02, 0.98))

print(list(benz_lims_robust = benz_lims, tol_lims_robust = tol_lims, xyl_lims_robust = xyl_lims))

# ----------------------------
# 7) Build 6 polygon maps (OPTIONAL LOG COLOR SCALE)
# ----------------------------
p1 <- make_poly_map(sf_benz, air_benz, "AirToxScreen Benzene (ppb)", benz_lims,
                    use_log = USE_LOG_FILL)
p2 <- make_poly_map(sf_benz, mob_benz, "Mobile Benzene (scaled; ppb)", benz_lims,
                    use_log = USE_LOG_FILL)

p3 <- make_poly_map(sf_tol,  air_tol,  "AirToxScreen Toluene (ppb)", tol_lims,
                    use_log = USE_LOG_FILL)
p4 <- make_poly_map(sf_tol,  mob_tol,  "Mobile Toluene (scaled; ppb)", tol_lims,
                    use_log = USE_LOG_FILL)

p5 <- make_poly_map(sf_xyl,  air_xyl,  "AirToxScreen Xylene (ppb)", xyl_lims,
                    use_log = USE_LOG_FILL)
p6 <- make_poly_map(sf_xyl,  mob_xyl,  "Mobile Xylene (scaled; ppb)", xyl_lims,
                    use_log = USE_LOG_FILL)

# ----------------------------
# 8) Save individual maps (high-res)
# ----------------------------
ggsave(file.path(out_dir, "map_AirTox_Benzene_ppb_polygons_ROBUST.png"),
       p1, width = 8, height = 6, dpi = dpi_out, bg = "white")
ggsave(file.path(out_dir, "map_Mobile_scaled_Benzene_ppb_polygons_ROBUST.png"),
       p2, width = 8, height = 6, dpi = dpi_out, bg = "white")

ggsave(file.path(out_dir, "map_AirTox_Toluene_ppb_polygons_ROBUST.png"),
       p3, width = 8, height = 6, dpi = dpi_out, bg = "white")
ggsave(file.path(out_dir, "map_Mobile_scaled_Toluene_ppb_polygons_ROBUST.png"),
       p4, width = 8, height = 6, dpi = dpi_out, bg = "white")

ggsave(file.path(out_dir, "map_AirTox_Xylene_ppb_polygons_ROBUST.png"),
       p5, width = 8, height = 6, dpi = dpi_out, bg = "white")
ggsave(file.path(out_dir, "map_Mobile_scaled_Xylene_ppb_polygons_ROBUST.png"),
       p6, width = 8, height = 6, dpi = dpi_out, bg = "white")

# ----------------------------
# 9) Combined 6-panel MAP figure
# ----------------------------
panel6_maps <- (p1 | p2) /
  (p3 | p4) /
  (p5 | p6) +
  patchwork::plot_annotation(
    title = paste0(
      "Overlapping Census Blocks: AirToxScreen vs Mobile (scaled) Concentrations (robust limits: 2–98%)",
      ifelse(USE_LOG_FILL, " — log10 color scale", "")
    ),
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(
  file.path(out_dir, "FIG_polygons_overlap_AirTox_vs_Mobile_scaled_6panel_ROBUST.png"),
  panel6_maps,
  width = 12, height = 12, units = "in",
  dpi = dpi_out,
  bg = "white"
)

# ----------------------------
# 10) Scatterplots (UNCHANGED): still use SAME limits object (now robust)
#     + Equal-sized 3-panel layout (single row)
# ----------------------------
s1 <- make_scatter(sf_benz, air_benz, mob_benz,
                   "Benzene: Mobile (scaled) vs AirToxScreen",
                   benz_lims,
                   xlab = "AirToxScreen Benzene (ppb)",
                   ylab = "Mobile Benzene (scaled; ppb)",
                   use_log_axes = USE_LOG_AXES)

s2 <- make_scatter(sf_tol, air_tol, mob_tol,
                   "Toluene: Mobile (scaled) vs AirToxScreen",
                   tol_lims,
                   xlab = "AirToxScreen Toluene (ppb)",
                   ylab = "Mobile Toluene (scaled; ppb)",
                   use_log_axes = USE_LOG_AXES)

s3 <- make_scatter(sf_xyl, air_xyl, mob_xyl,
                   "Xylene: Mobile (scaled) vs AirToxScreen",
                   xyl_lims,
                   xlab = "AirToxScreen Xylene (ppb)",
                   ylab = "Mobile Xylene (scaled; ppb)",
                   use_log_axes = USE_LOG_AXES)

ggsave(file.path(out_dir, "scatter_Benzene_MobileScaled_vs_AirTox_ROBUST.png"),
       s1, width = 7, height = 6, dpi = dpi_out, bg = "white")
ggsave(file.path(out_dir, "scatter_Toluene_MobileScaled_vs_AirTox_ROBUST.png"),
       s2, width = 7, height = 6, dpi = dpi_out, bg = "white")
ggsave(file.path(out_dir, "scatter_Xylene_MobileScaled_vs_AirTox_ROBUST.png"),
       s3, width = 7, height = 6, dpi = dpi_out, bg = "white")

panel3_scatter <- (s1 | s2 | s3) +
  patchwork::plot_annotation(
    title = paste0(
      "Mobile (scaled) vs AirToxScreen by Census Block (same ppb units; same limits as maps)",
      ifelse(USE_LOG_AXES, " — log10 axes", "")
    ),
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(file.path(out_dir, "FIG_scatter3_MobileScaled_vs_AirTox_equalPanels_ROBUST.png"),
       panel3_scatter, width = 14, height = 4.8, dpi = dpi_out, bg = "white")

message("Done. Saved maps + scatterplots to: ", out_dir)

# Print to viewer (optional)
panel6_maps
panel3_scatter
