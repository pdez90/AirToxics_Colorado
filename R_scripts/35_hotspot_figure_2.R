# ==============================================================
# 35  Hotspot figure 2
# Auto-split from Suncor.Rmd  (section 35 of 40)
# ==============================================================

#Hotspot figure 2

# ============================================================
# HOTSPOT × SOURCE DIRECTIONAL ENRICHMENT + DISTANCE ANALYSIS
# Builds off objects already created in your hotspot power-figure script:
#   df, df_sf_m, master, master_sf_m, context_facilities,
#   match_dist_m, global_thr, pollutant_cols_existing, out_dir
#
# Outputs:
#   1) hotspot_source_directional_metrics.csv
#   2) hotspot_source_distance_metrics.csv
#   3) hotspot_source_directional_metrics_bytype.csv
#   4) hotspot_source_distance_metrics_bytype.csv
#   5) FIG_hotspot_source_enrichment_heatmap.png
#   6) FIG_hotspot_source_frac_high_heatmap.png
#   7) FIG_hotspot_source_distance_plot.png
#
# UPDATED:
#   - forces WWTF1 and WWTF2 explicitly
#   - keeps Refuel collapsed in plotting only if multiple refuel sites exist
#   - uses current context_facilities labels directly
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ragg)
  library(forcats)
  library(scales)
})

# ----------------------------
# CHECK REQUIRED OBJECTS
# ----------------------------
req_objs <- c(
  "df", "df_sf_m", "master", "master_sf_m",
  "context_facilities", "match_dist_m",
  "global_thr", "pollutant_cols_existing", "out_dir"
)
missing_objs <- req_objs[!vapply(req_objs, exists, logical(1))]
if (length(missing_objs) > 0) {
  stop("Missing required objects from previous chunk: ",
       paste(missing_objs, collapse = ", "))
}

# ----------------------------
# SETTINGS
# ----------------------------
source_sector_halfwidth_deg <- 30
buffer_set_m <- c(100, 250, 500, 1000)

# ----------------------------
# 0) FORCE UPDATED SOURCE LABELS
# ----------------------------
# This protects against stale objects still using "WWTP"
context_facilities <- context_facilities %>%
  mutate(
    label = case_when(
      label == "WWTP" & row_number() == which(label == "WWTP")[1] ~ "WWTF1",
      TRUE ~ label
    )
  )

# If WWTF2 is missing entirely, stop and tell user
needed_sources <- c("Suncor", "Sinclair", "Phillips 66", "WWTF1", "WWTF2", "Woodshop")
missing_needed <- setdiff(needed_sources, unique(context_facilities$label))
if (length(missing_needed) > 0) {
  stop(
    "context_facilities is missing these expected labels: ",
    paste(missing_needed, collapse = ", "),
    ". Re-run the chunk where you created context_facilities with WWTF1 and WWTF2."
  )
}

# ----------------------------
# 1) PREP HOTSPOT + SOURCE SF
# ----------------------------
hotspots_sf <- master_sf_m %>%
  dplyr::select(group_id, pollutants, max_n_days, persistence_index_weighted)

sources_sf <- context_facilities %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(sf::st_crs(hotspots_sf)) %>%
  dplyr::mutate(
    source_id = dplyr::row_number(),
    source_type = facility_group,
    source_name = label,
    source_label = paste0(label, "_", source_id)
  )

# ----------------------------
# 2) HIGH-OBSERVATION FLAG
# ----------------------------
if (!("any_high_global" %in% names(df))) {
  hi <- rep(FALSE, nrow(df))
  for (cc in pollutant_cols_existing) {
    x <- df[[cc]]
    thr <- global_thr[[cc]]
    hi <- hi | (is.finite(x) & x > thr)
  }
  df$any_high_global <- hi
}

df <- df %>%
  dplyr::mutate(
    wd = suppressWarnings(as.numeric(.data$wd)),
    ws = suppressWarnings(as.numeric(.data$ws))
  )

# ----------------------------
# 3) HELPER FUNCTIONS
# ----------------------------
wrap_angle_diff <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

bearing_deg <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  (atan2(dx, dy) * 180 / pi + 360) %% 360
}

safe_fisher <- function(a, b, c, d) {
  mat <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
  out <- try(stats::fisher.test(mat), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(list(or = NA_real_, p = NA_real_))
  }
  list(or = unname(out$estimate), p = out$p.value)
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

# ----------------------------
# 4) HOTSPOT × SOURCE DIRECTIONAL METRICS
# ----------------------------
directional_results <- list()
distance_results <- list()

for (g in seq_len(nrow(hotspots_sf))) {

  hotspot_g <- hotspots_sf[g, ]
  gid <- hotspot_g$group_id

  d_to_hotspot <- as.numeric(sf::st_distance(df_sf_m, hotspot_g))
  idx <- which(d_to_hotspot <= match_dist_m)

  if (length(idx) == 0) next

  df_hot <- df[idx, , drop = FALSE]

  hot_xy <- sf::st_coordinates(hotspot_g)
  src_xy <- sf::st_coordinates(sources_sf)
  dist_m <- as.numeric(sf::st_distance(hotspot_g, sources_sf))

  dist_tbl <- tibble::tibble(
    group_id = gid,
    source_id = sources_sf$source_id,
    source_label = sources_sf$source_label,
    source_type = sources_sf$source_type,
    source_name = sources_sf$source_name,
    distance_m = dist_m,
    distance_km = dist_m / 1000
  )

  for (b in buffer_set_m) {
    dist_tbl[[paste0("within_", b, "m")]] <- dist_m <= b
  }

  distance_results[[length(distance_results) + 1]] <- dist_tbl

  for (s in seq_len(nrow(sources_sf))) {

    src_s <- sources_sf[s, ]
    src_xy_s <- src_xy[s, , drop = FALSE]

    src_bearing <- bearing_deg(
      x1 = hot_xy[1, "X"], y1 = hot_xy[1, "Y"],
      x2 = src_xy_s[1, "X"], y2 = src_xy_s[1, "Y"]
    )

    ang_diff <- wrap_angle_diff(df_hot$wd, src_bearing)
    aligned <- is.finite(ang_diff) & ang_diff <= source_sector_halfwidth_deg

    n_total <- nrow(df_hot)
    n_aligned <- sum(aligned, na.rm = TRUE)
    n_not_aligned <- sum(!aligned, na.rm = TRUE)

    high_aligned <- sum(df_hot$any_high_global & aligned, na.rm = TRUE)
    high_not_aligned <- sum(df_hot$any_high_global & !aligned, na.rm = TRUE)

    low_aligned <- sum((!df_hot$any_high_global) & aligned, na.rm = TRUE)
    low_not_aligned <- sum((!df_hot$any_high_global) & !aligned, na.rm = TRUE)

    frac_high_aligned <- if (n_aligned > 0) high_aligned / n_aligned else NA_real_
    frac_high_not_aligned <- if (n_not_aligned > 0) high_not_aligned / n_not_aligned else NA_real_
    overall_frac_high <- mean(df_hot$any_high_global, na.rm = TRUE)

    enrichment_ratio <- if (is.finite(overall_frac_high) && overall_frac_high > 0) {
      frac_high_aligned / overall_frac_high
    } else {
      NA_real_
    }

    contrast_ratio <- if (is.finite(frac_high_not_aligned) && frac_high_not_aligned > 0) {
      frac_high_aligned / frac_high_not_aligned
    } else {
      NA_real_
    }

    ft <- safe_fisher(
      a = high_aligned,
      b = low_aligned,
      c = high_not_aligned,
      d = low_not_aligned
    )

    directional_results[[length(directional_results) + 1]] <- tibble::tibble(
      group_id = gid,
      source_id = src_s$source_id,
      source_label = src_s$source_label,
      source_type = src_s$source_type,
      source_name = src_s$source_name,
      source_bearing_deg = src_bearing,
      source_sector_halfwidth_deg = source_sector_halfwidth_deg,
      n_total = n_total,
      n_aligned = n_aligned,
      n_not_aligned = n_not_aligned,
      high_aligned = high_aligned,
      high_not_aligned = high_not_aligned,
      frac_high_aligned = frac_high_aligned,
      frac_high_not_aligned = frac_high_not_aligned,
      overall_frac_high = overall_frac_high,
      enrichment_ratio = enrichment_ratio,
      contrast_ratio = contrast_ratio,
      fisher_or = ft$or,
      fisher_p = ft$p
    )
  }
}

directional_df <- dplyr::bind_rows(directional_results)
distance_df <- dplyr::bind_rows(distance_results)

# ----------------------------
# 5) ADD HOTSPOT / SOURCE ORDERING
# ----------------------------
hotspot_order <- master %>%
  dplyr::arrange(dplyr::desc(max_n_days), dplyr::desc(persistence_index_weighted)) %>%
  dplyr::pull(group_id)

source_order <- c(
  "Suncor", "Sinclair", "Phillips 66", "WWTF1", "WWTF2", "Woodshop", "Refuel"
)

directional_df <- directional_df %>%
  dplyr::mutate(
    group_id = factor(group_id, levels = hotspot_order),
    source_name = factor(source_name, levels = source_order)
  )

distance_df <- distance_df %>%
  dplyr::mutate(
    group_id = factor(group_id, levels = hotspot_order),
    source_name = factor(source_name, levels = source_order)
  )

# ----------------------------
# 6) COLLAPSE TO PLOT-LEVEL SOURCE CATEGORIES
# ----------------------------
# Keep WWTF1 and WWTF2 separate; collapse only Refuel replicates
directional_plot <- directional_df %>%
  dplyr::mutate(
    plot_source = dplyr::case_when(
      as.character(source_name) == "Refuel" ~ "Refuel",
      TRUE ~ as.character(source_name)
    )
  ) %>%
  dplyr::group_by(group_id, plot_source) %>%
  dplyr::summarise(
    best_enrichment_ratio = safe_max(enrichment_ratio),
    best_contrast_ratio = safe_max(contrast_ratio),
    min_fisher_p = safe_min(fisher_p),
    max_frac_high_aligned = safe_max(frac_high_aligned),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    plot_source = factor(plot_source, levels = source_order),
    group_id = factor(group_id, levels = hotspot_order)
  )

distance_plot <- distance_df %>%
  dplyr::mutate(
    plot_source = dplyr::case_when(
      as.character(source_name) == "Refuel" ~ "Refuel",
      TRUE ~ as.character(source_name)
    )
  ) %>%
  dplyr::group_by(group_id, plot_source) %>%
  dplyr::summarise(
    min_distance_km = safe_min(distance_km),
    any_within_100m = any(within_100m, na.rm = TRUE),
    any_within_250m = any(within_250m, na.rm = TRUE),
    any_within_500m = any(within_500m, na.rm = TRUE),
    any_within_1000m = any(within_1000m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    plot_source = factor(plot_source, levels = source_order),
    group_id = factor(group_id, levels = hotspot_order)
  )

# Keep original bytype outputs too
directional_by_type <- directional_plot %>% dplyr::rename(source_name = plot_source)
distance_by_type <- distance_plot %>% dplyr::rename(source_name = plot_source)

# ----------------------------
# 7) WRITE CSVs
# ----------------------------
readr::write_csv(directional_df,
                 file.path(out_dir, "hotspot_source_directional_metrics.csv"))
readr::write_csv(distance_df,
                 file.path(out_dir, "hotspot_source_distance_metrics.csv"))
readr::write_csv(directional_by_type,
                 file.path(out_dir, "hotspot_source_directional_metrics_bytype.csv"))
readr::write_csv(distance_by_type,
                 file.path(out_dir, "hotspot_source_distance_metrics_bytype.csv"))

# ----------------------------
# 8) FIGURE: ENRICHMENT HEATMAP
# ----------------------------
p_enrich <- directional_plot %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = plot_source,
      y = forcats::fct_rev(group_id),
      fill = best_enrichment_ratio
    )
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = ifelse(is.finite(best_enrichment_ratio),
                     sprintf("%.2f", best_enrichment_ratio),
                     "")
    ),
    size = 2.8
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    trans = "sqrt",
    name = "Enrichment\nratio"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
  ) +
  ggplot2::labs(
    title = "Directional enrichment of high observations by hotspot and candidate source",
    subtitle = paste0("Source considered upwind when wind direction is within ±",
                      source_sector_halfwidth_deg, "° of hotspot-to-source bearing"),
    x = "Candidate source",
    y = "Hotspot group"
  )

ragg::agg_png(
  file.path(out_dir, "FIG_hotspot_source_enrichment_heatmap.png"),
  width = 10.8, height = 7.5, units = "in", res = 300, background = "white"
)
print(p_enrich)
dev.off()

# ----------------------------
# 9) FIGURE: FRACTION HIGH WHEN SOURCE UPWIND
# ----------------------------
p_frac <- directional_plot %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = plot_source,
      y = forcats::fct_rev(group_id),
      fill = max_frac_high_aligned
    )
  ) +
  ggplot2::geom_tile(color = "white", linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = ifelse(is.finite(max_frac_high_aligned),
                     sprintf("%.2f", max_frac_high_aligned),
                     "")
    ),
    size = 2.8
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90",
    name = "Frac. high\nwhen upwind"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
  ) +
  ggplot2::labs(
    title = "Fraction of high observations when each source is upwind",
    x = "Candidate source",
    y = "Hotspot group"
  )

ragg::agg_png(
  file.path(out_dir, "FIG_hotspot_source_frac_high_heatmap.png"),
  width = 10.8, height = 7.5, units = "in", res = 300, background = "white"
)
print(p_frac)
dev.off()

# ----------------------------
# 10) FIGURE: MINIMUM DISTANCE TO SOURCE TYPE
# ----------------------------
p_dist <- distance_plot %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = min_distance_km,
      y = forcats::fct_rev(group_id),
      color = plot_source
    )
  ) +
  ggplot2::geom_point(size = 2.6, alpha = 0.9) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    title = "Minimum distance from each hotspot to candidate source types",
    x = "Distance (km)",
    y = "Hotspot group",
    color = "Candidate source"
  )

ragg::agg_png(
  file.path(out_dir, "FIG_hotspot_source_distance_plot.png"),
  width = 9.5, height = 7.5, units = "in", res = 300, background = "white"
)
print(p_dist)
dev.off()

message("Saved:")
message("  ", file.path(out_dir, "hotspot_source_directional_metrics.csv"))
message("  ", file.path(out_dir, "hotspot_source_distance_metrics.csv"))
message("  ", file.path(out_dir, "hotspot_source_directional_metrics_bytype.csv"))
message("  ", file.path(out_dir, "hotspot_source_distance_metrics_bytype.csv"))
message("  ", file.path(out_dir, "FIG_hotspot_source_enrichment_heatmap.png"))
message("  ", file.path(out_dir, "FIG_hotspot_source_frac_high_heatmap.png"))
message("  ", file.path(out_dir, "FIG_hotspot_source_distance_plot.png"))
