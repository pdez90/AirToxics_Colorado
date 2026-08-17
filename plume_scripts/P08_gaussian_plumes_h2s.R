# ==============================================================
# P08  Gaussian plumes H2S
# Auto-split from Suncor_plume.Rmd  (section 8 of 10)
# ==============================================================

#Gaussian plumes H2S

# ============================================================
# H2S GAUSSIAN PLUME INVERSION — FULL SENSITIVITY SUITE (WWTP)
# METRIC TONS/YEAR ONLY
#
# UPDATED FROM SUNCOR TO WWTP
# - Uses WWTP H2S plume centerline points
# - Sets default effective source height to WWTP stack height = 12.2 m
# - Evaluates stack/source heights: 1, 10, 15, 20, 30, 50 m
# - Updates output filenames/titles from Suncor/refinery to WWTP
#
# Requires: centerline_keep (from your WWTP plume QA chunk)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(stringr)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
out_dir <- "/Users/priyanka/Downloads/Suncor/FinalFig"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

z_m <- 1.5                       # instrument height above ground (m)
MW_H2S <- 34.08                  # g/mol
mol_m3_air <- 2.7e25 / 6.022e23  # legacy constant ~44.8 mol/m^3

# WWTP effective source / stack heights to evaluate
wwtp_stack_height_m <- 12.2
stack_heights_m     <- c(1, 10, 15, 20, 30, 50)

# Uncertainty knobs
wind_mults      <- c(0.8, 1.0, 1.2)           # ±20%
x_mults         <- c(0.9, 1.0, 1.1)           # ±10% downwind distance
y_offsets_m     <- c(-100, -50, 0, 50, 100)   # crosswind offset (m)

# OPTIONAL: annualization assumptions
op_fraction <- 1.0

# Figure export defaults
FIG_DPI   <- 600
BASE_SIZE <- 16

# Boxplot visibility controls
BOX_ZOOM_Q <- 0.98

# ----------------------------
# Helpers (safe column picking)
# ----------------------------
col_exists <- function(df, nm) nm %in% names(df)

pick_first <- function(df, nms) {
  for (nm in nms) {
    if (col_exists(df, nm)) return(suppressWarnings(as.numeric(df[[nm]])))
  }
  rep(NA_real_, nrow(df))
}

pick_first_chr <- function(df, nms) {
  for (nm in nms) {
    if (col_exists(df, nm)) return(as.character(df[[nm]]))
  }
  rep(NA_character_, nrow(df))
}

# ============================================================
# UNIT CONVERSIONS (METRIC TONS/YEAR ONLY)
# ============================================================
seconds_per_year <- 365.25 * 24 * 3600

# kg/s -> metric tons/year (t/yr), where 1 metric ton = 1000 kg
kg_s_to_tpy_metric <- function(kg_s, op_fraction = 1.0) {
  kg_s * seconds_per_year * op_fraction / 1000
}

# ----------------------------
# 1) Model helper functions
# ----------------------------

# Pasquill-Gifford sigmas (x in km, returns meters)
sigma_y_pg <- function(CAT, x_km) {
  X <- pmax(x_km, 1e-6) * 1000
  dplyr::case_when(
    CAT %in% c("A","B") ~ 0.32*X*(1+(0.0004*X))^(-0.5),
    CAT == "C"          ~ 0.22*X*(1+(0.0004*X))^(-0.5),
    CAT == "D"          ~ 0.16*X*(1+(0.0004*X))^(-0.5),
    CAT %in% c("E","F") ~ 0.11*X*(1+(0.0004*X))^(-0.5),
    TRUE                ~ NA_real_
  )
}

sigma_z_pg <- function(CAT, x_km) {
  X <- pmax(x_km, 1e-6) * 1000
  dplyr::case_when(
    CAT %in% c("A","B") ~ 0.24*X*(1+(0.001*X))^(0.5),
    CAT == "C"          ~ 0.20*X,
    CAT == "D"          ~ 0.14*X*(1+(0.0003*X))^(-0.5),
    CAT %in% c("E","F") ~ 0.08*X*(1+(0.00015*X))^(-0.5),
    TRUE                ~ NA_real_
  )
}

# Stability perturbation: CAT +/- 1 on A B C D E F
shift_cat <- function(cat, shift = 0) {
  lev <- c("A","B","C","D","E","F")
  idx <- match(cat, lev)
  ifelse(is.na(idx), NA_character_, lev[pmin(pmax(idx + shift, 1), length(lev))])
}

# Vectorized vertical term (direct + optional reflections)
vert_term_vec <- function(z, H, sigz, hpbl, reflections = TRUE) {
  ok <- is.finite(sigz) & sigz > 0 & is.finite(hpbl) & hpbl > 0
  out <- rep(NA_real_, length(sigz))
  if (!any(ok)) return(out)

  z0 <- z
  H0 <- H
  s  <- sigz[ok]
  L  <- hpbl[ok]

  a <- exp(-0.5 * ((z0 - H0)^2) / (s^2))
  if (!reflections) {
    out[ok] <- a
    return(out)
  }

  b <- exp(-0.5 * ((z0 + H0)^2) / (s^2))
  c <- exp(-0.5 * ((z0 + H0 - (2 * L))^2) / (s^2))
  d <- exp(-0.5 * ((z0 - H0 - (2 * L))^2) / (s^2))
  e <- exp(-0.5 * ((z0 - H0 + (2 * L))^2) / (s^2))
  out[ok] <- a + b + c + d + e
  out
}

# Core inversion for a scenario (vectorized over rows)
invert_gaussian <- function(inv_df, reflections, H_m, wind_mult, x_mult, cat_shift, y_m) {
  inv_df %>%
    dplyr::mutate(
      CAT_s  = shift_cat(CAT, cat_shift),
      u_ms_s = u_ms * wind_mult,

      # perturb x and recompute sigmas
      x_km_s = x_km * x_mult,
      sigy_s = sigma_y_pg(CAT_s, x_km_s),
      sigz_s = sigma_z_pg(CAT_s, x_km_s),

      # crosswind Gaussian factor
      crosswind = exp(-0.5 * (y_m^2) / (sigy_s^2)),

      # vertical term
      vertical = vert_term_vec(z = z_m, H = H_m, sigz = sigz_s, hpbl = hpbl_m, reflections = reflections),

      denom = crosswind * vertical,

      # concentration ppm
      dH2S_ppm = dH2S_ppb / 1000,

      # Q in ppm·m^3/s (legacy)
      Q_ppm_m3_s = (dH2S_ppm * u_ms_s * (2 * pi * sigy_s * sigz_s)) / denom,

      # kg/s conversion (legacy)
      kg_s = Q_ppm_m3_s * mol_m3_air * 1e-6 * (MW_H2S / 1000),

      # metric tons/year
      tpy_metric = kg_s_to_tpy_metric(kg_s, op_fraction = op_fraction)
    ) %>%
    dplyr::filter(
      is.finite(tpy_metric), tpy_metric >= 0,
      is.finite(kg_s), kg_s >= 0,
      is.finite(Q_ppm_m3_s),
      is.finite(sigy_s), sigy_s > 0,
      is.finite(sigz_s), sigz_s > 0,
      is.finite(vertical), vertical > 0,
      is.finite(crosswind), crosswind > 0,
      is.finite(denom), denom > 0
    )
}

# Mean + t CI helper
mean_ci <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  if (n < 2) return(tibble(n = n, mean = m, lci = NA_real_, uci = NA_real_))
  tcrit <- qt(0.975, df = n - 1)
  moe <- tcrit * (s / sqrt(n))
  tibble(n = n, mean = m, lci = m - moe, uci = m + moe)
}

# ----------------------------
# 2) Build base inversion inputs from centerline_keep (ROBUST)
# ----------------------------
stopifnot(exists("centerline_keep"))
df0 <- as.data.frame(centerline_keep)

datetime_vec <- if (col_exists(df0, "datetime")) {
  suppressWarnings(as.POSIXct(df0$datetime, tz = "UTC"))
} else {
  suppressWarnings(as.POSIXct(df0$date, tz = "UTC"))
}

dH2S_ppb_vec <- if (col_exists(df0, "dH2S")) {
  suppressWarnings(as.numeric(df0$dH2S))
} else if (col_exists(df0, "dH2S_ppb")) {
  suppressWarnings(as.numeric(df0$dH2S_ppb))
} else {
  stop("centerline_keep must contain dH2S (ppb). I cannot find dH2S or dH2S_ppb.")
}

u_ms_vec <- pick_first(df0, c(
  "windspd_at_peak", "windspd_at_peak_ms",
  "windspd", "wind_speed_ms", "wind_speed", "ws"
))

if (all(!is.finite(u_ms_vec)) && all(c("u10","v10") %in% names(df0))) {
  u_ms_vec <- sqrt(suppressWarnings(as.numeric(df0$u10))^2 +
                     suppressWarnings(as.numeric(df0$v10))^2)
}

# Prefer WWTP distance columns
x_km_vec <- pick_first(df0, c(
  "dist_at_peak_km", "distance_wwtp", "x_km"
))

hpbl_m_vec <- pick_first(df0, c("hpbl", "hpbl_m", "pbl_m"))

CAT_chr <- pick_first_chr(df0, c("CAT", "Stability_Class_simple", "Stability_Class", "stability"))
CAT_chr <- dplyr::recode(CAT_chr, "A-B" = "A", "B-C" = "B", .default = CAT_chr)

inv_base <- tibble::tibble(
  plume_id = if (col_exists(df0, "plume_id")) df0$plume_id else seq_len(nrow(df0)),
  datetime = datetime_vec,
  dH2S_ppb = dH2S_ppb_vec,
  u_ms     = u_ms_vec,
  x_km     = x_km_vec,
  hpbl_m   = hpbl_m_vec,
  CAT      = CAT_chr
) %>%
  dplyr::filter(
    is.finite(datetime),
    is.finite(dH2S_ppb), dH2S_ppb > 0,
    is.finite(u_ms), u_ms > 0,
    is.finite(x_km), x_km > 0,
    is.finite(hpbl_m), hpbl_m > 0,
    CAT %in% c("A","B","C","D","E","F")
  )

stopifnot(nrow(inv_base) > 0)

# ----------------------------
# 3) Scenario grid
# ----------------------------
scenarios <- dplyr::bind_rows(
  tibble::tibble(
    sens_group = "baseline",
    scenario   = paste0("baseline_H_", wwtp_stack_height_m, "m"),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_m = 0
  ),

  tibble::tibble(
    sens_group = "reflections",
    scenario   = c("reflections_TRUE", "reflections_FALSE"),
    reflections = c(TRUE, FALSE),
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_m = 0
  ),

  tibble::tibble(
    sens_group = "stack_height",
    scenario   = paste0("H_", stack_heights_m, "m"),
    reflections = TRUE,
    H_m = stack_heights_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_m = 0
  ),

  tibble::tibble(
    sens_group = "wind_uncertainty",
    scenario   = paste0("wind_x", wind_mults),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = wind_mults,
    x_mult = 1.0,
    cat_shift = 0,
    y_m = 0
  ),

  tibble::tibble(
    sens_group = "stability_CAT_shift",
    scenario   = c("CAT_minus1", "CAT_0", "CAT_plus1"),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = c(-1, 0, 1),
    y_m = 0
  ),

  tibble::tibble(
    sens_group = "x_distance_uncertainty",
    scenario   = paste0("x_mult_", x_mults),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = x_mults,
    cat_shift = 0,
    y_m = 0
  ),

  tibble::tibble(
    sens_group = "crosswind_y_uncertainty",
    scenario   = paste0("y_", ifelse(y_offsets_m >= 0, "+", ""), y_offsets_m, "m"),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_m = y_offsets_m
  )
) %>%
  dplyr::mutate(
    sens_group = factor(sens_group, levels = unique(sens_group)),
    scenario   = factor(scenario,   levels = unique(scenario))
  )

# ----------------------------
# 4) Run inversion across scenarios
# ----------------------------
results <- scenarios %>%
  dplyr::mutate(
    data = purrr::pmap(
      list(reflections, H_m, wind_mult, x_mult, cat_shift, y_m),
      ~invert_gaussian(
        inv_base,
        reflections = ..1,
        H_m = ..2,
        wind_mult = ..3,
        x_mult = ..4,
        cat_shift = ..5,
        y_m = ..6
      )
    )
  ) %>%
  dplyr::select(sens_group, scenario, reflections, H_m, wind_mult, x_mult, cat_shift, y_m, data) %>%
  tidyr::unnest(data) %>%
  dplyr::mutate(
    sens_group = factor(sens_group, levels = levels(scenarios$sens_group)),
    scenario   = factor(scenario,   levels = levels(scenarios$scenario))
  )

write.csv(
  results,
  file.path(out_dir, "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv"),
  row.names = FALSE
)

# ----------------------------
# 5) Summaries by scenario (METRIC tons/year ONLY)
# ----------------------------
summ <- results %>%
  dplyr::group_by(sens_group, scenario) %>%
  dplyr::summarise(ci = list(mean_ci(tpy_metric)), .groups = "drop") %>%
  tidyr::unnest_wider(ci) %>%
  dplyr::rename(
    metric_n    = n,
    metric_mean = mean,
    metric_lci  = lci,
    metric_uci  = uci
  ) %>%
  dplyr::mutate(
    sens_group = factor(sens_group, levels = levels(scenarios$sens_group)),
    scenario   = factor(scenario,   levels = levels(scenarios$scenario))
  )

write.csv(
  summ,
  file.path(out_dir, "WWTP_H2S_inversion_summary_mean_ci_METRIC_TPY.csv"),
  row.names = FALSE
)

# ----------------------------
# 6) FIGURES
# ----------------------------
base_theme <- ggplot2::theme_bw(base_size = BASE_SIZE) +
  ggplot2::theme(
    strip.text   = ggplot2::element_text(face = "bold", size = BASE_SIZE + 1),
    axis.text.x  = ggplot2::element_text(angle = 40, hjust = 1, size = BASE_SIZE - 2),
    axis.text.y  = ggplot2::element_text(size = BASE_SIZE - 2),
    axis.title   = ggplot2::element_text(size = BASE_SIZE),
    plot.title   = ggplot2::element_text(face = "bold"),
    legend.title = ggplot2::element_text(size = BASE_SIZE - 1),
    legend.text  = ggplot2::element_text(size = BASE_SIZE - 2),
    panel.grid.minor = ggplot2::element_blank()
  )

# A) Summary mean + CI
p_sum_tpy_metric <- ggplot2::ggplot(summ, ggplot2::aes(x = scenario, y = metric_mean)) +
  ggplot2::geom_point(size = 3.0) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = metric_lci, ymax = metric_uci), width = 0.18, linewidth = 0.8) +
  ggplot2::facet_wrap(~sens_group, scales = "free_x", ncol = 1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Mean emissions (metric tons/year; op_fraction=", op_fraction, ")"),
    title = "Sensitivity analysis of inferred WWTP H\u2082S emissions (metric tons/year)"
  ) +
  base_theme

ggplot2::ggsave(
  filename = file.path(out_dir, "FIG_WWTP_sens_summary_mean_ci_METRIC_TPY.png"),
  plot = p_sum_tpy_metric,
  width = 12, height = 10, units = "in", dpi = FIG_DPI, bg = "white", limitsize = FALSE
)

# B1) Boxplot (ZOOMED)
y_zoom_max <- as.numeric(stats::quantile(results$tpy_metric, probs = BOX_ZOOM_Q, na.rm = TRUE))
if (!is.finite(y_zoom_max) || y_zoom_max <= 0) y_zoom_max <- max(results$tpy_metric, na.rm = TRUE)

p_box_tpy_zoom <- ggplot2::ggplot(results, ggplot2::aes(x = scenario, y = tpy_metric)) +
  ggplot2::geom_boxplot(outlier.shape = NA, linewidth = 0.6) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(width = 0.15, height = 0),
    alpha = 0.35, size = 1.4
  ) +
  ggplot2::facet_wrap(~sens_group, scales = "free_x", ncol = 1) +
  ggplot2::coord_cartesian(ylim = c(0, y_zoom_max)) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Emissions (metric tons/year; op_fraction=", op_fraction, ")"),
    title = paste0(
      "Distribution of inferred WWTP H\u2082S emissions by scenario (metric t/yr; zoom to ",
      BOX_ZOOM_Q * 100, "th percentile)"
    )
  ) +
  base_theme

ggplot2::ggsave(
  filename = file.path(out_dir, "FIG_WWTP_sens_distributions_boxplot_METRIC_TPY_ZOOM.png"),
  plot = p_box_tpy_zoom,
  width = 13, height = 11, units = "in", dpi = FIG_DPI, bg = "white", limitsize = FALSE
)

# B2) Boxplot (LOG10)
eps <- 1e-6
p_box_tpy_log <- ggplot2::ggplot(results, ggplot2::aes(x = scenario, y = tpy_metric + eps)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.30, linewidth = 0.6) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(width = 0.15, height = 0),
    alpha = 0.30, size = 1.2
  ) +
  ggplot2::facet_wrap(~sens_group, scales = "free_x", ncol = 1) +
  ggplot2::scale_y_log10(labels = scales::comma) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Emissions (metric tons/year; log10 scale; op_fraction=", op_fraction, ")"),
    title = "Distribution of inferred WWTP H\u2082S emissions by scenario (metric t/yr; log10 scale)"
  ) +
  base_theme

ggplot2::ggsave(
  filename = file.path(out_dir, "FIG_WWTP_sens_distributions_boxplot_METRIC_TPY_LOG10.png"),
  plot = p_box_tpy_log,
  width = 13, height = 11, units = "in", dpi = FIG_DPI, bg = "white", limitsize = FALSE
)

# C) Baseline-only per-plume emissions plot
baseline_label <- paste0("baseline_H_", wwtp_stack_height_m, "m")

baseline_rows <- results %>%
  dplyr::filter(sens_group == "baseline", as.character(scenario) == baseline_label) %>%
  dplyr::arrange(datetime)

p_base_time_tpy <- ggplot2::ggplot(baseline_rows, ggplot2::aes(x = datetime, y = tpy_metric, color = x_km)) +
  ggplot2::geom_point(size = 2.7, alpha = 0.9) +
  ggplot2::scale_color_viridis_c(name = "Distance (km)") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(
    x = "Datetime (UTC)",
    y = paste0("Baseline emissions (metric tons/year; op_fraction=", op_fraction, ")"),
    title = paste0(
      "Baseline inferred WWTP H\u2082S emissions per plume ",
      "(H = ", wwtp_stack_height_m, " m; metric tons/year)"
    )
  ) +
  base_theme +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  filename = file.path(out_dir, "FIG_WWTP_baseline_per_plume_timeseries_METRIC_TPY.png"),
  plot = p_base_time_tpy,
  width = 12, height = 7.5, units = "in", dpi = FIG_DPI, bg = "white", limitsize = FALSE
)

# Print key plots
p_sum_tpy_metric
p_box_tpy_zoom
p_box_tpy_log
p_base_time_tpy
