# ==============================================================
# P10  Simulations cross wind distance =0
# Auto-split from Suncor_plume.Rmd  (section 10 of 10)
# ==============================================================

#Simulations cross wind distance =0

# ============================================================
# SIMULATION: Crosswind-mismatch error vs distance (WWTP)
#  - TRUE world: receptor is OFF centerline by y_true (25–500 m)
#  - INVERSION assumes crosswind = 0 m (i.e., receptor on centerline)
#  - Single source at WWTP (so ONLY crosswind mismatch drives error)
#  - Atmospheric variability from met (u10/v10 -> wind speed;
#    stability from cloud proxy)
#  - Output: summary table + figure (lines + points + ±1 SD)
#
# UPDATED FOR WWTP
#   WWTP coordinates: 39.81000446758592, -104.95562509611672
#   True and assumed source height default: 12.2 m
# ============================================================

# --------------------------------------------------------------------------
# UNITS OF THE OUTPUT (read before quoting these numbers)
#   error_pct    = 100 * (Q_hat - Q_TRUE_TOTAL) / Q_TRUE_TOTAL      [PERCENT]
#   mean_error / sd_error / median_error in the saved *_summary.csv are the
#   mean / sd / median of error_pct, so they are ALREADY IN PERCENT.
#   DO NOT multiply by 100 again when quoting them.
#
#   Sanity bound: the inversion recovers a NON-NEGATIVE source strength, so
#   Q_hat >= 0 and therefore error_pct >= -100% by construction. Any quoted
#   value below -100% is impossible and indicates a unit error.
# --------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
set.seed(42)  # reproducible met sampling (sample_size = 600)

# -------------------------
# SETTINGS (EDIT)
# -------------------------
# ---------------------------------------------------------------
# PLUME PHYSICS CORRECTIONS (2026-08-20)
# Applied identically in P08_gaussian_plumes_h2s.R,
# P09_simulations_real_stack_height_varies.R,
# P10_simulations_cross_wind_distance_0.R, 46_min_detectable_rate.R and
# 65_plume_cadence_sensitivity.R. If you change one, change all five.
#
# (1) MOLAR DENSITY OF AIR. The old constant 2.7e25/6.022e23 = 44.84 mol/m3
#     is Loschmidt's number, i.e. dry air at 0 C and 1013 hPa. These
#     measurements are made at about 1600 m, where air is a third less
#     dense. A ppb is a mixing ratio and carries no mass without a stated
#     air density, so the sea-level value inflated every inferred emission
#     rate by 34%. The site basis used here (25 C, 830 hPa -> 33.48
#     mol/m3) is the same one 18_...census_block_level_stats.R uses to
#     convert AirToxScreen ug/m3 to ppb and the same one the benzene
#     inhalation unit risks are expressed on, so the emissions and the
#     exposure halves of the paper now share a standard state.
#
# (2) VERTICAL REFLECTION SERIES. The old five-term sum was
#       (z-H), (z+H), (z+H-2L), (z-H-2L), (z-H+2L)
#     which is the n = -1, 0, +1 expansion with the (z+H+2L) image MISSING,
#     and it stopped at n = +-1 however large sigma_z had grown. Q is
#     proportional to 1/V, so an under-counted V inflates Q: at PBL 1000 m
#     the error is under 3%, but at PBL 300 m it reaches +57% for the
#     class B plume at 1.95 km and +65% for the class C plume at 4.3 km.
#     The sum is now carried to convergence (verified against a 4000-term
#     expansion: agreement better than 0.001% at every sigma_z/L tested),
#     and once sigma_z exceeds 1.6 L the plume is uniformly mixed through
#     the boundary layer, where the closed form V = sqrt(2*pi)*sigma_z/L
#     applies - the standard treatment, equivalent to
#     C = Q / (sqrt(2*pi) * u * sigma_y * L). The switch is continuous.
#
# (3) CROSSWIND OFFSET. See the geometry note in P08: the receptor is not
#     on the plume centreline, because the acceptance window admits a
#     wind-to-source bearing difference of up to 10 degrees.
# ---------------------------------------------------------------
R_GAS      <- 8.314462618   # J/(mol K)
SITE_T_C   <- 25            # nominal site air temperature
SITE_P_HPA <- 830           # nominal site pressure (~1600 m elevation)
molar_density_mol_m3 <- function(T_C = SITE_T_C, P_hPa = SITE_P_HPA) {
  (P_hPa * 100) / (R_GAS * (T_C + 273.15))
}
SITE_MOL_M3 <- molar_density_mol_m3()      # 33.48 mol/m3 (was 44.84)

WELL_MIXED_RATIO <- 1.6     # sigma_z / L above which the plume is uniform
VERT_N_IMAGES    <- 20      # images per side; later terms are below 1e-100

vertical_term <- function(z, H, sigz, hpbl, reflections = TRUE) {
  out <- rep(NA_real_, length(sigz))
  ok  <- is.finite(sigz) & sigz > 0 & is.finite(hpbl) & hpbl > 0
  if (!any(ok)) return(out)
  s <- sigz[ok]
  L <- rep_len(hpbl, length(sigz))[ok]
  v <- exp(-0.5 * ((z - H) / s)^2)
  if (!reflections) { out[ok] <- v; return(out) }
  v <- v + exp(-0.5 * ((z + H) / s)^2)
  for (n in seq_len(VERT_N_IMAGES)) {
    v <- v +
      exp(-0.5 * ((z - H + 2 * n * L) / s)^2) +
      exp(-0.5 * ((z + H + 2 * n * L) / s)^2) +
      exp(-0.5 * ((z - H - 2 * n * L) / s)^2) +
      exp(-0.5 * ((z + H - 2 * n * L) / s)^2)
  }
  wm <- s > WELL_MIXED_RATIO * L
  if (any(wm)) v[wm] <- sqrt(2 * pi) * s[wm] / L[wm]
  out[ok] <- v
  out
}

MW_H2S     <- 34.08
Z_RECEPTOR <- 1.5
MIN_U      <- 0.5
MIN_HPBL   <- 50
MIN_X_M    <- 50
C_EPS_PPM  <- 1e-30
DENOM_MIN  <- 1e-20

# WWTP source location
SRC_WWTP <- list(
  lat = 39.81000446758592,
  lon = -104.95562509611672
)

Q_TRUE_TOTAL <- 15      # kg/s (single equivalent source at WWTP)
H_TRUE_M     <- 12.2    # TRUE source height (m)
H_ASSUMED_M  <- 12.2    # assumed source height (m)

DISTANCES_KM    <- seq(0.5, 5.0, by = 0.5)
Y_TRUE_M        <- c(25, 50, 100, 200, 300, 500)
SAMPLE_SIZE_MET <- 600

# -------------------------
# Stability class (daytime) using cloud cover proxy
# -------------------------
determine_stability_class_daytime <- function(wind_speed_ms, cloud_cover_percent) {
  windspd <- as.numeric(wind_speed_ms)
  cc      <- as.numeric(cloud_cover_percent)
  cc[is.nan(cc)] <- NA_real_

  Insolation <- dplyr::case_when(
    is.na(cc)          ~ NA_character_,
    cc <= 25           ~ "strong",
    cc > 25 & cc <= 75 ~ "moderate",
    cc > 75            ~ "slight",
    TRUE               ~ NA_character_
  )

  sc <- dplyr::case_when(
    is.na(windspd) | is.na(Insolation) ~ NA_character_,

    windspd <= 2 & Insolation == "strong"   ~ "A",
    windspd <= 2 & Insolation == "moderate" ~ "A-B",
    windspd <= 2 & Insolation == "slight"   ~ "B",

    windspd > 2 & windspd <= 3 & Insolation == "strong"   ~ "A-B",
    windspd > 2 & windspd <= 3 & Insolation == "moderate" ~ "B",
    windspd > 2 & windspd <= 3 & Insolation == "slight"   ~ "C",

    windspd > 3 & windspd <= 5 & Insolation == "strong"   ~ "B",
    windspd > 3 & windspd <= 5 & Insolation == "moderate" ~ "B-C",
    windspd > 3 & windspd <= 5 & Insolation == "slight"   ~ "C",

    windspd > 5 & windspd <= 6 & Insolation == "strong"   ~ "C",
    windspd > 5 & windspd <= 6 & Insolation != "strong"   ~ "D",

    windspd > 6 & Insolation == "strong"                  ~ "C",
    windspd > 6 & Insolation != "strong"                  ~ "D",

    TRUE ~ NA_character_
  )

  dplyr::recode(sc, "A-B" = "A", "B-C" = "B", .default = sc)
}

# -------------------------
# Pasquill-Gifford sigmas
# -------------------------
calculate_sigma_y <- function(stability_class, distance_km) {
  X <- pmax(distance_km * 1000, MIN_X_M)
  if (stability_class %in% c("A","B"))       0.32 * X * (1 + (0.0004 * X))^(-0.5)
  else if (stability_class == "C")           0.22 * X * (1 + (0.0004 * X))^(-0.5)
  else if (stability_class == "D")           0.16 * X * (1 + (0.0004 * X))^(-0.5)
  else if (stability_class %in% c("E","F"))  0.11 * X * (1 + (0.0004 * X))^(-0.5)
  else NA_real_
}

calculate_sigma_z <- function(stability_class, distance_km) {
  X <- pmax(distance_km * 1000, MIN_X_M)
  if (stability_class %in% c("A","B"))       0.24 * X * (1 + (0.001 * X))^(0.5)
  else if (stability_class == "C")           0.20 * X
  else if (stability_class == "D")           0.14 * X * (1 + (0.0003 * X))^(-0.5)
  # BUGFIX (2026-08-20): Briggs urban sigma_z E-F is 0.08x(1+0.0015x)^(-1/2); the code had 0.00015, one order of magnitude low, which makes sigma_z ~1.75x too large at 2 km and inflates Q by the same factor. Every other Briggs coefficient in these files matches the published table exactly, so this is a transcription slip. E/F never arises from the daytime classifier, so the central estimates are unaffected - but P08's CAT_plus1 scenario shifts D->E, so the published upper stability bound was inflated by ~75%.
  else if (stability_class %in% c("E","F"))  0.08 * X * (1 + (0.0015 * X))^(-0.5)
  else NA_real_
}

# -------------------------
# Gaussian plume (ppm) + inversion
# -------------------------
gaussian_plume_ppm <- function(Q_kg_s, H_m, z_m, x_m, y_m, u_ms, stab, pbl_m) {
  if (!is.finite(Q_kg_s) || !is.finite(H_m) || !is.finite(z_m) ||
      !is.finite(x_m) || !is.finite(y_m) || !is.finite(u_ms) ||
      !is.finite(pbl_m) || is.na(stab)) return(NA_real_)

  if (x_m <= 0) return(NA_real_)

  U <- max(u_ms, MIN_U)
  L <- max(pbl_m, MIN_HPBL, H_m + 10)

  dist_km <- x_m / 1000
  sy <- calculate_sigma_y(stab, dist_km)
  sz <- calculate_sigma_z(stab, dist_km)
  if (!is.finite(sy) || !is.finite(sz) || sy <= 0 || sz <= 0) return(NA_real_)

  crosswind_term <- exp(-0.5 * (y_m^2) / (sy^2))

  zt <- vertical_term(z = z_m, H = H_m, sigz = sz, hpbl = L)   # (2)

  unit_conversion <- SITE_MOL_M3 / 1e6 * MW_H2S / 1000   # (1) site air density
  (Q_kg_s / U) * (1 / (2 * pi * sy * sz)) * crosswind_term * zt / unit_conversion
}

estimate_Q_from_C <- function(C_ppm, H_m, z_m, x_m, y_m, u_ms, stab, pbl_m) {
  if (!is.finite(C_ppm)) return(NA_real_)
  if (x_m <= 0) return(NA_real_)

  C_use <- max(C_ppm, C_EPS_PPM)

  U <- max(u_ms, MIN_U)
  L <- max(pbl_m, MIN_HPBL, H_m + 10)

  dist_km <- x_m / 1000
  sy <- calculate_sigma_y(stab, dist_km)
  sz <- calculate_sigma_z(stab, dist_km)
  if (!is.finite(sy) || !is.finite(sz) || sy <= 0 || sz <= 0) return(NA_real_)

  crosswind_term <- exp(-0.5 * (y_m^2) / (sy^2))
  zt <- vertical_term(z = z_m, H = H_m, sigz = sz, hpbl = L)   # (2)

  denom <- crosswind_term * zt
  if (!is.finite(denom) || denom < DENOM_MIN) return(NA_real_)

  unit_conversion <- SITE_MOL_M3 / 1e6 * MW_H2S / 1000   # (1) site air density
  C_use * U * (2 * pi * sy * sz) * unit_conversion / denom
}

# -------------------------
# Prepare met from res_met (u10/v10,hpbl,tcdc,lcc)
# -------------------------
prepare_met <- function(res_met, sample_size = 600) {
  df <- as.data.frame(res_met)

  df <- df %>%
    dplyr::mutate(
      u10_ms = as.numeric(u10),
      v10_ms = as.numeric(v10),
      wind_speed_ms = sqrt(u10_ms^2 + v10_ms^2),
      cloud_raw = dplyr::if_else(
        is.finite(as.numeric(tcdc)),
        as.numeric(tcdc),
        as.numeric(lcc)
      ),
      cloud_cover_percent = dplyr::case_when(
        !is.finite(cloud_raw) ~ NA_real_,
        cloud_raw <= 1.2      ~ 100 * cloud_raw,
        TRUE                  ~ cloud_raw
      ),
      pbl_m = as.numeric(hpbl)
    ) %>%
    dplyr::filter(
      is.finite(wind_speed_ms), wind_speed_ms > 0.1,
      is.finite(cloud_cover_percent),
      is.finite(pbl_m), pbl_m > MIN_HPBL
    ) %>%
    dplyr::mutate(
      stab = determine_stability_class_daytime(wind_speed_ms, cloud_cover_percent)
    ) %>%
    dplyr::filter(!is.na(stab))

  if (nrow(df) == 0) stop("No valid met rows after filtering.")
  if (nrow(df) > sample_size) df <- df[sample(seq_len(nrow(df)), sample_size), ]
  df
}

# ============================================================
# MAIN SIM: crosswind mismatch curves vs distance (WWTP)
# ============================================================
simulate_error_vs_distance_crosswind_mismatch_wwtp <- function(
    res_met,
    distances_km = DISTANCES_KM,
    y_true_m_vec = Y_TRUE_M,
    H_true_m = H_TRUE_M,
    H_assumed_m = H_ASSUMED_M,
    sample_size = SAMPLE_SIZE_MET) {

  met <- prepare_met(res_met, sample_size = sample_size)

  grid <- expand.grid(distance_km = distances_km, y_true_m = y_true_m_vec) %>%
    dplyr::mutate(
      distance_km = as.numeric(distance_km),
      y_true_m    = as.numeric(y_true_m)
    )

  base <- grid %>%
    tidyr::crossing(met_id = seq_len(nrow(met))) %>%
    dplyr::mutate(
      u_ms     = met$wind_speed_ms[met_id],
      pbl_m    = met$pbl_m[met_id],
      stab     = met$stab[met_id],
      x_m_true = distance_km * 1000,
      y_m_true = y_true_m,
      x_m_inv  = distance_km * 1000,
      y_m_inv  = 0
    )

  # TRUE concentration at off-centerline location
  C_true <- mapply(
    gaussian_plume_ppm,
    Q_kg_s = Q_TRUE_TOTAL,
    H_m    = H_true_m,
    z_m    = Z_RECEPTOR,
    x_m    = base$x_m_true,
    y_m    = base$y_m_true,
    u_ms   = base$u_ms,
    stab   = base$stab,
    pbl_m  = base$pbl_m
  )

  # INVERT Q using WRONG assumption y=0
  Q_hat <- mapply(
    estimate_Q_from_C,
    C_ppm = C_true,
    H_m   = H_assumed_m,
    z_m   = Z_RECEPTOR,
    x_m   = base$x_m_inv,
    y_m   = base$y_m_inv,
    u_ms  = base$u_ms,
    stab  = base$stab,
    pbl_m = base$pbl_m
  )

  out <- base %>%
    dplyr::mutate(
      C_true_ppm = as.numeric(C_true),
      Q_hat_kg_s = as.numeric(Q_hat)
    )

  message(
    "Diagnostics: met n=", nrow(met),
    " | total cases=", nrow(out),
    " | finite C_true=", sum(is.finite(out$C_true_ppm)),
    " | finite Q_hat=", sum(is.finite(out$Q_hat_kg_s))
  )

  out2 <- out %>%
    dplyr::filter(is.finite(C_true_ppm), is.finite(Q_hat_kg_s)) %>%
    dplyr::mutate(
      error_pct = 100 * (Q_hat_kg_s - Q_TRUE_TOTAL) / Q_TRUE_TOTAL
    )

  if (nrow(out2) == 0) stop("No valid rows after filtering; check met filters or constants.")

  summary <- out2 %>%
    dplyr::group_by(distance_km, y_true_m) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_error   = mean(error_pct),
      sd_error     = sd(error_pct),
      median_error = median(error_pct),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(
    summary,
    ggplot2::aes(x = distance_km, y = mean_error, color = factor(y_true_m))
  ) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_error - sd_error, ymax = mean_error + sd_error),
      width = 0.08, alpha = 0.6
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
    ggplot2::labs(
      title = "Emission-rate error vs distance with crosswind mismatch (WWTP)",
      subtitle = sprintf(
        "TRUE: crosswind offset y = 25–500 m; INVERT assumes y = 0 m | Q_true = %.1f kg/s | H_true = %.1f m, H_assumed = %.1f m | Error bars: ±1 SD across meteorology",
        Q_TRUE_TOTAL, H_true_m, H_assumed_m
      ),
      x = "Distance from WWTP (km)",
      y = "Error in emission-rate estimate (%)",
      color = "Crosswind\noffset (m)"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  list(raw = out2, summary = summary, plot = p, met = met)
}

# ============================================================
# USAGE
# ============================================================
load("/Users/priyanka/Downloads/Suncor/mobile_hrrr.RData")

res_met <- res[, c("u10", "v10", "hpbl", "tcdc", "lcc")]
res_met <- res_met[complete.cases(res_met), ]
res_met <- res_met[!duplicated(res_met), ]

cw_res_wwtp <- simulate_error_vs_distance_crosswind_mismatch_wwtp(
  res_met,
  distances_km = seq(0.5, 5.0, by = 0.5),
  y_true_m_vec = c(25, 50, 100, 200, 300, 500),
  H_true_m = 12.2,
  H_assumed_m = 12.2,
  sample_size = 600
)

print(cw_res_wwtp$plot)

ggsave(
  "/Users/priyanka/Downloads/Suncor/error_vs_distance_crosswind_mismatch_WWTP.png",
  cw_res_wwtp$plot,
  width = 9.5, height = 6.0, dpi = 300
)

write.csv(
  cw_res_wwtp$summary,
  "/Users/priyanka/Downloads/Suncor/error_vs_distance_crosswind_mismatch_WWTP_summary.csv",
  row.names = FALSE
)
