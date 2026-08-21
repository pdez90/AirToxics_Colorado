# ==============================================================
# P09  Simulations real stack height varies
# Auto-split from Suncor_plume.Rmd  (section 9 of 10)
# ==============================================================

#Simulations real stack height varies

# ============================================================
# UPDATED CHUNK — WWTP SOURCE IS TRUE + INVERTED
#  - TRUE: one plume at WWTP, Q_TRUE_TOTAL, height = H_true
#  - INVERT: one plume at WWTP, assumes H_assumed = 12.2 m by default
#  - ONLY mismatch is stack/source height
#  - Uses u10/v10 for wind + stability from cloud cover proxy
#  - Adds diagnostics + "ill-conditioned" guards to avoid infinities
#
# WWTP coordinates:
#   lat = 39.81000446758592
#   lon = -104.95562509611672
#
# Evaluates true/source heights:
#   1, 10, 15, 20, 30, 50 m
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

# SHARED MET HELPERS (2026-08-21): this script used to carry its own copies of
# winddir_from_uv() and determine_stability_class_daytime(), and its own cloud
# preparation. The classifier copies differed only cosmetically from P06's, but
# the CLOUD preparation had genuinely diverged - this file still rescaled cloud
# cover per row with `cloud_raw <= 1.2 ~ 100 * cloud_raw`, which turns a
# near-clear HRRR hour (TCDC 0.8%) into 80% cover and 1.2% into an impossible
# 120%. A simulation that classifies stability differently from the
# observational chain is not characterising that chain. One shared definition.
# Resolved rather than hard-coded, so a reader who clones the repository to any
# other path still gets ONE definition instead of silently falling back to a
# local copy. If none of these resolve, stop - running with a stale private copy
# of the classifier is exactly the failure this file was created to end.
.p00_cand <- c(
  "/Users/priyanka/Downloads/Suncor/rerun_pipeline/plume_scripts/P00_met_helpers.R",
  "P00_met_helpers.R",
  "plume_scripts/P00_met_helpers.R",
  "rerun_pipeline/plume_scripts/P00_met_helpers.R"
)
.p00 <- .p00_cand[file.exists(.p00_cand)]
if (!length(.p00))
  stop("P00_met_helpers.R not found. Looked in:\n  ", paste(.p00_cand, collapse = "\n  "))
message("[MET] shared helpers sourced from: ", .p00[1])
source(.p00[1])
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

# Prevent inversion blow-ups when receptor is essentially off-plume
DENOM_MIN  <- 1e-20
C_EPS_PPM  <- 1e-30

# WWTP source location
SRC_WWTP <- list(
  lat = 39.81000446758592,
  lon = -104.95562509611672
)

Q_TRUE_TOTAL <- 15.0   # kg/s total at WWTP; edit as needed

# True/source heights to test
H_TRUE_RANGE <- c(1, 10, 15, 20, 30, 50)

# Assumed height used in inversion
H_ASSUMED_M <- 12.2

# UPDATED: evaluate 0.5 to 5 km
DISTANCES_KM_SHOW <- seq(0.5, 5.0, by = 0.5)
BEARINGS_DEG      <- seq(0, 330, by = 30)
SAMPLE_SIZE_MET   <- 600

# -------------------------
# Wind + stability helpers
# -------------------------


# -------------------------
# Pasquill-Gifford sigmas
# -------------------------
calculate_sigma_y <- function(stability_class, distance_km) {
  X <- pmax(distance_km * 1000, MIN_X_M)
  if (stability_class %in% c("A", "B"))       0.32 * X * (1 + (0.0004 * X))^(-0.5)
  else if (stability_class == "C")            0.22 * X * (1 + (0.0004 * X))^(-0.5)
  else if (stability_class == "D")            0.16 * X * (1 + (0.0004 * X))^(-0.5)
  else if (stability_class %in% c("E", "F"))  0.11 * X * (1 + (0.0004 * X))^(-0.5)
  else NA_real_
}

calculate_sigma_z <- function(stability_class, distance_km) {
  X <- pmax(distance_km * 1000, MIN_X_M)
  if (stability_class %in% c("A", "B"))       0.24 * X * (1 + (0.001 * X))^(0.5)
  else if (stability_class == "C")            0.20 * X
  else if (stability_class == "D")            0.14 * X * (1 + (0.0003 * X))^(-0.5)
  # BUGFIX (2026-08-20): Briggs urban sigma_z E-F is 0.08x(1+0.0015x)^(-1/2); the code
  # had 0.00015, one order of magnitude low, which makes sigma_z ~1.75x too large at 2
  # km and inflates Q by the same factor. Every other Briggs coefficient in these files
  # matches the published table exactly, so this is a transcription slip. E/F never
  # arises from the daytime classifier, so the central estimates are unaffected - but
  # P08's CAT_plus1 scenario shifts D->E, so the published upper stability bound was
  # inflated by ~75%.
  else if (stability_class %in% c("E", "F"))  0.08 * X * (1 + (0.0015 * X))^(-0.5)
  else NA_real_
}

# -------------------------
# Geo helpers
# -------------------------
haversine_distance_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  lat1_rad <- lat1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

distance_bearing_km_deg <- function(lat_s, lon_s, lat_r, lon_r) {
  lat_s <- as.numeric(lat_s)
  lon_s <- as.numeric(lon_s)
  lat_r <- as.numeric(lat_r)
  lon_r <- as.numeric(lon_r)

  lat1 <- lat_s * pi / 180
  lat2 <- lat_r * pi / 180
  dlon <- (lon_r - lon_s) * pi / 180
  d_km <- haversine_distance_km(lat_s, lon_s, lat_r, lon_r)

  y <- sin(dlon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
  brng <- (atan2(y, x) * 180 / pi + 360) %% 360
  list(dist_km = d_km, bearing_deg = brng)
}

xy_from_wind <- function(dist_km, bearing_deg, winddir_from_deg) {
  wind_toward <- (winddir_from_deg + 180) %% 360
  theta <- (bearing_deg - wind_toward) * pi / 180
  x_m <- dist_km * 1000 * cos(theta)
  y_m <- dist_km * 1000 * sin(theta)
  list(x_m = x_m, y_m = y_m)
}

# -------------------------
# Gaussian plume + inversion
# -------------------------
gaussian_plume_ppm <- function(Q_kg_s, H_m, z_m, x_m, y_m, u_ms, stab, pbl_m,
                               upwind_returns_zero = FALSE) {
  if (!is.finite(Q_kg_s) || !is.finite(H_m) || !is.finite(z_m) ||
      !is.finite(x_m) || !is.finite(y_m) || !is.finite(u_ms) ||
      !is.finite(pbl_m) || is.na(stab)) {
    return(NA_real_)
  }

  if (x_m <= 0) return(if (upwind_returns_zero) 0 else NA_real_)

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
# Met prep from res_met (u10/v10,hpbl,tcdc,lcc)
# -------------------------
prepare_met <- function(res_met, sample_size = 600) {
  df <- as.data.frame(res_met)

  # cloud units asserted from the source, exactly as P06 now does
  .cloud_pct_sim <- cloud_percent_from(df, quiet = TRUE)$pct

  df <- df %>%
    dplyr::mutate(
      u10_ms = as.numeric(u10),
      v10_ms = as.numeric(v10),
      wind_speed_ms = sqrt(u10_ms^2 + v10_ms^2),
      winddir_deg   = winddir_from_uv(u10_ms, v10_ms),
      cloud_cover_percent = .cloud_pct_sim,
      pbl_m = as.numeric(hpbl)
    ) %>%
    dplyr::filter(
      is.finite(wind_speed_ms), wind_speed_ms > 0.1,
      is.finite(winddir_deg),
      is.finite(cloud_cover_percent),
      is.finite(pbl_m), pbl_m > MIN_HPBL
    ) %>%
    dplyr::mutate(
      stab = determine_stability_class_daytime(wind_speed_ms, cloud_cover_percent)
    ) %>%
    dplyr::filter(!is.na(stab))

  if (nrow(df) == 0) stop("No valid met rows after filtering.")
  if (nrow(df) > sample_size) {
    df <- df[sample(seq_len(nrow(df)), sample_size), ]
  }
  df
}

# -------------------------
# Receptor ring around WWTP
# -------------------------
make_receptor_grid <- function(src_lat, src_lon, distances_km, bearings_deg) {
  src_lat <- as.numeric(src_lat)
  src_lon <- as.numeric(src_lon)
  stopifnot(is.finite(src_lat), is.finite(src_lon))

  lat_per_km <- 1 / 110.574
  lon_per_km <- 1 / (111.320 * cos(src_lat * pi / 180))

  expand.grid(distance_km = distances_km, bearing_deg = bearings_deg) %>%
    dplyr::mutate(
      bearing_rad  = bearing_deg * pi / 180,
      receptor_lat = src_lat + distance_km * cos(bearing_rad) * lat_per_km,
      receptor_lon = src_lon + distance_km * sin(bearing_rad) * lon_per_km
    ) %>%
    dplyr::select(-bearing_rad)
}

# -------------------------
# TRUE concentration: ONE plume at WWTP
# -------------------------
one_plume_true_C_wwtp <- function(receptor_lat, receptor_lon,
                                  winddir_from_deg, wind_speed_ms, pbl_m, stab,
                                  H_true_m, Q_true_total = Q_TRUE_TOTAL,
                                  z_m = Z_RECEPTOR) {

  g <- distance_bearing_km_deg(SRC_WWTP$lat, SRC_WWTP$lon, receptor_lat, receptor_lon)
  xy <- xy_from_wind(g$dist_km, g$bearing_deg, winddir_from_deg)

  c <- gaussian_plume_ppm(
    Q_true_total, H_true_m, z_m,
    xy$x_m, xy$y_m, wind_speed_ms, stab, pbl_m,
    upwind_returns_zero = TRUE
  )
  if (!is.finite(c)) return(NA_real_)
  c
}

# -------------------------
# INVERT: ONE plume at WWTP with assumed height
# -------------------------
one_plume_invert_Q_wwtp <- function(C_obs_ppm, receptor_lat, receptor_lon,
                                    winddir_from_deg, wind_speed_ms, pbl_m, stab,
                                    H_assumed_m = H_ASSUMED_M,
                                    z_m = Z_RECEPTOR) {

  g <- distance_bearing_km_deg(SRC_WWTP$lat, SRC_WWTP$lon, receptor_lat, receptor_lon)
  xy <- xy_from_wind(g$dist_km, g$bearing_deg, winddir_from_deg)

  estimate_Q_from_C(
    C_obs_ppm, H_assumed_m, z_m,
    xy$x_m, xy$y_m, wind_speed_ms, stab, pbl_m
  )
}

# ============================================================
# MAIN SIM: error vs TRUE stack height (0.5–5 km)
# ============================================================
simulate_error_vs_true_stack_height_wwtp <- function(res_met,
                                                     distances_km = DISTANCES_KM_SHOW,
                                                     bearings_deg = BEARINGS_DEG,
                                                     H_true_range = H_TRUE_RANGE,
                                                     H_assumed_m  = H_ASSUMED_M,
                                                     sample_size  = SAMPLE_SIZE_MET) {

  met <- prepare_met(res_met, sample_size = sample_size)
  rec <- make_receptor_grid(SRC_WWTP$lat, SRC_WWTP$lon, distances_km, bearings_deg)

  base <- rec %>%
    tidyr::crossing(met_id = seq_len(nrow(met))) %>%
    dplyr::mutate(
      wind_speed_ms = met$wind_speed_ms[met_id],
      winddir_deg   = met$winddir_deg[met_id],
      pbl_m         = met$pbl_m[met_id],
      stab          = met$stab[met_id]
    )

  all_out <- vector("list", length(H_true_range))

  for (ii in seq_along(H_true_range)) {
    H_true_m <- H_true_range[ii]

    C_true <- mapply(
      one_plume_true_C_wwtp,
      receptor_lat     = base$receptor_lat,
      receptor_lon     = base$receptor_lon,
      winddir_from_deg = base$winddir_deg,
      wind_speed_ms    = base$wind_speed_ms,
      pbl_m            = base$pbl_m,
      stab             = base$stab,
      H_true_m         = H_true_m
    )

    Q_hat <- mapply(
      one_plume_invert_Q_wwtp,
      C_obs_ppm        = C_true,
      receptor_lat     = base$receptor_lat,
      receptor_lon     = base$receptor_lon,
      winddir_from_deg = base$winddir_deg,
      wind_speed_ms    = base$wind_speed_ms,
      pbl_m            = base$pbl_m,
      stab             = base$stab,
      MoreArgs         = list(H_assumed_m = H_assumed_m)
    )

    df <- base %>%
      dplyr::mutate(
        H_true_m   = H_true_m,
        C_true_ppm = as.numeric(C_true),
        Q_hat_kg_s = as.numeric(Q_hat)
      ) %>%
      dplyr::filter(is.finite(C_true_ppm), is.finite(Q_hat_kg_s)) %>%
      dplyr::mutate(error_pct = 100 * (Q_hat_kg_s - Q_TRUE_TOTAL) / Q_TRUE_TOTAL)

    all_out[[ii]] <- df
  }

  out <- dplyr::bind_rows(all_out)
  if (nrow(out) == 0) stop("No valid rows after filtering (likely DENOM_MIN too strict).")

  message(
    "Diagnostics: met n=", nrow(met),
    " | base rows=", nrow(base),
    " | kept rows=", nrow(out),
    " | kept fraction=", signif(nrow(out) / (nrow(base) * length(H_true_range)), 3)
  )

  stack_summary <- out %>%
    dplyr::group_by(distance_km, H_true_m) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_error   = mean(error_pct),
      sd_error     = sd(error_pct),
      median_error = median(error_pct),
      .groups = "drop"
    ) %>%
    dplyr::mutate(distance_km = as.numeric(distance_km))

  if (any(H_true_range == H_assumed_m)) {
    check_assumed <- stack_summary %>%
      dplyr::filter(H_true_m == H_assumed_m) %>%
      dplyr::summarise(
        max_abs_mean   = max(abs(mean_error), na.rm = TRUE),
        max_abs_median = max(abs(median_error), na.rm = TRUE)
      )
    message(
      "Sanity @ H_true=H_assumed=", H_assumed_m,
      " m | max |mean_error|=", signif(check_assumed$max_abs_mean, 4),
      " | max |median_error|=", signif(check_assumed$max_abs_median, 4)
    )
  }

  p <- ggplot2::ggplot(
    stack_summary,
    ggplot2::aes(x = H_true_m, y = mean_error, color = factor(distance_km))
  ) +
    ggplot2::geom_line(linewidth = 1.05) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_error - sd_error, ymax = mean_error + sd_error),
      width = 1.2, alpha = 0.6
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
    ggplot2::geom_vline(xintercept = H_assumed_m, linetype = "dashed", alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = sort(unique(H_TRUE_RANGE))) +
    ggplot2::labs(
      title = "Emission-rate error vs true WWTP source height",
      subtitle = paste0(
        "True source at WWTP; inversion assumes H = ", H_assumed_m,
        " m; error bars show ±1 SD across meteorological conditions"
      ),
      x = "True source height (m)",
      y = "Error in emission-rate estimate (%)",
      color = "Distance from\nWWTP (km)"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  list(raw = out, summary = stack_summary, plot = p, met = met)
}

# ============================================================
# USAGE
# ============================================================
load("/Users/priyanka/Downloads/Suncor/mobile_hrrr.RData")

res_met <- res[, c("u10", "v10", "hpbl", "tcdc", "lcc")]
res_met <- res_met[complete.cases(res_met), ]
res_met <- res_met[!duplicated(res_met), ]

wwtp_stack <- simulate_error_vs_true_stack_height_wwtp(
  res_met,
  distances_km = seq(0.5, 5, by = 0.5),
  bearings_deg = seq(0, 330, by = 30),
  H_true_range = c(1, 10, 15, 20, 30, 50),
  H_assumed_m  = 12.2,
  sample_size  = 600
)

print(wwtp_stack$plot)

ggplot2::ggsave(
  "/Users/priyanka/Downloads/Suncor/error_vs_true_stack_height_WWTP_0p5to5km.png",
  wwtp_stack$plot,
  width = 10.5, height = 6.8, dpi = 300
)

write.csv(
  wwtp_stack$summary,
  "/Users/priyanka/Downloads/Suncor/error_vs_true_stack_height_WWTP_0p5to5km_summary.csv",
  row.names = FALSE
)
