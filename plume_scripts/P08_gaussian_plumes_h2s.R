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

# Kept only so a reader can reproduce the superseded numbers:
mol_m3_air_LEGACY <- 2.7e25 / 6.022e23   # 44.84 mol/m3, 0 C / 1013 hPa

# WWTP effective source / stack heights to evaluate
wwtp_stack_height_m <- 12.2
stack_heights_m     <- c(1, 10, 15, 20, 30, 50)

# Uncertainty knobs
wind_mults      <- c(0.8, 1.0, 1.2)           # ±20%
x_mults         <- c(0.9, 1.0, 1.1)           # ±10% downwind distance
# CROSSWIND GEOMETRY (2026-08-20)
# The inversion previously set the crosswind offset y to 0 for the baseline
# and tested a fixed +-100 m band. Neither matches the geometry the plume
# set was selected under. P06 keeps an observation when the wind direction
# is within 10 degrees of the source-to-receptor bearing, so the receptor
# sits off the plume axis by up to d*sin(10 deg) - 339 m at the 1.95 km
# plume and 747 m at the 4.30 km plume, far outside +-100 m. Setting y = 0
# treats an off-axis concentration as if it were a centreline value, which
# UNDER-estimates Q; the effect runs opposite to (1) and (2) above.
#
# The straight-line source-to-receptor distance is therefore decomposed
# into its along-wind and cross-wind components using the measured angular
# offset theta:  x = d*cos(theta),  y = d*sin(theta). This is the same
# decomposition the P09/P10 simulations use, so the estimator being
# validated is now the estimator that runs.
#
# Sensitivity is now expressed as an uncertainty on the WIND DIRECTION -
# which is what is actually uncertain, HRRR wind being sampled at the
# rounded hour and nearest 3 km grid point - rather than as an arbitrary
# distance in metres:
#   "geom"       theta as measured                       (baseline)
#   "wd+/-5"     theta shifted by 5 degrees
#   "wd+/-10"    theta shifted by 10 degrees, the full acceptance window
#   "centerline" theta forced to 0, i.e. the previous behaviour, retained
#                so the superseded numbers stay reproducible
y_specs <- c("centerline", "wd-10", "wd-5", "geom", "wd+5", "wd+10")

# AVERAGING TIME (2026-08-20)
# The Pasquill-Gifford / Briggs dispersion coefficients are fitted to sampling
# times of order ten minutes, over which wind meander broadens the apparent
# plume. The measurement inverted here is a ONE-SECOND peak, which samples a
# much narrower instantaneous plume. Using a meander-broadened sigma_y with an
# instantaneous concentration overstates the plume's width and therefore the
# inferred source strength. The usual scaling is sigma_y proportional to
# t^p with p about 0.2; sigma_z is left unscaled, since vertical spread is
# driven by turbulence on much shorter timescales than meander.
# This is added as a SENSITIVITY AXIS rather than applied to the baseline: the
# reference sampling time of the Briggs urban curves is itself uncertain
# (quoted variously as 10 min to 1 h), so the honest statement is a range. It
# was previously not bracketed anywhere in the suite, which meant the single
# largest structural uncertainty in the emission estimate went unreported.
AVG_REF_S   <- 600                      # nominal sampling time of the curves
avg_times_s <- c(1, 60, 600, 3600)
sigma_y_avg_mult <- function(t_s, ref_s = AVG_REF_S, p = 0.2) (t_s / ref_s)^p

.theta_eff <- function(spec, theta_deg) {
  if (identical(spec, "centerline")) return(rep(0, length(theta_deg)))
  sh <- if (identical(spec, "geom")) 0 else as.numeric(sub("^wd", "", spec))
  pmin(abs(theta_deg + sh), 89.9)
}

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
    # BUGFIX (2026-08-20): Briggs urban sigma_z E-F is 0.08x(1+0.0015x)^(-1/2); the code
    # had 0.00015, one order of magnitude low, which makes sigma_z ~1.75x too large at 2
    # km and inflates Q by the same factor. Every other Briggs coefficient in these
    # files matches the published table exactly, so this is a transcription slip. E/F
    # never arises from the daytime classifier, so the central estimates are unaffected
    # - but P08's CAT_plus1 scenario shifts D->E, so the published upper stability bound
    # was inflated by ~75%.
    CAT %in% c("E","F") ~ 0.08*X*(1+(0.0015*X))^(-0.5),
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

  # (2) see the physics note at the top: the five-term sum was missing the
  # (z+H+2L) image and stopped at n = +-1. Delegate to the shared
  # convergent implementation.
  out[ok] <- vertical_term(z = z0, H = H0, sigz = s, hpbl = L, reflections = TRUE)
  out
}

# NUMERICAL FLOORS (2026-08-20)
# P09 and P10 guard their estimator with MIN_U, MIN_HPBL, MIN_X_M and
# DENOM_MIN; P08 had none of them, so the inversion whose bias those
# simulations characterise was not the inversion that produced the published
# rates. A 0.2 m/s HRRR wind, for instance, yields a Q two and a half times
# smaller here than under the simulations' 0.5 m/s floor. The four constants
# below are the same values P09/P10 use.
MIN_U      <- 0.5     # m/s   - Gaussian steady-state fails below this
MIN_HPBL   <- 50      # m     - mixing depth floor
MIN_X_M    <- 50      # m     - near-field floor on downwind distance
DENOM_MIN  <- 1e-20   # crosswind x vertical below this is off-plume noise

# Core inversion for a scenario (vectorized over rows)
invert_gaussian <- function(inv_df, reflections, H_m, wind_mult, x_mult, cat_shift, y_spec,
                            sigy_mult = 1) {
  inv_df %>%
    dplyr::mutate(
      CAT_s  = shift_cat(CAT, cat_shift),
      u_ms_s = pmax(u_ms * wind_mult, MIN_U),
      hpbl_m = pmax(hpbl_m, MIN_HPBL, H_m + 10),

      # (3) decompose the straight-line distance into along-wind and
      # cross-wind components at the effective angular offset, then perturb
      # the along-wind distance and recompute the sigmas on it.
      theta_s = .theta_eff(y_spec, theta_deg),
      y_m_s   = dist_m * sin(theta_s * pi / 180),
      x_km_s  = pmax((dist_m / 1000) * cos(theta_s * pi / 180) * x_mult, MIN_X_M / 1000),
      # (H5) sigma_y scaled for sampling time - see the averaging-time note
      sigy_s = sigma_y_pg(CAT_s, x_km_s) * sigy_mult,
      sigz_s = sigma_z_pg(CAT_s, x_km_s),

      # crosswind Gaussian factor
      y_over_sigy = abs(y_m_s) / sigy_s,
      crosswind = exp(-0.5 * (y_m_s^2) / (sigy_s^2)),

      # vertical term
      vertical = vert_term_vec(z = z_m, H = H_m, sigz = sigz_s, hpbl = hpbl_m, reflections = reflections),

      denom = crosswind * vertical,

      # concentration ppm
      dH2S_ppm = dH2S_ppb / 1000,

      # Q in ppm·m^3/s (legacy)
      Q_ppm_m3_s = (dH2S_ppm * u_ms_s * (2 * pi * sigy_s * sigz_s)) / denom,

      # kg/s conversion (legacy)
      # (1) per-row molar density at the measured temperature and pressure
      kg_s = Q_ppm_m3_s * mol_m3 * 1e-6 * (MW_H2S / 1000),

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
      is.finite(denom), denom > DENOM_MIN
    )
}

# WELL-POSEDNESS OF THE CROSSWIND CORRECTION (2026-08-20)
# Dividing by exp(-y^2 / 2 sigma_y^2) is exact, but it amplifies: at
# y = 2 sigma_y the divisor is 0.135 and at y = 3 sigma_y it is 0.011, so
# beyond about two sigma_y the inferred Q is governed by the tail of the
# Gaussian rather than by the measurement, and a 1-degree error in the HRRR
# wind direction moves it by tens of percent. That regime is reachable here:
# the acceptance window admits y up to d*sin(10 deg), which is 1.9 sigma_y at
# the 3.55 km class-D plume and 2.6 sigma_y at 3.90 km. This is a real
# limitation of intercepting a plume off-axis at range, not an artefact - the
# previous y = 0 assumption concealed it by discarding the geometry - so the
# ratio is carried through to the output and flagged rather than hidden.
report_wellposed <- function(res, label = "") {
  if (!"y_over_sigy" %in% names(res) || !nrow(res)) return(invisible(NULL))
  bad <- sum(res$y_over_sigy > 2, na.rm = TRUE)
  message(sprintf("  [CROSSWIND]%s median y/sigma_y = %.2f, max = %.2f; %d of %d rows beyond 2 sigma_y%s",
                  label, stats::median(res$y_over_sigy, na.rm = TRUE),
                  max(res$y_over_sigy, na.rm = TRUE), bad, nrow(res),
                  if (bad > 0) paste0("  <-- inversion is ill-conditioned for those: Q is set by the ",
                                      "Gaussian tail, not by the measurement, so those rows are NOT ",
                                      "constrained by that intercept (in either direction - the error ",
                                      "is not one-sided, so they are not an upper bound either)") else ""))
  invisible(NULL)
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

# Angular offset between the wind direction and the source-to-receptor
# bearing (degrees). P05 writes this as wwtf_wind_angle_diff; recompute it
# from the underlying columns if that is absent.
theta_deg_vec <- pick_first(df0, c("wwtf_wind_angle_diff", "angle_diff_deg"))
if (all(!is.finite(theta_deg_vec))) {
  .wd_v  <- pick_first(df0, c("winddir", "wd"))
  .ref_v <- pick_first(df0, c("wind_from_deg_wwtf"))
  theta_deg_vec <- abs(((.wd_v - .ref_v + 180) %% 360) - 180)
}
# BUGFIX (2026-08-21): a missing angle used to fall back to theta = 0, i.e. to
# the exact centreline geometry - the assumption this chain was rewritten to
# stop making. That is the worst possible default: it is silent, it is the one
# value that maximises the recovered concentration factor, and it biases Q
# DOWNWARDS by a median 20% (up to 80% at the acceptance limit) on precisely
# the rows where the geometry is unknown. P05 now writes the canonical angle and
# P06 admits plumes on the same field, so a retained row with no angle means
# something upstream is broken rather than merely absent. Fail.
.n_bad_theta <- sum(!is.finite(theta_deg_vec))
if (.n_bad_theta > 0) {
  stop(sprintf(paste0("P08: %d of %d retained rows have no wind/bearing angle. The crosswind ",
                      "geometry cannot be built for them, and defaulting to theta = 0 would ",
                      "silently re-impose the centreline assumption (a ~20%% median low bias ",
                      "on those rows). Check that P05 wrote `wwtf_wind_angle_diff` and that ",
                      "P06/P07 carried it through."),
               .n_bad_theta, length(theta_deg_vec)))
}
message(sprintf("[GEOM] |wind - source bearing|: median %.1f deg, max %.1f deg (acceptance window is 10 deg)",
                stats::median(theta_deg_vec), max(theta_deg_vec)))

# Air molar density from the MEASURED temperature and pressure where the
# plume was intercepted; site nominal (25 C, 830 hPa) where absent.
.temp_f  <- pick_first(df0, c("Temperature_F"))
.pres_mb <- pick_first(df0, c("Pressure_mb"))
mol_m3_vec <- molar_density_mol_m3((.temp_f - 32) * 5 / 9, .pres_mb)
.n_nom <- sum(!is.finite(mol_m3_vec) | mol_m3_vec <= 0)
mol_m3_vec[!is.finite(mol_m3_vec) | mol_m3_vec <= 0] <- SITE_MOL_M3
message(sprintf("[UNITS] air molar density: median %.2f mol/m3 (%d of %d rows fell back to the site nominal %.2f; the superseded constant was %.2f)",
                stats::median(mol_m3_vec), .n_nom, length(mol_m3_vec),
                SITE_MOL_M3, mol_m3_air_LEGACY))

CAT_chr <- pick_first_chr(df0, c("CAT", "Stability_Class_simple", "Stability_Class", "stability"))
CAT_chr <- dplyr::recode(CAT_chr, "A-B" = "A", "B-C" = "B", .default = CAT_chr)

inv_base <- tibble::tibble(
  plume_id = if (col_exists(df0, "plume_id")) df0$plume_id else seq_len(nrow(df0)),
  datetime = datetime_vec,
  dH2S_ppb = dH2S_ppb_vec,
  u_ms     = u_ms_vec,
  x_km     = x_km_vec,          # straight-line source-to-receptor distance
  dist_m   = x_km_vec * 1000,
  theta_deg = theta_deg_vec,
  mol_m3   = mol_m3_vec,
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
    y_spec = "geom"
  ),

  tibble::tibble(
    sens_group = "reflections",
    scenario   = c("reflections_TRUE", "reflections_FALSE"),
    reflections = c(TRUE, FALSE),
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_spec = "geom"
  ),

  tibble::tibble(
    sens_group = "stack_height",
    scenario   = paste0("H_", stack_heights_m, "m"),
    reflections = TRUE,
    H_m = stack_heights_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_spec = "geom"
  ),

  tibble::tibble(
    sens_group = "wind_uncertainty",
    scenario   = paste0("wind_x", wind_mults),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = wind_mults,
    x_mult = 1.0,
    cat_shift = 0,
    y_spec = "geom"
  ),

  tibble::tibble(
    sens_group = "stability_CAT_shift",
    scenario   = c("CAT_minus1", "CAT_0", "CAT_plus1"),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = c(-1, 0, 1),
    y_spec = "geom"
  ),

  tibble::tibble(
    sens_group = "x_distance_uncertainty",
    scenario   = paste0("x_mult_", x_mults),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = x_mults,
    cat_shift = 0,
    y_spec = "geom"
  ),

  tibble::tibble(
    sens_group = "crosswind_geometry",
    scenario   = y_specs,
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_spec = y_specs
  ),

  tibble::tibble(
    sens_group = "averaging_time",
    scenario   = paste0("avg_", avg_times_s, "s"),
    reflections = TRUE,
    H_m = wwtp_stack_height_m,
    wind_mult = 1.0,
    x_mult = 1.0,
    cat_shift = 0,
    y_spec = "geom",
    sigy_mult = sigma_y_avg_mult(avg_times_s)
  )
) %>%
  # every other scenario runs at the curves' own nominal sampling time
  dplyr::mutate(sigy_mult = dplyr::coalesce(sigy_mult, 1)) %>%
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
      list(reflections, H_m, wind_mult, x_mult, cat_shift, y_spec, sigy_mult),
      ~invert_gaussian(
        inv_base,
        reflections = ..1,
        H_m = ..2,
        wind_mult = ..3,
        x_mult = ..4,
        cat_shift = ..5,
        y_spec = ..6,
        sigy_mult = ..7
      )
    )
  ) %>%
  dplyr::select(sens_group, scenario, reflections, H_m, wind_mult, x_mult, cat_shift,
                y_spec, sigy_mult, data) %>%
  tidyr::unnest(data) %>%
  dplyr::mutate(
    sens_group = factor(sens_group, levels = levels(scenarios$sens_group)),
    scenario   = factor(scenario,   levels = levels(scenarios$scenario))
  )

report_wellposed(dplyr::filter(results, sens_group == "baseline"), " baseline:")
report_wellposed(results, " all scenarios:")

# The all-scenarios CSV is written AFTER the well-posedness flag is joined on
# (below), not here. R07_plume_inversion.R and R99_manuscript_numbers_report.R
# both read this file and report min/median/max over EVERY row, so writing it
# without `usable` meant the manuscript-numbers report quoted an ill-conditioned
# scenario as the maximum inferred emission rate - the same failure the figures
# had, propagated into the numbers that get walked into the text.

# ----------------------------
# 5) Summaries by scenario (METRIC tons/year ONLY)
# ----------------------------
# WELL-POSEDNESS PER SCENARIO (2026-08-21)
# Found by driving this inversion with the retained plumes' real geometry
# rather than by reading it. Q is divided by exp(-y^2 / 2 sigma_y^2), which
# collapses as the receptor moves off-axis: at y = 3 sigma_y the divisor is
# 0.011 and at y = 4.4 sigma_y it is 6e-5. Two scenario groups drive y/sigma_y
# into that regime by construction -
#   wind-direction offsets (wd+10 and wd-10), which move the receptor, and
#   the SAMPLING-TIME axis, which shrinks sigma_y by up to 3.6x at t = 1 s and
#     therefore inflates y/sigma_y by the same factor
# - and on the four retained plumes they return means of order 10^4 and 10^5
# t/yr. Those are not emission estimates; they are the inversion failing. They
# must not be quoted as the upper end of a sensitivity range.
#
# The rows are NOT dropped, because that would silently redefine the published
# sensitivity. Instead every scenario carries the count and fraction of rows
# beyond 2 sigma_y and a `usable` flag, and a scenario that rests on
# ill-conditioned rows is named in a warning. Report only usable scenarios as
# emission ranges; report the rest as "not constrained by this intercept".
ILL_SIGY <- 2

summ <- results %>%
  dplyr::group_by(sens_group, scenario) %>%
  dplyr::summarise(ci = list(mean_ci(tpy_metric)),
                   n_rows       = dplyr::n(),
                   n_ill        = sum(y_over_sigy > ILL_SIGY, na.rm = TRUE),
                   max_y_sigy   = max(y_over_sigy, na.rm = TRUE),
                   .groups = "drop") %>%
  tidyr::unnest_wider(ci) %>%
  dplyr::rename(
    metric_n    = n,
    metric_mean = mean,
    metric_lci  = lci,
    metric_uci  = uci
  ) %>%
  dplyr::mutate(
    frac_ill   = round(n_ill / pmax(n_rows, 1), 3),
    usable     = n_ill == 0,
    sens_group = factor(sens_group, levels = levels(scenarios$sens_group)),
    scenario   = factor(scenario,   levels = levels(scenarios$scenario))
  )

.bad <- summ[!summ$usable, ]
if (nrow(.bad)) {
  message("  [WELL-POSED] these scenarios contain intercepts beyond ", ILL_SIGY,
          " sigma_y, where a centreline inversion cannot constrain Q - do NOT quote their means as emission rates:")
  for (i in seq_len(nrow(.bad))) {
    message(sprintf("    %-14s %-12s %d of %d rows ill-conditioned (max y/sigma_y %.2f); mean would read %.0f t/yr",
                    as.character(.bad$sens_group[i]), as.character(.bad$scenario[i]),
                    .bad$n_ill[i], .bad$n_rows[i], .bad$max_y_sigy[i], .bad$metric_mean[i]))
  }
  message("  [WELL-POSED] ", sum(summ$usable), " of ", nrow(summ),
          " scenarios are usable; the reported range should be taken from those.")
} else {
  message("  [WELL-POSED] all ", nrow(summ), " scenarios are within ", ILL_SIGY, " sigma_y.")
}

write.csv(
  summ,
  file.path(out_dir, "WWTP_H2S_inversion_summary_mean_ci_METRIC_TPY.csv"),
  row.names = FALSE
)

# FIGURE FIX (2026-08-21): the flag above only existed in the console log and
# the CSV, so the figures still drew ill-conditioned scenarios as ordinary
# points on the same axis as the well-posed ones - and because those scenarios
# carry the largest means, they set the y-range and read as the high end of the
# sensitivity. A reader of the figure alone would quote them. The flag is now
# carried onto every scenario-level panel: unusable scenarios are drawn hollow
# and red, and the caption says what that means.
.scen_flag <- summ %>% dplyr::select(sens_group, scenario, usable, n_ill, frac_ill)
results <- results %>%
  dplyr::left_join(.scen_flag, by = c("sens_group", "scenario"))
stopifnot(!any(is.na(results$usable)))

write.csv(
  results,
  file.path(out_dir, "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv"),
  row.names = FALSE
)
message(sprintf(paste0("[WELL-POSED] all-scenarios CSV written with `usable`, `n_ill` and ",
                       "`y_over_sigy`: %d of %d rows are well-posed. Downstream readers must ",
                       "restrict to usable == TRUE before quoting a range."),
                sum(results$usable), nrow(results)))

WELLPOSED_COLS  <- c(`TRUE` = "black", `FALSE` = "#C0392B")
WELLPOSED_SHAPE <- c(`TRUE` = 16, `FALSE` = 1)
wellposed_caption <- paste0(
  "Red hollow scenarios contain intercepts beyond ", ILL_SIGY, " sigma_y, where a ",
  "centreline inversion cannot constrain Q. Their values are not emission ",
  "estimates and must not be quoted as the high end of the range."
)
.n_unusable <- sum(!summ$usable)

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
p_sum_tpy_metric <- ggplot2::ggplot(
    summ, ggplot2::aes(x = scenario, y = metric_mean, colour = usable, shape = usable)) +
  ggplot2::geom_point(size = 3.0) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = metric_lci, ymax = metric_uci), width = 0.18, linewidth = 0.8) +
  ggplot2::facet_wrap(~sens_group, scales = "free_x", ncol = 1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_colour_manual(values = WELLPOSED_COLS,
                               labels = c(`TRUE` = "well-posed", `FALSE` = "ill-conditioned"),
                               name = NULL, drop = FALSE) +
  ggplot2::scale_shape_manual(values = WELLPOSED_SHAPE,
                              labels = c(`TRUE` = "well-posed", `FALSE` = "ill-conditioned"),
                              name = NULL, drop = FALSE) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Mean emissions (metric tons/year; op_fraction=", op_fraction, ")"),
    title = "Sensitivity analysis of inferred WWTP H\u2082S emissions (metric tons/year)",
    caption = wellposed_caption
  ) +
  base_theme +
  ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = BASE_SIZE - 3))

ggplot2::ggsave(
  filename = file.path(out_dir, "FIG_WWTP_sens_summary_mean_ci_METRIC_TPY.png"),
  plot = p_sum_tpy_metric,
  width = 12, height = 10, units = "in", dpi = FIG_DPI, bg = "white", limitsize = FALSE
)

# B1) Boxplot (ZOOMED)
# The zoom limit is now set from the WELL-POSED rows only. Taking it from all
# rows let the ill-conditioned scenarios - whose values run to 10^4-10^5 t/yr -
# stretch the axis, which compressed every scenario that is actually an
# emission estimate into the bottom of the panel.
.wp_rows <- results$tpy_metric[results$usable]
if (!length(.wp_rows) || all(!is.finite(.wp_rows))) .wp_rows <- results$tpy_metric
y_zoom_max <- as.numeric(stats::quantile(.wp_rows, probs = BOX_ZOOM_Q, na.rm = TRUE))
if (!is.finite(y_zoom_max) || y_zoom_max <= 0) y_zoom_max <- max(.wp_rows, na.rm = TRUE)

# A scenario whose values lie entirely above the zoom limit leaves an EMPTY
# slot on the x axis, which reads as "no data" rather than "off scale". Label
# those explicitly so the zoomed panel cannot be misread as the full set.
.offscale <- results %>%
  dplyr::group_by(sens_group, scenario, usable) %>%
  dplyr::summarise(med = stats::median(tpy_metric, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(is.finite(med), med > y_zoom_max)

p_box_tpy_zoom <- ggplot2::ggplot(
    results, ggplot2::aes(x = scenario, y = tpy_metric, colour = usable)) +
  (if (nrow(.offscale)) ggplot2::geom_text(
     data = .offscale,
     ggplot2::aes(x = scenario, y = y_zoom_max * 0.5,
                  label = ifelse(usable, "off scale", "off scale\n(ill-conditioned)")),
     inherit.aes = FALSE, colour = WELLPOSED_COLS[["FALSE"]],
     size = BASE_SIZE / 4, lineheight = 0.9) else NULL) +
  ggplot2::geom_boxplot(outlier.shape = NA, linewidth = 0.6) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(width = 0.15, height = 0),
    alpha = 0.35, size = 1.4
  ) +
  ggplot2::facet_wrap(~sens_group, scales = "free_x", ncol = 1) +
  ggplot2::coord_cartesian(ylim = c(0, y_zoom_max)) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_colour_manual(values = WELLPOSED_COLS,
                               labels = c(`TRUE` = "well-posed", `FALSE` = "ill-conditioned"),
                               name = NULL, drop = FALSE) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Emissions (metric tons/year; op_fraction=", op_fraction, ")"),
    title = paste0(
      "Distribution of inferred WWTP H\u2082S emissions by scenario (metric t/yr; zoom to ",
      BOX_ZOOM_Q * 100, "th percentile of well-posed scenarios)"
    ),
    caption = wellposed_caption
  ) +
  base_theme +
  ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = BASE_SIZE - 3))

ggplot2::ggsave(
  filename = file.path(out_dir, "FIG_WWTP_sens_distributions_boxplot_METRIC_TPY_ZOOM.png"),
  plot = p_box_tpy_zoom,
  width = 13, height = 11, units = "in", dpi = FIG_DPI, bg = "white", limitsize = FALSE
)

# B2) Boxplot (LOG10)
eps <- 1e-6
p_box_tpy_log <- ggplot2::ggplot(
    results, ggplot2::aes(x = scenario, y = tpy_metric + eps, colour = usable)) +
  ggplot2::geom_boxplot(outlier.alpha = 0.30, linewidth = 0.6) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(width = 0.15, height = 0),
    alpha = 0.30, size = 1.2
  ) +
  ggplot2::facet_wrap(~sens_group, scales = "free_x", ncol = 1) +
  ggplot2::scale_y_log10(labels = scales::comma) +
  ggplot2::scale_colour_manual(values = WELLPOSED_COLS,
                               labels = c(`TRUE` = "well-posed", `FALSE` = "ill-conditioned"),
                               name = NULL, drop = FALSE) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Emissions (metric tons/year; log10 scale; op_fraction=", op_fraction, ")"),
    title = "Distribution of inferred WWTP H\u2082S emissions by scenario (metric t/yr; log10 scale)",
    caption = wellposed_caption
  ) +
  base_theme +
  ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = BASE_SIZE - 3))

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
