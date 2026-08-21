# ==============================================================
# P06  Distance from WWTF & Stability class
# Auto-split from Suncor_plume.Rmd  (section 6 of 10)
# ==============================================================

#Distance from WWTF & Stability class

# ============================================================
# Stability classes consistent with simulations (DAYTIME ONLY)
# - Uses cloud cover proxy (tcdc preferred, else lcc)
# - Uses windspd if available; else computes from u10/v10
# - Produces Stability_Class and Stability_Class_simple
# - UPDATED for WWTP:
#     * source = Robert W. Hite WWTP
#     * distance filter = >0.5 km and <=5 km
#     * wind alignment uses wind_from_deg_wwtf
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(geosphere)
})

# SHARED HELPERS (2026-08-21): winddir_from_uv() and
# determine_stability_class_daytime() used to be defined here AND again in
# P09 and P10. The three copies of the classifier differed only cosmetically,
# but the cloud-cover preparation feeding it had genuinely diverged, so the
# simulations were no longer exercising the algorithm this script runs. One
# definition now, sourced by all three.
# Repository-relative, in the order the pipeline is actually run from. No
# absolute path: a public reproducibility repository should not need one, and a
# private one that happens to resolve on the author's machine is exactly how a
# stale local copy of the classifier gets sourced without anyone noticing.
# Set SUNCOR_ROOT if you run these from somewhere else entirely. If nothing
# resolves, stop - do not carry on with no shared definition.
.p00_cand <- c(
  "P00_met_helpers.R",                              # run from plume_scripts/
  "plume_scripts/P00_met_helpers.R",                # run from the repository root
  "rerun_pipeline/plume_scripts/P00_met_helpers.R", # run from the working folder
  file.path(Sys.getenv("SUNCOR_ROOT", "."), "plume_scripts", "P00_met_helpers.R")
)
.p00 <- .p00_cand[file.exists(.p00_cand)]
if (!length(.p00))
  stop("P00_met_helpers.R not found. Looked in:\n  ", paste(.p00_cand, collapse = "\n  "))
message("[MET] shared helpers sourced from: ", .p00[1])
source(.p00[1])

# ============================================================
# Load data
# ============================================================
load("/Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf.RData")

if (!exists("res")) {
  stop("Expected object `res` in mobile_hrrr_windfromwwtf.RData")
}

# ============================================================
# WWTP coordinates + distance
# ============================================================
wwtp <- c(lon = -104.95562509611672, lat = 39.81000446758592)

res <- res %>%
  dplyr::mutate(
    distance_wwtp = geosphere::distHaversine(
      cbind(Longitude, Latitude),
      matrix(c(wwtp["lon"], wwtp["lat"]), nrow = 1)
    ) / 1000
  )

# ============================================================
# Met inputs for stability
# ============================================================
# BUGFIX (2026-08-21): the two column fallbacks below were written with
# dplyr::case_when, which evaluates EVERY right-hand side regardless of which
# condition is true. Verified in R: with `windspd` present but `u10`/`v10`
# absent, case_when still evaluates `sqrt(u10^2 + v10^2)` and aborts with
# "object 'u10' not found" - and symmetrically with only u10/v10 present. So
# the fallback worked only when ALL the columns were present, i.e. exactly when
# no fallback was needed. Choose the source once, outside the row context.
.n <- nrow(res)
if ("windspd" %in% names(res)) {
  .ws_src <- "windspd"; .ws <- as.numeric(res$windspd)
} else if (all(c("u10", "v10") %in% names(res))) {
  .ws_src <- "sqrt(u10^2 + v10^2)"; .ws <- sqrt(as.numeric(res$u10)^2 + as.numeric(res$v10)^2)
} else {
  .ws_src <- "NONE"; .ws <- rep(NA_real_, .n)
}
message("[MET] wind speed from: ", .ws_src)
if (.ws_src == "NONE")
  stop("P06: no wind-speed field (need `windspd`, or both `u10` and `v10`) - ",
       "the stability classification cannot be computed.")

# Cloud cover, via the shared helper: the unit is asserted from the source
# (HRRR tcdc/lcc are percentages) rather than inferred from the observed
# values, because inference is unsafe in both directions - a percentage field
# sampled only in clear weather has a small maximum and would be mistaken for
# a 0-1 fraction.
.cl_info   <- cloud_percent_from(res)
.cl_src    <- .cl_info$src
.cloud_pct <- .cl_info$pct
.cl        <- if (.cl_src == "NONE") rep(NA_real_, .n) else as.numeric(res[[.cl_src]])

# A missing cloud field leaves Stability_Class NA. P07 must not then admit the
# event: the Briggs sigmas are indexed by stability class, so an event with no
# stability cannot be inverted, and admitting it makes P07's retained count
# disagree with the sample P08 actually inverts.
if (.cl_src == "NONE")
  stop("P06: no cloud-cover field (need `tcdc` or `lcc`) - stability cannot be ",
       "classified, and every downstream plume would carry NA stability.")

res <- res %>%
  dplyr::mutate(
    wind_speed_ms       = .ws,
    cloud_raw           = .cl,
    cloud_cover_percent = .cloud_pct,
    Stability_Class_simple = determine_stability_class_daytime(wind_speed_ms, cloud_cover_percent)
  )

# Uncollapsed classes kept alongside the collapsed ones. Both now come from the
# SAME ladder in P00_met_helpers.R: this block used to re-write the entire
# Pasquill table by hand, a second copy that could drift from the one the
# simulations run. `collapse = FALSE` returns the A-B / B-C labels.
res <- res %>%
  dplyr::mutate(
    Insolation      = insolation_from_cloud(cloud_cover_percent),
    Stability_Class = determine_stability_class_daytime(
      wind_speed_ms, cloud_cover_percent, collapse = FALSE)
  )

# The collapsed and uncollapsed labels must agree after folding, or the two
# calls have somehow diverged.
stopifnot(identical(
  dplyr::recode(res$Stability_Class, "A-B" = "A", "B-C" = "B",
                .default = res$Stability_Class),
  res$Stability_Class_simple
))
message("[MET] stability classes: ",
        paste(sprintf("%s=%d", names(table(res$Stability_Class_simple, useNA = "ifany")),
                      as.integer(table(res$Stability_Class_simple, useNA = "ifany"))),
              collapse = "  "))

# ============================================================
# Subset: distance + wind-from filtering
# ============================================================
angle_diff <- function(a, b) {
  d <- (a - b + 180) %% 360 - 180
  abs(d)
}

# BUGFIX (2026-08-21): this picked the wind column independently of P05, which
# had written wwtf_wind_angle_diff from `wd` while this filter preferred HRRR
# `winddir`. P08 then inverts using P05's angle, so a plume could be admitted
# under one wind field and inverted under another. Both stages now read the
# single canonical choice from P00_met_helpers.R.
wind_col <- pick_wind_geom_col(res)
if (!is.null(res$wwtf_angle_source) &&
    !identical(unique(stats::na.omit(res$wwtf_angle_source))[1], wind_col)) {
  stop("P06: plume admission would use `", wind_col, "` but P05 built the ",
       "geometry from `", unique(stats::na.omit(res$wwtf_angle_source))[1],
       "`. Re-run P05 so admission and inversion share one wind field.")
}
message("[GEOM] plume admission filtered on `", wind_col, "`")

res_sub <- res %>%
  dplyr::filter(distance_wwtp > 0.5, distance_wwtp <= 5) %>%
  dplyr::filter(angle_diff(.data[[wind_col]], wind_from_deg_wwtf) <= 10)

# Optional quick checks
res_sub %>% count(Stability_Class_simple)
summary(res_sub$distance_wwtp)

# Optional save
save(res, res_sub, file = "/Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf_stability_filtered.RData")
