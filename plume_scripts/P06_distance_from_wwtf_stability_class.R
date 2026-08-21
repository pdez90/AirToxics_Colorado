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

# --- helper: meteorological wind direction FROM u/v (if needed)
winddir_from_uv <- function(u, v) {
  dir_toward <- (atan2(u, v) * 180 / pi + 360) %% 360
  (dir_toward + 180) %% 360
}

# --- helper: daytime stability class
determine_stability_class_daytime <- function(wind_speed_ms, cloud_cover_percent) {
  windspd <- as.numeric(wind_speed_ms)
  cc <- as.numeric(cloud_cover_percent)
  cc[is.nan(cc)] <- NA_real_

  Insolation <- dplyr::case_when(
    is.na(cc)           ~ NA_character_,
    cc <= 25            ~ "strong",
    cc > 25 & cc <= 75  ~ "moderate",
    cc > 75             ~ "slight",
    TRUE                ~ NA_character_
  )

  Stability_Class <- dplyr::case_when(
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

  dplyr::recode(
    Stability_Class,
    "A-B" = "A",
    "B-C" = "B",
    .default = Stability_Class
  )
}

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
if ("tcdc" %in% names(res)) {
  .cl_src <- "tcdc"; .cl <- as.numeric(res$tcdc)
} else if ("lcc" %in% names(res)) {
  .cl_src <- "lcc";  .cl <- as.numeric(res$lcc)
} else {
  .cl_src <- "NONE"; .cl <- rep(NA_real_, .n)
}
message("[MET] wind speed from: ", .ws_src, " | cloud cover from: ", .cl_src)
if (.ws_src == "NONE")
  stop("P06: no wind-speed field (need `windspd`, or both `u10` and `v10`) - ",
       "the stability classification cannot be computed.")

# BUGFIX (2026-08-21): cloud cover was rescaled PER ROW with
#   cloud_raw <= 1.2 ~ 100 * cloud_raw
# meant to convert a 0-1 fraction to percent. But HRRR TCDC is already a
# percentage, so a genuinely near-clear hour was promoted rather than
# converted: 0.8% became 80% cover, and 1.2% became a physically impossible
# 120%. Either error moves the insolation category, hence the stability class,
# hence sigma_y and sigma_z, hence the inferred emission rate. The unit is a
# property of the FIELD, not of an individual value, so decide it once from the
# column and clamp the result to a physical range.
.cl_max <- suppressWarnings(max(.cl, na.rm = TRUE))
.is_fraction <- is.finite(.cl_max) && .cl_max <= 1.5
.cloud_pct <- if (.is_fraction) 100 * .cl else .cl
.cloud_pct <- pmin(pmax(.cloud_pct, 0), 100)
message(sprintf("[MET] cloud field read as %s (max observed %.3g); cover %.0f-%.0f%%, median %.0f%%",
                if (.is_fraction) "a 0-1 FRACTION" else "a PERCENTAGE", .cl_max,
                suppressWarnings(min(.cloud_pct, na.rm = TRUE)),
                suppressWarnings(max(.cloud_pct, na.rm = TRUE)),
                suppressWarnings(stats::median(.cloud_pct, na.rm = TRUE))))

res <- res %>%
  dplyr::mutate(
    wind_speed_ms       = .ws,
    cloud_raw           = .cl,
    cloud_cover_percent = .cloud_pct,
    Stability_Class_simple = determine_stability_class_daytime(wind_speed_ms, cloud_cover_percent)
  )

# Optional: keep uncollapsed classes too
res <- res %>%
  dplyr::mutate(
    Insolation = dplyr::case_when(
      is.na(cloud_cover_percent) ~ NA_character_,
      cloud_cover_percent <= 25 ~ "strong",
      cloud_cover_percent > 25 & cloud_cover_percent <= 75 ~ "moderate",
      cloud_cover_percent > 75 ~ "slight",
      TRUE ~ NA_character_
    ),
    Stability_Class = dplyr::case_when(
      is.na(wind_speed_ms) | is.na(Insolation) ~ NA_character_,

      wind_speed_ms <= 2 & Insolation == "strong"   ~ "A",
      wind_speed_ms <= 2 & Insolation == "moderate" ~ "A-B",
      wind_speed_ms <= 2 & Insolation == "slight"   ~ "B",

      wind_speed_ms > 2 & wind_speed_ms <= 3 & Insolation == "strong"   ~ "A-B",
      wind_speed_ms > 2 & wind_speed_ms <= 3 & Insolation == "moderate" ~ "B",
      wind_speed_ms > 2 & wind_speed_ms <= 3 & Insolation == "slight"   ~ "C",

      wind_speed_ms > 3 & wind_speed_ms <= 5 & Insolation == "strong"   ~ "B",
      wind_speed_ms > 3 & wind_speed_ms <= 5 & Insolation == "moderate" ~ "B-C",
      wind_speed_ms > 3 & wind_speed_ms <= 5 & Insolation == "slight"   ~ "C",

      wind_speed_ms > 5 & wind_speed_ms <= 6 & Insolation == "strong"   ~ "C",
      wind_speed_ms > 5 & wind_speed_ms <= 6 & Insolation != "strong"   ~ "D",

      wind_speed_ms > 6 & Insolation == "strong"                        ~ "C",
      wind_speed_ms > 6 & Insolation != "strong"                        ~ "D",

      TRUE ~ NA_character_
    )
  )

# ============================================================
# Subset: distance + wind-from filtering
# ============================================================
angle_diff <- function(a, b) {
  d <- (a - b + 180) %% 360 - 180
  abs(d)
}

# Use whichever wind-direction column exists
wind_col <- if ("winddir" %in% names(res)) {
  "winddir"
} else if ("wd" %in% names(res)) {
  "wd"
} else {
  stop("Could not find wind direction column. Expected `winddir` or `wd`.")
}

res_sub <- res %>%
  dplyr::filter(distance_wwtp > 0.5, distance_wwtp <= 5) %>%
  dplyr::filter(angle_diff(.data[[wind_col]], wind_from_deg_wwtf) <= 10)

# Optional quick checks
res_sub %>% count(Stability_Class_simple)
summary(res_sub$distance_wwtp)

# Optional save
save(res, res_sub, file = "/Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf_stability_filtered.RData")
