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
res <- res %>%
  dplyr::mutate(
    wind_speed_ms = dplyr::case_when(
      "windspd" %in% names(.) ~ as.numeric(windspd),
      all(c("u10", "v10") %in% names(.)) ~ sqrt(as.numeric(u10)^2 + as.numeric(v10)^2),
      TRUE ~ NA_real_
    ),
    cloud_raw = dplyr::case_when(
      "tcdc" %in% names(.) ~ as.numeric(tcdc),
      "lcc"  %in% names(.) ~ as.numeric(lcc),
      TRUE ~ NA_real_
    ),
    cloud_cover_percent = dplyr::case_when(
      !is.finite(cloud_raw) ~ NA_real_,
      cloud_raw <= 1.2      ~ 100 * cloud_raw,
      TRUE                  ~ cloud_raw
    ),
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
