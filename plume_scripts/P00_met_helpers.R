# ==============================================================
# P00_met_helpers.R
#
# One definition of the meteorological preparation and the Pasquill
# stability classification, shared by the observational plume chain (P06)
# and by the simulations that are supposed to characterise it (P09, P10).
#
# Why this file exists (2026-08-21): the stability classifier was written
# out three times - in P06, P09 and P10. The three copies differed only
# cosmetically, but the CLOUD-COVER PREPARATION feeding them had genuinely
# diverged: P06 was repaired to decide the cloud unit once for the field,
# while P09 and P10 still carried the old per-row rule
#     cloud_raw <= 1.2 ~ 100 * cloud_raw
# which promotes a genuinely near-clear HRRR hour (TCDC = 0.8%) to 80%
# cover and turns 1.2% into a physically impossible 120%. The simulations
# were therefore no longer simulating the stability algorithm that the
# observational inversion actually runs, which is precisely the thing they
# exist to validate. Sourcing one file from all three makes that class of
# divergence impossible rather than merely unlikely.
# ==============================================================

suppressPackageStartupMessages({ library(dplyr) })

# Meteorological "from" direction (degrees clockwise from north) given the
# u/v wind components.
winddir_from_uv <- function(u, v) {
  dir_toward <- (atan2(u, v) * 180 / pi + 360) %% 360
  (dir_toward + 180) %% 360
}

# Smallest absolute angular separation between two compass bearings.
angle_diff_deg <- function(a, b) {
  abs(((as.numeric(a) - as.numeric(b) + 180) %% 360) - 180)
}

# ---------------------------------------------------------------
# Wind speed. Chosen ONCE from the columns present, outside any row
# context: dplyr::case_when evaluates every right-hand side regardless of
# which condition holds, so a case_when "fallback" referring to a column
# that is absent aborts with "object not found" instead of falling back.
# ---------------------------------------------------------------
wind_speed_from <- function(df) {
  n <- nrow(df)
  if ("windspd" %in% names(df)) {
    list(src = "windspd", ms = as.numeric(df$windspd))
  } else if (all(c("u10", "v10") %in% names(df))) {
    list(src = "sqrt(u10^2 + v10^2)",
         ms  = sqrt(as.numeric(df$u10)^2 + as.numeric(df$v10)^2))
  } else {
    list(src = "NONE", ms = rep(NA_real_, n))
  }
}

# ---------------------------------------------------------------
# Cloud cover, as a PERCENTAGE.
#
# HRRR TCDC and LCC are percentages (0-100) by product definition, so the
# unit is asserted from the SOURCE rather than inferred from the observed
# values. Inferring it is unsafe in both directions: a percentage field
# sampled only during clear weather has a small maximum and would be
# mistaken for a 0-1 fraction, and a single low value would be rescaled on
# its own under the old per-row rule.
#
# A field whose values look like a fraction is reported loudly rather than
# silently converted, because that would mean the input is not the HRRR
# product this pipeline expects.
# ---------------------------------------------------------------
cloud_percent_from <- function(df, quiet = FALSE) {
  n <- nrow(df)
  src <- if ("tcdc" %in% names(df)) "tcdc" else if ("lcc" %in% names(df)) "lcc" else NA_character_
  if (is.na(src)) return(list(src = "NONE", pct = rep(NA_real_, n)))

  raw <- as.numeric(df[[src]])
  obs_max <- suppressWarnings(max(raw, na.rm = TRUE))
  if (is.finite(obs_max) && obs_max <= 1.5) {
    warning(sprintf(
      paste0("cloud field `%s` has a maximum of %.3g, which does not look like the ",
             "0-100 percentage HRRR is documented to provide. It is being used AS A ",
             "PERCENTAGE without rescaling. If this input really is a 0-1 fraction, ",
             "convert it at the source rather than letting this code guess."),
      src, obs_max), call. = FALSE)
  }
  pct <- pmin(pmax(raw, 0), 100)
  if (!quiet) {
    message(sprintf("[MET] cloud cover from `%s` (percent): %.0f-%.0f%%, median %.0f%%",
                    src,
                    suppressWarnings(min(pct, na.rm = TRUE)),
                    suppressWarnings(max(pct, na.rm = TRUE)),
                    suppressWarnings(stats::median(pct, na.rm = TRUE))))
  }
  list(src = src, pct = pct)
}

# ---------------------------------------------------------------
# Pasquill daytime stability class from wind speed and insolation.
#
# Single canonical definition. With collapse = TRUE (the default) the
# intermediate A-B and B-C classes are folded into A and B so the result
# indexes the Briggs sigma tables directly; with collapse = FALSE the
# uncollapsed labels are returned. P06 needs both, and it used to get the
# uncollapsed one from a SECOND hand-written copy of this same ladder sitting
# in that script - one more place for the two to drift apart. One ladder now.
# ---------------------------------------------------------------
determine_stability_class_daytime <- function(wind_speed_ms, cloud_cover_percent,
                                              collapse = TRUE) {
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
    is.na(windspd) | is.na(Insolation)                          ~ NA_character_,
    windspd <= 2 & Insolation == "strong"                       ~ "A",
    windspd <= 2 & Insolation == "moderate"                     ~ "A-B",
    windspd <= 2 & Insolation == "slight"                       ~ "B",
    windspd > 2 & windspd <= 3 & Insolation == "strong"         ~ "A-B",
    windspd > 2 & windspd <= 3 & Insolation == "moderate"       ~ "B",
    windspd > 2 & windspd <= 3 & Insolation == "slight"         ~ "C",
    windspd > 3 & windspd <= 5 & Insolation == "strong"         ~ "B",
    windspd > 3 & windspd <= 5 & Insolation == "moderate"       ~ "B-C",
    windspd > 3 & windspd <= 5 & Insolation == "slight"         ~ "C",
    windspd > 5 & windspd <= 6 & Insolation == "strong"         ~ "C",
    windspd > 5 & windspd <= 6 & Insolation != "strong"         ~ "D",
    windspd > 6 & Insolation == "strong"                        ~ "C",
    windspd > 6 & Insolation != "strong"                        ~ "D",
    TRUE                                                        ~ NA_character_
  )
  if (!collapse) return(sc)
  dplyr::recode(sc, "A-B" = "A", "B-C" = "B", .default = sc)
}

# The insolation category behind the class, exposed so P06 can report it
# without re-deriving the thresholds.
insolation_from_cloud <- function(cloud_cover_percent) {
  cc <- as.numeric(cloud_cover_percent)
  cc[is.nan(cc)] <- NA_real_
  dplyr::case_when(
    is.na(cc)          ~ NA_character_,
    cc <= 25           ~ "strong",
    cc > 25 & cc <= 75 ~ "moderate",
    cc > 75            ~ "slight",
    TRUE               ~ NA_character_
  )
}

# ---------------------------------------------------------------
# THE wind field used for plume geometry.
#
# P05, P06 and P08 must agree on this or a plume is admitted under one
# geometry and inverted under another. HRRR `winddir` is the designated
# model field; `wd` (the EPA-station wind joined to the mobile record) is
# retained only as a comparison. WIND_GEOM_COL names the canonical choice
# in one place so all three stages read the same thing.
# ---------------------------------------------------------------
WIND_GEOM_COL <- "winddir"

pick_wind_geom_col <- function(df) {
  if (WIND_GEOM_COL %in% names(df)) return(WIND_GEOM_COL)
  if ("wd" %in% names(df)) {
    warning(sprintf("`%s` not present; falling back to `wd` for plume geometry. ",
                    WIND_GEOM_COL),
            "Plume admission and inversion will both use `wd`, but the HRRR ",
            "geometry the method is described against is unavailable.",
            call. = FALSE)
    return("wd")
  }
  stop("No wind-direction column for plume geometry (need `", WIND_GEOM_COL, "` or `wd`).")
}
