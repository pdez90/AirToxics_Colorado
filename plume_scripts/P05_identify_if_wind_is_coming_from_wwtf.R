# ==============================================================
# P05  Identify if wind is coming from WWTF
# Auto-split from Suncor_plume.Rmd  (section 5 of 10)
# ==============================================================

#Identify if wind is coming from WWTF

# ============================================================
# Compute wind-from direction relative to WWTF
# Source = Robert W. Hite WWTF
# Coordinates: 39.81000446758592, -104.95562509611672
# ============================================================

load("/Users/priyanka/Downloads/Suncor/mobile_hrrr.RData")

# Expect object named `res`
if (!exists("res")) {
  stop("mobile_hrrr.RData must contain an object named `res`.")
}

# Check required columns
req_cols <- c("Latitude", "Longitude")
missing_cols <- setdiff(req_cols, names(res))
if (length(missing_cols) > 0) {
  stop("Missing required columns in `res`: ", paste(missing_cols, collapse = ", "))
}

# ------------------------------------------------------------
# Wind "from" direction (0–360°, 0 = from North)
# Returns the wind direction that would bring air
# from the fixed source to each receptor location.
# Vectorized over lat/lon.
# ------------------------------------------------------------
wind_from_dir <- function(lat, lon, src_lat, src_lon) {
  rad <- pi / 180
  deg <- 180 / pi

  phi1 <- lat * rad
  phi2 <- src_lat * rad
  dlon <- (src_lon - lon) * rad

  x <- sin(dlon) * cos(phi2)
  y <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dlon)

  theta   <- atan2(x, y)
  bearing <- (theta * deg + 360) %% 360

  # same point -> undefined direction
  same_pt <- (abs(lat - src_lat) < 1e-12) & (abs(lon - src_lon) < 1e-12)
  bearing[same_pt] <- NA_real_

  bearing
}

# ------------------------------------------------------------
# WWTF source coordinates
# ------------------------------------------------------------
wwtf_lat <- 39.81000446758592
wwtf_lon <- -104.95562509611672

# Compute source-relative wind-from direction
res$wind_from_deg_wwtf <- wind_from_dir(
  lat     = res$Latitude,
  lon     = res$Longitude,
  src_lat = wwtf_lat,
  src_lon = wwtf_lon
)

# BUGFIX (2026-08-21): this computed a single `wwtf_wind_angle_diff` from
# `wd` - the EPA-station wind joined to the mobile record. But P06 admits
# plumes using HRRR `winddir` where it exists, and P08 then reads
# `wwtf_wind_angle_diff` for the crosswind geometry of the inversion. So a
# plume could be SELECTED under HRRR geometry and INVERTED under station
# geometry, which is exactly what P08's own comment says it does not do, and
# it silently undermines the off-axis correction: y = d*sin(theta) was being
# built from a different wind field than the +-10 deg acceptance test.
#
# Both angles are now computed and named for their source, and the canonical
# one - the field named by WIND_GEOM_COL in P00_met_helpers.R, i.e. HRRR - is
# also written to `wwtf_wind_angle_diff` so downstream readers get the
# designated geometry. `wwtf_angle_source` records which field that was.
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

if ("winddir" %in% names(res)) {
  res$wwtf_angle_diff_hrrr <- angle_diff_deg(res$winddir, res$wind_from_deg_wwtf)
}
if ("wd" %in% names(res)) {
  res$wwtf_angle_diff_obs  <- angle_diff_deg(res$wd, res$wind_from_deg_wwtf)
}

.geom_col <- pick_wind_geom_col(res)
res$wwtf_wind_angle_diff <- angle_diff_deg(res[[.geom_col]], res$wind_from_deg_wwtf)
res$wwtf_angle_source    <- .geom_col
message("[GEOM] plume geometry angle computed from `", .geom_col,
        "` and written to wwtf_wind_angle_diff (P06 admission and P08 inversion ",
        "both read this field)")

if (all(c("wwtf_angle_diff_hrrr", "wwtf_angle_diff_obs") %in% names(res))) {
  .d <- abs(res$wwtf_angle_diff_hrrr - res$wwtf_angle_diff_obs)
  message(sprintf("[GEOM] HRRR vs station wind disagree by median %.1f deg, p95 %.1f deg, max %.1f deg",
                  stats::median(.d, na.rm = TRUE),
                  stats::quantile(.d, 0.95, na.rm = TRUE),
                  max(.d, na.rm = TRUE)))
  message("[GEOM] that disagreement is the size of the error the previous ",
          "mismatched-field version could introduce into the crosswind geometry.")
}

# Save
save(
  res,
  file = "/Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf.RData"
)

message("Saved updated object to: /Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf.RData")
