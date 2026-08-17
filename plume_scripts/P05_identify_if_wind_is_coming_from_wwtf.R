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

# Optional: angular difference between observed wind direction and WWTF-aligned wind
# Assumes `wd` exists and is meteorological wind direction in degrees
if ("wd" %in% names(res)) {
  res$wwtf_wind_angle_diff <- abs(((res$wd - res$wind_from_deg_wwtf + 180) %% 360) - 180)
}

# Save
save(
  res,
  file = "/Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf.RData"
)

message("Saved updated object to: /Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf.RData")
