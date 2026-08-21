# ==============================================================
# 06  Merge with wind
# Auto-split from Suncor.Rmd  (section 6 of 40)
# ==============================================================

#Merge with wind

# ============================================================
# Join MOBILE (df) to WIND (wind) by:
#   - same-hour (floor to hour) wind values
#   - choose the CLOSEST station that has NON-NA ws & wd for that hour
#   - optional max distance cap (km)
# Efficient approach: compute station rank once using sf, then pick first
# station with available wind for each (Asset,Site,hour).
# ============================================================
require(tidyverse)
suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(lubridate)
  library(units)
})

load("/Users/priyanka/Downloads/Suncor/wind_suncor_pueblo1.RData")
wind <- wind_2023 %>%
  dplyr::rename(
    Lat = Latitude,
    Lon = Longitude
  )
rm(wind_2023)

load("/Users/priyanka/Downloads/Suncor/mobile.RData")
df <- df_out
rm(df_out)

# ---------------------------------------------------------------
# GUARD (2026-08-20): the *_raw columns below carry the DELIVERED
# (un-averaged) H2S/HCN signal that the plume branch depends on -
# native-cadence averaging (03, section 3b) flattens the sub-5-s rise/fall
# shape, so P07's detector must see the raw trace. These columns only exist
# if NATIVE_CADENCE is TRUE in 03_checks_flags.R. This select() previously
# used dplyr::any_of(), which matches ZERO columns without error: script 10
# then built no *_raw baselines, R06's exemption guard evaluated FALSE, and
# the whole raw-signal plume branch disappeared behind a single log line
# while the retained-plume count collapsed. Fail loudly instead.
stopifnot(
  "mobile.RData lacks the *_raw columns: set NATIVE_CADENCE <- TRUE in 03_checks_flags.R and re-run 03 before 06" =
    all(c("Hydrogen_Sulfide_ppb_raw", "Hydrogen_Cyanide_ppb_raw") %in% names(df))
)
message("[GUARD] raw (un-averaged) H2S/HCN columns present - plume exemption will be available downstream.")
# ---------------------------------------------------------------

df <- df %>%
   dplyr::select(
    date,
    Asset,
    Site,
    Latitude,
    Longitude,
    ws,
    wd,
    Relative_Humidity_percent,
    Pressure_mb,
    Temperature_F,
    Benzene_ppb,
    Toluene_ppb,
    Trimethylbenzene_ppb,
    Xylene_ppb,
    Hydrogen_Sulfide_ppb,
    Hydrogen_Cyanide_ppb,
    dplyr::all_of(c("Hydrogen_Sulfide_ppb_raw", "Hydrogen_Cyanide_ppb_raw")),  # raw H2S/HCN for the plume branch
    AssetSiteDay,
    interpolated
  ) %>%
  dplyr::rename(
    ws_mobile = ws,
    wd_mobile = wd
  )

# ---- ensure POSIXct + same tz
# TIME CONVENTION (2026-08-21). This join works on CLOCK READINGS, not on
# instants, and it is correct only because both sides carry the SAME clock:
#   * mobile `date` is a FIXED-MST WALL CLOCK STORED WITH A UTC ATTRIBUTE -
#     not an absolute UTC instant (see the note in 02_newmobile_data.R);
#   * AQS `Date.Local`/`Time.Local` are LOCAL STANDARD time - also MST here -
#     and 05_wind_speed_and_direction.R parses them with ymd_hm(), whose
#     default tz is UTC. Same convention.
# The two lines below only re-label; they do not convert. So if `date` ever
# arrives labelled "America/Denver" - which is what parsing Local_Time_MST with
# that zone would produce - the mobile side becomes an instant 6-7 h away from
# the AQS clock reading, and the hourly join SILENTLY pairs each observation
# with wind measured 6 h later in summer and 7 h later in winter. It does not
# fail; it returns a full set of matches that are all wrong. Verified by
# running this join logic on synthetic data under all three candidate parses.
# Hence the hard stop.
.tz_df <- attr(df$date, "tzone"); .tz_wd <- attr(wind$date, "tzone")
if (!identical(.tz_df, "UTC") || !identical(.tz_wd, "UTC")) {
  stop("06: expected both `df$date` (", paste(.tz_df, collapse = "/"), ") and `wind$date` (",
       paste(.tz_wd, collapse = "/"), ") to be fixed-MST wall clocks stored with a UTC ",
       "attribute, not absolute UTC instants. ",
       "See the time-convention note in 02_newmobile_data.R. Joining hours across ",
       "two different conventions pairs each mobile record with wind from 6-7 hours away.")
}
message("[TIME] mobile and AQS wind are both fixed-MST wall clocks stored with a UTC attribute - hourly join is like-for-like.")

df$date   <- as.POSIXct(df$date,   tz = attr(df$date, "tzone"))
wind$date <- as.POSIXct(wind$date, tz = attr(df$date, "tzone"))

df_dt  <- as.data.table(df)
wnd_dt <- as.data.table(wind)
# ============================================================
# 0) Prep: hourly wind with NON-NA ws & wd
# ============================================================
wnd_dt[, hour := floor_date(date, "hour")]
# Keep only non-NA ws & wd for the hour selection logic
wnd_ok <- wnd_dt[!is.na(ws) & !is.na(wd)]

# If wind has multiple rows within an hour, keep the latest row in that hour
dup_hour_rows <- wnd_ok[
  wnd_ok[, .N, by = .(SiteNum, hour)][N > 1],
  on = .(SiteNum, hour)
]

setorder(wnd_ok, SiteNum, hour, date)
wnd_ok_hour <- wnd_ok[, .SD[.N], by = .(SiteNum, hour)]

setkey(wnd_ok_hour, SiteNum, hour)

# Station locations (one per SiteNum)
stn_loc <- unique(wnd_dt[, .(SiteNum, Lat, Lon)])
stn_loc <- stn_loc[order(SiteNum)]

# ============================================================
# 1) For each (Asset,Site,hour) mobile group, find closest station with wind
# ============================================================
df_dt[, hour := floor_date(date, "hour")]

# representative location per hour-group (median is robust)
grp <- df_dt[, .(
  Latitude  = median(Latitude,  na.rm = TRUE),
  Longitude = median(Longitude, na.rm = TRUE)
), by = .(Asset, Site, hour)]

# build sf and compute station ranking by distance (per group)
grp_sf <- st_as_sf(grp, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
stn_sf <- st_as_sf(stn_loc, coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)

# distance matrix: groups x stations (OK because groups << rows; stations ~ 20)
Dm <- drop_units(st_distance(grp_sf, stn_sf))  # numeric matrix, meters

# build long table of station candidates ranked by distance for each group
cand <- as.data.table(
  expand.grid(
    grp_idx = seq_len(nrow(Dm)),
    stn_idx = seq_len(ncol(Dm))
  )
)

# fill distances in the same order as expand.grid created rows
# (grp_idx varies slowest, stn_idx varies fastest)
cand[, dist_m := as.vector(Dm)]
cand[, dist_km := dist_m / 1000]

# rank stations by distance within each group
setorder(cand, grp_idx, dist_km)
cand[, rank := seq_len(.N), by = grp_idx]
# attach keys + station ids/coords
grp_dt <- as.data.table(grp)
cand[, `:=`(
  Asset = grp_dt$Asset[grp_idx],
  Site  = grp_dt$Site[grp_idx],
  hour  = grp_dt$hour[grp_idx],
  SiteNum = stn_loc$SiteNum[stn_idx],
  Lat_wind = stn_loc$Lat[stn_idx],
  Lon_wind = stn_loc$Lon[stn_idx]
)]

# optional distance cap (set to Inf to disable)
max_km <- 50
cand <- cand[dist_km <= max_km]

# ============================================================
# 2) Keep the FIRST (closest) station that has NON-NA wind in that hour
# ============================================================
# join candidate stations to hourly wind availability
setkey(cand, SiteNum, hour)
cand_w <- wnd_ok_hour[cand, on = .(SiteNum, hour), nomatch = 0L]

# pick closest available station per (Asset,Site,hour)
setorder(cand_w, Asset, Site, hour, dist_km, rank)
best <- cand_w[, .SD[1], by = .(Asset, Site, hour)]

# keep only what we need to merge back
best <- best[, .(
  Asset, Site, hour,
  SiteNum_wind = SiteNum,
  Lat_wind, Lon_wind,
  dist_km,
  wind_date = date,
  ws_wind = ws,
  wd_wind = wd
)]

setkey(best, Asset, Site, hour)
setkey(df_dt, Asset, Site, hour)

# merge onto all df rows by hour-group
out <- best[df_dt]

# ============================================================
# 3) Aave
# ============================================================

message("Rows with matched wind (same-hour, fallback to next closest): ",
        sum(!is.na(out$ws_wind) & !is.na(out$wd_wind)), " / ", nrow(out))
message("Unique wind stations used: ", uniqueN(out$SiteNum_wind))
message("Median distance (km): ", round(median(out$dist_km, na.rm = TRUE), 2))

out <- out %>%
  dplyr::rename(
    ws = ws_wind,
    wd = wd_wind
  )

save(out, file = "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")

head(out)
