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
    dplyr::any_of(c("Hydrogen_Sulfide_ppb_raw", "Hydrogen_Cyanide_ppb_raw")),  # raw H2S/HCN for the plume branch
    AssetSiteDay,
    interpolated
  ) %>%
  dplyr::rename(
    ws_mobile = ws,
    wd_mobile = wd
  )

# ---- ensure POSIXct + same tz
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
