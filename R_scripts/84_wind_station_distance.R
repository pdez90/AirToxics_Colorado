# ==============================================================
# 84_wind_station_distance.R
# Manuscript section 2.3 describes how 06_merge_with_wind.R assigns wind: one
# EPA station per laboratory x route x hour, the station closest to that hour's
# median sampling position (or the next-closest reporting station when the
# closest has no wind that hour). This script quantifies the assignment from
# the merged record:
#   - median distance from the hourly median position to the station used
#     (the dist_km that 06 stores);
#   - median distance from the individual measurements to the station used;
#   - share of measurements for which another of the stations used lies nearer
#     the measurement itself;
#   - the fallback: measurements whose hourly position's closest station had no
#     wind that hour (station used farther than the closest), and the median
#     extra distance to the station used.
#   -> TABLE_wind_station_distance.csv
#   Rscript R_scripts/84_wind_station_distance.R
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
load(file.path(SUNCOR_BASE, "mobile_wswd.RData")); d <- as.data.table(out); rm(out)
d <- d[Site != "Goodrich Corporation (Collins Aerospace)" & is.finite(Latitude) & is.finite(Lat_wind)]
hav <- function(la1, lo1, la2, lo2) { r <- pi / 180
  a <- sin((la2 - la1) * r / 2)^2 + cos(la1 * r) * cos(la2 * r) * sin((lo2 - lo1) * r / 2)^2
  2 * 6371.0088 * asin(sqrt(a)) }
st <- unique(d[, .(SiteNum_wind, Lat_wind, Lon_wind)])
D <- sapply(seq_len(nrow(st)), function(i) hav(d$Latitude, d$Longitude, st$Lat_wind[i], st$Lon_wind[i]))
d[, `:=`(d_meas_km = hav(Latitude, Longitude, Lat_wind, Lon_wind),
         nearest = st$SiteNum_wind[max.col(-D, ties.method = "first")])]
# fallback: per laboratory x route x hour, the station used vs the closest station
# to that hour's median position (06 stores dist_km from that position)
g <- d[, .(mlat = median(Latitude), mlon = median(Longitude), dist_km = dist_km[1], n = .N),
       by = .(Asset, Site, hour)]
Dg <- sapply(seq_len(nrow(st)), function(i) hav(g$mlat, g$mlon, st$Lat_wind[i], st$Lon_wind[i]))
g[, dmin := apply(Dg, 1, min)][, fallback := dist_km > dmin + 0.01]   # 10 m tolerance
fb <- g[fallback == TRUE]
res <- data.table(n_measurements = nrow(d), n_stations = nrow(st),
                  n_fallback = sum(fb$n), pct_fallback = 100 * sum(fb$n) / nrow(d),
                  median_extra_km_fallback = median(rep(fb$dist_km - fb$dmin, fb$n)),
                  median_dist_hourly_position_km = median(d$dist_km),
                  median_dist_measurement_km = median(d$d_meas_km),
                  pct_other_station_nearer = 100 * mean(d$nearest != d$SiteNum_wind))
fwrite(res, file.path(SUNCOR_BASE, "TABLE_wind_station_distance.csv"))
print(res)
message("-> ", file.path(SUNCOR_BASE, "TABLE_wind_station_distance.csv"))
