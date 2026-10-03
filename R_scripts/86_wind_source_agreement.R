# ==============================================================
# 86_wind_source_agreement.R
# Manuscript sections 3.6 and 3.8 (and SI S6.8) compare the two wind products
# used in the study over the merged mobile record (mobile_hrrr.RData, written
# by R06 / P04): the HRRR wind direction (winddir) and the nearest EPA station
# wind direction (wd).
#   - absolute wind-direction difference: median and 95th percentile;
#   - difference in angular offset from the bearing to the Hite WWTF (WWTF1),
#     the quantity that matters for plume acceptance (as P05 [GEOM]).
#   -> TABLE_wind_source_agreement.csv (read by P11 for its summary line)
#   Rscript R_scripts/86_wind_source_agreement.R
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
e <- new.env(); load(file.path(SUNCOR_BASE, "mobile_hrrr.RData"), envir = e); r <- as.data.table(e$res); rm(e)
ang <- function(a, b) abs(((a - b + 180) %% 360) - 180)
d0 <- ang(r$winddir, r$wd); k0 <- is.finite(d0)
lat0 <- 39.81000446758592; lon0 <- -104.95562509611672; p_ <- pi / 180   # WWTF1, as in P05
dlon <- (r$Longitude - lon0) * p_
y <- sin(dlon) * cos(r$Latitude * p_)
x <- cos(lat0 * p_) * sin(r$Latitude * p_) - sin(lat0 * p_) * cos(r$Latitude * p_) * cos(dlon)
brg <- (atan2(y, x) / p_) %% 360
d1 <- abs(ang(r$winddir, brg) - ang(r$wd, brg)); k1 <- is.finite(d1)
res <- data.table(n_pairs = sum(k0),
                  median_wd_difference_deg = median(d0[k0]), p95_wd_difference_deg = unname(quantile(d0[k0], 0.95)),
                  median_offset_difference_deg = median(d1[k1]), p95_offset_difference_deg = unname(quantile(d1[k1], 0.95)))
fwrite(res, file.path(SUNCOR_BASE, "TABLE_wind_source_agreement.csv"))
print(res)
message("-> ", file.path(SUNCOR_BASE, "TABLE_wind_source_agreement.csv"))
