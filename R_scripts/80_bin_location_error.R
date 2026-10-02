# ==============================================================
# 80_bin_location_error.R  (2026-10-02)
# How far is a 5-s H2S / 2-s HCN bin value from where its air was sampled?
#
# Since the one-value-per-bin change (03, section 3d) each bin's mean sits on
# the bin's middle delivered second. The van moves during the bin, so the
# value stands for air sampled along a short stretch of road. For every bin
# this script measures, from the delay-corrected positions of the bin's
# delivered seconds:
#   max_off   distance from the assigned (middle-second) position to the
#             farthest delivered second of the bin
#   cen_off   distance from the assigned position to the mean position of
#             the bin's delivered seconds
#   full_len  distance driven over the whole acquisition interval (mean speed
#             over the delivered seconds x 5 s or 2 s)
# for all bins and for bins above the campaign 99th percentile (the hotspot
# events of section 2.5.3, same strict '>' test as 28_...R), and compares them with the 100 m
# DBSCAN radius and hotspot buffer. Bins are rebuilt exactly as in 03:
# (Asset, Site, UTC day, floor((epoch + delay) / B)), delays CAT/EMU
# H2S 21/17 s, HCN 6/3 s.
#   -> TABLE_bin_location_error.csv           (SI section S5.3)
#   Rscript R_scripts/80_bin_location_error.R
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
load(file.path(SUNCOR_BASE, "mobile_wswd.RData")); d <- as.data.table(out); rm(out)
hav <- function(la1, lo1, la2, lo2) {
  r <- pi / 180; a <- sin((la2 - la1) * r / 2)^2 + cos(la1 * r) * cos(la2 * r) * sin((lo2 - lo1) * r / 2)^2
  2 * 6371000 * asin(sqrt(a)) }
d[, `:=`(.epoch = as.numeric(date), .day = as.Date(date))]

bin_offsets <- function(raw, thin, delay_cat, delay_emu, B) {
  x <- d[!is.na(get(raw)) & is.finite(Latitude) & is.finite(Longitude),
         .(Asset, Site, .day, .epoch, Latitude, Longitude, v = get(thin))]
  x[, .blk := floor((.epoch + ifelse(toupper(Asset) == "CAT", delay_cat, delay_emu)) / B)]
  setorder(x, Asset, Site, .day, .blk, .epoch)
  b <- x[, {
    m <- which(!is.na(v))
    if (length(m) != 1) NULL else {
      off  <- hav(Latitude[m], Longitude[m], Latitude, Longitude)
      path <- if (.N > 1) sum(hav(Latitude[-.N], Longitude[-.N], Latitude[-1], Longitude[-1])) else 0
      span <- max(.epoch) - min(.epoch)
      .(val = v[m], max_off = max(off), cen_off = hav(Latitude[m], Longitude[m], mean(Latitude), mean(Longitude)),
        speed = if (span > 0) path / span else NA_real_)
    }}, by = .(Asset, Site, .day, .blk)]
  b[, full_len := speed * B]
  stopifnot(nrow(b) == sum(!is.na(d[[thin]])))      # one middle row per bin, every bin found
  b
}
summ <- function(b, pol, sub, thr) {
  q <- function(z, p) as.numeric(quantile(z, p, na.rm = TRUE))
  data.table(pollutant = pol, subset = sub, threshold_ppb = thr, n_bins = nrow(b),
             max_off_p50 = q(b$max_off, .5), max_off_p95 = q(b$max_off, .95), max_off_p99 = q(b$max_off, .99),
             cen_off_p50 = q(b$cen_off, .5), cen_off_p95 = q(b$cen_off, .95),
             full_len_p50 = q(b$full_len, .5), full_len_p95 = q(b$full_len, .95),
             pct_max_off_gt50 = 100 * mean(b$max_off > 50), pct_max_off_gt100 = 100 * mean(b$max_off > 100),
             n_max_off_gt100 = sum(b$max_off > 100),
             speed_kmh_p50 = 3.6 * q(b$speed, .5), speed_kmh_p95 = 3.6 * q(b$speed, .95))
}
res <- rbindlist(lapply(list(
  list("H2S", "Hydrogen_Sulfide_ppb_raw", "Hydrogen_Sulfide_ppb", 21, 17, 5),
  list("HCN", "Hydrogen_Cyanide_ppb_raw", "Hydrogen_Cyanide_ppb", 6, 3, 2)), function(a) {
    b <- bin_offsets(a[[2]], a[[3]], a[[4]], a[[5]], a[[6]])
    thr <- as.numeric(quantile(d[[a[[3]]]], 0.99, na.rm = TRUE))   # campaign p99 of the bin values
    rbind(summ(b, a[[1]], "all bins", NA_real_), summ(b[val > thr], a[[1]], "bins > campaign p99 (hotspot events)", thr))
  }))
fwrite(res, file.path(SUNCOR_BASE, "TABLE_bin_location_error.csv"))
print(res[, .(pollutant, subset, threshold_ppb, n_bins, max_off_p50 = round(max_off_p50, 1), max_off_p95 = round(max_off_p95, 1),
              max_off_p99 = round(max_off_p99, 1), cen_off_p50 = round(cen_off_p50, 1), cen_off_p95 = round(cen_off_p95, 1),
              full_len_p50 = round(full_len_p50), full_len_p95 = round(full_len_p95),
              pct_gt100 = round(pct_max_off_gt100, 2), n_gt100 = n_max_off_gt100)])
message("-> ", file.path(SUNCOR_BASE, "TABLE_bin_location_error.csv"))
