# ==============================================================
# 81_group13_hq_annulus.R
# Group 13 lies near the CDPHE mobile-laboratory headquarters, whose 300 m
# surround is excluded (03_checks_flags.R, section 3c). Manuscript section
# 3.4.2 quotes two geometric facts and one diagnostic for it:
#   - the distance from the Group 13 centroid to the headquarters and the
#     bearing from the group to the headquarters;
#   - among retained measurements, the fraction above each pollutant's campaign
#     99th percentile (the hotspot-event test of 28_...R: strictly above the
#     threshold) in the 300-400 m and 400-500 m annuli around the headquarters,
#     against the campaign-wide fraction (not exactly 1% because reported
#     values tie at the threshold).
# Distances are great-circle distances from the delay-corrected positions,
# the same positions the 300 m screen uses.
#   -> TABLE_group13_hq_annulus.csv
#   Rscript R_scripts/81_group13_hq_annulus.R
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
HQ_LAT <- 39.785189; HQ_LON <- -105.104411      # as in 03_checks_flags.R
hav <- function(la1, lo1, la2, lo2) {
  r <- pi / 180; a <- sin((la2 - la1) * r / 2)^2 + cos(la1 * r) * cos(la2 * r) * sin((lo2 - lo1) * r / 2)^2
  2 * 6371008.8 * asin(sqrt(a)) }
bearing <- function(la1, lo1, la2, lo2) {
  r <- pi / 180; y <- sin((lo2 - lo1) * r) * cos(la2 * r)
  x <- cos(la1 * r) * sin(la2 * r) - sin(la1 * r) * cos(la2 * r) * cos((lo2 - lo1) * r)
  (atan2(y, x) / r) %% 360 }

load(file.path(SUNCOR_BASE, "mobile_wswd.RData")); d <- as.data.table(out); rm(out)
d <- d[Site != "Goodrich Corporation (Collins Aerospace)" & is.finite(Latitude) & is.finite(Longitude)]
d[, d_hq := hav(Latitude, Longitude, HQ_LAT, HQ_LON)]
stopifnot(min(d$d_hq) > 300)                      # the 300 m screen holds

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb", Trimethylbenzene = "Trimethylbenzene_ppb",
           Xylene = "Xylene_ppb", H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")
res <- rbindlist(lapply(names(POLLS), function(p) {
  v <- d[[POLLS[[p]]]]; f <- is.finite(v)
  thr <- as.numeric(quantile(v[f], 0.99))          # campaign 99th percentile, as in 28_...R
  a1 <- f & d$d_hq > 300 & d$d_hq <= 400; a2 <- f & d$d_hq > 400 & d$d_hq <= 500
  data.table(pollutant = p, aromatic = p %in% c("Benzene", "Toluene", "Trimethylbenzene", "Xylene"),
             threshold_ppb = thr,
             pct_above_campaign = 100 * mean(v[f] > thr),
             n_300_400 = sum(a1), pct_above_300_400 = 100 * mean(v[a1] > thr),
             n_400_500 = sum(a2), pct_above_400_500 = 100 * mean(v[a2] > thr))
}))

m <- fread(file.path(SUNCOR_BASE, "MASTER_hotspot_group_index.csv"))[group_id == 13]
stopifnot(nrow(m) == 1)
res[, `:=`(group13_dist_to_hq_m = hav(m$Latitude, m$Longitude, HQ_LAT, HQ_LON),
           bearing_group13_to_hq_deg = bearing(m$Latitude, m$Longitude, HQ_LAT, HQ_LON))]
fwrite(res, file.path(SUNCOR_BASE, "TABLE_group13_hq_annulus.csv"))
print(res[, .(pollutant, threshold_ppb, pct_above_campaign = round(pct_above_campaign, 2),
              n_300_400, pct_300_400 = round(pct_above_300_400, 2),
              n_400_500, pct_400_500 = round(pct_above_400_500, 2))])
cat(sprintf("Group 13 centroid: %.0f m from the headquarters, which lies at a bearing of %.0f degrees from the group\n",
            res$group13_dist_to_hq_m[1], res$bearing_group13_to_hq_deg[1]))
message("-> ", file.path(SUNCOR_BASE, "TABLE_group13_hq_annulus.csv"))
