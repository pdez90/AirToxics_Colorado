# ==============================================================
# 76  PER-GROUP EXCEEDANCE DAYS WITHIN 100 m
# Rebuilds the per-group numbers quoted in SI Table S5.1:
#   "Days above the campaign 99th percentile within 100 m:
#    benzene N; toluene N; trimethylbenzene N; xylene N; H2S N; HCN N."
# for every group in MASTER_hotspot_group_index.csv.
#
# Definition (matches the hotspot detection in 33/60): campaign 99th
# percentile of the finite analysis-set values for that pollutant;
# a day counts once if any observation within 100 m of the group
# centroid exceeded it. Pollutants with no exceedance day are omitted
# from the sentence, exactly as in the submitted table.
#
# Output: TABLE_S5.1_group_exceedance_days.csv
# ==============================================================
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({ library(data.table); library(sf) })
BASE <- SUNCOR_BASE
load(file.path(BASE, "mobile_wswd.RData")); df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]

POLLS <- c(benzene = "Benzene_ppb", toluene = "Toluene_ppb",
           trimethylbenzene = "Trimethylbenzene_ppb", xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")

pts <- st_transform(st_as_sf(df[, .(Longitude, Latitude)],
        coords = c("Longitude", "Latitude"), crs = 4326), 32613)
xy <- st_coordinates(pts); df[, `:=`(px = xy[, 1], py = xy[, 2])]; rm(pts, xy); gc()

master <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
mxy <- st_coordinates(st_transform(st_as_sf(master[, .(Longitude, Latitude)],
        coords = c("Longitude", "Latitude"), crs = 4326), 32613))

thr <- vapply(POLLS, function(cn) { v <- df[[cn]]; as.numeric(stats::quantile(v[is.finite(v)], .99)) }, numeric(1))
cat("campaign p99 thresholds (ppb):\n"); print(round(thr, 3))

res <- rbindlist(lapply(seq_len(nrow(master)), function(i) {
  d2 <- (df$px - mxy[i, 1])^2 + (df$py - mxy[i, 2])^2
  sub <- df[d2 <= 100^2]
  out <- data.table(group_id = master$group_id[i], n_rows_100m = nrow(sub),
                    n_days_100m = uniqueN(sub$day))
  for (pn in names(POLLS)) {
    v <- sub[[POLLS[[pn]]]]
    ok <- is.finite(v) & v > thr[[pn]]
    set(out, j = pn, value = if (any(ok)) uniqueN(sub$day[ok]) else 0L)
  }
  out
}))
res <- merge(res, master[, .(group_id, n_pollutants, pollutants, max_n_days,
                             total_n_days, tri_dist_km, tri_name)], by = "group_id")
setorder(res, group_id)
fwrite(res, file.path(BASE, "TABLE_S5.1_group_exceedance_days.csv"))
print(res)
cat("\nwrote TABLE_S5.1_group_exceedance_days.csv\n")
