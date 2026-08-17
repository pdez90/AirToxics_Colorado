# ==============================================================
# M03_hotspots.R
# Methane hotspot analysis, mirroring the manuscript pipeline:
#   - high-concentration events: raw 1-s values >= 99th percentile
#   - DBSCAN clustering (eps = 100 m, minPts = 5)
#   - persistence: cluster must exceed 10% of all high events AND
#     occur on >10% of sampling days with any high event
#   - centroids + summary CSVs, comparison to the 17 existing
#     multi-pollutant hotspot groups
# Outputs: hs_df_methane.RData, cent_out_methane_all.csv,
#          cent_out_methane_persistent.csv, methane_hotspot_summary.csv
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("M03: Methane hotspots (99th pct -> DBSCAN 100 m -> 10%/10% persistence)")

suppressPackageStartupMessages({
  library(data.table); library(dbscan); library(geosphere)
})

load(file.path(BASE, "mobile_methane_wind_bg.RData"))  # df_ch4_bg
ch4 <- as.data.table(df_ch4_bg)

# ----------------------------------------------------------------
# 1) Threshold: 99th percentile of raw 1-s values (as in ms 2.5.3)
# ----------------------------------------------------------------
p99 <- quantile(ch4$ch4_ppm, 0.99, na.rm = TRUE)
p95 <- quantile(ch4$ch4_ppm, 0.95, na.rm = TRUE)
diag_msg(sprintf("  CH4 thresholds: p95 = %.3f ppm | p99 = %.3f ppm (report BOTH in SI)", p95, p99))
diag_msg(sprintf("  For context, toxics p99s (ppb): %s",
                 paste(names(REF$p99), signif(REF$p99, 3), sep = "=", collapse = ", ")))

hs <- ch4[ch4_ppm >= p99 & !is.na(Latitude)]
n_days_total <- uniqueN(as.Date(ch4$date))
n_days_high  <- uniqueN(as.Date(hs$date))
diag_msg(sprintf("  high-concentration events: %s (on %d of %d sampling days)",
                 format(nrow(hs), big.mark = ","), n_days_high, n_days_total))

# ----------------------------------------------------------------
# 2) DBSCAN in meters (project to local ENU around domain center)
# ----------------------------------------------------------------
ctr <- c(median(hs$Longitude), median(hs$Latitude))
hs[, x_m := (Longitude - ctr[1]) * cos(ctr[2] * pi / 180) * 111320]
hs[, y_m := (Latitude - ctr[2]) * 110540]
db <- dbscan::dbscan(as.matrix(hs[, .(x_m, y_m)]), eps = 100, minPts = 5)
hs[, cluster := db$cluster]
n_clusters <- uniqueN(hs$cluster[hs$cluster > 0])
noise_pct <- 100 * mean(hs$cluster == 0)
diag_msg(sprintf("  DBSCAN(eps=100 m, minPts=5): %d clusters | %.1f%% noise points",
                 n_clusters, noise_pct))
diag_msg("  (toxics analogue in ms: 160 initial clusters across all pollutants)")

# ----------------------------------------------------------------
# 3) Persistence: >10% of high events AND >10% of high-event days
# ----------------------------------------------------------------
thr_events <- 0.10 * nrow(hs)
thr_days   <- 0.10 * n_days_high
cl_sum <- hs[cluster > 0, .(
  n_events = .N,
  n_days   = uniqueN(as.Date(date)),
  lat      = mean(Latitude), lon = mean(Longitude),
  ch4_med  = median(ch4_ppm), ch4_max = max(ch4_ppm),
  enh_med  = median(ch4_enh, na.rm = TRUE), enh_max = max(ch4_enh, na.rm = TRUE),
  first    = min(as.Date(date)), last = max(as.Date(date))
), by = cluster][order(-n_events)]
cl_sum[, persistent := n_events > thr_events & n_days > thr_days]
diag_msg(sprintf("  persistence thresholds: n_events > %.1f AND n_days > %.1f", thr_events, thr_days))
diag_msg(sprintf("  persistent methane hotspots: %d of %d clusters", sum(cl_sum$persistent), nrow(cl_sum)))
diag_msg("  top 10 clusters:")
for (i in seq_len(min(10, nrow(cl_sum))))
  diag_msg(sprintf("    cl %-3d n=%-6d days=%-3d CH4med=%.2f max=%.1f ppm  (%.5f, %.5f)%s",
                   cl_sum$cluster[i], cl_sum$n_events[i], cl_sum$n_days[i],
                   cl_sum$ch4_med[i], cl_sum$ch4_max[i], cl_sum$lat[i], cl_sum$lon[i],
                   ifelse(cl_sum$persistent[i], "  <-- PERSISTENT", "")))

# ----------------------------------------------------------------
# 4) SENSITIVITY (reviewer 2 comment 7 applies to methane too):
#    vary eps and persistence thresholds, report stability
# ----------------------------------------------------------------
diag_section("M03-DIAG: sensitivity of persistent-cluster count")
for (eps in c(50, 100, 200)) {
  db_s <- dbscan::dbscan(as.matrix(hs[, .(x_m, y_m)]), eps = eps, minPts = 5)
  tmp <- copy(hs)[, cl := db_s$cluster]
  cs <- tmp[cl > 0, .(n = .N, d = uniqueN(as.Date(date))), by = cl]
  for (fr in c(0.05, 0.10, 0.15)) {
    np <- cs[n > fr * nrow(hs) & d > fr * n_days_high, .N]
    diag_msg(sprintf("  eps=%3d m, threshold=%2.0f%%: %d persistent clusters", eps, 100 * fr, np))
  }
}

# ----------------------------------------------------------------
# 5) Compare methane hotspots to the 17 multi-pollutant groups
# ----------------------------------------------------------------
diag_section("M03-DIAG: proximity to existing multi-pollutant hotspot groups")
idx_f <- file.path(BASE, "MASTER_hotspot_group_index.csv")
if (file.exists(idx_f)) {
  grp <- fread(idx_f)
  latc <- grep("lat", names(grp), ignore.case = TRUE, value = TRUE)[1]
  lonc <- grep("lon", names(grp), ignore.case = TRUE, value = TRUE)[1]
  gidc <- grep("group", names(grp), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(latc) && !is.na(lonc)) {
    for (i in which(cl_sum$persistent)) {
      d <- distHaversine(cbind(cl_sum$lon[i], cl_sum$lat[i]),
                         cbind(grp[[lonc]], grp[[latc]]))
      j <- which.min(d)
      diag_msg(sprintf("  CH4 cluster %d -> nearest toxics group %s at %.0f m %s",
                       cl_sum$cluster[i], as.character(grp[[gidc]][j]), min(d),
                       ifelse(min(d) < 200, "<-- likely SAME hotspot (add CH4 to its fingerprint)",
                              "(new methane-specific location)")))
    }
  } else diag_msg("  [WARN] lat/lon columns not identified in group index")
} else diag_msg("  [WARN] MASTER_hotspot_group_index.csv not found — run R05 first")

# ----------------------------------------------------------------
# 6) Save outputs
# ----------------------------------------------------------------
hs_df_methane <- as.data.frame(hs)
save(hs_df_methane, file = file.path(BASE, "hs_df_methane.RData"))
fwrite(cl_sum, file.path(BASE, "cent_out_methane_all.csv"))
fwrite(cl_sum[persistent == TRUE], file.path(BASE, "cent_out_methane_persistent.csv"))
fwrite(data.table(pollutant = "Methane_ppm", p95 = p95, p99 = p99,
                  n_high_events = nrow(hs), n_days_high = n_days_high,
                  thr_events = thr_events, thr_days = thr_days,
                  n_clusters = n_clusters, n_persistent = sum(cl_sum$persistent)),
       file.path(BASE, "methane_hotspot_summary.csv"))
diag_msg("\nSaved: hs_df_methane.RData, cent_out_methane_all/persistent.csv, methane_hotspot_summary.csv")
