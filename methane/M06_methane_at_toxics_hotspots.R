# ==============================================================
# M06_methane_at_toxics_hotspots.R
# Uses methane as an independent tracer to further discriminate the
# the persistent multi-pollutant air-toxics hotspot groups (count read from MASTER):
#   - CH4 behavior within 100 m of each toxics group centroid
#     (n obs, days, median, % >= p95, % >= p99 of campaign CH4)
#   - distance from each group to the nearest CH4 cluster and to the
#     2 persistent CH4 hotspots
#   - a CH4 co-elevation index that separates gas-infrastructure /
#     fugitive-petroleum groups from traffic / solvent groups
# Outputs:
#   methane_at_toxics_hotspots.csv
#   FinalFig/FIG_methane_at_toxics_hotspots.png (heatmap + map panel)
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("M06: Methane as a discriminator at the persistent toxics hotspot groups")

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(patchwork)
})

RADIUS_M <- as.numeric(Sys.getenv("CH4_RADIUS", "100"))  # match Fig 4C convention
diag_msg("  radius around each toxics group: ", RADIUS_M, " m (CH4_RADIUS to override)")

# ---- inputs ----
load(file.path(BASE, "mobile_methane_wind_bg.RData"))
ch4 <- as.data.table(df_ch4_bg)
groups <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
cl <- fread(file.path(BASE, "cent_out_methane_all.csv"))
diag_msg("  methane rows: ", format(nrow(ch4), big.mark = ","),
         " | toxics groups: ", nrow(groups), " | CH4 clusters: ", nrow(cl))
stopifnot(nrow(groups) >= 15)

p95 <- quantile(ch4$ch4_ppm, 0.95, na.rm = TRUE)
p99 <- quantile(ch4$ch4_ppm, 0.99, na.rm = TRUE)
camp_med <- median(ch4$ch4_ppm, na.rm = TRUE)
diag_msg(sprintf("  campaign CH4: median %.3f | p95 %.3f | p99 %.3f ppm", camp_med, p95, p99))

# date column for day counts
dcol <- grep("^date$|^day$", names(ch4), value = TRUE)[1]
if (is.na(dcol)) dcol <- grep("date", names(ch4), value = TRUE)[1]
ch4[, day_ := as.Date(get(dcol))]

# ---- per-group CH4 stats within RADIUS_M ----
to_m <- function(lon, lat) {
  p <- st_transform(st_as_sf(data.frame(lon = lon, lat = lat),
                             coords = c("lon", "lat"), crs = 4326), 32613)
  st_coordinates(p)
}
gxy <- to_m(groups$Longitude, groups$Latitude)
cxy <- to_m(ch4$Longitude, ch4$Latitude)
ch4[, `:=`(mx = cxy[, 1], my = cxy[, 2])]

res <- rbindlist(lapply(seq_len(nrow(groups)), function(i) {
  d2 <- (ch4$mx - gxy[i, 1])^2 + (ch4$my - gxy[i, 2])^2
  s <- ch4[d2 <= RADIUS_M^2]
  data.table(
    group_id   = groups$group_id[i],
    pollutants = groups$pollutants[i],
    n_ch4_obs  = nrow(s),
    n_ch4_days = uniqueN(s$day_),
    ch4_med    = round(median(s$ch4_ppm, na.rm = TRUE), 3),
    ch4_p95_here = round(quantile(s$ch4_ppm, 0.95, na.rm = TRUE), 3),
    pct_ge_p95 = round(100 * mean(s$ch4_ppm >= p95, na.rm = TRUE), 1),
    pct_ge_p99 = round(100 * mean(s$ch4_ppm >= p99, na.rm = TRUE), 1),
    days_with_high = uniqueN(s[ch4_ppm >= p99][["day_"]])
  )
}))

# distance to nearest CH4 cluster + to the 2 persistent CH4 hotspots
clxy <- to_m(cl$lon, cl$lat)
pers <- cl[persistent == TRUE]
pxy <- to_m(pers$lon, pers$lat)
res[, dist_nearest_ch4_cluster_km := round(sapply(seq_len(.N), function(i)
  sqrt(min((clxy[, 1] - gxy[i, 1])^2 + (clxy[, 2] - gxy[i, 2])^2)) / 1000), 2)]
res[, dist_nearest_persistent_ch4_km := round(sapply(seq_len(.N), function(i)
  sqrt(min((pxy[, 1] - gxy[i, 1])^2 + (pxy[, 2] - gxy[i, 2])^2)) / 1000), 2)]

# CH4 co-elevation index: fraction >= p95, scaled by day-consistency
res[, ch4_index := round(pct_ge_p95 * pmin(1, days_with_high / 10), 1)]
res[, ch4_class := fifelse(pct_ge_p95 >= 15 & days_with_high >= 5, "CH4-enriched",
                    fifelse(pct_ge_p95 >= 5, "CH4-intermediate", "CH4-quiet"))]

setorder(res, -pct_ge_p95)
diag_section("M06: per-group results (sorted by % of obs >= campaign CH4 p95)")
print(res[, .(group_id, n_ch4_obs, n_ch4_days, ch4_med, pct_ge_p95, pct_ge_p99,
              days_with_high, dist_nearest_ch4_cluster_km, ch4_class)])
capture.output(print(res), file = .DIAG_LOG, append = TRUE)

diag_msg("\n  [INTERPRET] CH4-enriched groups co-locate with natural-gas / fugitive-",
         "petroleum sources; CH4-quiet groups with 4-5 aromatic pollutants are",
         " consistent with traffic/solvent sources (compare Figure 4D classes).")
n_cov <- res[n_ch4_obs < 100, .N]
if (n_cov > 0) diag_msg("  [CAVEAT] ", n_cov, " group(s) have < 100 CH4 obs within ",
                        RADIUS_M, " m — methane sampling began later; treat those as low-confidence.")

fwrite(res, file.path(BASE, "methane_at_toxics_hotspots.csv"))

# ---- figure: heatmap of CH4 metrics by group + context map ----
hm <- data.table::melt(res[, .(group_id, `% obs >= p95` = pct_ge_p95, `% obs >= p99` = pct_ge_p99,
                   `days with high CH4` = days_with_high,
                   `median CH4 (ppm)` = ch4_med)],
           id.vars = "group_id")
hm[, group_id := factor(group_id, levels = res$group_id)]
p1 <- ggplot(hm, aes(x = variable, y = group_id, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 2.8) +
  scale_fill_viridis_c(option = "C", trans = "sqrt", name = NULL) +
  facet_wrap(~variable, scales = "free_x", nrow = 1) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.background = element_rect(fill = "grey95")) +
  labs(title = sprintf("Methane at the %d persistent air-toxics hotspot groups", nrow(groups)),
       subtitle = sprintf("Within %g m of each group centroid | campaign p95 = %.2f ppm, p99 = %.2f ppm",
                          RADIUS_M, p95, p99),
       x = NULL, y = "Toxics hotspot group")

gmap <- data.frame(gxy, group_id = groups$group_id,
                   ch4_class = res[match(groups$group_id, res$group_id), ch4_class])
cmap <- data.frame(clxy, persistent = cl$persistent)
p2 <- ggplot() +
  geom_point(data = cmap, aes(X, Y, shape = persistent), color = "grey40", size = 2) +
  geom_point(data = gmap, aes(X, Y, color = ch4_class), size = 3.5) +
  geom_text(data = gmap, aes(X, Y, label = group_id), size = 2.5, vjust = -1) +
  scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 1),
                     name = "CH4 cluster\n(persistent)") +
  scale_color_manual(values = c("CH4-enriched" = "red3",
                                "CH4-intermediate" = "orange2",
                                "CH4-quiet" = "steelblue"), name = NULL) +
  coord_equal() + theme_bw(base_size = 11) +
  labs(title = "Toxics groups (colored by CH4 class) vs CH4 clusters (grey)",
       x = NULL, y = NULL)

ggsave(file.path(BASE, "FinalFig", "FIG_methane_at_toxics_hotspots.png"),
       p1 / p2 + plot_layout(heights = c(1, 1.1)),
       width = 11, height = 12, dpi = 300, bg = "white")
diag_msg("\nSaved: methane_at_toxics_hotspots.csv + FinalFig/FIG_methane_at_toxics_hotspots.png")
