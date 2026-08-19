# ==============================================================
# 63  DE-HELD (SAMPLING-RATE) SENSITIVITY  (SI)
# The CDPHE-delivered record is on a uniform 1-second time base.
# The Vocus Eiger (aromatics) acquires at 1 s, but the Vocus B
# (HCN, 2 s) and Picarro G2204 (H2S/CH4, 5 s) acquire more slowly,
# and their most recent reading is carried forward to each
# intervening second. This script re-derives the paper's core
# quantities using ONLY genuinely-new values ("de-held": within
# each Asset-Site-day, a value is kept only when it differs from
# the immediately preceding value) and reports how much changes.
#
# Interpretation: de-holding is deliberately AGGRESSIVE - a run of
# genuinely-measured identical values (e.g. below-MDL H2S sitting at
# 0) is also collapsed, so the de-held N is a LOWER bound on the true
# independent-sample count. If the maps/blocks/hotspots are stable
# even under this aggressive thinning, they are stable to the
# sampling-rate difference.
#
# Outputs (BASE):
#   TABLE_deheld_summary.csv     campaign stats, filled vs de-held
#   TABLE_deheld_cellmaps.csv    500 m cell-median map agreement
#   TABLE_deheld_blocks.csv      census-block benzene metric
#   TABLE_deheld_hotspots.csv    hotspot-group recovery
#   FinalFig/FIG_deheld_sensitivity.png
# Runtime ~3-6 min (hotspot block ~a few min).
# ==============================================================
suppressPackageStartupMessages({
  library(data.table); library(sf); library(dbscan)
  library(ggplot2); library(scales); library(patchwork)
})
set.seed(42)
BASE  <- "/Users/priyanka/Downloads/Suncor"
DOHOT <- TRUE   # set FALSE to skip the (slower) hotspot-recovery block

message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]
stopifnot(all(c("date", "AssetSiteDay") %in% names(df)))
df[, day := as.Date(date)]
message("  rows: ", format(nrow(df), big.mark = ","),
        " | Asset-Site-days: ", uniqueN(df$AssetSiteDay))

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")
stopifnot(all(unlist(POLLS) %in% names(df)))

# --------------------------------------------------------------
# De-held mask: TRUE where the value is genuinely new within the
# Asset-Site-day (differs from the immediately preceding row's value).
# Holds are strictly consecutive equal rows on the 1-s grid, so a
# simple lag comparison recovers the native acquisition instants.
# --------------------------------------------------------------
setorder(df, AssetSiteDay, date)
genuine_mask <- function(col) {
  v      <- df[[col]]
  prev_v <- shift(v, 1L)
  same   <- df$AssetSiteDay == shift(df$AssetSiteDay, 1L)
  # genuine if finite AND (first row of group, OR previous value NA/absent,
  # OR value changed from the previous row)
  is.finite(v) & !(same & is.finite(prev_v) & (v == prev_v))
}
for (pn in names(POLLS)) df[, (paste0("gen_", pn)) := genuine_mask(POLLS[[pn]])]

# ---- 1) campaign summary: filled vs de-held -------------------
message("\n[1] Campaign summary (filled vs de-held)")
summ <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]; v <- df[[col]]; fin <- is.finite(v)
  gen <- df[[paste0("gen_", pn)]]
  rbind(
    data.table(pollutant = pn, case = "filled", n = sum(fin),
               median = round(median(v[fin]), 3),
               p95 = round(quantile(v[fin], .95), 3),
               p99 = round(quantile(v[fin], .99), 3)),
    data.table(pollutant = pn, case = "de-held", n = sum(gen),
               median = round(median(v[gen]), 3),
               p95 = round(quantile(v[gen], .95), 3),
               p99 = round(quantile(v[gen], .99), 3)))
}))
summ[, pct_genuine := round(100 * n / n[case == "filled"], 1), by = pollutant]
fwrite(summ, file.path(BASE, "TABLE_deheld_summary.csv")); print(summ)

# ---- 2) 500 m cell-median map agreement -----------------------
message("\n[2] 500 m cell-median map agreement (median of daily medians)")
grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"), quiet = TRUE)
st_crs(grid) <- 26913
cent <- st_centroid(st_geometry(grid))
pts  <- st_transform(st_as_sf(df[, .(Longitude, Latitude)],
          coords = c("Longitude", "Latitude"), crs = 4326), 26913)
df[, cell := grid$id[st_nearest_feature(pts, cent)]]
rm(pts); gc()

cellmap <- function(col, gencol) {
  # returns per-cell median of daily medians for filled and de-held
  d  <- df[is.finite(get(col)) & !is.na(cell)]
  ff <- d[, .(dmed = median(get(col))), by = .(cell, day)][
          , .(filled = median(dmed), nd = .N), by = cell]
  dg <- d[get(gencol) == TRUE, .(dmed = median(get(col))), by = .(cell, day)][
          , .(deheld = median(dmed), nd_g = .N), by = cell]
  merge(ff, dg, by = "cell", all = TRUE)
}
cmres <- rbindlist(lapply(names(POLLS), function(pn) {
  m <- cellmap(POLLS[[pn]], paste0("gen_", pn))
  m3 <- m[nd >= 3 & nd_g >= 3 & is.finite(filled) & is.finite(deheld)]
  data.table(pollutant = pn,
             cells = nrow(m3),
             spearman = round(cor(m3$filled, m3$deheld, method = "spearman"), 3),
             pearson  = round(cor(m3$filled, m3$deheld), 3),
             median_abs_pct_diff = round(100 * median(
               abs(m3$deheld - m3$filled) / pmax(m3$filled, 1e-6)), 1))
}))
fwrite(cmres, file.path(BASE, "TABLE_deheld_cellmaps.csv")); print(cmres)
# keep benzene cell scatter for the figure
bz_cells <- cellmap("Benzene_ppb", "gen_Benzene")[nd >= 3 & nd_g >= 3 &
              is.finite(filled) & is.finite(deheld)]

# ---- 3) census-block benzene metric ---------------------------
message("\n[3] Census-block benzene (the AirToxScreen comparison)")
g   <- st_read(file.path(BASE,
        "censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"),
        quiet = TRUE)
gll <- st_transform(g, 4326)
idcol <- grep("GEOID", names(gll), value = TRUE)[1]
bz <- df[is.finite(Benzene_ppb) & is.finite(Latitude) & is.finite(Longitude),
         .(Benzene_ppb, gen = gen_Benzene, Latitude, Longitude, day)]
bz[, `:=`(rlon = round(Longitude, 5), rlat = round(Latitude, 5))]
uloc <- unique(bz[, .(rlon, rlat)])
up <- st_as_sf(uloc, coords = c("rlon", "rlat"), crs = 4326, remove = FALSE)
w  <- st_within(up, gll)
uloc[, block := st_drop_geometry(gll)[[idcol]][
        vapply(w, function(z) if (length(z)) z[1] else NA_integer_, 1L)]]
bz <- merge(bz, uloc, by = c("rlon", "rlat"))[!is.na(block)]
message("  benzene obs in common blocks: ", format(nrow(bz), big.mark = ","))

blkmetric <- function(sub) sub[, .(dmed = median(Benzene_ppb)), by = .(block, day)][
                             , .(bval = median(dmed)), by = block]
b_fill <- blkmetric(bz);                 setnames(b_fill, "bval", "filled")
b_deh  <- blkmetric(bz[gen == TRUE]);    setnames(b_deh,  "bval", "deheld")
blk <- merge(b_fill, b_deh, by = "block")
ats <- as.data.table(st_drop_geometry(gll))[, .(block = get(idcol),
        ats = benzene_ppb_airtox)]
blk <- merge(blk, ats, by = "block")
SCALE <- 1.149
bres <- data.table(
  blocks               = nrow(blk),
  spearman_fill_deheld = round(cor(blk$filled, blk$deheld, method = "spearman"), 3),
  pearson_fill_deheld  = round(cor(blk$filled, blk$deheld), 3),
  agg_ratio_filled = round(sum(blk$filled * SCALE) / sum(blk$ats), 3),
  agg_ratio_deheld = round(sum(blk$deheld * SCALE) / sum(blk$ats), 3),
  blocks_gt2x_filled = sum(blk$filled * SCALE / blk$ats > 2, na.rm = TRUE),
  blocks_gt2x_deheld = sum(blk$deheld * SCALE / blk$ats > 2, na.rm = TRUE))
fwrite(bres, file.path(BASE, "TABLE_deheld_blocks.csv")); print(t(bres))

# ---- 4) hotspot-group recovery --------------------------------
hres <- NULL
if (DOHOT) {
  message("\n[4] Hotspot-group recovery (filled vs de-held)")
  dfx <- df[is.finite(Latitude) & is.finite(Longitude)]
  xy  <- st_coordinates(st_transform(st_as_sf(dfx[, .(Longitude, Latitude)],
           coords = c("Longitude", "Latitude"), crs = 4326), 32613))
  dfx[, `:=`(px = xy[, 1], py = xy[, 2])]
  master <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
  mxy <- st_coordinates(st_transform(st_as_sf(master[, .(Longitude, Latitude)],
           coords = c("Longitude", "Latitude"), crs = 4326), 32613))
  run_groups <- function(dd, use_gen) {
    keep <- list()
    for (pn in names(POLLS)) {
      col <- POLLS[[pn]]; v <- dd[[col]]
      sel <- is.finite(v); if (use_gen) sel <- sel & dd[[paste0("gen_", pn)]]
      if (sum(sel) < 500) next
      thr <- quantile(v[sel], .99)
      s <- dd[sel & v > thr, .(px, py, day)]
      if (nrow(s) < 10) next
      cid <- dbscan::dbscan(as.matrix(s[, .(px, py)]), eps = 100, minPts = 1)$cluster
      cs <- data.table(clust = cid, x = s$px, y = s$py, day = s$day)[
        , .(n = .N, n_days = uniqueN(day), x = mean(x), y = mean(y)), by = clust]
      keep[[pn]] <- cs[n >= quantile(n, .9) & n_days >= quantile(n_days, .9)][
        , pollutant := pn]
    }
    keep <- rbindlist(keep)
    if (nrow(keep) == 0) return(keep[0])
    keep[, group := dbscan::dbscan(as.matrix(keep[, .(x, y)]), eps = 100,
                                   minPts = 1)$cluster]
    keep[, .(n_poll = uniqueN(pollutant), x = mean(x), y = mean(y)),
         by = group][n_poll >= 3]
  }
  recov <- function(g3) if (nrow(g3) == 0) 0 else
    mean(vapply(seq_len(nrow(mxy)), function(i)
      min(sqrt((g3$x - mxy[i, 1])^2 + (g3$y - mxy[i, 2])^2)) <= 300, TRUE))
  g_fill <- run_groups(dfx, FALSE)
  g_deh  <- run_groups(dfx, TRUE)
  hres <- data.table(
    case = c("filled", "de-held"),
    multipoll_groups = c(nrow(g_fill), nrow(g_deh)),
    recovery_of_17_master = c(round(recov(g_fill), 3), round(recov(g_deh), 3)))
  fwrite(hres, file.path(BASE, "TABLE_deheld_hotspots.csv")); print(hres)
}

# ---- 5) figure ------------------------------------------------
message("\n[5] Figure")
sm <- copy(summ)
sm[, pollutant := factor(pollutant, levels = names(POLLS))]
pA <- ggplot(sm[case == "de-held"], aes(pollutant, pct_genuine)) +
  geom_col(fill = "#4292c6", color = "grey25", width = 0.7) +
  geom_text(aes(label = paste0(pct_genuine, "%")), vjust = -0.4, size = 3) +
  ylim(0, 105) +
  labs(x = NULL, y = "Genuinely-new values (% of 1-s records)",
       title = "A) Independent samples after de-holding") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
pB <- ggplot(bz_cells, aes(filled, deheld)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
  geom_point(alpha = 0.5, size = 0.8, color = "#2166ac") +
  labs(x = "Filled (delivered 1-s)", y = "De-held (genuine only)",
       title = "B) Benzene 500 m cell medians (ppb)") +
  theme_bw(base_size = 11)
cm <- copy(cmres); cm[, pollutant := factor(pollutant, levels = names(POLLS))]
pC <- ggplot(cm, aes(pollutant, spearman)) +
  geom_col(fill = "#41ab5d", color = "grey25", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", spearman)), vjust = -0.4, size = 3) +
  coord_cartesian(ylim = c(0, 1.05)) +
  labs(x = NULL, y = "Spearman r (filled vs de-held cell maps)",
       title = "C) Concentration-map agreement") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(BASE, "FinalFig", "FIG_deheld_sensitivity.png"),
       (pA | pC) / pB, width = 10, height = 8, dpi = 350, bg = "white")
message("[Saved] FinalFig/FIG_deheld_sensitivity.png")
message("DONE.")
