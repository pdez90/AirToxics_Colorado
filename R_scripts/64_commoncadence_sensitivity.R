# ==============================================================
# 64  COMMON-CADENCE (TIME-BIN) SENSITIVITY  (SI)
# The preferred robustness check for the different instrument
# sampling rates when native-cadence data are unavailable.
#
# Rather than distinguishing "held" from "genuine" values (script
# 63, which over-discards below-MDL runs and just adds noise to the
# slow species), we put every species on a common coarse time base
# and AVERAGE. Within each Asset-Site-day the delay-corrected 1-s
# record is binned into fixed windows of B seconds; per window we
# take the mean of each pollutant and the mean GPS position. This:
#   - removes the forward-fill artifact (averaging a run of held
#     H2S values returns that value, at the window-mean position);
#   - keeps ALL genuine information (nothing discarded);
#   - places every species at one shared, honest position;
#   - directly measures how the ~36-80 m spatial smear of the slow
#     species (H2S 5 s, HCN 2 s) at driving speed affects the maps,
#     hotspots, and the census-block risk comparison.
#
# ORDER OF OPERATIONS: the instrument delay is applied per 1-s
# measurement BEFORE binning (it is already baked into
# mobile_wswd; script 03). Delay-then-average keeps each
# concentration paired with the correct GPS position and avoids
# rounding the delay (e.g. H2S 21 s) to the bin size. This is the
# well-posed version of "average to native cadence, then evaluate".
#
# B = 5 s is the primary check (slowest instrument, Picarro); 2 and
# 10 s bracket it. The 1-s delivered record is the baseline.
#
# Outputs (BASE):
#   TABLE_cadence_cellmaps.csv   cell-map Spearman vs 1-s, by bin
#   TABLE_cadence_blocks.csv     census-block benzene, by bin
#   TABLE_cadence_hotspots.csv   hotspot-group recovery, by bin
#   FinalFig/FIG_cadence_sensitivity.png
# Runtime ~5-10 min.
# ==============================================================
suppressPackageStartupMessages({
  library(data.table); library(sf); library(dbscan)
  library(ggplot2); library(scales); library(patchwork)
})
set.seed(42)
BASE <- "/Users/priyanka/Downloads/Suncor"
BINS <- c(2, 5, 10)   # seconds; the 1-s delivered record is the baseline
DOHOT <- TRUE

message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData")); df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]
df <- df[is.finite(Latitude) & is.finite(Longitude)]
df[, day := as.Date(date)]
df[, epoch := as.numeric(date)]              # POSIXct -> seconds (correct in R)
message("  rows: ", format(nrow(df), big.mark = ","))

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")

# ---- spatial scaffolding --------------------------------------
grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"), quiet = TRUE)
st_crs(grid) <- 26913
cent <- st_centroid(st_geometry(grid))
g    <- st_read(file.path(BASE,
         "censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"), quiet = TRUE)
gll  <- st_transform(g, 4326)
idcol <- grep("GEOID", names(gll), value = TRUE)[1]
# (2026-08-20) population carried so the aggregate ratio below is
# POPULATION-WEIGHTED, matching the published quantity in scripts 20 and 58.
ats  <- as.data.table(st_drop_geometry(gll))[, .(block = get(idcol),
                                                 ats = benzene_ppb_airtox,
                                                 pop = Population_airtox)]
SCALE <- 1.149

assign_cell <- function(d) {
  p <- st_transform(st_as_sf(d[, .(Longitude, Latitude)],
        coords = c("Longitude", "Latitude"), crs = 4326), 26913)
  grid$id[st_nearest_feature(p, cent)]
}
assign_block <- function(d) {                # proper st_within on THESE positions
  u <- unique(d[, .(Longitude, Latitude)])
  up <- st_as_sf(u, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  w  <- st_within(up, gll)
  u[, block := st_drop_geometry(gll)[[idcol]][
        vapply(w, function(z) if (length(z)) z[1] else NA_integer_, 1L)]]
  # BUGFIX (2026-08-20): this was
  #   merge(d, u, by = c("Longitude","Latitude"), all.x = TRUE)$block
  # merge() returns its rows SORTED BY THE JOIN KEY, but the caller assigns the
  # result positionally (`b0[, block := assign_block(b0)]`), so the block label
  # for one row landed on a different row. Every figure in TABLE_cadence_blocks
  # .csv - including the 1-s BASELINE row - was computed on scrambled block
  # membership, which homogenises blocks and would manufacture the "risk ratio
  # is invariant to bin size" result. A data.table join keyed on `d` returns
  # the result in `d`'s order.
  stopifnot(is.data.table(u), is.data.table(d))
  u[d, on = c("Longitude", "Latitude"), x.block]
}

# ---- binning: mean conc + mean position per B-second window ----
make_binned <- function(B) {
  if (B <= 1) return(copy(df))
  df[, tbin := floor(epoch / B)]
  agg <- df[, c(list(Longitude = mean(Longitude, na.rm = TRUE),
                     Latitude  = mean(Latitude,  na.rm = TRUE), day = day[1]),
                setNames(lapply(POLLS, function(cc) mean(get(cc), na.rm = TRUE)),
                         unname(POLLS))),
            by = .(AssetSiteDay, tbin)]
  for (cc in unname(POLLS)) set(agg, which(is.nan(agg[[cc]])), cc, NA_real_)
  agg
}

cell_medmap <- function(d, col) {
  dd <- d[is.finite(get(col))]
  dd[, .(dmed = median(get(col))), by = .(cell, day)][
     , .(v = median(dmed), nd = .N), by = cell][nd >= 3]
}
block_bz <- function(d) {
  d[is.finite(Benzene_ppb) & !is.na(block),
    .(dmed = median(Benzene_ppb)), by = .(block, day)][
    , .(bval = median(dmed)), by = block]
}

# ---- hotspot recovery machinery -------------------------------
if (DOHOT) {
  master <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
  mxy <- st_coordinates(st_transform(st_as_sf(master[, .(Longitude, Latitude)],
           coords = c("Longitude", "Latitude"), crs = 4326), 32613))
}
run_groups <- function(dd) {
  xy <- st_coordinates(st_transform(st_as_sf(dd[, .(Longitude, Latitude)],
          coords = c("Longitude", "Latitude"), crs = 4326), 32613))
  dd <- copy(dd); dd[, `:=`(px = xy[, 1], py = xy[, 2])]
  keep <- list()
  for (pn in names(POLLS)) {
    col <- POLLS[[pn]]; v <- dd[[col]]; sel <- is.finite(v)
    if (sum(sel) < 500) next
    thr <- quantile(v[sel], .99)
    s <- dd[sel & v > thr, .(px, py, day)]
    if (nrow(s) < 10) next
    cid <- dbscan::dbscan(as.matrix(s[, .(px, py)]), eps = 100, minPts = 1)$cluster
    cs <- data.table(clust = cid, x = s$px, y = s$py, day = s$day)[
      , .(n = .N, n_days = uniqueN(day), x = mean(x), y = mean(y)), by = clust]
    keep[[pn]] <- cs[n >= quantile(n, .9) & n_days >= quantile(n_days, .9)][, pollutant := pn]
  }
  keep <- rbindlist(keep)
  if (nrow(keep) == 0) return(keep[0])
  keep[, group := dbscan::dbscan(as.matrix(keep[, .(x, y)]), eps = 100, minPts = 1)$cluster]
  keep[, .(n_poll = uniqueN(pollutant), x = mean(x), y = mean(y)), by = group][n_poll >= 3]
}
recov <- function(g3) if (nrow(g3) == 0) 0 else
  mean(vapply(seq_len(nrow(mxy)), function(i)
    min(sqrt((g3$x - mxy[i, 1])^2 + (g3$y - mxy[i, 2])^2)) <= 300, TRUE))

# ---- baseline (1-s) -------------------------------------------
message("\n=== baseline: 1 s ===")
b0 <- make_binned(1); b0[, cell := assign_cell(b0)]; b0[, block := assign_block(b0)]
base_cell <- setNames(lapply(names(POLLS), function(pn) cell_medmap(b0, POLLS[[pn]])), names(POLLS))
bb0 <- merge(block_bz(b0), ats, by = "block")
base_ratio <- sum(bb0$bval * SCALE) / sum(bb0$ats)
base_gt2   <- sum(bb0$bval * SCALE / bb0$ats > 2, na.rm = TRUE)
g0 <- if (DOHOT) run_groups(b0) else NULL

cm_all <- list(); bl_all <- list(); ht_all <- list()
bl_all[["1"]] <- data.table(bin_s = 1, blocks = nrow(bb0),
  agg_ratio = round(base_ratio, 3), blocks_gt2x = base_gt2, spearman_vs_1s = 1)
if (DOHOT) ht_all[["1"]] <- data.table(bin_s = 1, groups = nrow(g0),
  recovery_of_17 = round(recov(g0), 3))
bz_cell_base <- base_cell[["Benzene"]]

for (B in BINS) {
  message("\n=== bin = ", B, " s ===")
  d <- make_binned(B); d[, cell := assign_cell(d)]; d[, block := assign_block(d)]
  for (pn in names(POLLS)) {
    m <- merge(base_cell[[pn]][, .(cell, base = v)],
               cell_medmap(d, POLLS[[pn]])[, .(cell, binned = v)], by = "cell")
    cm_all[[paste(pn, B)]] <- data.table(pollutant = pn, bin_s = B, cells = nrow(m),
      spearman = round(cor(m$base, m$binned, method = "spearman"), 3))
  }
  bb <- merge(block_bz(d), ats, by = "block")
  bcmp <- merge(bb0[, .(block, base = bval)], bb[, .(block, binned = bval)], by = "block")
  bl_all[[as.character(B)]] <- data.table(bin_s = B, blocks = nrow(bb),
    agg_ratio = round(with(bb[is.finite(pop) & pop > 0 & is.finite(ats)],
                           sum(pop * bval * SCALE) / sum(pop * ats)), 3),
    blocks_gt2x = sum(bb$bval * SCALE / bb$ats > 2, na.rm = TRUE),
    spearman_vs_1s = round(cor(bcmp$base, bcmp$binned, method = "spearman"), 3))
  message("  block benzene: ratio ", bl_all[[as.character(B)]]$agg_ratio,
          " | >2x ", bl_all[[as.character(B)]]$blocks_gt2x,
          " | Spearman ", bl_all[[as.character(B)]]$spearman_vs_1s)
  if (DOHOT) {
    gg <- run_groups(d)
    ht_all[[as.character(B)]] <- data.table(bin_s = B, groups = nrow(gg),
      recovery_of_17 = round(recov(gg), 3))
    message("  hotspots: ", nrow(gg), " groups, recovery ", ht_all[[as.character(B)]]$recovery_of_17)
  }
}
cm <- rbindlist(cm_all); bl <- rbindlist(bl_all)
fwrite(cm, file.path(BASE, "TABLE_cadence_cellmaps.csv"))
fwrite(bl, file.path(BASE, "TABLE_cadence_blocks.csv"))
message("\n--- cell-map Spearman vs 1-s ---"); print(data.table::dcast(cm, pollutant ~ bin_s, value.var = "spearman"))
message("--- census-block benzene by bin ---"); print(bl)
if (DOHOT) { ht <- rbindlist(ht_all)
  fwrite(ht, file.path(BASE, "TABLE_cadence_hotspots.csv"))
  message("--- hotspot recovery by bin ---"); print(ht) }

# ---- figure ---------------------------------------------------
cmx <- cm; cmx[, pollutant := factor(pollutant, levels = names(POLLS))]
pA <- ggplot(cmx, aes(pollutant, spearman, fill = factor(bin_s))) +
  geom_col(position = position_dodge(0.8), width = 0.72, color = "grey25") +
  geom_text(aes(label = sprintf("%.2f", spearman)),
            position = position_dodge(0.8), vjust = -0.35, size = 2.6) +
  scale_fill_brewer(palette = "Greens", name = "Bin (s)") +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(x = NULL, y = "Spearman r vs 1-s cell maps",
       title = "A) Concentration-map agreement vs common-cadence averaging") +
  theme_bw(base_size = 11) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
pB <- ggplot(bl, aes(factor(bin_s), agg_ratio)) +
  geom_col(fill = "#4292c6", color = "grey25", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", agg_ratio)), vjust = -0.4, size = 3) +
  geom_hline(yintercept = 1, linetype = 2, color = "red") +
  coord_cartesian(ylim = c(0, 1.25)) +
  labs(x = "Averaging window (s); 1 = delivered record",
       y = "Aggregate mobile:AirToxScreen ratio",
       title = "B) Census-block benzene risk ratio is invariant to bin size") +
  theme_bw(base_size = 11)
plots <- list(pA, pB)
if (DOHOT) {
  ht <- rbindlist(ht_all)
  pC <- ggplot(ht, aes(factor(bin_s), recovery_of_17)) +
    geom_col(fill = "#807dba", color = "grey25", width = 0.6) +
    geom_text(aes(label = sprintf("%.0f%%", 100 * recovery_of_17)), vjust = -0.4, size = 3) +
    coord_cartesian(ylim = c(0, 1.1)) +
    labs(x = "Averaging window (s)", y = "Recovery of 17 hotspot groups",
         title = "C) Hotspot-group recovery vs bin size") +
    theme_bw(base_size = 11)
  plots <- list(pA, pB, pC)
}
ggsave(file.path(BASE, "FinalFig", "FIG_cadence_sensitivity.png"),
       wrap_plots(plots, ncol = 1), width = 9.5,
       height = if (DOHOT) 11 else 8, dpi = 350, bg = "white")
message("[Saved] FinalFig/FIG_cadence_sensitivity.png\nDONE.")
