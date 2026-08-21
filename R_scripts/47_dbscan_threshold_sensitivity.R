# ==============================================================
# 47  DBSCAN / THRESHOLD SENSITIVITY OF THE HOTSPOT ANALYSIS (SI)
# Reruns the full hotspot chain (script 28 -> 30 logic) under a
# factorial perturbation of its three tuning parameters:
#   - event threshold:        p98.5, p99 (baseline), p99.5
#   - DBSCAN eps:             50, 100 (baseline), 200 m
#   - persistence percentile: p85, p90 (baseline), p95
# For each of the 27 variants: per-pollutant persistent clusters,
# cross-pollutant groups (same eps for grouping, minPts = 1), and
# recovery of the baseline groups (fraction of MASTER centroids
# within 300 m of a variant >=3-pollutant group).
# Outputs:
#   TABLE_dbscan_sensitivity.csv
#   FinalFig/FIG_dbscan_sensitivity.png
# Runtime: ~5-15 min.
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(dbscan); library(ggplot2)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]

POLLS <- c(benzene = "Benzene_ppb", toluene = "Toluene_ppb",
           trimethylbenzene = "Trimethylbenzene_ppb", xylene = "Xylene_ppb",
           hydrogen_sulfide = "Hydrogen_Sulfide_ppb",
           hydrogen_cyanide = "Hydrogen_Cyanide_ppb")

THRS <- c(0.985, 0.99, 0.995)
EPSS <- c(50, 100, 200)
PERS <- c(0.85, 0.90, 0.95)
BASELINE <- list(thr = 0.99, eps = 100, pers = 0.90)

# canonical groups (MASTER) in meters for recovery matching
master <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
mxy <- st_coordinates(st_transform(st_as_sf(
  master[, .(Longitude, Latitude)], coords = c("Longitude", "Latitude"),
  crs = 4326), 32613))
message("Baseline groups (MASTER): ", nrow(master))

# ---- precompute exceedance subsets per pollutant x threshold ---
message("Precomputing exceedance subsets (18 = 6 pollutants x 3 thresholds)...")
subsets <- list()
for (pn in names(POLLS)) {
  col <- POLLS[[pn]]
  v <- df[[col]]
  fin <- is.finite(v) & is.finite(df$Longitude) & is.finite(df$Latitude)
  qs <- quantile(v[is.finite(v)], THRS)
  for (k in seq_along(THRS)) {
    sel <- fin & v > qs[k]
    s <- df[sel, .(day)]
    xy <- st_coordinates(st_transform(st_as_sf(
      df[sel, .(Longitude, Latitude)],
      coords = c("Longitude", "Latitude"), crs = 4326), 32613))
    subsets[[paste(pn, THRS[k])]] <- list(xy = xy, day = s$day)
    message(sprintf("  %-17s p%.1f thr=%.3g ppb  n=%s", pn, 100 * THRS[k],
                    qs[k], format(sum(sel), big.mark = ",")))
  }
}

# ---- run the 27 variants --------------------------------------
res <- list(); t0 <- Sys.time()
for (thr in THRS) for (eps in EPSS) {
  # per-pollutant clusters at this (thr, eps): computed once, reused across PERS
  cl <- list()
  for (pn in names(POLLS)) {
    ss <- subsets[[paste(pn, thr)]]
    if (nrow(ss$xy) < 2) next
    cid <- dbscan::dbscan(ss$xy, eps = eps, minPts = 1)$cluster
    cs <- data.table(clust = cid, x = ss$xy[, 1], y = ss$xy[, 2], day = ss$day)[
      , .(n = .N, n_days = uniqueN(day), x = mean(x), y = mean(y)), by = clust]
    cs[, pollutant := pn]
    cl[[pn]] <- cs
  }
  cl <- rbindlist(cl)
  for (pers in PERS) {
    keep <- cl[, .SD[n >= quantile(n, pers) & n_days >= quantile(n_days, pers)],
               by = pollutant]
    if (nrow(keep) == 0) next
    gid <- dbscan::dbscan(as.matrix(keep[, .(x, y)]), eps = eps,
                          minPts = 1)$cluster
    keep[, group := gid]
    gs <- keep[, .(n_poll = uniqueN(pollutant), x = mean(x), y = mean(y)),
               by = group]
    g3 <- gs[n_poll >= 3]
    recov <- if (nrow(g3)) {
      dm <- outer(seq_len(nrow(mxy)), seq_len(nrow(g3)), Vectorize(function(i, j)
        sqrt((mxy[i, 1] - g3$x[j])^2 + (mxy[i, 2] - g3$y[j])^2)))
      mean(apply(dm, 1, min) <= 300)
    } else 0
    npp <- data.table::dcast(keep[, .N, by = pollutant], . ~ pollutant, value.var = "N")
    res[[length(res) + 1]] <- data.table(
      thr_pctl = thr, eps_m = eps, pers_pctl = pers,
      n_persistent_total = nrow(keep),
      groups_2plus = sum(gs$n_poll >= 2), groups_3plus = nrow(g3),
      groups_4plus = sum(gs$n_poll >= 4), max_poll = max(gs$n_poll),
      recovery_of_17 = round(recov, 3),
      baseline = (thr == BASELINE$thr & eps == BASELINE$eps &
                  pers == BASELINE$pers))
    message(sprintf(
      "thr p%.1f | eps %3d m | pers p%2.0f -> %3d persistent, %2d groups >=3, recovery %.0f%%  (%.1f min)",
      100 * thr, eps, 100 * pers, nrow(keep), nrow(g3), 100 * recov,
      as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
}
res <- rbindlist(res)
stopifnot(nrow(res) == 27, sum(res$baseline) == 1)
# LABEL FIX (2026-08-20): "17" was hard-coded here and in the figure panel
# title below, while the value is computed against nrow(mxy), the CURRENT
# master index (18 groups). The number plotted was right; every label said 17.
message(sprintf("Baseline check (should be ~%d groups >=3): %d", nrow(mxy),
                res[baseline == TRUE, groups_3plus]))
fwrite(res, file.path(BASE, "TABLE_dbscan_sensitivity.csv"))
print(res[order(-baseline, thr_pctl, eps_m, pers_pctl)])

# ---- figure ---------------------------------------------------
res[, thr_lab := sprintf("Event threshold p%.1f", 100 * thr_pctl)]
res[, pers_lab := sprintf("persistence p%.0f", 100 * pers_pctl)]
long <- data.table::melt(res, id.vars = c("thr_lab", "eps_m", "pers_lab", "baseline"),
             measure.vars = c("groups_3plus", "recovery_of_17"))
long[variable == "recovery_of_17", value := value * 100]
long[, panel := ifelse(variable == "groups_3plus",
                       "Groups persistent in >=3 pollutants (n)",
                       sprintf("Baseline %d groups recovered within 300 m (%%)", nrow(mxy)))]
p <- ggplot(long, aes(factor(eps_m), value, color = pers_lab,
                      group = pers_lab)) +
  geom_line(linewidth = 0.6) + geom_point(size = 2.2) +
  geom_point(data = long[baseline == TRUE], shape = 21, size = 4.5,
             stroke = 1.1, color = "black", show.legend = FALSE) +
  facet_grid(panel ~ thr_lab, scales = "free_y", switch = "y") +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  labs(x = "DBSCAN eps (m)", y = NULL,
       caption = "Black circle marks the baseline configuration (event p99, eps 100 m, persistence p90).") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.placement = "outside",
        plot.caption = element_text(size = 9, hjust = 0))
ggsave(file.path(BASE, "FinalFig", "FIG_dbscan_sensitivity.png"),
       p, width = 10, height = 6.5, dpi = 400, bg = "white")
message("[Saved] FinalFig/FIG_dbscan_sensitivity.png")
message("DONE.")
