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

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({
  library(data.table); library(sf); library(dbscan); library(ggplot2)
})

BASE <- SUNCOR_BASE
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
baseline_groups <- NULL
for (thr in THRS) for (eps in EPSS) {
  # per-pollutant clusters at this (thr, eps): computed once, reused across PERS
  cl <- list()
  for (pn in names(POLLS)) {
    ss <- subsets[[paste(pn, thr)]]
    if (nrow(ss$xy) < 2) next
    cid <- dbscan::dbscan(ss$xy, eps = eps, minPts = 1)$cluster
    # CENTROID DEFINITION (2026-09-27): this used mean(x), mean(y) over every
    # observation in the cluster. The published chain
    # (28_hotspot_analysis_identifying_most_persistent_hotspots.R) instead takes
    # st_centroid(st_union(geometry)), and st_union drops duplicate point
    # geometries - so its centroid is the unweighted mean of the DISTINCT
    # sampling locations, not the observation-weighted mean. A cell driven
    # repeatedly puts many observations at the same coordinates, so the two
    # definitions differ by tens of metres, which at eps = 100 m with
    # single-link chaining is enough to merge clusters that the published chain
    # keeps apart. That is what produced the 15-vs-14 baseline mismatch: three
    # separate published groups within 190 m of each other near
    # (-104.903, 39.778) - group 21 benzene+xylene, group 132 h2s and group 74
    # trimethylbenzene - chained into a single >=3-pollutant group here.
    # Matching the published definition is the point of a sensitivity analysis:
    # the 26 perturbed cells have to be perturbations of the published baseline.
    cs <- data.table(clust = cid, x = ss$xy[, 1], y = ss$xy[, 2], day = ss$day)[
      , { u <- unique(data.table(x = x, y = y))
          .(n = .N, n_days = uniqueN(day), x = mean(u$x), y = mean(u$y)) }, by = clust]
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
      # MASKING FIX (2026-09-22): terra/raster make apply() and mean() S4
      # generics with no matrix method, so this died inside the figure driver.
      base::mean(base::apply(dm, 1, min) <= 300)
    } else 0
    # BASELINE DIAGNOSTIC (2026-09-27): this script reimplements the hotspot
    # chain (28 -> 29 -> 30) in-line, so its baseline cell must reproduce the
    # published group set exactly or the 26 perturbed cells are being measured
    # against a different baseline than the one the manuscript reports. It does
    # not: the published chain yields 14 groups persistent in >=3 pollutants and
    # this cell yields 15, at identical parameters and from the same input. The
    # extra group is written out below so the cause can be found in one run
    # rather than inferred. Everything else in this script is unchanged.
    if (thr == BASELINE$thr && eps == BASELINE$eps && pers == BASELINE$pers) {
      .d_to_master <- vapply(seq_len(nrow(g3)), function(j)
        min(sqrt((mxy[, 1] - g3$x[j])^2 + (mxy[, 2] - g3$y[j])^2)), numeric(1))
      .bx <- st_coordinates(st_transform(st_as_sf(
        data.frame(x = g3$x, y = g3$y), coords = c("x", "y"), crs = 32613), 4326))
      baseline_groups <<- data.table(
        variant_group = g3$group, n_poll = g3$n_poll,
        Longitude = round(.bx[, 1], 6), Latitude = round(.bx[, 2], 6),
        dist_to_nearest_master_m = round(.d_to_master, 1),
        matched_within_300m = .d_to_master <= 300)
    }
    npp <- data.table::dcast(keep[, .N, by = pollutant], . ~ pollutant, value.var = "N")
    res[[length(res) + 1]] <- data.table(
      thr_pctl = thr, eps_m = eps, pers_pctl = pers,
      n_persistent_total = nrow(keep),
      groups_2plus = sum(gs$n_poll >= 2), groups_3plus = nrow(g3),
      groups_4plus = sum(gs$n_poll >= 4), max_poll = max(gs$n_poll),
      recovery_of_baseline = round(recov, 3),
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
.n_base <- res[baseline == TRUE, groups_3plus]
message(sprintf("Baseline check: published chain has %d groups >=3 pollutants; ",
                nrow(mxy)), sprintf("this reimplementation gives %d", .n_base))
if (!is.null(baseline_groups)) {
  fwrite(baseline_groups, file.path(BASE, "TABLE_dbscan_baseline_diagnostic.csv"))
  message("[Saved] TABLE_dbscan_baseline_diagnostic.csv")
}
if (.n_base != nrow(mxy)) {
  message("\n*** BASELINE MISMATCH ***")
  message("The sensitivity baseline does not reproduce the published group set, so the")
  message("26 perturbed cells are measured against a baseline the manuscript does not")
  message("report. Do NOT quote this script's baseline count as the study's group count.")
  if (!is.null(baseline_groups)) {
    message("Groups found here with no published counterpart within 300 m:")
    print(as.data.frame(baseline_groups[matched_within_300m == FALSE]), row.names = FALSE)
    message("Full baseline group list:")
    print(as.data.frame(baseline_groups), row.names = FALSE)
  }
  warning("47: baseline reimplementation gives ", .n_base, " groups, published chain gives ",
          nrow(mxy), " - see TABLE_dbscan_baseline_diagnostic.csv", call. = FALSE)
}
fwrite(res, file.path(BASE, "TABLE_dbscan_sensitivity.csv"))
print(res[order(-baseline, thr_pctl, eps_m, pers_pctl)])

# ---- figure ---------------------------------------------------
res[, thr_lab := sprintf("Event threshold p%.1f", 100 * thr_pctl)]
res[, pers_lab := sprintf("persistence p%.0f", 100 * pers_pctl)]
long <- data.table::melt(res, id.vars = c("thr_lab", "eps_m", "pers_lab", "baseline"),
             measure.vars = c("groups_3plus", "recovery_of_baseline"))
long[variable == "recovery_of_baseline", value := value * 100]
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
