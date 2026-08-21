# ==============================================================
# 52  SPLIT-SAMPLE HOTSPOT REPRODUCIBILITY (SI internal validation)
# Reruns the full hotspot chain (baseline parameters: p99 events,
# eps 100 m, persistence p90, grouping at 100 m, >=3 pollutants)
# independently on two splits of the campaign:
#   Split 1: odd vs even sampling days (interleaved; controls season)
#   Split 2: 2023-2024 vs 2025 (calendar; unequal halves, stated)
# Thresholds and persistence cutoffs are recomputed WITHIN each
# half, so each half is a fully self-contained mini-campaign.
# Metrics: groups >=3 per half; cross-half location agreement
# (within 300 m); recovery of the full-campaign groups (count from MASTER).
# Outputs:
#   TABLE_split_sample_hotspots.csv
#   FinalFig/FIG_split_sample_hotspots.png
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(dbscan); library(ggplot2)
  library(ggspatial)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]

POLLS <- c(benzene = "Benzene_ppb", toluene = "Toluene_ppb",
           trimethylbenzene = "Trimethylbenzene_ppb", xylene = "Xylene_ppb",
           hydrogen_sulfide = "Hydrogen_Sulfide_ppb",
           hydrogen_cyanide = "Hydrogen_Cyanide_ppb")

master <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
mxy <- st_coordinates(st_transform(st_as_sf(
  master[, .(Longitude, Latitude)], coords = c("Longitude", "Latitude"),
  crs = 4326), 32613))

# precompute projected coordinates once
message("Projecting all points once...")
xy_all <- st_coordinates(st_transform(st_as_sf(
  df[, .(Longitude, Latitude)], coords = c("Longitude", "Latitude"),
  crs = 4326), 32613))
df[, `:=`(px = xy_all[, 1], py = xy_all[, 2])]
rm(xy_all); gc()

run_half <- function(sub, label) {
  keep <- list()
  for (pn in names(POLLS)) {
    col <- POLLS[[pn]]
    v <- sub[[col]]
    fin <- is.finite(v)
    if (sum(fin) < 1000) next
    thr <- quantile(v[fin], 0.99)
    s <- sub[fin & v > thr, .(px, py, day)]
    if (nrow(s) < 10) next
    cid <- dbscan::dbscan(as.matrix(s[, .(px, py)]), eps = 100,
                          minPts = 1)$cluster
    cs <- data.table(clust = cid, x = s$px, y = s$py, day = s$day)[
      , .(n = .N, n_days = uniqueN(day), x = mean(x), y = mean(y)),
      by = clust]
    pers <- cs[n >= quantile(n, 0.90) & n_days >= quantile(n_days, 0.90)]
    pers[, pollutant := pn]
    keep[[pn]] <- pers
  }
  keep <- rbindlist(keep)
  gid <- dbscan::dbscan(as.matrix(keep[, .(x, y)]), eps = 100,
                        minPts = 1)$cluster
  keep[, group := gid]
  gs <- keep[, .(n_poll = uniqueN(pollutant), x = mean(x), y = mean(y)),
             by = group]
  g3 <- gs[n_poll >= 3]
  message(sprintf("  %-12s: %s obs, %d sampling days -> %d persistent, %d groups >=3",
                  label, format(nrow(sub), big.mark = ","),
                  uniqueN(sub$day), nrow(keep), nrow(g3)))
  g3
}
near_frac <- function(A, B, dmax = 300) {
  # fraction of rows of A within dmax of any row of B
  if (nrow(A) == 0 || nrow(B) == 0) return(0)
  mean(sapply(seq_len(nrow(A)), function(i)
    min(sqrt((B$x - A$x[i])^2 + (B$y - A$y[i])^2)) <= dmax))
}

days <- sort(unique(df$day))
splits <- list(
  odd_even = list(A = df[day %in% days[seq(1, length(days), 2)]],
                  B = df[day %in% days[seq(2, length(days), 2)]],
                  labA = "odd days", labB = "even days"),
  calendar = list(A = df[year(day) <= 2024], B = df[year(day) == 2025],
                  labA = "2023-2024", labB = "2025"))

res <- list(); mapdat <- list()
for (sn in names(splits)) {
  sp <- splits[[sn]]
  message("Split: ", sn)
  gA <- run_half(sp$A, sp$labA)
  gB <- run_half(sp$B, sp$labB)
  mm <- data.table(x = mxy[, 1], y = mxy[, 2])
  res[[sn]] <- data.table(
    split = sn, half_A = sp$labA, half_B = sp$labB,
    groups3_A = nrow(gA), groups3_B = nrow(gB),
    frac_A_near_B = round(near_frac(gA, gB), 2),
    frac_B_near_A = round(near_frac(gB, gA), 2),
    frac_base_near_A = round(near_frac(mm, gA), 2),
    frac_base_near_B = round(near_frac(mm, gB), 2))
  gA[, `:=`(half = sp$labA, split = sn)]
  gB[, `:=`(half = sp$labB, split = sn)]
  mapdat[[sn]] <- rbind(gA, gB)
}
res <- rbindlist(res)
fwrite(res, file.path(BASE, "TABLE_split_sample_hotspots.csv"))
print(res)

# ---- map figure -----------------------------------------------
md <- rbindlist(mapdat)
ll <- st_coordinates(st_transform(st_as_sf(
  as.data.frame(md[, .(x, y)]), coords = c("x", "y"), crs = 32613), 4326))
md[, `:=`(lon = ll[, 1], lat = ll[, 2])]
mll <- data.table(lon = master$Longitude, lat = master$Latitude)
p <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_point(data = md, aes(lon, lat, color = half), size = 3, alpha = 0.85) +
  geom_point(data = mll, aes(lon, lat), shape = 4, size = 3.4, stroke = 1.2,
             color = "black") +
  facet_wrap(~split, labeller = labeller(split = c(
    odd_even = "Odd vs even sampling days",
    calendar = "2023-2024 vs 2025"))) +
  scale_color_brewer(palette = "Set1", name = "Half-campaign groups (>=3 pollutants)") +
  coord_sf(crs = 4326, default_crs = 4326,
           xlim = range(c(md$lon, mll$lon)) + c(-0.01, 0.01),
           ylim = range(c(md$lat, mll$lat)) + c(-0.01, 0.01), expand = FALSE) +
  labs(x = NULL, y = NULL,
       caption = sprintf("X symbols: the %d full-campaign persistent multi-pollutant groups. Colored points: groups identified independently within each half using identical parameters (p99 within-half, eps 100 m, persistence p90). Basemap: CARTO Positron.", nrow(master))) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.caption = element_text(size = 8.5, hjust = 0))
ggsave(file.path(BASE, "FinalFig", "FIG_split_sample_hotspots.png"),
       p, width = 11, height = 6, dpi = 400, bg = "white")
message("[Saved] FinalFig/FIG_split_sample_hotspots.png")
message("DONE.")
