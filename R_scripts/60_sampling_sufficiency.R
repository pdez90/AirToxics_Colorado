# ==============================================================
# 60  SAMPLING-SUFFICIENCY CURVES — how many days are enough?
# For k in {10,20,40,60,80,120,160,200} sampling days (M draws
# each): (a) Spearman correlation of subsampled 500 m cell benzene
# medians with the full-campaign values (cells with >=3 sampled
# days); (b) fraction of the full-campaign hotspot groups
# recovered (>=3-pollutant groups within 300 m; within-subset
# thresholds; baseline parameters).
# Outputs: TABLE_sampling_sufficiency.csv,
#          FinalFig/FIG_sampling_sufficiency.png
# Runtime ~15-30 min.
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(sf); library(dbscan); library(ggplot2); library(scales) })
set.seed(42)
BASE <- "/Users/priyanka/Downloads/Suncor"
KS <- c(10,20,40,60,80,120,160,200); M_MAP <- 20; M_HOT <- 10
load(file.path(BASE,"mobile_wswd.RData")); df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]
days <- sort(unique(df$day))
grid <- st_read(file.path(BASE,"Grid_500m_generated","grid_500m.shp"), quiet=TRUE)
st_crs(grid) <- 26913
cent <- st_centroid(st_geometry(grid))
pts <- st_transform(st_as_sf(df[,.(Longitude,Latitude)],
        coords=c("Longitude","Latitude"), crs=4326), 26913)
df[, cell := grid$id[st_nearest_feature(pts, cent)]]
xy <- st_coordinates(pts); df[, `:=`(px=xy[,1], py=xy[,2])]
rm(pts, xy); gc()
POLLS <- c(benzene="Benzene_ppb", toluene="Toluene_ppb",
           trimethylbenzene="Trimethylbenzene_ppb", xylene="Xylene_ppb",
           hydrogen_sulfide="Hydrogen_Sulfide_ppb", hydrogen_cyanide="Hydrogen_Cyanide_ppb")
master <- fread(file.path(BASE,"MASTER_hotspot_group_index.csv"))
mxy <- st_coordinates(st_transform(st_as_sf(master[,.(Longitude,Latitude)],
        coords=c("Longitude","Latitude"), crs=4326), 32613))

bz_daily <- df[is.finite(Benzene_ppb), .(dmed=median(Benzene_ppb)), by=.(cell,day)]
full_cell <- bz_daily[, .(m=median(dmed), nd=.N), by=cell][nd>=3]
message("Full campaign: ", nrow(full_cell), " benzene cells with >=3 days")

run_groups <- function(sub) {
  keep <- list()
  for (pn in names(POLLS)) {
    v <- sub[[POLLS[[pn]]]]; fin <- is.finite(v)
    if (sum(fin) < 500) next
    thr <- quantile(v[fin], .99)
    s <- sub[fin & v > thr, .(px,py,day)]
    if (nrow(s) < 10) next
    cid <- dbscan::dbscan(as.matrix(s[,.(px,py)]), eps=100, minPts=1)$cluster
    cs <- data.table(clust=cid, x=s$px, y=s$py, day=s$day)[
      , .(n=.N, n_days=uniqueN(day), x=mean(x), y=mean(y)), by=clust]
    keep[[pn]] <- cs[n>=quantile(n,.9) & n_days>=quantile(n_days,.9)][, pollutant:=pn]
  }
  keep <- rbindlist(keep)
  if (nrow(keep)==0) return(keep[0])
  keep[, group := dbscan::dbscan(as.matrix(keep[,.(x,y)]), eps=100, minPts=1)$cluster]
  keep[, .(n_poll=uniqueN(pollutant), x=mean(x), y=mean(y)), by=group][n_poll>=3]
}
recov <- function(g3) if (nrow(g3)==0) 0 else
  mean(vapply(seq_len(nrow(mxy)), function(i)
    min(sqrt((g3$x-mxy[i,1])^2+(g3$y-mxy[i,2])^2)) <= 300, TRUE))

res <- list(); t0 <- Sys.time()
for (k in KS) {
  cors <- numeric(M_MAP)
  for (m in seq_len(M_MAP)) {
    sd_ <- sample(days, k)
    sub <- bz_daily[day %in% sd_][, .(m2=median(dmed), nd=.N), by=cell][nd>=3]
    j <- merge(sub, full_cell, by="cell")
    cors[m] <- if (nrow(j) > 20) cor(j$m2, j$m, method="spearman") else NA
  }
  recs <- numeric(M_HOT)
  for (m in seq_len(M_HOT)) {
    sd_ <- sample(days, k)
    recs[m] <- recov(run_groups(df[day %in% sd_]))
  }
  res[[as.character(k)]] <- data.table(k_days=k,
    map_cor_median=round(median(cors, na.rm=TRUE),3),
    map_cor_lo=round(quantile(cors,.1,na.rm=TRUE),3),
    map_cor_hi=round(quantile(cors,.9,na.rm=TRUE),3),
    recovery_median=round(median(recs),3),
    recovery_lo=round(quantile(recs,.1),3),
    recovery_hi=round(quantile(recs,.9),3))
  message(sprintf("k=%3d: map cor %.2f | recovery %.0f%%  (%.1f min)",
    k, median(cors, na.rm=TRUE), 100*median(recs),
    as.numeric(difftime(Sys.time(), t0, units="mins"))))
}
res <- rbindlist(res)
fwrite(res, file.path(BASE,"TABLE_sampling_sufficiency.csv")); print(res)
long <- rbind(
  res[,.(k_days, mid=map_cor_median, lo=map_cor_lo, hi=map_cor_hi,
         panel="Benzene cell-median map: Spearman r vs full campaign")],
  res[,.(k_days, mid=recovery_median, lo=recovery_lo, hi=recovery_hi,
         panel=sprintf("Fraction of the %d hotspot groups recovered (300 m)", nrow(master)))])
p <- ggplot(long, aes(k_days, mid)) +
  geom_ribbon(aes(ymin=lo, ymax=hi), fill="#4292c6", alpha=0.25) +
  geom_line(color="#2166ac", linewidth=0.8) + geom_point(size=1.8) +
  facet_wrap(~panel, scales="free_y") +
  scale_x_continuous(breaks=KS) +
  labs(x="Number of sampling days (random subsets of the 203-day campaign)",
       y=NULL,
       caption="Lines: median over random draws (20 for maps, 10 for hotspots); ribbons: 10th-90th percentiles. Subset thresholds recomputed within each draw.") +
  theme_bw(base_size=11) + theme(plot.caption=element_text(size=8.5, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_sampling_sufficiency.png"), p,
       width=10, height=4.8, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_sampling_sufficiency.png  DONE.")
