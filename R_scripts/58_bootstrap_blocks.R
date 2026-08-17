# ==============================================================
# 58  DAY-BOOTSTRAP UNCERTAINTY — block benzene + aggregate ratio
# Resamples sampling days with replacement (B=500), recomputing
# (a) each block's benzene median-of-daily-medians and
# (b) the population-weighted aggregate mobile:AirToxScreen ratio
#     (scaled x1.149; unit risk cancels).
# Reports: 95% CI on the aggregate ratio; for blocks whose point
# estimate exceeds 2x ATS, the bootstrap probability of remaining
# >2x; count of "robust" blocks (Pr >= 0.95).
# Outputs: TABLE_bootstrap_ratio.csv, TABLE_bootstrap_blocks.csv,
#          FinalFig/FIG_bootstrap_blocks.png
# Runtime ~10-20 min.
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2); library(scales) })
set.seed(42)
BASE <- "/Users/priyanka/Downloads/Suncor"
B <- 500; SCALE <- 1.149
load(file.path(BASE, "mobile_wswd.RData")); df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Benzene_ppb) & is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]
g <- st_read(file.path(BASE,"censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"), quiet=TRUE)
gll <- st_transform(g, 4326)
idcol <- grep("GEOID", names(gll), value=TRUE)[1]
df[, `:=`(rlon=round(Longitude,5), rlat=round(Latitude,5))]
ul <- unique(df[, .(rlon, rlat)])
up <- st_as_sf(ul, coords=c("rlon","rlat"), crs=4326)
w <- st_within(up, gll)
ul[, block := st_drop_geometry(gll)[[idcol]][
      vapply(w, function(z) if (length(z)) z[1] else NA_integer_, 1L)]]
df <- merge(df, ul, by=c("rlon","rlat"))[!is.na(block)]
daily <- df[, .(dmed = median(Benzene_ppb)), by=.(block, day)]
days <- sort(unique(daily$day))
message(uniqueN(daily$block), " blocks | ", length(days), " days | ",
        nrow(daily), " block-day rows")
ats <- as.data.table(st_drop_geometry(gll))
ats <- ats[, .(block=get(idcol), ats=benzene_ppb_airtox, pop=Population_airtox)]

full <- daily[, .(bval = median(dmed)), by=block]
full <- merge(full, ats, by="block")
ratio_full <- with(full, sum(pop*bval*SCALE, na.rm=TRUE)/sum(pop*ats, na.rm=TRUE))
gt2_full <- full[bval*SCALE/ats > 2, block]
message(sprintf("Point estimates: aggregate ratio %.3f | blocks >2x: %d",
                ratio_full, length(gt2_full)))

setkey(daily, day)
ratios <- numeric(B)
gt2_count <- setNames(integer(length(gt2_full)), gt2_full)
t0 <- Sys.time()
for (b in seq_len(B)) {
  sel <- data.table(day = sample(days, replace=TRUE))
  dd <- daily[sel, on="day", allow.cartesian=TRUE]
  bs <- dd[, .(bval = median(dmed)), by=block]
  bs <- merge(bs, ats, by="block")
  ratios[b] <- with(bs, sum(pop*bval*SCALE, na.rm=TRUE)/sum(pop*ats, na.rm=TRUE))
  hit <- bs[block %in% gt2_full & bval*SCALE/ats > 2, block]
  gt2_count[hit] <- gt2_count[hit] + 1L
  if (b %% 50 == 0) message("  ", b, "/", B, " (",
      round(difftime(Sys.time(), t0, units="mins"),1), " min)")
}
ci <- quantile(ratios, c(.025,.975))
out1 <- data.table(ratio_point=round(ratio_full,3),
                   ci_lo=round(ci[1],3), ci_hi=round(ci[2],3), B=B)
fwrite(out1, file.path(BASE,"TABLE_bootstrap_ratio.csv")); print(out1)
out2 <- data.table(block=names(gt2_count),
                   pr_gt2 = round(gt2_count/B, 3))[order(-pr_gt2)]
fwrite(out2, file.path(BASE,"TABLE_bootstrap_blocks.csv"))
message("Blocks >2x with bootstrap Pr>=0.95: ", sum(out2$pr_gt2 >= 0.95),
        " | >=0.80: ", sum(out2$pr_gt2 >= 0.80), " | of ", nrow(out2))
pA <- ggplot(data.frame(r=ratios), aes(r)) +
  geom_histogram(bins=40, fill="#4292c6", color="grey30", linewidth=0.2) +
  geom_vline(xintercept=c(1, ratio_full), linetype=c(2,1),
             color=c("red","black")) +
  labs(title=sprintf("A) Aggregate mobile:AirToxScreen ratio  (point %.2f; 95%% CI %.2f-%.2f)",
                     ratio_full, ci[1], ci[2]),
       x="Bootstrap ratio (500 day-resamples)", y="Count") + theme_bw(base_size=11)
pB <- ggplot(out2, aes(pr_gt2)) +
  geom_histogram(bins=20, fill="#e34a33", color="grey30", linewidth=0.2) +
  geom_vline(xintercept=0.95, linetype=2) +
  labs(title=sprintf("B) Robustness of the %d blocks exceeding 2x AirToxScreen", nrow(out2)),
       x="Bootstrap probability the block remains >2x", y="Blocks") +
  theme_bw(base_size=11)
library(patchwork)
ggsave(file.path(BASE,"FinalFig","FIG_bootstrap_blocks.png"), pA/pB,
       width=8.5, height=7, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_bootstrap_blocks.png  DONE.")
