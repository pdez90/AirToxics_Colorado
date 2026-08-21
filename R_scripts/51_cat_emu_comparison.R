# ==============================================================
# 51  CAT vs EMU SIDE-BY-SIDE COMPARISON (QA/QC, SI)
# The two labs normally drive different routes, so a true side-by-
# side is limited to occasions when both sampled the SAME 500 m
# cell on the SAME day. For every such cell-day and pollutant we
# compare the two vans' daily medians.
# Outputs:
#   TABLE_cat_emu_comparison.csv   (n, r, median ratio per pollutant)
#   FinalFig/FIG_cat_emu_comparison.png (scatter panels)
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data + grid...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]
stopifnot("Asset" %in% names(df))
df[, van := toupper(trimws(as.character(Asset)))]
print(df[, .N, by = van])

grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"),
                quiet = TRUE)
st_crs(grid) <- 26913
cent_m <- st_centroid(st_geometry(grid))
pts <- st_transform(st_as_sf(df[, .(Longitude, Latitude)],
                             coords = c("Longitude", "Latitude"), crs = 4326),
                    26913)
df[, cell := grid$id[st_nearest_feature(pts, cent_m)]]
rm(pts); gc()

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")

res <- list(); plot_dt <- list()
for (pn in names(POLLS)) {
  col <- POLLS[[pn]]
  dd <- df[is.finite(get(col)) & van %in% c("CAT", "EMU"),
           .(med = median(get(col)), n = .N), by = .(cell, day, van)]
  w <- data.table::dcast(dd, cell + day ~ van, value.var = "med")
  w <- w[is.finite(CAT) & is.finite(EMU)]
  message(sprintf("%-17s cell-days with BOTH vans: %s", pn,
                  format(nrow(w), big.mark = ",")))
  if (nrow(w) >= 5) {
    res[[pn]] <- data.table(
      pollutant = pn, n_celldays = nrow(w),
      n_days = uniqueN(w$day), n_cells = uniqueN(w$cell),
      r_pearson = round(cor(w$CAT, w$EMU), 3),
      r_spearman = round(cor(w$CAT, w$EMU, method = "spearman"), 3),
      median_ratio_EMU_over_CAT = round(median(w$EMU / w$CAT), 2),
      median_abs_diff_ppb = round(median(abs(w$EMU - w$CAT)), 3))
    w[, pollutant := pn]
    plot_dt[[pn]] <- w
  } else {
    res[[pn]] <- data.table(pollutant = pn, n_celldays = nrow(w),
      n_days = uniqueN(w$day), n_cells = uniqueN(w$cell),
      r_pearson = NA_real_, r_spearman = NA_real_,
      median_ratio_EMU_over_CAT = NA_real_, median_abs_diff_ppb = NA_real_)
  }
}
res <- rbindlist(res)
fwrite(res, file.path(BASE, "TABLE_cat_emu_comparison.csv"))
print(res)

if (length(plot_dt)) {
  pd <- rbindlist(plot_dt)
  p <- ggplot(pd, aes(CAT, EMU)) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
    geom_point(alpha = 0.4, size = 1) +
    facet_wrap(~pollutant, scales = "free") +
    scale_x_log10(labels = label_number()) +
    scale_y_log10(labels = label_number()) +
    labs(x = "CAT daily median in shared 500 m cell (ppb, log scale)",
         y = "EMU daily median (ppb, log scale)",
         caption = "Each point is a 500 m cell sampled by BOTH mobile laboratories on the same day; values are within-van daily medians. Red dashed line = 1:1.") +
    theme_bw(base_size = 11) +
    theme(plot.caption = element_text(size = 8.5, hjust = 0))
  ggsave(file.path(BASE, "FinalFig", "FIG_cat_emu_comparison.png"),
         p, width = 10, height = 6.5, dpi = 400, bg = "white")
  message("[Saved] FinalFig/FIG_cat_emu_comparison.png")
}
message("DONE.")
