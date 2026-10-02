# ==============================================================
# 50  BELOW-MDL-GRAYED CONCENTRATION MAPS (SI alternative to Fig 2)
# For each pollutant: 500 m cell median of daily medians (raw
# reported values), with cells whose median falls below the
# reference MDL shown in gray. This separates cells whose
# mapped value is instrument noise from cells with a real ambient
# signal - the reviewer's point about the HCN panel, extended to
# all six pollutants.
# MDLs: reference (CAT) audit values (CDPHE quarterly data packets, Quarterly Summary tab /
# Table S1.2): benzene 0.5, toluene 0.18, xylene 0.19, TMB 0.22,
# H2S 5 ppb. HCN (2026-09-27): 13 ppb was the CAT value BEFORE the
# retained HCN record (22 Jan - 23 Jun 2025). Over that record the
# audit values in force are CAT 5 (Q1) / 10 (Q2) and EMU 18 (Q1) /
# 2 (Q2) ppb; weighted by retained HCN observations (25,205 / 75,253 /
# 151,137 / 227,065 rows) the median MDL is 5 ppb, which is used here.
# At the 2 ppb value that applied to 47% of HCN observations, 3 of the
# 359 HCN cells would be colored; at 5, 10 or 18 ppb none are.
# Output: FinalFig/FIG_belowMDL_maps.png (+ per-panel CSV summary)
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(ggspatial)
  library(scales); library(patchwork)
})

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
message("Loading mobile data + grid...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]

grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"),
                quiet = TRUE)
st_crs(grid) <- 26913
cent_m <- st_centroid(st_geometry(grid))
cent_ll <- st_coordinates(st_transform(cent_m, 4326))
pts <- st_transform(st_as_sf(df[, .(Longitude, Latitude)],
                             coords = c("Longitude", "Latitude"), crs = 4326),
                    26913)
message("Snapping ", format(nrow(df), big.mark = ","), " points to cells...")
df[, cell := grid$id[st_nearest_feature(pts, cent_m)]]
cells <- data.table(cell = grid$id, lon = cent_ll[, 1], lat = cent_ll[, 2])
rm(pts); gc()

POLLS <- list(
  list(name = "Benzene",          col = "Benzene_ppb",          mdl = 0.5),
  list(name = "Toluene",          col = "Toluene_ppb",          mdl = 0.18),
  list(name = "Trimethylbenzene", col = "Trimethylbenzene_ppb", mdl = 0.22),
  list(name = "Xylene",           col = "Xylene_ppb",           mdl = 0.19),
  list(name = "H2S",              col = "Hydrogen_Sulfide_ppb", mdl = 5),
  list(name = "HCN",              col = "Hydrogen_Cyanide_ppb", mdl = 5))   # see header

padx <- 0.012; pady <- 0.012
xlim <- unname(range(cells[cell %in% df$cell, lon])) + c(-padx, padx)
ylim <- unname(range(cells[cell %in% df$cell, lat])) + c(-pady, pady)

panels <- list(); summ <- list()
for (i in seq_along(POLLS)) {
  P <- POLLS[[i]]
  daily <- df[is.finite(get(P$col)),
              .(dmed = median(get(P$col))), by = .(cell, day)]
  cm <- daily[, .(med = median(dmed), n_days = .N), by = cell]

  # (2026-08-20) The Figure 2 caption calls this map "an alternative
  # presentation" of Figure 2, so it has to be drawn on the same cell set.
  # 55_figure2_sharedscale.R shows only cells sampled on at least three
  # separate days; this script computed n_days and never used it, so it was
  # displaying every cell including single-visit ones - and the below-MDL
  # percentages quoted in the Figure 2 caption were computed over that wider
  # set. Apply the same minimum. NOTE the basis deliberately stays RAW here:
  # a detection limit is defined on the reported value, not on a
  # background-corrected one, which is the one respect in which this map is
  # legitimately not a re-presentation of Figure 2.
  MIN_DAYS_MAP <- 3
  .n_all <- nrow(cm)
  cm <- cm[n_days >= MIN_DAYS_MAP]
  message(sprintf("%-17s cells %d -> %d after the >=%d sampled-day rule (matching Figure 2)",
                  P$name, .n_all, nrow(cm), MIN_DAYS_MAP))
  if (!nrow(cm)) {
    message(sprintf("%-17s no cell reaches the minimum; panel skipped", P$name))
    next
  }
  cm <- merge(cm, cells, by = "cell")
  cm[, below := med < P$mdl]
  pct <- round(100 * mean(cm$below), 1)
  summ[[i]] <- data.table(pollutant = P$name, mdl_ppb = P$mdl,
                          n_cells = nrow(cm), pct_cells_below_mdl = pct)
  message(sprintf("%-17s MDL %5.2f ppb | %4d cells | %5.1f%% below MDL",
                  P$name, P$mdl, nrow(cm), pct))
  # Colour scale only where some cell is above the MDL. When none is (H2S and
  # HCN), a colour bar fitted to the below-MDL medians would describe cells
  # that are all drawn gray, so the panel carries no colour scale at all.
  n_above <- sum(!cm$below)
  p <- ggplot() +
    annotation_map_tile(type = Sys.getenv("SUNCOR_TILES", "osm"), zoom = 11) +
    geom_point(data = cm[(below)], aes(lon, lat), color = "grey55",
               size = 0.55, alpha = 0.8)
  if (n_above > 0) {
    lims <- quantile(cm[!(below), med], c(0.02, 0.98), na.rm = TRUE)
    if (!all(is.finite(lims)) || lims[1] >= lims[2])
      lims <- range(cm[!(below), med], na.rm = TRUE) + c(-1e-6, 1e-6)
    p <- p +
      geom_point(data = cm[!(below)], aes(lon, lat, color = med),
                 size = 0.65, alpha = 0.95) +
      scale_color_viridis_c(limits = lims, oob = scales::squish,
                            name = "ppb", option = "D")
  }
  p <- p +
    coord_sf(crs = 4326, default_crs = 4326, xlim = xlim, ylim = ylim,
             expand = FALSE) +
    labs(title = sprintf("%s", P$name),
         subtitle = if (n_above > 0) sprintf("%.0f%% of cells below MDL (%.2g ppb, gray)", pct, P$mdl)
                    else sprintf("All %d cells below MDL (%.2g ppb, gray)", nrow(cm), P$mdl),
         x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 8.5),
          legend.key.height = unit(0.9, "lines"))
  # keep the map the same size as in the panels that carry a colour bar
  if (n_above == 0) p <- p + theme(plot.margin = margin(5.5, 5.5 + 44, 5.5, 5.5))
  panels[[i]] <- p
}
summ <- rbindlist(summ)
fwrite(summ, file.path(BASE, "TABLE_belowMDL_cells.csv"))
print(summ)

fig <- (panels[[1]] | panels[[2]] | panels[[3]]) /
       (panels[[4]] | panels[[5]] | panels[[6]]) +
  plot_annotation(
    caption = paste0("Cell values are medians of daily medians of raw reported concentrations on the 500 m grid. ",
                     "Gray cells fall below the reference MDL (CAT laboratory audit values from the Quarterly Summary\n",
                     "tab of the CDPHE quarterly data packets; HCN: observation-weighted median over its 2025 record, 5 ppb). ",
                     "Basemap: \u00a9 OpenStreetMap contributors."),
    theme = theme(plot.caption = element_text(size = 8, hjust = 0)))
out_png <- file.path(BASE, "FinalFig", "FIG_belowMDL_maps.png")
ggsave(out_png, fig, width = 13.5, height = 6.9, dpi = 400, bg = "white")   # 6.9 in: no blank band between the map rows
message("[Saved] ", out_png)
message("DONE.")
