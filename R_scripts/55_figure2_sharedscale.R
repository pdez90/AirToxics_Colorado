# ==============================================================
# 55  FIGURE 2 REBUILD — SHARED AROMATIC COLOR SCALE
# Rebuilds the six-panel Figure 2 (bg-corrected median of daily
# medians per 500 m segment, script-15 styling) with ONE shared
# color scale for the four aromatics (pooled 2-98% limits);
# H2S and HCN keep their own scales. Panels tagged (a)-(f).
# Output: FinalFig/Figure2_sharedscale.png
# ==============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(ggplot2); library(ggspatial)
  library(scales); library(patchwork)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
f <- file.path(BASE, "segment500_summaries_clean.RData")
message("Loading ", f)
load(f)   # seg_wide_sf among others
stopifnot(exists("seg_wide_sf"))

V <- c(Benzene = "bgcorr_Benzene_median_of_daily_medians",
       Toluene = "bgcorr_Toluene_median_of_daily_medians",
       Trimethylbenzene = "bgcorr_Trimethylbenzene_median_of_daily_medians",
       Xylene = "bgcorr_Xylene_median_of_daily_medians",
       H2S = "bgcorr_H2S_median_of_daily_medians",
       HCN = "bgcorr_HCN_median_of_daily_medians")
missing <- setdiff(unname(V), names(seg_wide_sf))
if (length(missing)) stop("columns missing: ", paste(missing, collapse = ", "),
                          "\nAvailable: ", paste(names(seg_wide_sf)[1:30], collapse = ", "))

seg_ll <- seg_wide_sf |> st_make_valid() |> st_zm(drop = TRUE) |> st_transform(4326)
xy <- st_coordinates(st_centroid(st_geometry(seg_ll)))
pd <- st_drop_geometry(seg_ll) |>
  mutate(Lon = xy[, 1], Lat = xy[, 2]) |>
  filter(is.finite(Lon), is.finite(Lat))
stopifnot(median(pd$Lon) < -100)
bb <- range(pd$Lon); bl <- range(pd$Lat)
xlim <- bb + c(-1, 1) * max(diff(bb) * 0.06, 0.01)
ylim <- bl + c(-1, 1) * max(diff(bl) * 0.06, 0.01)

# shared aromatic limits: pooled 2-98% across the four aromatics
arom_vals <- unlist(lapply(V[1:4], function(v) {
  x <- suppressWarnings(as.numeric(pd[[v]])); x[is.finite(x)] }))
arom_lims <- as.numeric(quantile(arom_vals, c(0.02, 0.98)))
message(sprintf("Shared aromatic limits (pooled 2-98%%): %.3f to %.3f ppb",
                arom_lims[1], arom_lims[2]))

panel <- function(varname, title_txt, tag, lims) {
  dfv <- pd |> mutate(val = suppressWarnings(as.numeric(.data[[varname]]))) |>
    filter(is.finite(val))
  message(sprintf("  %-38s n=%5d | range %.3f-%.2f | lims %.3f-%.2f",
                  varname, nrow(dfv), min(dfv$val), max(dfv$val),
                  lims[1], lims[2]))
  ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12) +
    geom_point(data = dfv, aes(Lon, Lat, color = val), size = 1.2, alpha = 0.95) +
    coord_sf(crs = 4326, xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_color_viridis_c(option = "plasma", limits = lims,
                          oob = scales::squish, name = "ppb") +
    labs(title = paste0(tag, " ", title_txt), x = NULL, y = NULL) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 12))
}

lims_h2s <- as.numeric(quantile(as.numeric(pd[[V["H2S"]]]),
                                c(0.02, 0.98), na.rm = TRUE))
lims_hcn <- as.numeric(quantile(as.numeric(pd[[V["HCN"]]]),
                                c(0.02, 0.98), na.rm = TRUE))
p <- (panel(V["Benzene"], "Benzene (bg-corrected) - median of daily medians",
            "(a)", arom_lims) |
      panel(V["Toluene"], "Toluene (bg-corrected) - median of daily medians",
            "(b)", arom_lims)) /
     (panel(V["Trimethylbenzene"],
            "Trimethylbenzene (bg-corrected) - median of daily medians",
            "(c)", arom_lims) |
      panel(V["Xylene"], "Xylene (bg-corrected) - median of daily medians",
            "(d)", arom_lims)) /
     (panel(V["H2S"], "H2S (bg-corrected) - median of daily medians",
            "(e)", lims_h2s) |
      panel(V["HCN"], "HCN (bg-corrected) - median of daily medians",
            "(f)", lims_hcn)) +
  plot_annotation(caption = paste(
    "Panels (a)-(d) share a single color scale (pooled 2nd-98th percentiles",
    "across the four aromatics); H2S and HCN use their own scales.",
    "Basemap: CARTO Positron."),
    theme = theme(plot.caption = element_text(size = 9, hjust = 0)))

out <- file.path(BASE, "FinalFig", "Figure2_sharedscale.png")
ggsave(out, p, width = 12, height = 15, dpi = 400, bg = "white")
message("[Saved] ", out)
message("DONE.")
