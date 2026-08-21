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
# BUGFIX (2026-08-20): this loaded segment500_summaries_clean.RData, which is
# 13_...R's PER-ROUTE aggregation. That file casts on `id + Site`, so a grid
# cell driven on both the Suncor/P66 and the HEP Terminal routes carries TWO
# rows. This script plots every row at its cell centroid with no Site handling
# and no de-duplication, so those cells were over-plotted in arbitrary order
# and the shared 2-98% colour limits were computed over a set in which they
# appear twice. Figure 2's own caption describes one value per 500 m cell
# "across the study domain", not a per-route surface, so the across-sites
# aggregation from 14_...R - which casts on `id` alone and yields exactly one
# row per cell - is the correct input. The two files export identical object
# names, which is why the substitution is invisible below.
f <- file.path(BASE, "segment500_summaries_acrossSites.RData")
message("Loading ", f)
load(f)   # seg_wide_sf among others
stopifnot(exists("seg_wide_sf"))
# one row per grid cell, or the shared colour scale is computed over duplicates
stopifnot(!anyDuplicated(seg_wide_sf$id))
message("Figure 2 input: ", nrow(seg_wide_sf), " unique 500 m cells (across-route aggregation)")

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

# MINIMUM SAMPLING (2026-08-20)
# Figure 2 previously displayed every cell that held at least one observation,
# so a cell visited on a single day was drawn identically to one visited on a
# hundred. The paper's own sampling-sufficiency analysis
# (60_sampling_sufficiency.R:38) restricts the benzene cell map to cells with
# at least three sampled days, and SI Section S5.7 reports the convergence of
# THAT map - so the published figure and the map whose stability is
# characterised were different cell sets. Apply the same rule here.
# The day count is per pollutant, because H2S and HCN have different
# missingness from the aromatics and HCN begins only on 2025-01-22.
MIN_DAYS_MAP <- 3

.map_vals <- function(v) {
  ndcol <- sub("_median_of_daily_medians$", "_n_days_any", v)
  x  <- suppressWarnings(as.numeric(pd[[v]]))
  nd <- if (ndcol %in% names(pd)) suppressWarnings(as.numeric(pd[[ndcol]])) else rep(Inf, length(x))
  keep <- is.finite(x) & is.finite(nd) & nd >= MIN_DAYS_MAP
  list(val = x[keep], keep = keep, n_all = sum(is.finite(x)), n_kept = sum(keep))
}

for (.v in V) {
  .m <- .map_vals(.v)
  message(sprintf("  %-42s cells %5d -> %5d after the >=%d sampled-day rule",
                  .v, .m$n_all, .m$n_kept, MIN_DAYS_MAP))
}

# shared aromatic limits: pooled 2-98% across the four aromatics,
# computed over the cells that are actually drawn
arom_vals <- unlist(lapply(V[1:4], function(v) .map_vals(v)$val))
arom_lims <- as.numeric(quantile(arom_vals, c(0.02, 0.98)))
message(sprintf("Shared aromatic limits (pooled 2-98%%): %.3f to %.3f ppb",
                arom_lims[1], arom_lims[2]))

# GUARD (2026-08-20): found by running this chain end to end rather than by
# reading it. If a pollutant has no cells left to draw - because it is entirely
# NA, or because no cell reaches MIN_DAYS_MAP - then min()/max() on an empty
# vector return Inf and -Inf with only a warning, ggplot draws an empty panel
# with a full legend, and the result looks like a region of zero concentration
# rather than an absence of data. HCN is the species at risk: it exists only
# from 2025-01-22, so requiring three sampled days per cell can thin or empty
# that panel. Fail loudly if every panel is empty, and mark an individual empty
# panel on its face so the blank cannot be mistaken for a measurement.
.empty_panels <- names(V)[vapply(V, function(v) .map_vals(v)$n_kept == 0L, logical(1))]
if (length(.empty_panels) == length(V)) {
  stop("Figure 2: no pollutant has any cell meeting the >= ", MIN_DAYS_MAP,
       " sampled-day minimum. Check that 14_...R ran and that the ",
       "bgcorr_*_n_days_any columns are present.")
}
if (length(.empty_panels)) {
  message("[FIG2] EMPTY PANEL(S): ", paste(.empty_panels, collapse = ", "),
          " - no cell reaches the >= ", MIN_DAYS_MAP, " sampled-day minimum. ",
          "These panels are annotated as no-data; do not read them as low concentrations.")
}

panel <- function(varname, title_txt, tag, lims) {
  .m  <- .map_vals(varname)
  dfv <- pd[.m$keep, ] |> mutate(val = .m$val)
  .empty <- nrow(dfv) == 0L
  message(sprintf("  %-38s n=%5d | range %s | lims %.3f-%.2f",
                  varname, nrow(dfv),
                  if (.empty) "NO DATA" else sprintf("%.3f-%.2f", min(dfv$val), max(dfv$val)),
                  lims[1], lims[2]))
  ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 12) +
    geom_point(data = dfv, aes(Lon, Lat, color = val), size = 1.2, alpha = 0.95) +
    {if (.empty) annotate("text", x = mean(xlim), y = mean(ylim),
                          label = paste0("no cell sampled on >= ", MIN_DAYS_MAP, " days"),
                          size = 4, fontface = "italic", colour = "grey30") } +
    coord_sf(crs = 4326, xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_color_viridis_c(option = "plasma", limits = lims,
                          oob = scales::squish, name = "ppb") +
    labs(title = paste0(tag, " ", title_txt), x = NULL, y = NULL) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 12))
}

# BUGFIX (2026-08-20): these took the 2-98% limits over EVERY cell, while the
# shared aromatic limits are computed over the cells actually drawn. After the
# minimum-sampling rule was introduced the two scales were therefore built on
# different populations, so a sparse cell excluded from the H2S panel could
# still be setting that panel's colour range. Use the same displayed set.
# Also guard the all-NA case: quantile() on an empty vector returns NA, and
# scale_color_viridis_c(limits = c(NA, NA)) silently falls back to the data
# range, which for an empty panel is undefined.
.panel_lims <- function(nm) {
  v <- .map_vals(V[nm])$val
  if (!length(v) || !all(is.finite(range(v)))) {
    message("[FIG2] ", nm, ": no cells to scale; panel will be drawn as no-data")
    return(c(0, 1))
  }
  as.numeric(quantile(v, c(0.02, 0.98), na.rm = TRUE))
}
lims_h2s <- .panel_lims("H2S")
lims_hcn <- .panel_lims("HCN")
message(sprintf("H2S limits (2-98%% of displayed cells): %.3f to %.3f ppb", lims_h2s[1], lims_h2s[2]))
message(sprintf("HCN limits (2-98%% of displayed cells): %.3f to %.3f ppb", lims_hcn[1], lims_hcn[2]))
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
