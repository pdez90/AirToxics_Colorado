# ==============================================================
# 21  Road network plot
# Auto-split from Suncor.Rmd  (section 21 of 40)
# ==============================================================

#Road network plot

# ============================================================
# Road class distributions + segment ranking + nonparametric tests
# - Nearest road segment assignment (fast; avoids heavy st_join)
# - Violin + box plots with medians above panel
# - Segment-level summaries to identify "highest" road segments
# - Kruskal–Wallis per pollutant (road_class)
# - Pairwise Mann–Whitney U (Wilcoxon rank-sum) per pollutant with BH adjust
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(viridis)
})

# ----------------------------
# 0) Load
# ----------------------------
roads_rdata <- "/Users/priyanka/Downloads/Suncor/all_colorado_roads.RData"
points_rdata <- "/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData"
out_dir <- "/Users/priyanka/Downloads/Suncor/FinalFig"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load(roads_rdata)   # all_colorado_roads (sf)
load(points_rdata)  # df

stopifnot(exists("all_colorado_roads"), inherits(all_colorado_roads, "sf"))
stopifnot(exists("df"))

# ----------------------------
# 1) Make points sf (keep lon/lat) and align CRS
# ----------------------------
pts_sf <- st_as_sf(df, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
pts_sf <- st_transform(pts_sf, st_crs(all_colorado_roads))

# ----------------------------
# 2) Assign nearest road segment (FASTER than st_join)
# ----------------------------
message("Assigning nearest road segment (st_nearest_feature)...")
t0 <- Sys.time()
nearest_idx <- st_nearest_feature(pts_sf, all_colorado_roads)
message("Done. Elapsed: ", round(difftime(Sys.time(), t0, units = "secs"), 1), " sec")

# Build a robust segment id
roads_dt <- as.data.table(st_drop_geometry(all_colorado_roads))
seg_candidates <- c("LINEARID","linearid","OBJECTID","ObjectID","OID","FID","fid","id","ID")
seg_col <- seg_candidates[seg_candidates %in% names(roads_dt)][1]
if (is.na(seg_col) || is.null(seg_col)) {
  # fallback: row index of road feature
  roads_dt[, seg_id := .I]
} else {
  roads_dt[, seg_id := as.character(get(seg_col))]
}

# Keep only needed road fields for joining to points
keep_road_cols <- intersect(c("seg_id","MTFCC"), names(roads_dt))
roads_keep <- roads_dt[, ..keep_road_cols]

# Attach to points (no geometry carry)
DT <- as.data.table(st_drop_geometry(pts_sf))
DT[, road_row := nearest_idx]
DT[, seg_id  := roads_keep$seg_id[road_row]]
DT[, MTFCC   := roads_keep$MTFCC[road_row]]
DT[, road_row := NULL]

stopifnot("MTFCC" %in% names(DT))

# ----------------------------
# 3) Road class labels + filter to keep_classes
# ----------------------------
mtfcc_labs <- c(
  S1100 = "Primary Roads",
  S1200 = "Secondary Roads",
  S1400 = "Local Roads",
  S1630 = "Ramps",
  S1640 = "Service Drives",
  S1740 = "Private Road"
)

DT[, road_class := dplyr::recode(as.character(MTFCC), !!!mtfcc_labs, .default = NA_character_)]
DT <- DT[!is.na(road_class)]

keep_classes <- c("Primary Roads","Secondary Roads","Local Roads","Ramps","Service Drives","Private Road")
DT <- DT[road_class %in% keep_classes]
DT[, road_class := factor(road_class, levels = keep_classes)]

# ----------------------------
# 4) Plot function (median labels ABOVE panel)
# ----------------------------
make_clean_road_figure <- function(DT, pollutant_cols, out_file,
                                   ylab = "Pollutant (ppb)",
                                   digits = 2) {

  cols_use <- intersect(pollutant_cols, names(DT))
  stopifnot(length(cols_use) > 0)

  L <- data.table::melt(   # qualified: reshape2::melt masks and returns a data.frame
    DT,
    id.vars = "road_class",
    measure.vars = cols_use,
    variable.name = "Pollutant",
    value.name = "value"
  )
  L <- L[is.finite(value) & value > 0]

  med_dt <- L[, .(med = median(value, na.rm = TRUE)), by = .(Pollutant, road_class)]
  med_dt[, lab := formatC(med, format = "f", digits = digits)]
  med_dt[, y_lab := Inf]

  p <- ggplot(L, aes(x = road_class, y = value)) +
    geom_violin(aes(fill = road_class), trim = TRUE, alpha = 0.70, linewidth = 0.25) +
    geom_boxplot(width = 0.18, outlier.shape = NA, linewidth = 0.30, alpha = 0.95) +
    geom_label(
      data = med_dt,
      aes(x = road_class, y = y_lab, label = lab),
      inherit.aes = FALSE,
      size = 4.0,
      fontface = "bold",
      fill = "white",
      label.size = 0.25,
      label.padding = unit(0.10, "lines"),
      vjust = 1.2
    ) +
    facet_wrap(~ Pollutant, ncol = 2, scales = "free_y") +
    scale_fill_viridis_d(option = "E", guide = "none") +
    scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 13) +
    theme(
      strip.background = element_rect(fill = "grey95", color = NA),
      axis.text.x = element_text(angle = 35, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 18, r = 8, b = 8, l = 8)
    )

  ggsave(out_file, p, width = 11, height = 8.5, dpi = 600, bg = "white", limitsize = FALSE)
  p
}

# ----------------------------
# 5) Make figures
# ----------------------------
polls_bgcorr <- c("sBenzene","sToluene","sTrimethylbenzene","sXylene","sH2S","sHCN")
polls_raw    <- c("Benzene","Toluene","Trimethylbenzene","Xylene","H2S","HCN")

polls_bgcorr <- intersect(polls_bgcorr, names(DT))
polls_raw    <- intersect(polls_raw,    names(DT))

if (length(polls_bgcorr) > 0) {
  make_clean_road_figure(
    DT, polls_bgcorr,
    file.path(out_dir, "roadclass_bgcorrected_clean_v3.png")
  )
}

if (length(polls_raw) > 0) {
  make_clean_road_figure(
    DT, polls_raw,
    file.path(out_dir, "roadclass_raw_clean_v3.png")
  )
}

# ----------------------------
# 6) Segment-level ranking: "which road segments are highest?"
# ----------------------------
# Choose which pollutants to rank (bg-corrected here; swap to polls_raw if desired)
rank_polls <- polls_bgcorr
if (length(rank_polls) == 0) stop("No bg-corrected pollutant columns found in DT for segment ranking.")

# Long for segment summaries
seg_long <- data.table::melt(
  DT,
  id.vars = c("seg_id", "road_class"),
  measure.vars = rank_polls,
  variable.name = "Pollutant",
  value.name = "value"
)
seg_long <- seg_long[is.finite(value)]

# Per-segment summaries (median + p95 are usually better than mean for skew)
seg_summ <- seg_long[
  ,
  .(
    n      = .N,
    mean   = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    p95    = as.numeric(quantile(value, 0.95, na.rm = TRUE))
  ),
  by = .(Pollutant, road_class, seg_id)
]

# Only rank segments with enough observations (tune this)
min_obs_seg <- 50
seg_summ_f <- seg_summ[n >= min_obs_seg]

# Top segments overall per pollutant (by median; also save p95 ranking)
topN <- 25
top_by_median <- seg_summ_f[order(-median)][, head(.SD, topN), by = Pollutant]
top_by_p95    <- seg_summ_f[order(-p95)][,    head(.SD, topN), by = Pollutant]

fwrite(top_by_median, file.path(out_dir, "top_road_segments_byMedian_bgcorrected.csv"))
fwrite(top_by_p95,    file.path(out_dir, "top_road_segments_byP95_bgcorrected.csv"))

message("Saved segment ranking tables:")
message("  - top_road_segments_byMedian_bgcorrected.csv")
message("  - top_road_segments_byP95_bgcorrected.csv")

# OPTIONAL: make an sf you can map (segment geometry + summaries)
# (One file with summaries; easiest is a GeoPackage with one layer)
roads_sf <- all_colorado_roads
if (!("seg_id" %in% names(roads_sf))) {
  # add seg_id to roads_sf matching how we built it
  if (exists("seg_col") && !is.null(seg_col) && seg_col %in% names(roads_sf)) {
    roads_sf$seg_id <- as.character(roads_sf[[seg_col]])
  } else {
    roads_sf$seg_id <- as.character(seq_len(nrow(roads_sf)))
  }
}
seg_for_map <- seg_summ_f[order(-median)]
# keep one row per segment per pollutant (already)
seg_map_sf <- merge(roads_sf, as.data.frame(seg_for_map), by = "seg_id", all.x = FALSE, all.y = TRUE)

st_write(seg_map_sf, file.path(out_dir, "road_segments_bgcorrected_summaries.gpkg"),
         layer = "seg_summ", delete_layer = TRUE, quiet = TRUE)

# ----------------------------
# 7) Kruskal–Wallis + pairwise Mann–Whitney U tests (per pollutant)
# ----------------------------
test_long <- data.table::melt(
  DT,
  id.vars = "road_class",
  measure.vars = rank_polls,
  variable.name = "Pollutant",
  value.name = "value"
)
test_long <- test_long[is.finite(value)]

# Kruskal–Wallis per pollutant
kw_res <- test_long[
  ,
  {
    kt <- kruskal.test(value ~ road_class)
    .(kw_statistic = unname(kt$statistic),
      df = unname(kt$parameter),
      p_value = kt$p.value,
      n = .N)
  },
  by = Pollutant
]
kw_res[, p_adj_BH := p.adjust(p_value, method = "BH")]
setorder(kw_res, p_value)

fwrite(kw_res, file.path(out_dir, "kruskal_wallis_by_pollutant_bgcorrected.csv"))

# Pairwise Wilcoxon per pollutant (BH adjusted within pollutant)
# ----------------------------
# Pairwise Wilcoxon per pollutant (BH adjusted within pollutant)
# Robust reshaping of pw$p.value into long format
# ----------------------------

pairwise_list <- lapply(unique(test_long$Pollutant), function(pol) {

  dd <- test_long[Pollutant == pol & is.finite(value)]

  # Need at least 2 non-empty groups
  if (length(unique(dd$road_class)) < 2) return(NULL)

  pw <- pairwise.wilcox.test(
    x = dd$value,
    g = dd$road_class,
    p.adjust.method = "BH",
    exact = FALSE
  )

  M <- pw$p.value
  if (is.null(M) || length(M) == 0) return(NULL)

  # Convert matrix -> long safely
  df_long <- as.data.frame(as.table(M), stringsAsFactors = FALSE)
  # Column names are often Var1/Var2/Freq, but make it bulletproof:
  if (ncol(df_long) < 3) return(NULL)

  # Standardize names
  names(df_long)[1:3] <- c("group1", "group2", "p_adj_BH")

  df_long <- df_long[is.finite(df_long$p_adj_BH) & !is.na(df_long$p_adj_BH), , drop = FALSE]
  if (nrow(df_long) == 0) return(NULL)

  out <- as.data.table(df_long)
  out[, Pollutant := pol]
  out[]
})

pairwise_res <- rbindlist(pairwise_list, fill = TRUE)
setorder(pairwise_res, Pollutant, p_adj_BH)

fwrite(pairwise_res, file.path(out_dir, "pairwise_wilcoxon_BH_by_pollutant_bgcorrected.csv"))


message("Saved nonparametric test outputs:")
message("  - kruskal_wallis_by_pollutant_bgcorrected.csv")
message("  - pairwise_wilcoxon_BH_by_pollutant_bgcorrected.csv")

# ----------------------------
# 8) Counts table (non-NA measurements by road class)
# ----------------------------
tab_counts <- test_long[!is.na(value), .(n_nonNA = .N), by = .(road_class, Pollutant)]
tab_wide <- data.table::dcast(tab_counts, road_class ~ Pollutant, value.var = "n_nonNA", fill = 0)
tab_wide <- tab_wide[match(levels(DT$road_class), road_class)]

print(tab_wide)
write.csv(tab_wide, file.path(out_dir, "roadclass_bgcorrected_counts.csv"), row.names = FALSE)
