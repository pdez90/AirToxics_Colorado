# ==============================================================
# 30  Multiple pollutant hotspots
# Auto-split from Suncor.Rmd  (section 30 of 40)
# ==============================================================

#Multiple pollutant hotspots

# ============================================================
# Multi-pollutant hotspot overlap + persistence (RUN ALL AT ONCE)
# REQUIREMENT: hs_all and/or hs_persist already exist (sf with:
#   Longitude, Latitude, n, n_days, pollutant, geometry)
# Outputs (printed + CSVs):
#   - group_summary_<which>.csv
#   - top10_multi_<which>.csv
#   - pair_counts_<which>.csv
#   - super_hotspots_3plus_<which>.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(dbscan)
  library(tidyr)
  library(purrr)
})

# ----------------------------
# USER SETTINGS
# ----------------------------
which_use <- "persistent"   # "persistent" or "all"
match_dist_m <- 100         # same as your DBSCAN eps (meters)
out_dir <- "/Users/priyanka/Downloads/Suncor"

# ----------------------------
# INPUT
# ----------------------------
if (which_use == "persistent") {
  stopifnot(exists("hs_persist"))
  hs <- hs_persist
} else if (which_use == "all") {
  stopifnot(exists("hs_all"))
  hs <- hs_all
} else {
  stop("which_use must be 'persistent' or 'all'")
}

need_cols <- c("Longitude","Latitude","n","n_days","pollutant")
miss <- setdiff(need_cols, names(hs))
if (length(miss) > 0) stop("Missing columns in hs: ", paste(miss, collapse=", "))

cat("\n============================\n")
cat("Multi-pollutant overlap run\n")
cat("============================\n")
cat("Using:", which_use, "\n")
cat("match_dist_m:", match_dist_m, "m\n")
cat("Total hotspot points:", nrow(hs), "\n")
cat("By pollutant:\n")
print(table(hs$pollutant))

# ----------------------------
# STEP 1: Group hotspots across pollutants within 100 m (DBSCAN)
# ----------------------------
hs_m <- st_transform(hs, 32613)  # UTM 13N meters
xy <- st_coordinates(hs_m)

hs_m$group_id <- dbscan::dbscan(xy, eps = match_dist_m, minPts = 1)$cluster
cat("\n[Step 1] Groups formed:", length(unique(hs_m$group_id)), "\n")

# ----------------------------
# STEP 2: Summarize groups (pollutants present + persistence + measurement counts)
# ----------------------------
group_summary <- hs_m %>%
  st_drop_geometry() %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarise(
    n_pollutants = n_distinct(pollutant),
    pollutants = paste(sort(unique(pollutant)), collapse = "+"),
    # persistence + intensity across member pollutants
    total_n_days = sum(n_days, na.rm = TRUE),
    max_n_days   = max(n_days, na.rm = TRUE),
    mean_n_days  = mean(n_days, na.rm = TRUE),
    total_measurements = sum(n, na.rm = TRUE),
    max_measurements   = max(n, na.rm = TRUE),
    mean_measurements  = mean(n, na.rm = TRUE),
    .groups = "drop"
  )

# Add centroid lat/lon for the group
grp_cent <- hs_m %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop") %>%
  st_transform(4326)

cent_xy <- st_coordinates(grp_cent)
group_summary <- group_summary %>%
  left_join(
    grp_cent %>% st_drop_geometry() %>%
      dplyr::mutate(Longitude = cent_xy[,1], Latitude = cent_xy[,2]),
    by = "group_id"
  ) %>%
  dplyr::select(group_id, Longitude, Latitude, everything()) %>%
  dplyr::arrange(desc(n_pollutants), desc(max_n_days), desc(max_measurements))

cat("\n[Step 2] Group summary (top 10 rows):\n")
print(head(group_summary, 10))

# ----------------------------
# STEP 3: Define “super hotspots” (>=3 pollutants) + persistence indices
# ----------------------------
group_summary <- group_summary %>%
  dplyr::mutate(
    persistence_index_simple   = n_pollutants * mean_n_days,
    persistence_index_weighted = n_pollutants * mean_n_days * log1p(total_measurements)
  ) %>%
  dplyr::arrange(desc(persistence_index_weighted))

super_hotspots_3plus <- group_summary %>% filter(n_pollutants >= 3)
super_hotspots_4plus <- group_summary %>% filter(n_pollutants >= 4)

cat("\n[Step 3] Super hotspots:\n")
cat("  >=2 pollutants:", sum(group_summary$n_pollutants >= 2), "\n")
cat("  >=3 pollutants:", nrow(super_hotspots_3plus), "\n")
cat("  >=4 pollutants:", nrow(super_hotspots_4plus), "\n")
cat("  Max pollutants in any group:", max(group_summary$n_pollutants), "\n")

cat("\nTop 10 groups by persistence_index_weighted:\n")
top10_multi <- group_summary %>% slice_max(persistence_index_weighted, n = 10)
print(top10_multi)

# ----------------------------
# STEP 4: Pairwise overlap counts (how often pollutant pairs co-occur in groups)
# ----------------------------
pair_counts <- hs_m %>%
  st_drop_geometry() %>%
  dplyr::distinct(group_id, pollutant) %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarise(pols = list(sort(unique(pollutant))), .groups="drop") %>%
  dplyr::mutate(pairs = purrr::map(pols, ~{
    if (length(.x) < 2) return(character(0))
    combn(.x, 2, FUN = function(z) paste(z, collapse = " & "))
  })) %>%
  dplyr::select(group_id, pairs) %>%
  tidyr::unnest(pairs) %>%
  dplyr::count(pairs, name="n_common_groups") %>%
  dplyr::arrange(desc(n_common_groups))

cat("\n[Step 4] Pairwise overlaps (top 20):\n")
print(head(pair_counts, 20))

# ----------------------------
# STEP 5: One-line summary stats + save CSV outputs
# ----------------------------
summary_stats <- data.frame(
  which = which_use,
  n_hotspot_points = nrow(hs),
  n_groups = n_distinct(group_summary$group_id),
  n_groups_2plus_pollutants = sum(group_summary$n_pollutants >= 2),
  n_groups_3plus_pollutants = sum(group_summary$n_pollutants >= 3),
  n_groups_4plus_pollutants = sum(group_summary$n_pollutants >= 4),
  max_pollutants_in_group = max(group_summary$n_pollutants),
  stringsAsFactors = FALSE
)

cat("\n[Step 5] Summary stats:\n")
print(summary_stats)

# ---- write outputs
f_group   <- file.path(out_dir, paste0("group_summary_", which_use, ".csv"))
f_top10   <- file.path(out_dir, paste0("top10_multi_", which_use, ".csv"))
f_pairs   <- file.path(out_dir, paste0("pair_counts_", which_use, ".csv"))
f_super3  <- file.path(out_dir, paste0("super_hotspots_3plus_", which_use, ".csv"))
f_stats   <- file.path(out_dir, paste0("summary_stats_", which_use, ".csv"))

write.csv(group_summary,          f_group,  row.names = FALSE)
write.csv(top10_multi,            f_top10,  row.names = FALSE)
write.csv(pair_counts,            f_pairs,  row.names = FALSE)
write.csv(super_hotspots_3plus,   f_super3, row.names = FALSE)
write.csv(summary_stats,          f_stats,  row.names = FALSE)

cat("\nSaved:\n")
cat(" -", f_group,  "\n")
cat(" -", f_top10,  "\n")
cat(" -", f_pairs,  "\n")
cat(" -", f_super3, "\n")
cat(" -", f_stats,  "\n")
