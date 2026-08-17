# ==============================================================
# 28  Hotspot analysis + identifying most persistent hotspots
# Auto-split from Suncor.Rmd  (section 28 of 40)
# ==============================================================

#Hotspot analysis + identifying most persistent hotspots

# ============================================================
# Hotspot clustering for MULTIPLE pollutants + threshold outputs
# - Top 1% threshold (b99) per pollutant
# - DBSCAN 100 m
# - Writes per pollutant:
#     (1) cent_out_<pol>_all.csv
#     (2) cent_out_<pol>_persistent.csv  (top 10% in BOTH n and n_days)
# - Writes one summary:
#     hotspot_thresholds_summary.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(dbscan)
})

load("/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")
df <- out
rm(out)

# ----------------------------
# Settings
# ----------------------------
eps_m   <- 100
min_pts <- 1
crs_ll  <- 4326
crs_m   <- 32613  # UTM 13N
out_dir <- "/Users/priyanka/Downloads/Suncor"

pollutants <- c("Benzene_ppb", "Toluene_ppb",
                "Trimethylbenzene_ppb", "Xylene_ppb",
                "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb")
pollutants <- pollutants[pollutants %in% names(df)]
stopifnot(length(pollutants) > 0)

# will store thresholds/cutoffs per pollutant
thresholds_log <- list()

# ----------------------------
# Function
# ----------------------------
cluster_and_export <- function(pol_col) {

  pol_name <- gsub("_ppb", "", pol_col)
  pol_name <- tolower(pol_name)

  message("\n----------------------------")
  message("Processing: ", pol_name)

  # Top 1% threshold (b99)
  b99 <- as.numeric(quantile(df[[pol_col]], 0.99, na.rm = TRUE))
  message("b99 (99th percentile) for ", pol_col, ": ", signif(b99, 5))

  hs_df <- df %>%
    dplyr::filter(is.finite(.data[[pol_col]]),
           .data[[pol_col]] > b99) %>%
    dplyr::mutate(day = as.Date(date)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = crs_ll, remove = FALSE)

  if (nrow(hs_df) == 0) {
    message("No extreme values for ", pol_name, " (nothing written).")

    # log summary even if empty
    thresholds_log[[pol_name]] <<- data.frame(
      pollutant = pol_name,
      pollutant_col = pol_col,
      b99_ppb = b99,
      n_clusters_all = 0,
      n_cutoff_p90 = NA_real_,
      n_days_cutoff_p90 = NA_real_,
      n_persistent_clusters = 0,
      stringsAsFactors = FALSE
    )
    return(invisible(NULL))
  }

  # Project to meters for DBSCAN
  hs_m <- st_transform(hs_df, crs_m)
  xy <- st_coordinates(hs_m)

  # DBSCAN clustering
  hs_m$clust <- dbscan::dbscan(xy, eps = eps_m, minPts = min_pts)$cluster

  # Cluster summary
  cent_sf <- hs_m %>%
    dplyr::group_by(clust) %>%
    dplyr::summarise(
      n = n(),
      n_days = n_distinct(day),
      geometry = st_centroid(st_union(geometry)),
      .groups = "drop"
    )

  # Back to lon/lat + table
  cent_ll <- st_transform(cent_sf, crs_ll)
  coords <- st_coordinates(cent_ll)

  cent_out_all <- cent_ll %>%
    st_drop_geometry() %>%
    dplyr::mutate(Longitude = coords[,1],
           Latitude  = coords[,2]) %>%
    dplyr::select(clust, Longitude, Latitude, n, n_days) %>%
    dplyr::arrange(desc(n_days), desc(n))

  # Write ALL clusters
  out_file_all <- file.path(out_dir, paste0("cent_out_", pol_name, "_all.csv"))
  write.csv(cent_out_all, out_file_all, row.names = FALSE)
  message("Wrote ALL: ", out_file_all, " | clusters: ", nrow(cent_out_all))

  # Persistent cutoffs: top 10% in BOTH n and n_days (i.e., >= 90th percentile)
  n_cut  <- as.numeric(quantile(cent_out_all$n, 0.90, na.rm = TRUE))
  d_cut  <- as.numeric(quantile(cent_out_all$n_days, 0.90, na.rm = TRUE))

  cent_out_persistent <- cent_out_all %>%
    dplyr::filter(n >= n_cut, n_days >= d_cut) %>%
    dplyr::arrange(desc(n_days), desc(n))

  out_file_p <- file.path(out_dir, paste0("cent_out_", pol_name, "_persistent.csv"))
  write.csv(cent_out_persistent, out_file_p, row.names = FALSE)

  message("Persistent cutoffs: n >= ", n_cut, " AND n_days >= ", d_cut)
  message("Wrote PERSISTENT: ", out_file_p, " | clusters: ", nrow(cent_out_persistent))

  # Log thresholds + cutoffs
  thresholds_log[[pol_name]] <<- data.frame(
    pollutant = pol_name,
    pollutant_col = pol_col,
    b99_ppb = b99,
    n_clusters_all = nrow(cent_out_all),
    n_cutoff_p90 = n_cut,
    n_days_cutoff_p90 = d_cut,
    n_persistent_clusters = nrow(cent_out_persistent),
    stringsAsFactors = FALSE
  )

  invisible(list(all = cent_out_all, persistent = cent_out_persistent))
}

# ----------------------------
# Run for all pollutants
# ----------------------------
for (pol in pollutants) {
  cluster_and_export(pol)
}

# ----------------------------
# Write thresholds summary CSV
# ----------------------------
thresholds_df <- bind_rows(thresholds_log)

out_summary <- file.path(out_dir, "hotspot_thresholds_summary.csv")
write.csv(thresholds_df, out_summary, row.names = FALSE)
message("\nWrote thresholds summary CSV: ", out_summary)
print(thresholds_df)
