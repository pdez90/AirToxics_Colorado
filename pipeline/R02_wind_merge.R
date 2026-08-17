# ==============================================================
# R02_wind_merge.R
# Re-runs the EPA AQS wind merge on the corrected 1-s dataset,
# then checks join rates against the manuscript numbers
# (11.1% missing wind; median station distance 4.6 km).
#
# Sources:
#   R_scripts/05_wind_speed_and_direction.R
#   R_scripts/06_merge_with_wind.R
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R02: Wind merge on corrected data")

t0 <- Sys.time()
source(file.path(BASE, "R_scripts", "05_wind_speed_and_direction.R"))
source(file.path(BASE, "R_scripts", "06_merge_with_wind.R"))
diag_msg("Section scripts completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# The merge script produces the merged object; find it robustly
cand <- intersect(c("df", "df_out", "out"), ls())
merged_name <- cand[1]
m <- get(merged_name)
diag_msg("Using merged object: ", merged_name)

diag_section("R02-DIAG: wind join quality")
diag_df_summary(m, "merged mobile+wind", key_cols = c("ws", "wd"))

if (all(c("ws", "wd") %in% names(m))) {
  miss_pct <- 100 * mean(is.na(m$ws) | is.na(m$wd))
  diag_check_value("% rows missing wind (ms says 11.1%)", miss_pct, REF$missing_wind_pct, tol_pct = 25)
}
if ("dist_km" %in% names(m)) {
  diag_check_value("median distance to met station (km)", stats::median(m$dist_km, na.rm = TRUE),
                   REF$median_dist_met_km, tol_pct = 25)
}

# old vs new
diag_compare_rdata_rows("mobile_wswd.RData", "df")

diag_msg("\nR02 complete.")
