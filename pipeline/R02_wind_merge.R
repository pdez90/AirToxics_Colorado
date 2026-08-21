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

# The merge script produces the merged object; find it robustly.
# BUGFIX (2026-08-20): this used `cand[1]` after
# `intersect(c("df","df_out","out"), ls())`. intersect() preserves the order
# of its FIRST argument, so cand[1] was always "df" - the PRE-merge table,
# whose wind columns script 06 renames to ws_mobile / wd_mobile. `m` then had
# no ws, wd or dist_km, so both checks below (the 11.1% missing-wind and the
# 4.6 km median station distance, both Section 2.3 numbers) silently never
# ran. Select on CONTENT instead of name order: require the joined wind
# columns to be present.
.has_wind <- function(nm) {
  o <- try(get(nm), silent = TRUE)
  !inherits(o, "try-error") && is.data.frame(o) && all(c("ws", "wd") %in% names(o))
}
cand <- intersect(c("df_out", "out", "df"), ls())
merged_name <- Filter(.has_wind, cand)
if (!length(merged_name)) {
  stop("R02: no post-merge object with ws/wd found among: ",
       paste(cand, collapse = ", "),
       " - script 06 did not produce a joined table, so the wind-join ",
       "diagnostics cannot run.")
}
merged_name <- merged_name[[1]]
m <- get(merged_name)
diag_msg("Using merged object: ", merged_name,
         "  (", nrow(m), " rows; has dist_km: ", "dist_km" %in% names(m), ")")

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
# BUGFIX (2026-08-20): the object inside mobile_wswd.RData is `out`
# (06_merge_with_wind.R:183), not `df`; this only worked via the ls(e)[1]
# fallback, which would silently pick a different object if one were added.
diag_compare_rdata_rows("mobile_wswd.RData", "out")

diag_msg("\nR02 complete.")
