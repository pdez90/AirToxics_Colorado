# ==============================================================
# R05_hotspots.R
# Re-runs the hotspot pipeline on corrected data:
#   - source-probability surfaces (Figure 3)
#   - DBSCAN clustering + persistence filtering (Figure 4, Table S5.1)
# Then checks the manuscript's cluster counts:
#   160 initial clusters -> 40 (>=2 pollutants) -> 17 (>=3) -> 8 (>=4)
#   -> 17 final persistent multi-pollutant groups.
#
# Sources:
#   R_scripts/26_hotspot_rotated_wind_source_probability_profiles.R
#   R_scripts/28_hotspot_analysis_identifying_most_persistent_hotspots.R
#   R_scripts/29_add_on_identifying_hotspots_across_pollutants.R
#   R_scripts/30_multiple_pollutant_hotspots.R
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R05: Hotspot pipeline")

t0 <- Sys.time()
source(file.path(BASE, "R_scripts", "26_hotspot_rotated_wind_source_probability_profiles.R"))
source(file.path(BASE, "R_scripts", "28_hotspot_analysis_identifying_most_persistent_hotspots.R"))
source(file.path(BASE, "R_scripts", "29_add_on_identifying_hotspots_across_pollutants.R"))
source(file.path(BASE, "R_scripts", "30_multiple_pollutant_hotspots.R"))
diag_msg("Section scripts completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 1: per-pollutant hotspot events (old vs new)
# ----------------------------------------------------------------
diag_section("R05-DIAG 1: high-concentration events per pollutant (old vs new)")
# BUGFIX (2026-08-20): this loop compared six hs_df_*.RData files that NO
# script writes - 28_hotspot_analysis...R:62 keeps `hs_df` as a loop-local and
# never save()s it. Both sides of the comparison were pre-fix artifacts, so the
# "old vs new" event counts were meaningless. The per-pollutant event counts
# that actually drive Section 2.5.3.2 are in cent_out_*.csv, which script 28
# does write; report those instead.
.cent <- list.files(BASE, pattern = "^cent_out_.*[.]csv$", full.names = TRUE)
if (length(.cent)) {
  for (f in .cent) {
    x <- try(utils::read.csv(f), silent = TRUE)
    if (!inherits(x, "try-error")) {
      diag_msg(sprintf("  [EVENTS] %-42s %s cluster rows", basename(f),
                       format(nrow(x), big.mark = ",")))
    }
  }
} else {
  diag_msg("  [WARN] no cent_out_*.csv found - script 28 did not write its ",
           "per-pollutant cluster tables.")
}
diag_compare_rdata_rows("hs_df_methane.RData", "hs_df_methane")

# ----------------------------------------------------------------
# DIAG 2: cluster pipeline counts vs manuscript
# ----------------------------------------------------------------
diag_section("R05-DIAG 2: cluster counts vs manuscript (Section 2.5.3.2)")
idx_file <- file.path(BASE, "MASTER_hotspot_group_index.csv")
if (file.exists(idx_file)) {
  idx <- utils::read.csv(idx_file)
  diag_msg("  MASTER_hotspot_group_index.csv: ", nrow(idx), " rows; cols: ",
           paste(head(names(idx), 12), collapse = ", "))
  # final persistent multi-pollutant groups
  gid_col <- grep("group", names(idx), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(gid_col)) {
    n_groups <- length(unique(idx[[gid_col]]))
    # tol_pct = 0: this one is exact by construction, so it must be checked
    # against the CURRENT expected count (REF, updated 2026-08-20), not the
    # as-submitted 17 (still available as REF_SUBMITTED$final_groups).
    diag_check_value("final persistent hotspot groups (expect 18)", n_groups,
                     REF$final_groups, tol_pct = 0)
  }
} else diag_msg("  [WARN] MASTER_hotspot_group_index.csv not found")

pc_file <- file.path(BASE, "pair_counts_persistent.csv")
if (file.exists(pc_file)) {
  pc <- utils::read.csv(pc_file)
  diag_msg("  pair_counts_persistent.csv:")
  for (i in seq_len(min(nrow(pc), 10))) diag_msg("    ", paste(pc[i, ], collapse = " | "))
}
diag_msg(sprintf("  Manuscript counts to verify: %d initial DBSCAN clusters; %d clusters with >=2",
                 REF$dbscan_initial, REF$clusters_ge2))
diag_msg(sprintf("  pollutants; %d with >=3; %d with >=4; %d final groups. Check the printed output",
                 REF$clusters_ge3, REF$clusters_ge4, REF$final_groups))
diag_msg("  of scripts 28-30 above for the new values of each step.")

# ----------------------------------------------------------------
# DIAG 3: pollutant-specific persistence thresholds (n and days)
# manuscript: benzene n=63.8/12d; toluene 43/7; TMB 36/5; xylene 53.5/8;
#             H2S 28/9; HCN 24/3
# ----------------------------------------------------------------
diag_section("R05-DIAG 3: persistence thresholds")
th_file <- file.path(BASE, "hotspot_thresholds_summary.csv")
if (file.exists(th_file)) {
  th <- utils::read.csv(th_file)
  for (i in seq_len(nrow(th))) diag_msg("  ", paste(names(th), "=", unlist(th[i, ]), collapse = "  "))
  diag_msg("  Compare to ms: benzene 63.8/12d, toluene 43/7, TMB 36/5, xylene 53.5/8, H2S 28/9, HCN 24/3.")
} else diag_msg("  [WARN] hotspot_thresholds_summary.csv not found")

diag_msg("\nR05 complete. If group membership changed, Table S5.1, Figures 3-4, and the")
diag_msg("hotspot-type narrative (Groups 4, 10, 13, 24, 28, 83, ...) must be re-checked.")
