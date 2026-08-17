# ==============================================================
# R00_backup_old_outputs.R
# RUN THIS FIRST, ONCE, BEFORE ANY REPROCESSING.
# Copies every key output produced under the OLD delays into
# old_outputs_predelayfix/ so each later stage can compare
# old vs new. Copies (does not move) — originals stay in place
# until the pipeline overwrites them.
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R00: Backing up pre-fix outputs")

dir.create(BACKUP, showWarnings = FALSE, recursive = TRUE)

files_to_backup <- c(
  # Stage 1: delay-corrected 1-s dataset
  "mobile.RData", "mobile.csv",
  # Stage 2: wind merge
  "mobile_wswd.RData",
  # Stage 3: background correction + segments
  "bgcorrected_out_merge.RData", "bgcorrected_out_merge_rolling.RData",
  "mobile_corrected.RData",
  "segment500_summaries_clean.RData", "segment500_summaries_acrossSites.RData",
  # Stage 4: scaling factors + census blocks (risk)
  "lacasa_scaling_factors_option1.RData",
  "lacasa_scaling_factors_option1_binweighted.RData",
  "blocks_summaries_clean.RData",
  "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData",
  "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData",
  # Stage 5: hotspots
  "hs_df_benzene.RData", "hs_df_toluene.RData", "hs_df_trimethylbenzene.RData",
  "hs_df_xylene.RData", "hs_df_h2s.RData", "hs_df_hcn.RData",
  "MASTER_hotspot_group_index.csv", "MASTER_hotspot_group_index_with_TRI.csv",
  "group_summary_persistent.csv", "super_hotspots_3plus_persistent.csv",
  "cent_out_benzene_all.csv", "cent_out_benzene_persistent.csv",
  "cent_out_toluene_all.csv", "cent_out_toluene_persistent.csv",
  "cent_out_trimethylbenzene_all.csv", "cent_out_trimethylbenzene_persistent.csv",
  "cent_out_xylene_all.csv", "cent_out_xylene_persistent.csv",
  "cent_out_hydrogen_sulfide_all.csv", "cent_out_hydrogen_sulfide_persistent.csv",
  "cent_out_hydrogen_cyanide_all.csv", "cent_out_hydrogen_cyanide_persistent.csv",
  "hotspot_thresholds_summary.csv",
  # Stage 6-7: HRRR + plume inversion
  "mobile_hrrr.RData",
  "mobile_hrrr_windfromwwtf.RData",
  "mobile_hrrr_windfromwwtf_stability_filtered.RData",
  "res_h2s.csv",
  "WWTP_H2S_plume_step_counts.csv", "WWTP_H2S_plume_funnel.csv",
  "Suncor_BTEX_plumes.csv"
)

n_ok <- 0; n_missing <- 0
for (f in files_to_backup) {
  src <- file.path(BASE, f); dst <- file.path(BACKUP, f)
  if (file.exists(src)) {
    if (file.exists(dst)) {
      diag_msg("  [SKIP] already backed up: ", f)   # never overwrite a backup
    } else {
      ok <- file.copy(src, dst, copy.date = TRUE)
      diag_msg(sprintf("  [%s] %-60s (%.1f MB)", ifelse(ok, "OK  ", "FAIL"), f,
                       file.size(src) / 1e6))
      if (ok) n_ok <- n_ok + 1
    }
  } else {
    diag_msg("  [MISSING] not found (fine if never generated): ", f)
    n_missing <- n_missing + 1
  }
}

diag_msg("\nBackup complete: ", n_ok, " copied, ", n_missing, " not found.")
diag_msg("Backups in: ", BACKUP)
diag_msg("NOTE: backups are never overwritten on re-runs of this script, so the")
diag_msg("      pre-fix snapshot is protected even if you run R00 again later.")
