# ==============================================================
# RUN_ALL_from_raw.R
# ONE COMMAND, RAW DATA IN -> MANUSCRIPT NUMBERS OUT.
# Accepts ONLY primary inputs (see manifest below); regenerates every
# intermediate. Optionally quarantines all pre-existing intermediates
# first so nothing stale can leak into the results.
#
# Usage:
#   Rscript RUN_ALL_from_raw.R            # verify inputs, run everything
#   CLEAN=1 Rscript RUN_ALL_from_raw.R    # quarantine intermediates first (recommended)
#
# Expected runtime: ~4-6 h total; the HRRR join dominates (~3 h; much
# faster when hrrr_hour_cache/ is warm — the cache holds raw NOAA model
# fields keyed by UTC hour, so it is an input cache, not a derived product).
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("RUN_ALL_from_raw: reproducible pipeline (raw CDPHE inputs only)")

# ----------------------------------------------------------------
# 1) INPUT MANIFEST — primary data only. Fail fast if missing.
# ----------------------------------------------------------------
diag_section("Step 1: verify primary inputs")
raw_csv_dir <- file.path(BASE, "Updated", "csv")
mobile_csvs <- list.files(raw_csv_dir, pattern = "^(Suncor|Terminal)_.*\\.csv$", full.names = TRUE)
inputs <- list(
  "CDPHE mobile monthly CSVs (Updated/csv)" = length(mobile_csvs) >= 55,
  "CDPHE methane deployment CSVs"           = length(list.files("/Users/priyanka/Toxics_EST/MethaneData",
                                               pattern = "_Methane\\.csv$", recursive = TRUE)) >= 290,
  "EPA AQS wind 2023"                       = file.exists(file.path(BASE, "hourly_WIND_2023.csv")),
  "EPA AQS wind 2024"                       = file.exists(file.path(BASE, "hourly_WIND_2024.csv")),
  "EPA AQS wind 2025"                       = file.exists(file.path(BASE, "hourly_WIND_2025.csv")),
  "La Casa ASCENT 2023"                     = file.exists(file.path(BASE, "ascent_2023.csv")),
  "La Casa ASCENT 2024"                     = file.exists(file.path(BASE, "ascent_2024.csv")),
  "La Casa (lacasa3.csv)"                   = file.exists(file.path(BASE, "lacasa3.csv")),
  "EPA AirToxScreen (xlsx)"                 = file.exists(file.path(BASE, "airtoxscreen.xlsx")),
  "EPA AirToxScreen (csv, population)"      = file.exists(file.path(BASE, "airtoxscreen.csv")),
  "EPA TRI"                                 = file.exists(file.path(BASE, "TRI.csv"))
)
for (nm in names(inputs)) diag_msg(sprintf("  [%s] %s", ifelse(inputs[[nm]], "OK  ", "MISS"), nm))
stopifnot(all(unlist(inputs)))
diag_msg("  (Census blocks: fetched live via tigris; HRRR: fetched via Herbie/AWS.)")
diag_msg("  CDPHE mobile CSVs found: ", length(mobile_csvs))

# ----------------------------------------------------------------
# 2) Optional CLEAN: quarantine every pipeline intermediate so the
#    run provably regenerates everything from raw inputs.
# ----------------------------------------------------------------
if (nzchar(Sys.getenv("CLEAN"))) {
  diag_section("Step 2: CLEAN mode — quarantining intermediates")
  qdir <- file.path(BASE, paste0("quarantine_intermediates_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(qdir, showWarnings = FALSE)
  intermediates <- c(
    "mobile.RData", "mobile.csv", "wind_suncor_pueblo1.RData", "mobile_wswd.RData",
    "bgcorrected_out_merge_rolling.RData", "bgcorrected_out_merge.RData",
    "mobile_corrected.RData",
    "segment500_summaries_clean.RData", "segment500_summaries_acrossSites.RData",
    "lacasa_scaling_factors_option1_binweighted.RData",
    "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData",
    "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData",
    "TRI_subset.csv",
    "mobile_hrrr.RData", "mobile_hrrr_windfromwwtf.RData",
    "mobile_hrrr_windfromwwtf_stability_filtered.RData",
    "WWTP_H2S_plume_step_counts.csv", "WWTP_H2S_plume_funnel.csv",
    "mobile_methane.RData", "mobile_methane.csv", "mobile_methane_wind_bg.RData",
    "hs_df_methane.RData", "cent_out_methane_all.csv", "cent_out_methane_persistent.csv",
    "lacasa_pbl.RData"
  )
  # CORRECTION (2026-08-20): these seven files ARE still quarantined, but the
  # reason above was wrong and the wrong reason mattered.
  #   - NOTHING writes hs_df_benzene/toluene/trimethylbenzene/xylene/h2s/hcn:
  #     28_hotspot_analysis...R keeps `hs_df` as a loop-local and never save()s
  #     it. (Only hs_df_methane.RData has a writer, M03:118 - and it is listed
  #     above as a genuine regenerable intermediate.)
  #   - NOTHING writes res_h2s.csv either: P08 writes
  #     FinalFig/WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv.
  # They are not "orphans nothing consumes": R05, R07 and R99 all READ them.
  # But because nothing WRITES them, and R00 copies these same pre-fix
  # artifacts into BACKUP, those three diagnostics were comparing a stale
  # artifact against ITSELF while labelling one side "NEW". Retaining them
  # would only make a dead check look alive. R07 and R99 now read P08's real
  # output instead (see the D8 edits), and R05 reports the cent_out_*.csv
  # tables that script 28 actually writes.
  .legacy_orphans <- c(
    "hs_df_benzene.RData", "hs_df_toluene.RData", "hs_df_trimethylbenzene.RData",
    "hs_df_xylene.RData", "hs_df_h2s.RData", "hs_df_hcn.RData", "res_h2s.csv"
  )
  intermediates <- union(intermediates, .legacy_orphans)
  cent_files <- list.files(BASE, pattern = "^cent_out_.*\\.csv$")
  intermediates <- union(intermediates, cent_files)
  n_moved <- 0
  for (f in intermediates) {
    src <- file.path(BASE, f)
    if (file.exists(src)) { file.rename(src, file.path(qdir, f)); n_moved <- n_moved + 1 }
  }
  # also the Downloads-root stale copy that old script 18 used
  if (file.exists("/Users/priyanka/Downloads/mobile_corrected.RData")) {
    file.rename("/Users/priyanka/Downloads/mobile_corrected.RData",
                file.path(qdir, "mobile_corrected_DOWNLOADSROOT.RData"))
    n_moved <- n_moved + 1
  }
  diag_msg("  quarantined ", n_moved, " files to ", qdir)
  diag_msg("  (old_outputs_predelayfix/ backups are untouched.)")
}

# ----------------------------------------------------------------
# 3) Run the full chain via the diagnostic wrappers, in DAG order
# ----------------------------------------------------------------
run_stage <- function(script) {
  diag_section(paste0("RUN: ", script))
  t0 <- Sys.time()
  # each wrapper runs in ITS OWN R process so package conflicts and
  # leftover objects cannot leak between stages
  status <- system2("Rscript", file.path(PIPE, script),
                    stdout = "", stderr = "")
  el <- round(difftime(Sys.time(), t0, units = "mins"), 1)
  if (status != 0) stop(script, " FAILED (exit ", status, ") after ", el, " min — stopping chain.")
  diag_msg(script, " finished in ", el, " min")
}

run_stage("R00a_verify_raw_inputs.R")          # official-repo inventory + coverage check
run_stage("R00b_make_grids.R")                 # generates 500 m + 5 km grids from scratch
run_stage("R01_delay_reprocessing.R")          # raw CSVs -> delay-corrected 1-s data
run_stage("R02_wind_merge.R")                  # + EPA AQS wind
run_stage("R03_background_segments.R")         # background + 500 m segments (Fig 2)
run_stage("R04_scaling_census_risk.R")         # La Casa scaling + census blocks
run_stage("R04b_build_block_sf_risk.R")        # CANONICAL block risk (med-of-daily-med, scaled)
run_stage("R05_hotspots.R")                    # source-prob maps + hotspot groups (Figs 3-4)
run_stage("R06_hrrr_plume_prep.R")             # HRRR + WWTF + stability
run_stage("R07_plume_inversion.R")             # H2S plumes + Gaussian inversion
run_stage("methane/M01_ingest_delay_garage.R")
run_stage("methane/M02_wind_background.R")
run_stage("methane/M03_hotspots.R")
run_stage("methane/M04_sourceprob_map.R")
run_stage("R99_manuscript_numbers_report.R")

diag_section("RUN_ALL complete")
diag_msg("Every intermediate regenerated from primary inputs. Manuscript numbers in:")
diag_msg("  ", file.path(PIPE, "manuscript_numbers_old_vs_new.csv"))
diag_msg("See REPRODUCIBILITY.md for the input manifest and DAG.")
