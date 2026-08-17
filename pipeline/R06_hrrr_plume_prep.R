# ==============================================================
# R06_hrrr_plume_prep.R
# Re-runs HRRR meteorology join + WWTF wind-alignment + stability
# classes on the corrected dataset (inputs to the plume inversion).
#
# Sources (split from Suncor_plume.Rmd — the chain P05 depends on,
# since P05 expects an object named `res` saved by P04):
#   plume_scripts/P01_libraries.R
#   plume_scripts/P02_virtual_environment.R   (reticulate/Herbie setup)
#   plume_scripts/P03_code_to_download_hrrr.R (defines the HRRR sampler)
#   plume_scripts/P04_join_with_mobile_toxics_data.R (-> mobile_hrrr.RData, object `res`)
#   plume_scripts/P05_identify_if_wind_is_coming_from_wwtf.R
#   plume_scripts/P06_distance_from_wwtf_stability_class.R
#
# ALTERNATIVE: hrrr_scripts/H03+H04 (from Suncor_plume2.Rmd) is the faster,
# per-hour-cached HRRR variant, but it saves the object as `out_hrrr`.
# If you use it instead of P03+P04, rename before saving:
#   res <- out_hrrr; save(res, file=".../mobile_hrrr.RData")
#
# NOTE on the HRRR cache (hrrr_hour_cache/): cache keys are UTC hours
# and quantized lat/lon, so the seconds-level delay fix leaves nearly
# all cache entries valid — expect mostly cache hits, fast re-run.
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R06: HRRR + WWTF alignment + stability classes")

t0 <- Sys.time()
source(file.path(PIPE, "plume_scripts", "P01_libraries.R"))
source(file.path(PIPE, "plume_scripts", "P02_virtual_environment.R"))
source(file.path(PIPE, "plume_scripts", "P03_code_to_download_hrrr.R"))
source(file.path(PIPE, "plume_scripts", "P04_join_with_mobile_toxics_data.R"))
diag_msg("HRRR join completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 1: HRRR join quality
# ----------------------------------------------------------------
diag_section("R06-DIAG 1: HRRR join quality")
if (exists("res")) {
  diag_df_summary(res, "res (mobile + HRRR)", key_cols = c("u10", "v10", "hpbl", "lcc", "tcdc", "windspd", "winddir"))
  for (cc in c("u10", "v10", "hpbl")) {
    if (cc %in% names(res)) {
      diag_msg(sprintf("  [JOIN] %% missing %-8s: %.2f%%", cc, 100 * mean(is.na(res[[cc]]))))
    }
  }
  if (all(c("windspd", "ws") %in% names(res))) {
    r_ws <- suppressWarnings(cor(res$windspd, res$ws, use = "pairwise.complete.obs"))
    r_wd <- if ("winddir" %in% names(res) && "wd" %in% names(res))
      suppressWarnings(cor(res$winddir, res$wd, use = "pairwise.complete.obs")) else NA
    diag_msg(sprintf("  [SANITY] cor(HRRR windspd, station ws) = %.3f (expect clearly positive)", r_ws))
    diag_msg(sprintf("  [SANITY] cor(HRRR winddir, station wd) = %.3f", r_wd))
  }
}
diag_compare_rdata_rows("mobile_hrrr.RData", "res")

# ----------------------------------------------------------------
# Run WWTF alignment + stability classes
# ----------------------------------------------------------------
t1 <- Sys.time()
source(file.path(PIPE, "plume_scripts", "P05_identify_if_wind_is_coming_from_wwtf.R"))
source(file.path(PIPE, "plume_scripts", "P06_distance_from_wwtf_stability_class.R"))
diag_msg("WWTF/stability scripts completed in ", round(difftime(Sys.time(), t1, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 2: stability class distribution (feeds R2.5 response!)
# ----------------------------------------------------------------
diag_section("R06-DIAG 2: stability classes + WWTF alignment")
stab_file <- file.path(BASE, "mobile_hrrr_windfromwwtf_stability_filtered.RData")
if (file.exists(stab_file)) {
  e_s <- new.env(); load(stab_file, envir = e_s)
  diag_msg("  objects: ", paste(ls(e_s), collapse = ", "))
  sdat <- get(ls(e_s)[1], envir = e_s)
  if (is.data.frame(sdat)) {
    sc_col <- grep("Stability_Class", names(sdat), value = TRUE)
    for (cc in sc_col) {
      tb <- table(sdat[[cc]], useNA = "ifany")
      diag_msg("  ", cc, " distribution:")
      for (k in names(tb)) diag_msg(sprintf("    %-12s %s (%.1f%%)", k,
                                            format(as.integer(tb[k]), big.mark = ","),
                                            100 * tb[k] / sum(tb)))
    }
    ad_col <- names(sdat)[grepl("winddiff|angdiff|wind_diff", names(sdat))]
    if (length(ad_col)) {
      diag_msg(sprintf("  [ALIGN] median |wind - WWTF bearing| = %.1f deg",
                       stats::median(abs(sdat[[ad_col[1]]]), na.rm = TRUE)))
    }
  }
  diag_compare_rdata_rows("mobile_hrrr_windfromwwtf_stability_filtered.RData", "res_sub")
} else diag_msg("  [WARN] stability-filtered file not found: ", stab_file)

diag_msg("\nR06 complete. This stability distribution is also what the response to")
diag_msg("Reviewer 2 comment 5 (plume touchdown regimes) will cite.")
