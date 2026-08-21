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
# PLUME EXEMPTION: detect on the RAW (un-averaged) H2S.
# Native-cadence 5-s averaging (script 03) is correct for maps/hotspots
# but flattens the sub-5-s plume rise/fall shape, collapsing the retained
# set. Here we point the plume object's H2S / baseline_H2S / plume_H2S at
# the delivered-signal versions (built in script 10 as *_raw) so the
# inversion runs on genuine plume structure. Maps/hotspots are unaffected.
# ----------------------------------------------------------------
.swap_raw_h2s <- function(d) {
  if (all(c("H2S_raw", "baseline_H2S_raw", "plume_H2S_raw") %in% names(d))) {
    d$H2S          <- d$H2S_raw
    d$baseline_H2S <- d$baseline_H2S_raw
    d$plume_H2S    <- d$plume_H2S_raw
    attr(d, ".h2s_raw_applied") <- TRUE
  }
  d
}
# BUGFIX (2026-08-20): the check below read attr(res_sub, ...) even when the
# guarded exists("res_sub") above was FALSE, and if only `res` carried the
# *_raw columns the swap was computed and then never saved while the log
# still claimed the raw signal was in use. Track both objects explicitly and
# require BOTH to have been swapped before re-saving.
.res_ok     <- exists("res")     && isTRUE(attr(res     <- .swap_raw_h2s(res),     ".h2s_raw_applied"))
.res_sub_ok <- exists("res_sub") && isTRUE(attr(res_sub <- .swap_raw_h2s(res_sub), ".h2s_raw_applied"))
if (exists("res") && exists("res_sub") && xor(.res_ok, .res_sub_ok)) {
  stop("R06: raw H2S columns present in only ONE of res / res_sub (res: ", .res_ok,
       ", res_sub: ", .res_sub_ok, "). Re-run 03/06/10 with NATIVE_CADENCE <- TRUE ",
       "so both objects carry H2S_raw / baseline_H2S_raw / plume_H2S_raw.")
}
if (.res_ok && .res_sub_ok) {
  save(res, res_sub, file = file.path(BASE, "mobile_hrrr_windfromwwtf_stability_filtered.RData"))
  save(res, file = file.path(BASE, "mobile_hrrr_windfromwwtf.RData"))
  diag_msg("  [PLUME EXEMPTION] H2S/baseline_H2S/plume_H2S set to RAW delivered signal ",
           "for plume detection; files re-saved.")
} else {
  diag_msg("  [PLUME EXEMPTION] raw H2S columns (*_raw) not found — plume branch ",
           "using averaged H2S (set NATIVE_CADENCE and re-run 03/06/10 if plumes collapse).")
}

# ----------------------------------------------------------------
# DIAG 2: stability class distribution (feeds R2.5 response!)
# ----------------------------------------------------------------
diag_section("R06-DIAG 2: stability classes + WWTF alignment")
stab_file <- file.path(BASE, "mobile_hrrr_windfromwwtf_stability_filtered.RData")
if (file.exists(stab_file)) {
  e_s <- new.env(); load(stab_file, envir = e_s)
  diag_msg("  objects: ", paste(ls(e_s), collapse = ", "))
  # BUGFIX (2026-08-20): this was get(ls(e_s)[1]). The file contains `res` and
  # `res_sub`; ls() sorts alphabetically, so sdat was `res` - the UNFILTERED
  # HRRR join - not `res_sub`, which P06 restricts to 0.5-5 km from the WWTF
  # and |wind - bearing| <= 10 deg. The stability-class distribution and the
  # [ALIGN] median printed below are the numbers the Reviewer-2 response cites,
  # and they were describing the wrong population.
  sdat <- get(if ("res_sub" %in% ls(e_s)) "res_sub" else ls(e_s)[1], envir = e_s)
  diag_msg("  using object: ", if ("res_sub" %in% ls(e_s)) "res_sub (WWTF-aligned, distance-filtered)" else ls(e_s)[1])
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
