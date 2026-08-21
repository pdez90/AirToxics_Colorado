# ==============================================================
# R07_plume_inversion.R
# Re-runs H2S plume identification + Gaussian plume inversion on
# corrected data. This is the stage MOST sensitive to the delay
# fix: H2S moved from a 9 s to a 21 s (CAT) / 17 s (EMU) shift,
# i.e. plume positions move ~12 s x driving speed (~30-130 m)
# along the route. Expect plume counts and geometry to change.
#
# Sources (split from Suncor_plume.Rmd):
#   plume_scripts/P07_identify_and_plot_plumes_for_h2s.R
#   plume_scripts/P08_gaussian_plumes_h2s.R
# Simulation sections (P09, P10) depend only on synthetic data and
# do NOT need re-running for the delay fix.
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R07: H2S plume identification + Gaussian inversion")

t0 <- Sys.time()
source(file.path(PIPE, "plume_scripts", "P07_identify_and_plot_plumes_for_h2s.R"))
source(file.path(PIPE, "plume_scripts", "P08_gaussian_plumes_h2s.R"))
diag_msg("Plume scripts completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 1: plume funnel (candidates -> retained) vs manuscript 137 -> 7
# ----------------------------------------------------------------
diag_section("R07-DIAG 1: plume filtering funnel")
fun_file <- file.path(BASE, "WWTP_H2S_plume_step_counts.csv")
if (file.exists(fun_file)) {
  fun <- utils::read.csv(fun_file)
  diag_msg("  New filtering funnel:")
  for (i in seq_len(nrow(fun))) diag_msg("    ", paste(names(fun), "=", unlist(fun[i, ]), collapse = "  "))
  old_fun_file <- file.path(BACKUP, "WWTP_H2S_plume_step_counts.csv")
  if (file.exists(old_fun_file)) {
    ofun <- utils::read.csv(old_fun_file)
    diag_msg("  Old (pre-fix) funnel:")
    for (i in seq_len(nrow(ofun))) diag_msg("    ", paste(names(ofun), "=", unlist(ofun[i, ]), collapse = "  "))
  }
} else diag_msg("  [WARN] funnel file not found; read counts from script printout above.")
diag_msg(sprintf("  Expected (current run): %d candidate events, %d retained.  ",
                 REF$plume_candidates, REF$plume_retained),
         sprintf("As submitted: %d and %d.",
                 REF_SUBMITTED$plume_candidates, REF_SUBMITTED$plume_retained))

# ----------------------------------------------------------------
# DIAG 2: emission-rate estimates vs manuscript (10^2-10^3 tons/yr)
# ----------------------------------------------------------------
diag_section("R07-DIAG 2: emission rates old vs new")
# BUGFIX (2026-08-20): this looked for res_h2s.csv in BASE. Nothing writes it -
# P08_gaussian_plumes_h2s.R:374 writes
# FinalFig/WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv (column tpy_metric).
# DIAG 2, the only quantitative plume check in the pipeline, therefore always
# printed "[WARN] NEW results not found" and never ran.
.RES_REL <- file.path("FinalFig", "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv")
res_new_f <- file.path(BASE, .RES_REL); res_old_f <- file.path(BACKUP, .RES_REL)
if (!file.exists(res_new_f) && file.exists(file.path(BASE, "res_h2s.csv"))) {
  res_new_f <- file.path(BASE, "res_h2s.csv")
  diag_msg("  [NOTE] falling back to the legacy res_h2s.csv - it is a pre-fix ",
           "artifact that no script regenerates; treat it as provenance only.")
}
summarize_res <- function(f, tag) {
  if (!file.exists(f)) { diag_msg("  [WARN] ", tag, " results not found: ", f); return(invisible(NULL)) }
  r <- utils::read.csv(f)
  er_col <- grep("emission|rate|tons|Q_", names(r), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(er_col)) er_col <- names(r)[vapply(r, is.numeric, TRUE)][1]
  v <- r[[er_col]]
  diag_msg(sprintf("  [%s] n=%d | %s: min %s | median %s | max %s",
                   tag, nrow(r), er_col, signif(min(v, na.rm = TRUE), 3),
                   signif(stats::median(v, na.rm = TRUE), 3), signif(max(v, na.rm = TRUE), 3)))
  invisible(v)
}
v_new <- summarize_res(res_new_f, "NEW")
v_old <- summarize_res(res_old_f, "OLD")
if (!is.null(v_new)) {
  # NOTE (2026-08-20): emission_range_tpy was NOT updated when the other REF
  # benchmarks were, so this compares against the AS-SUBMITTED 10^2-10^3 t/yr
  # envelope. Labelled as such rather than silently re-benchmarked, because the
  # current envelope should be read off this run, not asserted in advance.
  .rng <- REF_SUBMITTED$emission_range_tpy
  in_range <- mean(v_new >= .rng[1] & v_new <= .rng[2], na.rm = TRUE)
  diag_msg(sprintf("  [CHECK] fraction of new estimates within the AS-SUBMITTED %g-%g t/yr envelope: %.0f%%",
                   .rng[1], .rng[2], 100 * in_range))
  diag_msg(sprintf("  [RANGE] this run: min %s | median %s | max %s t/yr",
                   signif(min(v_new, na.rm = TRUE), 3),
                   signif(stats::median(v_new, na.rm = TRUE), 3),
                   signif(max(v_new, na.rm = TRUE), 3)))
}

# ----------------------------------------------------------------
# DIAG 3: physical sanity of retained plumes
# ----------------------------------------------------------------
diag_section("R07-DIAG 3: retained plume physical sanity")
diag_msg("  For each retained plume verify (from script output/figures):")
diag_msg("   - downwind distance in the robust window (~2-5 km per Section 3.6);")
diag_msg("   - wind speed > 1 m/s and stable wind direction during transect;")
diag_msg("   - stability class consistent with daytime touchdown (A-D);")
diag_msg("   - plume shape approximately Gaussian (see FIG_H2S_* diagnostics PNGs).")
diag_msg("  QA figures regenerated by P07/P08 in ", BASE, " (FIG_H2S_*, QA_H2S_*).")

diag_msg("\nR07 complete. If retained-plume count or emission range changed, update")
diag_msg("Abstract, Section 3.6, Discussion, and SI S6; also the response letter (R2.3, R2.5).")
