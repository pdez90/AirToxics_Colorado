# ==============================================================
# diagnostics_helpers.R
# Shared diagnostics for the corrected-delay rerun pipeline.
# Source this at the top of every R* wrapper script.
# ==============================================================

BASE    <- "/Users/priyanka/Downloads/Suncor"
PIPE    <- file.path(BASE, "rerun_pipeline")
BACKUP  <- file.path(BASE, "old_outputs_predelayfix")   # snapshots of pre-fix outputs
LOGDIR  <- file.path(PIPE, "logs")
dir.create(LOGDIR, showWarnings = FALSE, recursive = TRUE)

# One log file per pipeline run-session (set once, reused if already set)
if (!exists(".DIAG_LOG")) {
  .DIAG_LOG <<- file.path(LOGDIR, paste0("diagnostics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
}

diag_msg <- function(...) {
  txt <- paste0(...)
  message(txt)
  cat(txt, "\n", file = .DIAG_LOG, append = TRUE)
}

diag_section <- function(title) {
  bar <- strrep("=", 66)
  diag_msg("\n", bar, "\nDIAG | ", title, "   [", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]\n", bar)
}

# Compare a newly computed value against a reference (old manuscript value
# or expected value). Flags relative changes larger than tol_pct.
diag_check_value <- function(label, new, reference, tol_pct = 15) {
  if (is.null(new) || length(new) == 0 || all(is.na(new))) {
    diag_msg(sprintf("  [CHECK-FAIL] %-45s new value is NA/empty (reference: %s)", label, paste(signif(reference, 4), collapse = ", ")))
    return(invisible(FALSE))
  }
  pct <- 100 * (new - reference) / ifelse(reference == 0, NA, reference)
  flag <- ifelse(is.na(pct) | abs(pct) > tol_pct, "  <-- CHANGED beyond tolerance, update manuscript", "")
  diag_msg(sprintf("  [CHECK] %-45s old/ref: %-10s new: %-10s (%+.1f%%)%s",
                   label, paste(signif(reference, 4), collapse = ","),
                   paste(signif(new, 4), collapse = ","), pct, flag))
  invisible(abs(pct) <= tol_pct)
}

# Quick data.frame health report
diag_df_summary <- function(df, name, key_cols = NULL) {
  diag_msg(sprintf("  [DF] %s: %s rows x %s cols", name, format(nrow(df), big.mark = ","), ncol(df)))
  if ("date" %in% names(df)) {
    diag_msg(sprintf("       date range: %s to %s", min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE)))
  }
  if ("Asset" %in% names(df)) {
    tb <- table(df$Asset, useNA = "ifany")
    diag_msg(paste0("       Asset counts: ", paste(names(tb), format(as.integer(tb), big.mark = ","), sep = "=", collapse = " | ")))
  }
  cols <- if (is.null(key_cols)) intersect(
    c("Benzene_ppb","Toluene_ppb","Trimethylbenzene_ppb","Xylene_ppb",
      "Hydrogen_Sulfide_ppb","Hydrogen_Cyanide_ppb","ws","wd","Latitude","Longitude"),
    names(df)) else intersect(key_cols, names(df))
  for (cc in cols) {
    v <- df[[cc]]
    diag_msg(sprintf("       %-24s NA: %5.1f%%  median: %-10s p99: %-10s max: %s",
                     cc, 100 * mean(is.na(v)),
                     signif(stats::median(v, na.rm = TRUE), 4),
                     signif(stats::quantile(v, 0.99, na.rm = TRUE), 4),
                     signif(max(v, na.rm = TRUE), 4)))
  }
  invisible(NULL)
}

# Compare row counts between the pre-fix backup of an .RData and the new one.
# obj_name = name of the object inside the RData (e.g., "df_out").
diag_compare_rdata_rows <- function(filename, obj_name) {
  oldf <- file.path(BACKUP, filename); newf <- file.path(BASE, filename)
  get_n <- function(f) {
    if (!file.exists(f)) return(NA_integer_)
    e <- new.env(); load(f, envir = e)
    nm <- if (obj_name %in% ls(e)) obj_name else ls(e)[1]
    obj <- get(nm, envir = e)
    if (is.data.frame(obj)) nrow(obj) else NA_integer_
  }
  n_old <- get_n(oldf); n_new <- get_n(newf)
  diag_msg(sprintf("  [ROWS] %-50s old: %-12s new: %-12s", filename,
                   format(n_old, big.mark = ","), format(n_new, big.mark = ",")))
  invisible(c(old = n_old, new = n_new))
}

# ---------------------------------------------------------------
# Reference values from the SUBMITTED manuscript (old delays).
# Every stage checks its new numbers against these; changes beyond
# tolerance are flagged so the manuscript/SI text can be updated.
# ---------------------------------------------------------------
REF <- list(
  # delays actually applied (seconds)
  delay_old = c(btex = 6.5, hcn = 7.5, h2s = 9),               # old, both platforms
  delay_cat = c(btex = 4,   hcn = 6,   h2s = 21),              # new, CAT
  delay_emu = c(btex = 5,   hcn = 3,   h2s = 17),              # new, EMU

  # Section 2.3
  missing_wind_pct = 11.1,
  median_dist_met_km = 4.6,

  # Section 2.5.3: pollutant-specific 99th percentiles (ppb)
  p99 = c(Benzene_ppb = 1.8, Toluene_ppb = 4.31, Trimethylbenzene_ppb = 2.59,
          Xylene_ppb = 3.19, Hydrogen_Sulfide_ppb = 5, Hydrogen_Cyanide_ppb = 12),

  # Section 2.5.2 / Fig S4.1: La Casa temporal scaling factors
  scaling = c(benzene = 1.15, toluene = 1.23, xylene = 1.38),

  # Section 3.3 risk numbers
  n_blocks = 1120, population = 83828,
  risk_airtox = c(0.077, 0.275), risk_mobile = c(0.183, 0.650), risk_ratio = 2.4,

  # Section 2.5.3.2 hotspot pipeline counts
  dbscan_initial = 160, clusters_ge2 = 40, clusters_ge3 = 17, clusters_ge4 = 8,
  final_groups = 17,

  # Section 3.6 plume inversion
  plume_candidates = 137, plume_retained = 7,
  emission_range_tpy = c(1e2, 1e3)
)

# --------------------------------------------------------------------
# BENCHMARK FIX (2026-08-20): the list above is the AS-SUBMITTED
# provenance record and every diag_check_value() call compared against it,
# so after the delay correction + native-cadence reprocessing each stage
# reported a "CHANGED beyond tolerance" discrepancy on every run - and
# R05's final_groups check runs at tol_pct = 0, so it could never pass.
# The as-submitted values are preserved under REF_SUBMITTED; REF now holds
# the CURRENT expected values, so a genuine regression is once again
# visible against a benchmark that is supposed to hold.
#
# Provenance for each updated entry (2026-08-19 native-cadence run):
#   p99 H2S/HCN      4.6 / 11      manuscript Section 2.5.3 (was 5 / 12)
#   n_blocks         1668          health-exposure block set (was 1120)
#   population       126607        same set (was 83828)
#   risk_airtox      0.117, 0.416  Section 3.3 (was 0.077, 0.275)
#   risk_mobile      0.113, 0.402  Section 3.3 (was 0.183, 0.650)
#   risk_ratio       0.97          2.4 was the KNOWN-ERRONEOUS value that
#                                  this reprocessing was undertaken to fix
#   dbscan_initial   2650          Section 2.5.3.2 (was 160)
#   clusters_ge3     18            (was 17)
#   final_groups     18            (was 17)
#   plume_candidates 33            Section 3.6 (was 137)
#   plume_retained   4             Section 3.6 (was 7)
# Unchanged and therefore not restated: delays, missing_wind_pct,
# median_dist_met_km, aromatic p99s, scaling, clusters_ge2, clusters_ge4.
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# BUGFIX (2026-08-21): R07 and R99 both picked the emission column out of
# P08's output with
#     grep("emission|rate|tons|Q_", names(r), ignore.case = TRUE)[1]
# The ONLY column that matches is `Q_ppm_m3_s` - a legacy volumetric
# quantity in ppm*m3/s - while `tpy_metric`, the actual metric tons/year
# column, matches none of those words. So every "emission rate (t/yr)" line
# in the manuscript-numbers report was printing ppm*m3/s under a t/yr label
# (3,189-243,131 instead of 126-9,410), and the "fraction within the
# as-submitted 100-1000 t/yr envelope" check compared ppm*m3/s against t/yr
# and reported 0% when the true figure is 42%.
#
# Name the column instead of guessing at it, and refuse to fall back to a
# unit we cannot identify.
pick_emission_col <- function(nms) {
  if ("tpy_metric" %in% nms) return("tpy_metric")
  hit <- grep("^tpy|tons_per_year|t_per_yr|_tpy$", nms, ignore.case = TRUE, value = TRUE)
  if (length(hit)) return(hit[1])
  stop("No metric-tons/year column found (looked for `tpy_metric`). Columns present: ",
       paste(nms, collapse = ", "),
       ". Refusing to guess - `Q_ppm_m3_s` and `kg_s` are NOT t/yr.")
}

REF_SUBMITTED <- REF

REF <- utils::modifyList(REF, list(
  p99 = c(Benzene_ppb = 1.8, Toluene_ppb = 4.31, Trimethylbenzene_ppb = 2.59,
          Xylene_ppb = 3.19, Hydrogen_Sulfide_ppb = 4.8, Hydrogen_Cyanide_ppb = 11),
  n_blocks = 1668, population = 126607,
  # refreshed 2026-08-22 from the run's own [CHECK] lines (were 11.1 / 4.6,
  # which had drifted just past the rounding boundary the documents quote)
  missing_wind_pct = 10.6, median_dist_met_km = 4.55,
  # BENCHMARK REFRESH (2026-08-21, full CLEAN re-run): risk_mobile and the
  # ratio drifted slightly from the 2026-08-19 values as the GPS/QA screen
  # removed 105,123 rows; plume_retained is 3, not 4. risk_airtox, n_blocks
  # and population reproduced exactly.
  risk_airtox = c(0.117, 0.416), risk_mobile = c(0.108, 0.384), risk_ratio = 0.92,
  # BENCHMARK REFRESH (2026-08-22, full CLEAN re-run with H2S retained across
  # the 2023 inlet window and the 28-30 May 2025 HCN calibration window
  # dropped). Verified against the run's own outputs:
  #   p99 H2S            4.8   hotspot_thresholds_summary.csv (was 4.6)
  #   dbscan_initial     2713  sum of n_clusters_all over the six pollutants
  #   clusters_ge3 / final_groups  17  MASTER_hotspot_group_index.csv
  #   plume_candidates   37    WWTP_H2S_plume_step_counts.csv
  #   plume_retained     4     same
  #   emission_baseline_mean_tpy 1036  WWTP_..._summary_mean_ci_METRIC_TPY.csv
  #   emission_range_tpy_wellposed  unchanged at 126-3929 (per-plume tpy_metric
  #                                 across the 26 usable scenarios)
  # clusters_ge2 re-derived 2026-08-22 from group_summary_persistent.csv:
  # n_pollutants >= 2 now holds for 37 groups (was 40 as submitted);
  # >= 3 gives 17 and >= 4 gives 8, both unchanged.
  dbscan_initial = 2713, clusters_ge2 = 37, clusters_ge3 = 17, final_groups = 17,
  plume_candidates = 37, plume_retained = 4,
  emission_range_tpy_wellposed = c(126, 3929),
  emission_baseline_mean_tpy = 1036
))

# --------------------------------------------------------------------
# BENCHMARK REFRESH (2026-09-23, 300 m ATOPs-headquarters exclusion).
# The block above was the state of the pipeline BEFORE the exclusion, so
# after the HQ300 re-run every stage was checking itself against numbers the
# run was no longer supposed to produce. Nothing failed loudly only because
# R05's zero-tolerance final_groups check sits inside a branch that is
# skipped when MASTER_hotspot_group_index.csv does not yet exist (group N
# runs after R05 in the phase-1 driver), so the one check that would have
# caught it never executed. Refreshed here so a real regression is visible
# again. As-submitted values remain in REF_SUBMITTED.
#
# Provenance for each entry (HQ300 run, 2026-09-22/23):
#   missing_wind_pct   0        R02 log: every row matched a station after the
#                               hourly fallback (2,555,285 / 2,555,285). The
#                               manuscript still says 10.6% - see below.
#   median_dist_met_km 4.51     R02 log
#   p99 toluene        3.8      TABLE_S3.1.csv / hotspot_thresholds_summary.csv
#   p99 TMB            2.14     (was 4.31 / 2.59 / 3.19 pre-exclusion)
#   p99 xylene         2.83
#   scaling            1.165/1.274/1.443   TABLE_scaling_sensitivity_factors.csv
#                                          (A_binweighted column)
#   n_blocks           1667     benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS.csv
#   population         126527   same file
#   risk_mobile        0.108-0.383   same file (risk_5_75 / risk_20_40)
#   risk_ratio         0.92     pop-weighted 0.1483 / 0.1611
#   dbscan_initial     2652     sum of n_clusters_all over the six pollutants
#   clusters_ge3       14       summary_stats_persistent.csv
#   final_groups       14       MASTER_hotspot_group_index.csv (14 rows)
# Unchanged and therefore not restated: delays, risk_airtox, clusters_ge2,
# clusters_ge4, and the whole plume branch (plume_candidates, plume_retained,
# emission_*) - the Gaussian plume stages were deliberately not re-run because
# every plume candidate lies 0.5-5 km from the wastewater facility and >9 km
# from the headquarters, so the 300 m exclusion cannot reach them.
#
# NOTE for the manuscript: missing_wind_pct is now 0, not 10.6%. Section 2.3
# still reads "Wind speed and direction data were missing for 10.6% of
# measurements". 06_merge_with_wind.R picks the closest station that HAS an
# observation in that hour, so after the fallback no measurement lacks wind.
# The sentence needs rewording, not just a new number.
# --------------------------------------------------------------------
REF <- utils::modifyList(REF, list(
  missing_wind_pct = 0, median_dist_met_km = 4.51,
  p99 = c(Benzene_ppb = 1.8, Toluene_ppb = 3.8, Trimethylbenzene_ppb = 2.14,
          Xylene_ppb = 2.83, Hydrogen_Sulfide_ppb = 4.8, Hydrogen_Cyanide_ppb = 11),
  scaling = c(benzene = 1.165, toluene = 1.274, xylene = 1.443),
  n_blocks = 1667, population = 126527,
  risk_mobile = c(0.108, 0.383), risk_ratio = 0.92,
  dbscan_initial = 2652, clusters_ge3 = 14, final_groups = 14
))

.ref_delta <- names(REF)[vapply(names(REF), function(k)
  !isTRUE(all.equal(REF[[k]], REF_SUBMITTED[[k]])), logical(1))]
diag_msg("[BENCHMARKS] REF updated to current values for: ",
         paste(.ref_delta, collapse = ", "))
diag_msg("[BENCHMARKS] as-submitted values remain available as REF_SUBMITTED$<name>")

diag_msg("diagnostics_helpers.R loaded. Log: ", .DIAG_LOG)
