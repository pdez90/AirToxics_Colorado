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

diag_msg("diagnostics_helpers.R loaded. Log: ", .DIAG_LOG)
