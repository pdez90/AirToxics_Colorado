# ==============================================================
# R01_delay_reprocessing.R
# Re-runs raw-data loading + the CORRECTED asset-specific delay
# alignment (CAT: BTEX 4 s, HCN 6 s, H2S 21 s;
#            EMU: BTEX 5 s, HCN 3 s, H2S 17 s),
# producing mobile.csv / mobile.RData, then runs diagnostics.
#
# Sources your own section scripts (verbatim from Suncor.Rmd):
#   R_scripts/01_libraries.R
#   R_scripts/02_newmobile_data.R
#   R_scripts/03_checks_flags.R   <- contains .asset_delay()
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R01: Delay reprocessing (corrected asset-specific delays)")

# BUGFIX (2026-08-20): this was a hard stopifnot(dir.exists(BACKUP)), but
# RUN_ALL_from_raw.R advertises "ONE COMMAND, RAW DATA IN" and never calls
# R00_backup_old_outputs.R, so a genuine from-raw run died here. BACKUP only
# supplies the OLD side of the old-vs-new comparisons; its absence should
# degrade the diagnostics, not stop the pipeline.
if (!dir.exists(BACKUP)) {
  dir.create(BACKUP, showWarnings = FALSE, recursive = TRUE)
  diag_msg("  [NOTE] ", BACKUP, " did not exist and has been created EMPTY. ",
           "This run has no pre-fix snapshot to compare against, so every ",
           "old-vs-new diagnostic will report the old side as NA. That is ",
           "expected for a clean from-raw run; run R00_backup_old_outputs.R ",
           "first if you want the comparisons.")
}

t0 <- Sys.time()
source(file.path(BASE, "R_scripts", "01_libraries.R"))
source(file.path(BASE, "R_scripts", "02_newmobile_data.R"))
source(file.path(BASE, "R_scripts", "03_checks_flags.R"))
diag_msg("Section scripts completed in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# ----------------------------------------------------------------
# DIAG 1: unit-test the delay function itself
# ----------------------------------------------------------------
diag_section("R01-DIAG 1: .asset_delay() unit tests")
tests <- list(
  c("CAT", "btex", 4),  c("CAT", "hcn", 6),  c("CAT", "h2s", 21),
  c("EMU", "btex", 5),  c("EMU", "hcn", 3),  c("EMU", "h2s", 17),
  c("cat", "h2s", 21),                      # case-insensitivity
  c(" CAT ", "btex", 4),                    # whitespace robustness
  c("Emu", "hcn", 3)
)
all_pass <- TRUE
for (tt in tests) {
  got <- .asset_delay(tt[1], tt[2])
  pass <- isTRUE(all.equal(as.numeric(got), as.numeric(tt[3])))
  if (!pass) all_pass <- FALSE
  diag_msg(sprintf("  [%s] .asset_delay('%s','%s') = %s (expected %s)",
                   ifelse(pass, "PASS", "FAIL"), tt[1], tt[2], got, tt[3]))
}
# vectorized behaviour (per-row delays)
v <- .asset_delay(c("CAT", "EMU", "CAT"), "h2s")
diag_msg(sprintf("  [%s] vectorized h2s delays for c(CAT,EMU,CAT): %s (expected 21,17,21)",
                 ifelse(identical(as.numeric(v), c(21, 17, 21)), "PASS", "FAIL"),
                 paste(v, collapse = ",")))
if (!all_pass) stop("Delay unit tests FAILED — do not proceed.")

# ----------------------------------------------------------------
# DIAG 2: new dataset health report
# ----------------------------------------------------------------
diag_section("R01-DIAG 2: new mobile.RData health")
diag_df_summary(df_out, "df_out (new, corrected delays)")

# ----------------------------------------------------------------
# DIAG 3: old vs new row counts + per-asset counts
# ----------------------------------------------------------------
diag_section("R01-DIAG 3: old vs new comparison")
# BUGFIX (2026-08-20): this load() was unguarded, so on the from-raw run that
# the BACKUP fix above was written for - BACKUP freshly created and EMPTY -
# it threw "cannot open compressed file", R01 exited non-zero, and RUN_ALL
# stopped the whole chain at stage 3. DIAG 3 and DIAG 4 are old-vs-new
# comparisons; with no snapshot there is simply nothing to compare.
.old_mobile <- file.path(BACKUP, "mobile.RData")
.have_old <- file.exists(.old_mobile)
if (!.have_old) {
  diag_msg("  [SKIP] no pre-fix snapshot at ", .old_mobile,
           " - DIAG 3 and DIAG 4 (old-vs-new rows and lag verification) cannot run. ",
           "Run R00_backup_old_outputs.R first if you want them.")
}
if (.have_old) {
e_old <- new.env(); load(.old_mobile, envir = e_old)
df_old <- get(ls(e_old)[1], envir = e_old)
diag_msg(sprintf("  rows  old: %s   new: %s   (%+.2f%%)",
                 format(nrow(df_old), big.mark = ","), format(nrow(df_out), big.mark = ","),
                 100 * (nrow(df_out) - nrow(df_old)) / nrow(df_old)))
for (a in unique(df_out$Asset)) {
  n_o <- sum(df_old$Asset == a, na.rm = TRUE); n_n <- sum(df_out$Asset == a, na.rm = TRUE)
  diag_msg(sprintf("  rows [%s]  old: %s  new: %s", a, format(n_o, big.mark = ","), format(n_n, big.mark = ",")))
}

# ----------------------------------------------------------------
# DIAG 4: LAG VERIFICATION — the decisive check.
# For each Asset x pollutant, the new series should be the old series
# shifted by (new_delay - old_delay) seconds. We scan lags -25..+25 s
# on a sample of days and report the lag with maximum correlation.
# Expected lags (new - old, i.e. how much further back the new data
# were shifted relative to the old processing):
#   CAT: BTEX 4-6.5   = -2.5 -> -2 or -3 (floor effects), HCN 6-7.5 = -1.5, H2S 21-9 = +12
#   EMU: BTEX 5-6.5   = -1.5,  HCN 3-7.5 = -4.5,           H2S 17-9 = +8
# (positive = new timestamps are EARLIER by that many seconds)
# ----------------------------------------------------------------
}  # end if (.have_old) for DIAG 3

if (.have_old) {
diag_section("R01-DIAG 4: lag verification old->new (sampled days)")
suppressPackageStartupMessages(library(data.table))

lag_expected <- list(
  CAT = c(Benzene_ppb = 4 - 6.5, Hydrogen_Cyanide_ppb = 6 - 7.5, Hydrogen_Sulfide_ppb = 21 - 9),
  EMU = c(Benzene_ppb = 5 - 6.5, Hydrogen_Cyanide_ppb = 3 - 7.5, Hydrogen_Sulfide_ppb = 17 - 9)
)

dt_old <- as.data.table(df_old)[, .(Asset, Site, date,
                                    Benzene_ppb, Hydrogen_Cyanide_ppb, Hydrogen_Sulfide_ppb)]
dt_new <- as.data.table(df_out)[, .(Asset, Site, date,
                                    Benzene_ppb, Hydrogen_Cyanide_ppb, Hydrogen_Sulfide_ppb)]
dt_old[, day := as.Date(date)]; dt_new[, day := as.Date(date)]

set.seed(42)
for (a in intersect(names(lag_expected), unique(dt_new$Asset))) {
  days_a <- intersect(unique(dt_old[Asset == a, day]), unique(dt_new[Asset == a, day]))
  samp_days <- if (length(days_a) > 12) sample(days_a, 12) else days_a
  o <- dt_old[Asset == a & day %in% samp_days]
  n <- dt_new[Asset == a & day %in% samp_days]
  for (poll in names(lag_expected[[a]])) {
    best_lag <- NA_integer_; best_cor <- -Inf
    for (lag in -25:25) {
      m <- merge(o[!is.na(get(poll)), .(Site, date, v_old = get(poll))],
                 n[!is.na(get(poll)), .(Site, date = date + lag, v_new = get(poll))],
                 by = c("Site", "date"))
      if (nrow(m) > 500) {
        cc <- suppressWarnings(cor(m$v_old, m$v_new, use = "complete.obs"))
        if (!is.na(cc) && cc > best_cor) { best_cor <- cc; best_lag <- lag }
      }
    }
    exp_lag <- lag_expected[[a]][poll]
    # floor_date on x.5 offsets means the recovered lag can land on either
    # integer adjacent to the expected fractional lag
    ok <- !is.na(best_lag) && abs(best_lag - exp_lag) <= 1
    diag_msg(sprintf("  [%s] %s %-22s best lag: %+d s (r=%.3f) | expected: %+.1f s",
                     ifelse(ok, "PASS", "CHECK"), a, poll, best_lag, best_cor, exp_lag))
  }
}
}  # end if (.have_old) for DIAG 4

diag_msg("\nInterpretation: 'best lag' is how far the new series is shifted vs the old.")
diag_msg("If PASS on all rows, the corrected delays were applied exactly as intended.")

diag_msg("\nR01 complete. Outputs: mobile.csv, mobile.RData (corrected delays).")
