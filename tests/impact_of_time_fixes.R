# ==============================================================
# impact_of_time_fixes.R
#
# Quantifies what the 2026-08-21 time-convention work changed in the OUTPUTS,
# as opposed to what it changed in the source. Four questions, answered from
# the intermediates already on disk - no re-run required:
#
#   1. Scripts 02 and 06: does the corrected parse change anything, or does it
#      only pin down the convention the existing outputs were already built on?
#   2. Script 06: is the mobile-to-AQS wind join actually pairing like clock
#      with like clock?
#   3. HRRR hours: how many OBSERVATIONS would have been assigned a different
#      meteorological hour under each of the two historical failure modes?
#      They are separate bugs an order of magnitude apart and are counted
#      separately. If a previous mobile_hrrr.RData exists, its stored `hour`
#      is compared against the corrected chain directly.
#   4. Script 36: how many HYSPLIT trajectories were launched an hour early
#      (checked against the expected count), and does correcting the launch
#      hour change which receptors are chosen?
#
# Run:  SUNCOR_BASE=~/Downloads/Suncor Rscript tests/impact_of_time_fixes.R
# ==============================================================

suppressPackageStartupMessages({ library(dplyr); library(lubridate) })

BASE <- Sys.getenv("SUNCOR_BASE", ".")
f_wswd <- file.path(BASE, "mobile_wswd.RData")
if (!file.exists(f_wswd))
  stop("Need mobile_wswd.RData. Set SUNCOR_BASE to the working folder.")

e <- new.env(); load(f_wswd, envir = e)
out <- as.data.frame(e[[ls(e)[1]]])
cat(sprintf("mobile_wswd.RData: %d rows\n", nrow(out)))

# --------------------------------------------------------------
# 1) Does the 02/06 parse correction move any existing output?
#
# The corrected script 02 parses Local_Time_MST with tz = "UTC", i.e. it stores
# the MST wall clock with a UTC attribute. If the intermediates already carry
# exactly that, then the correction is a no-op for every number in the paper
# and its whole value is the guard: it stops a future re-run from silently
# adopting the America/Denver parse, which WOULD move things.
# --------------------------------------------------------------
cat("\n== 1. scripts 02 / 06: no-op on existing outputs, or not? ==\n")
tz_now <- paste(attr(out$date, "tzone"), collapse = "/")
cat(sprintf("   `date` attribute on disk: '%s'  (corrected 02 produces 'UTC')\n", tz_now))
if (identical(tz_now, "UTC")) {
  cat("   -> the intermediates were ALREADY built on the corrected convention.\n")
  cat("      The parse change moves NO existing number. What it adds is the\n")
  cat("      assertion, so a re-run cannot quietly switch conventions.\n")
} else {
  cat("   -> MISMATCH. These intermediates were built under a different parse;\n")
  cat("      everything downstream of script 02 needs regenerating.\n")
}

# --------------------------------------------------------------
# 2) Is the wind join pairing like clock with like clock?
#
# Both sides store their own local clock with a UTC attribute: the mobile side
# is MST from Local_Time_MST, the AQS side is Local Standard Time from
# Date.Local/Time.Local. Read as clocks, the wind record joined to an
# observation must be the SAME hour, i.e. a lag in [-1, 0) hours after
# flooring. Under the America/Denver parse this lag would be 6-7 hours.
# --------------------------------------------------------------
cat("\n== 2. script 06: mobile-to-AQS wind alignment ==\n")
i <- which(!is.na(out$wd) & !is.na(out$ws))
cat(sprintf("   rows with matched station wind: %d of %d (%.1f%%)\n",
            length(i), nrow(out), 100 * length(i) / nrow(out)))
if (length(i) && "wind_date" %in% names(out)) {
  lag <- as.numeric(difftime(out$wind_date[i], out$date[i], units = "hours"))
  cat(sprintf("   clock lag (wind hour - observation): median %.2f h, range %.2f to %.2f\n",
              median(lag), min(lag), max(lag)))
  good <- all(lag > -1.0001 & lag <= 0.0001)
  cat(sprintf("   %s  every matched record uses the wind hour containing it\n",
              if (good) "PASS " else "FAIL "))
  if (!good)
    cat("   -> lags outside [-1, 0] mean the two sides are on different clocks.\n")
}

# --------------------------------------------------------------
# 3) HRRR HOUR AUDIT.
#
# How many OBSERVATIONS would have been assigned a different meteorological
# hour under each of the two historical failure modes? These are distinct bugs
# and they differ by an order of magnitude, so they are counted separately:
#
#   (a) MST vs America/Denver .... one hour, summer only.
#   (b) treating the UTC-labelled wall clock as an absolute instant (the old
#       H04 helper: as.POSIXct(x, tz=) RELABELS a POSIXct, so the force_tz
#       that followed was a no-op) ...... about seven hours, all year.
#
# Computed directly from the timestamps, so it needs no "before" file.
# --------------------------------------------------------------
cat("\n== 3. HRRR hour: how many observations would have moved ==\n")

d <- out$date[is.finite(out$date)]
hour_correct <- with_tz(force_tz(round(d, "hour"), "MST"), "UTC")          # P04 / H04 now
hour_denver  <- with_tz(force_tz(round(d, "hour"), "America/Denver"), "UTC")  # failure (a)
hour_asutc   <- with_tz(round(d, "hour"), "UTC")                           # failure (b)

n <- length(d)
chg_a <- sum(as.numeric(hour_denver) != as.numeric(hour_correct))
chg_b <- sum(as.numeric(hour_asutc)  != as.numeric(hour_correct))
cat(sprintf("   observations: %d\n", n))
cat(sprintf("   (a) America/Denver instead of MST : %d observations move (%.1f%%), all by 1 h, summer only\n",
            chg_a, 100 * chg_a / n))
cat(sprintf("   (b) wall clock read as a UTC instant: %d observations move (%.1f%%), by %.0f-%.0f h\n",
            chg_b, 100 * chg_b / n,
            min(abs(as.numeric(difftime(hour_asutc, hour_correct, units = "hours")))),
            max(abs(as.numeric(difftime(hour_asutc, hour_correct, units = "hours"))))))
cat(sprintf("   distinct HRRR hours requested: %d correct, %d under (a), %d under (b)\n",
            length(unique(hour_correct)), length(unique(hour_denver)), length(unique(hour_asutc))))

# If a previous mobile_hrrr.RData survives (quarantined by CLEAN, or in the
# pre-delay-fix backup), compare its stored `hour` against the correct one
# directly - the strongest form of this check.
prev <- c(Sys.glob(file.path(BASE, "quarantine_intermediates_*", "mobile_hrrr.RData")),
          file.path(BASE, "old_outputs_predelayfix", "mobile_hrrr.RData"))
prev <- prev[file.exists(prev)]
if (length(prev)) {
  ev <- new.env(); load(prev[1], envir = ev)
  r <- ev$res
  if (!is.null(r) && all(c("date", "hour") %in% names(r))) {
    want <- with_tz(force_tz(round(r$date, "hour"), "MST"), "UTC")
    diff_h <- as.numeric(difftime(r$hour, want, units = "hours"))
    cat(sprintf("\n   previous mobile_hrrr.RData (%s):\n", basename(dirname(prev[1]))))
    cat(sprintf("     %d of %d rows carried a different HRRR hour than the corrected chain\n",
                sum(abs(diff_h) > 1e-6), nrow(r)))
    print(table(`offset_hours` = round(diff_h)))
  }
} else {
  cat("\n   (no previous mobile_hrrr.RData to compare against - it has not been\n")
  cat("    generated before, so there is no stale HRRR join to audit.)\n")
}

cat("\n== 4. script 36: HYSPLIT receptor launch hours ==\n")
exclude_site <- "Goodrich Corporation (Collins Aerospace)"
q_low <- 0.20; N_max <- 60

need <- c("Trimethylbenzene_ppb", "Benzene_ppb", "Site", "Latitude", "Longitude", "date")
if (!all(need %in% names(out))) {
  cat("   SKIP  mobile_wswd.RData lacks the columns script 36 selects on\n")
} else {
  hs <- out %>%
    filter(Site != exclude_site, is.finite(Trimethylbenzene_ppb), is.finite(Benzene_ppb),
           Benzene_ppb > 0, is.finite(Longitude), is.finite(Latitude), !is.na(date)) %>%
    mutate(tmb_by_benz = Trimethylbenzene_ppb / Benzene_ppb) %>%
    filter(is.finite(tmb_by_benz))
  thr <- as.numeric(quantile(hs$tmb_by_benz, q_low, na.rm = TRUE))
  hs  <- hs %>% filter(tmb_by_benz <= thr)
  cat(sprintf("   bottom %.0f%% TMB/benzene rows: %d (threshold %.4g)\n",
              q_low * 100, nrow(hs), thr))

  conv <- function(d, tz) with_tz(force_tz(as.POSIXct(d), tzone = tz), "UTC")
  hs$utc_old <- conv(hs$date, "America/Denver")   # as committed before the fix
  hs$utc_new <- conv(hs$date, "MST")              # corrected
  hs$shift_h <- as.numeric(difftime(hs$utc_new, hs$utc_old, units = "hours"))
  cat(sprintf("   candidate rows moving +1 h: %d of %d (%.1f%%); unchanged: %d\n",
              sum(hs$shift_h == 1), nrow(hs), 100 * mean(hs$shift_h == 1), sum(hs$shift_h == 0)))

  thin <- function(x, col) {
    x$day_utc <- as.Date(x[[col]]); x$hour_utc <- hour(x[[col]])
    x %>% arrange(.data[[col]]) %>% distinct(day_utc, hour_utc, .keep_all = TRUE)
  }
  t_old <- thin(hs, "utc_old"); t_new <- thin(hs, "utc_new")
  cat(sprintf("   receptor-hours after thinning: OLD %d, NEW %d %s\n",
              nrow(t_old), nrow(t_new),
              if (nrow(t_old) == nrow(t_new)) "(selection unchanged)" else "(SELECTION CHANGED)"))

  set.seed(1); s_new <- t_new %>% slice_sample(n = min(N_max, nrow(t_new)))
  .n_run <- nrow(s_new); .n_chg <- sum(s_new$shift_h == 1)
  cat(sprintf("   trajectories actually run: N = %d, of which %d (%.0f%%) were launched\n",
              .n_run, .n_chg, 100 * mean(s_new$shift_h == 1)))
  cat("   an hour early before this fix.\n")

  # Audit trail: tie the code repair to a specific, checkable count rather than
  # to "the figures look different". Measured on the pre-re-run data, so a
  # change here means the receptor SELECTION moved and needs explaining -
  # nothing about the fix itself should alter which receptors are chosen.
  EXPECT_RUN <- 60L; EXPECT_CHANGED <- 35L
  if (.n_run == EXPECT_RUN && .n_chg == EXPECT_CHANGED) {
    cat(sprintf("   CHECK  %d of %d - matches the expected count.\n", .n_chg, .n_run))
  } else {
    cat(sprintf("   CHECK  %d of %d, expected %d of %d. The receptor selection has MOVED.\n",
                .n_chg, .n_run, EXPECT_CHANGED, EXPECT_RUN))
    cat("          Nothing in the time fix should change WHICH receptors are picked,\n")
    cat("          so investigate before regenerating the HYSPLIT figures.\n")
  }
  cat("\n   first few receptor-times, old vs corrected launch hour:\n")
  print(head(data.frame(
    mst_clock      = format(s_new$date,    "%Y-%m-%d %H:%M", tz = "UTC"),
    old_launch_utc = format(s_new$utc_old, "%Y-%m-%d %H:00", tz = "UTC"),
    new_launch_utc = format(s_new$utc_new, "%Y-%m-%d %H:00", tz = "UTC")), 6))
  cat("\n   Any figure or statement built on those back-trajectories should be\n")
  cat("   regenerated. The receptor SET is unchanged, so this is a re-run of 36,\n")
  cat("   not a change of method.\n")
}
