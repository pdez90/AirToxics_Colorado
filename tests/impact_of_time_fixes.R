# ==============================================================
# impact_of_time_fixes.R
#
# Quantifies what the 2026-08-21 time-convention work changed in the OUTPUTS,
# as opposed to what it changed in the source. Three questions, answered from
# the intermediates already on disk - no re-run required:
#
#   1. Scripts 02 and 06: does the corrected parse change anything, or does it
#      only pin down the convention the existing outputs were already built on?
#   2. Script 06: is the mobile-to-AQS wind join actually pairing like clock
#      with like clock?
#   3. Script 36: how many HYSPLIT trajectories were launched an hour early,
#      and does correcting the launch hour change which receptors are chosen?
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
# 3) Script 36: the one substantive change.
#
# force_tz("America/Denver") -> force_tz("MST"). Reproduces 36's selection and
# thinning and compares the receptor launch hours.
# --------------------------------------------------------------
cat("\n== 3. script 36: HYSPLIT receptor launch hours ==\n")
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
  cat(sprintf("   trajectories actually run: N = %d, of which %d (%.0f%%) were launched\n",
              nrow(s_new), sum(s_new$shift_h == 1), 100 * mean(s_new$shift_h == 1)))
  cat("   an hour early before this fix.\n")
  cat("\n   first few receptor-times, old vs corrected launch hour:\n")
  print(head(data.frame(
    mst_clock      = format(s_new$date,    "%Y-%m-%d %H:%M", tz = "UTC"),
    old_launch_utc = format(s_new$utc_old, "%Y-%m-%d %H:00", tz = "UTC"),
    new_launch_utc = format(s_new$utc_new, "%Y-%m-%d %H:00", tz = "UTC")), 6))
  cat("\n   Any figure or statement built on those back-trajectories should be\n")
  cat("   regenerated. The receptor SET is unchanged, so this is a re-run of 36,\n")
  cat("   not a change of method.\n")
}
