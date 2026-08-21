# ==============================================================
# M02_wind_background.R
# (1) Attach wind (ws/wd) to the methane 1-s dataset by joining to
#     the already-wind-merged toxics dataset (mobile_wswd.RData)
#     on Asset + second — the van is one place at a time, so this
#     inherits the closest-AQS-station-with-data logic exactly.
# (2) Apply the SAME hourly background correction used for the
#     toxics (rolling lowest-20th-percentile): CH4 enhancement =
#     ch4 - hourly background per Asset-day.
# Output: mobile_methane_wind_bg.RData
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("M02: Methane wind join + background correction")

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(lubridate); library(zoo)
})

load(file.path(BASE, "mobile_methane.RData"))       # df_ch4
ch4 <- as.data.table(df_ch4)

# ----------------------------------------------------------------
# 1) Wind join via the corrected toxics dataset
#    NOTE: run this AFTER R01-R02 so mobile_wswd.RData reflects the
#    corrected delays.
# ----------------------------------------------------------------
wswd_f <- file.path(BASE, "mobile_wswd.RData")
stopifnot(file.exists(wswd_f))
e_w <- new.env(); load(wswd_f, envir = e_w)
tox <- as.data.table(get(ls(e_w)[1], envir = e_w))
diag_msg("Toxics wind dataset: ", format(nrow(tox), big.mark = ","), " rows; cols include: ",
         paste(intersect(c("Asset","Site","date","ws","wd","Hydrogen_Sulfide_ppb"), names(tox)), collapse = ", "))

wind_cols <- intersect(c("ws", "wd", "Site"), names(tox))
tox_w <- unique(tox[, c("Asset", "date", wind_cols), with = FALSE], by = c("Asset", "date"))
setkey(tox_w, Asset, date); setkey(ch4, Asset, date)
ch4 <- tox_w[ch4]                                    # left join wind onto methane

join_rate <- 100 * mean(!is.na(ch4$ws))
diag_msg(sprintf("  [JOIN] methane seconds matched to a toxics second with wind: %.1f%%", join_rate))
diag_msg("  (unmatched seconds = methane-only periods or wind gaps; filled by hour below)")

# fallback: hourly wind per Asset from the toxics data for unmatched seconds
tox_w[, hour := floor_date(date, "hour")]
hr_wind <- tox_w[!is.na(ws), .(ws_h = median(ws, na.rm = TRUE), wd_h = median(wd, na.rm = TRUE)),
                 by = .(Asset, hour)]
ch4[, hour := floor_date(date, "hour")]
ch4 <- merge(ch4, hr_wind, by = c("Asset", "hour"), all.x = TRUE)
ch4[is.na(ws), `:=`(ws = ws_h, wd = wd_h)]
ch4[, c("ws_h", "wd_h") := NULL]
diag_msg(sprintf("  [JOIN] wind coverage after hourly fallback: %.1f%% (ms toxics analogue: ~%.0f%%)",
                 100 * mean(!is.na(ch4$ws)), 100 - REF$missing_wind_pct))

# ----------------------------------------------------------------
# 2) Background correction: hourly rolling lowest-20th-percentile
#    (same principle as the toxics pipeline scripts 10-11)
# ----------------------------------------------------------------
diag_section("M02: background correction (rolling lowest 20th pct, 20-min windows)")
setorder(ch4, Asset, date)
ch4[, AssetDay := paste0(Asset, "_", as.Date(date))]

# ---------------------------------------------------------------
# BACKGROUND CONSISTENCY FIX (2026-08-20)
#
# This claimed to use "the SAME hourly background correction used for
# the toxics", but it did not. zoo::rollapply(width = 1200) is indexed
# by ROW, and ch4 has a row only for seconds that were measured, so any
# within-day gap - a stop, a route change, an instrument dropout -
# silently stretched the "20-minute" window across a much longer stretch
# of wall-clock time. Under 5-s native cadence a 1200-ROW window spans
# about 100 minutes of real time, not 20. It also grouped on Asset-day
# only, where the toxics group on date x Site x Asset, and it used a
# plain subtraction where the toxics use bg_correct().
#
# Rebuilt to mirror 10_calculating_background...R and
# 11_correcting_for_background.R exactly:
#   - window: TIME-indexed, +/- 600 s (slider::slide_index_dbl on the
#     numeric timestamp), partial windows allowed
#   - statistic: 20th percentile, requiring >= 30 finite points
#   - grouping: date x route x Asset (the methane analogue of
#     date x Site x Asset; Route is carried from M01 and, unlike the
#     Site joined from the toxics table, is never NA)
#   - a rolling sd and a plume flag on the same window, as script 10
#   - sCH4 via the same bg_correct() rule script 11 applies to the
#     toxics, so a background-corrected methane column exists on the
#     same footing as sBenzene, sH2S and the rest
#
# ch4_enh (the raw enhancement over local background) is retained
# because M03 reports it. Note that the methane HOTSPOTS are defined on
# the raw ch4_ppm 99th percentile, not on ch4_enh, so this fix changes
# the enhancement statistics reported per cluster and NOT the cluster
# inventory, the 2.576 ppm threshold, or the hotspot co-elevation
# fractions in Section 3.7.
# ---------------------------------------------------------------
suppressPackageStartupMessages(library(slider))

.BG_BEFORE <- 600      # seconds; script 10 uses +/- 600 s
.BG_AFTER  <- 600
.BG_MIN_N  <- 30       # script 10 requires >= 30 finite points in the window
.BG_PCT    <- 0.20     # lowest-20th-percentile baseline

.q20 <- function(v, min_n = .BG_MIN_N) {
  vv <- v[is.finite(v)]
  if (length(vv) < min_n) return(NA_real_)
  unname(stats::quantile(vv, .BG_PCT, names = FALSE))
}
.sd_safe <- function(v, min_n = .BG_MIN_N) {
  vv <- v[is.finite(v)]
  if (length(vv) < min_n) return(NA_real_)
  stats::sd(vv)
}

# GUARD (2026-08-20): the fallback was rep("ALL", nrow(ch4)), which silently
# collapses the key back to date x Asset - exactly the grouping this rewrite
# replaced - with no warning. M01 always carries Route; if it ever stops, that
# is a pipeline change that should be noticed, not absorbed.
stopifnot("M02: `Route` missing from the methane table - M01 must carry it so the background can be grouped date x route x Asset, matching script 10" =
            "Route" %in% names(ch4))
.route <- ch4$Route
ch4[, bgkey := paste0(as.Date(date), "_", .route, "_", Asset)]
setorder(ch4, bgkey, date)
diag_msg(sprintf("  background groups (date x route x Asset): %d", uniqueN(ch4$bgkey)))

ch4[, ch4_bg := NA_real_][, ch4_sd := NA_real_]
for (.k in unique(ch4$bgkey)) {
  .i <- which(ch4$bgkey == .k)
  .t <- as.numeric(ch4$date[.i])
  .x <- as.numeric(ch4$ch4_ppm[.i])
  ch4$ch4_bg[.i] <- slider::slide_index_dbl(.x, .t, .before = .BG_BEFORE,
                                            .after = .BG_AFTER, .complete = FALSE, .f = .q20)
  ch4$ch4_sd[.i] <- slider::slide_index_dbl(.x, .t, .before = .BG_BEFORE,
                                            .after = .BG_AFTER, .complete = FALSE, .f = .sd_safe)
}
ch4[, ch4_plume := !is.na(ch4_ppm) & !is.na(ch4_bg) & !is.na(ch4_sd) &
                   (ch4_ppm >= ch4_bg + 3 * ch4_sd)]
ch4[, ch4_enh := ch4_ppm - ch4_bg]                   # enhancement over local background

# script 11's rule, verbatim: obs - baseline + median_bg when baseline <= obs,
# otherwise obs * median_bg / baseline (guarding a zero baseline).
ch4[, median_bgCH4 := median(ch4_bg[is.finite(ch4_bg)], na.rm = TRUE), by = bgkey]
ch4[, median_bgCH4 := ifelse(is.finite(median_bgCH4), median_bgCH4, NA_real_)]
.bg_correct <- function(obs, base, med) {
  # Mirrors 11_correcting_for_background.R including its 2026-08-20 fix: the
  # multiplicative branch is only meaningful for a STRICTLY POSITIVE baseline.
  out <- rep(NA_real_, length(obs))
  ok  <- is.finite(obs) & is.finite(base) & is.finite(med)
  ok_add   <- ok & ((base <= obs) | (base <= 0))
  out[ok_add] <- obs[ok_add] - base[ok_add] + med[ok_add]
  ok_ratio <- ok & (base > obs) & (base > 0)
  out[ok_ratio] <- obs[ok_ratio] * med[ok_ratio] / base[ok_ratio]
  out
}
ch4[, sCH4 := .bg_correct(ch4_ppm, ch4_bg, median_bgCH4)]
diag_msg(sprintf("  [BG] ch4_bg defined on %.1f%% of rows | sCH4 on %.1f%% | plume flag on %.2f%%",
                 100 * mean(is.finite(ch4$ch4_bg)), 100 * mean(is.finite(ch4$sCH4)),
                 100 * mean(ch4$ch4_plume)))

# DIAG: background plausibility (regional ambient CH4 ~1.9-2.2 ppm)
bg_med <- median(ch4$ch4_bg, na.rm = TRUE)
diag_msg(sprintf("  [SANITY] median CH4 background: %.3f ppm (expect ~1.9-2.2)", bg_med))
diag_msg(sprintf("  [SANITY] median enhancement: %.4f ppm (expect near 0, slightly positive)",
                 median(ch4$ch4_enh, na.rm = TRUE)))
diag_msg(sprintf("  [SANITY] p99 enhancement: %.3f ppm | max: %.1f ppm",
                 quantile(ch4$ch4_enh, .99, na.rm = TRUE), max(ch4$ch4_enh, na.rm = TRUE)))
neg_frac <- mean(ch4$ch4_enh < -0.05, na.rm = TRUE)
diag_msg(sprintf("  [SANITY] fraction of enhancements < -0.05 ppm: %.2f%% (large => background too high)",
                 100 * neg_frac))

# DIAG: co-located CH4 vs H2S alignment (same Picarro!) — after correct
# delays BOTH series should be synchronized: scan lags +-10 s and confirm
# the correlation peak sits at lag 0 (uses plume-rich seconds only).
diag_section("M02-DIAG: CH4 vs H2S lag check (same instrument => peak at 0)")
if ("Hydrogen_Sulfide_ppb" %in% names(tox)) {
  h2s <- tox[!is.na(Hydrogen_Sulfide_ppb), .(Asset, date, h2s = Hydrogen_Sulfide_ppb)]
  best <- data.table()
  for (a in unique(ch4$Asset)) {
    cc_by_lag <- sapply(-10:10, function(lag) {
      m <- merge(ch4[Asset == a & ch4_enh > quantile(ch4_enh, .95, na.rm = TRUE),
                     .(date = date + lag, ch4_enh)],
                 h2s[Asset == a], by = "date")
      if (nrow(m) < 200) return(NA_real_)
      suppressWarnings(cor(m$ch4_enh, m$h2s, use = "complete.obs"))
    })
    if (all(is.na(cc_by_lag))) { diag_msg("  [", a, "] insufficient overlap for lag check"); next }
    bl <- (-10:10)[which.max(cc_by_lag)]
    diag_msg(sprintf("  [%s] %s: max CH4-H2S correlation at lag %+d s (r=%.3f) — expect 0 (+-2)",
                     ifelse(abs(bl) <= 2, "PASS", "CHECK"), a, bl, max(cc_by_lag, na.rm = TRUE)))
  }
  diag_msg("  (CH4 and H2S are co-measured by the Picarro; a peak away from 0 means the")
  diag_msg("   CH4 delay and the H2S delay are inconsistent — revisit before hotspots.)")
} else diag_msg("  [WARN] H2S column not found in toxics data; lag check skipped")

df_ch4_bg <- as.data.frame(ch4)
save(df_ch4_bg, file = file.path(BASE, "mobile_methane_wind_bg.RData"))
diag_msg("\nSaved: mobile_methane_wind_bg.RData (wind + background-corrected)")
