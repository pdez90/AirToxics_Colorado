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

# 20-minute rolling 20th percentile per Asset-day, then hourly interpolation
bg_fun <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  r <- zoo::rollapply(x, width = 1200, FUN = function(v) quantile(v, 0.20, na.rm = TRUE),
                      fill = NA, align = "center", partial = 300)
  zoo::na.approx(r, na.rm = FALSE, rule = 2)
}
ch4[, ch4_bg := bg_fun(ch4_ppm), by = AssetDay]
ch4[, ch4_enh := ch4_ppm - ch4_bg]                   # enhancement over local background

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
