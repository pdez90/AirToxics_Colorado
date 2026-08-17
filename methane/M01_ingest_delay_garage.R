# ==============================================================
# M01_ingest_delay_garage.R
# Ingest all quarterly methane CSVs (Picarro, same instrument as
# H2S), apply the SAME asset-specific inlet delay as H2S
# (CAT: 21 s, EMU: 17 s), remove garage (non-ambient) measurements
# within 100 m of ATOPs HQ (39.785359, -105.104331), and save a
# clean 1-s dataset: mobile_methane.RData / mobile_methane.csv.
#
# Data notes (from Anna, CDPHE):
#  - columns: Asset_CAT_EMU, UTC_Time, Methane_ppmv, Latitude, Longitude
#  - times are UTC; files are TAB-separated with CR-only line endings
#  - data NOT formally calibrated; one informal check on 2025-06-26
#    recovered 96.4-97.8% of a 5 ppm standard (accurate within ~5%)
#  - measurements at deployment start are often taken in the garage
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("M01: Methane ingest + delay + garage filter")

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(lubridate); library(geosphere)
})

METH_DIR <- "/Users/priyanka/Toxics_EST/MethaneData"
ATOPS_HQ <- c(lon = -105.104331, lat = 39.785359)   # garage location
GARAGE_RADIUS_M <- 100
DELAY <- c(CAT = 21, EMU = 17)                      # seconds, same Picarro as H2S

# ----------------------------------------------------------------
# Robust reader: files are tab-separated with CR-only line endings
# ----------------------------------------------------------------
NEED_COLS <- c("Asset_CAT_EMU", "UTC_Time", "Methane_ppmv", "Latitude", "Longitude")
read_methane_csv <- function(f) {
  txt <- readChar(f, file.size(f), useBytes = TRUE)
  txt <- gsub("\r\n|\r", "\n", txt)                 # normalize line endings
  dt <- tryCatch(fread(text = txt, sep = "\t", header = TRUE,
                       colClasses = "character", showProgress = FALSE),
                 error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  setnames(dt, names(dt), trimws(names(dt)))
  # some deployment files have NO header row (first line is data, so fread
  # consumed a data row as column names) — detect and re-read headerless
  if (!all(NEED_COLS %in% names(dt)) && ncol(dt) == 5 &&
      toupper(trimws(names(dt)[1])) %in% c("CAT", "EMU")) {
    dt <- tryCatch(fread(text = txt, sep = "\t", header = FALSE,
                         colClasses = "character", showProgress = FALSE),
                   error = function(e) NULL)
    if (!is.null(dt) && ncol(dt) == 5) setnames(dt, NEED_COLS)
  }
  dt
}

files <- list.files(METH_DIR, pattern = "_Methane\\.csv$", recursive = TRUE, full.names = TRUE)
n_appledouble <- sum(startsWith(basename(files), "._"))
files <- files[!startsWith(basename(files), "._")]   # drop macOS resource-fork files
diag_msg("Found ", length(files), " deployment CSVs under ", METH_DIR,
         " (excluded ", n_appledouble, " macOS '._' resource-fork files)")

# parse filename metadata: YYYYMMDD_Route_ASSET_Methane.csv
meta <- data.table(file = files)
meta[, base := basename(file)]
meta[, `:=`(dep_date = substr(base, 1, 8),
            Route = sub("^\\d{8}_(.*)_(CAT|EMU)_Methane\\.csv$", "\\1", base),
            Asset_file = sub("^.*_(CAT|EMU)_Methane\\.csv$", "\\1", base))]
diag_msg("Route breakdown from filenames:")
print(meta[, .N, by = .(Route, Asset_file)][order(Route)])
capture.output(print(meta[, .N, by = .(Route, Asset_file)][order(Route)]),
               file = .DIAG_LOG, append = TRUE)

# ----------------------------------------------------------------
# Study-domain filter: the manuscript covers the two NDCC routes.
# CollinsAerospace (Pueblo) deployments are outside the domain and
# are excluded by default. Set METHANE_ALL_ROUTES=1 to keep them.
# ----------------------------------------------------------------
if (!nzchar(Sys.getenv("METHANE_ALL_ROUTES"))) {
  keep <- meta$Route %in% c("SuncorP66", "HEPTerminal")
  diag_msg(sprintf("  [DOMAIN] keeping %d NDCC-route files; excluding %d (CollinsAerospace/Pueblo etc.)",
                   sum(keep), sum(!keep)))
  meta <- meta[keep]; files <- files[keep]
} else diag_msg("  [DOMAIN] METHANE_ALL_ROUTES=1 — keeping all routes")

# ----------------------------------------------------------------
# Read everything
# ----------------------------------------------------------------
read_fail <- character(0); all_list <- vector("list", length(files))
for (i in seq_along(files)) {
  dt <- read_methane_csv(files[i])
  if (is.null(dt)) { read_fail <- c(read_fail, basename(files[i])); next }
  need <- c("Asset_CAT_EMU", "UTC_Time", "Methane_ppmv", "Latitude", "Longitude")
  if (!all(need %in% names(dt))) { read_fail <- c(read_fail, paste0(basename(files[i]), " [cols: ", paste(names(dt), collapse=","), "]")); next }
  dt <- dt[, ..need]
  dt[, `:=`(file = basename(files[i]), Route = meta$Route[i], dep_date = meta$dep_date[i])]
  all_list[[i]] <- dt
  if (i %% 50 == 0) diag_msg("  read ", i, " / ", length(files), " files")
}
ch4 <- rbindlist(all_list, use.names = TRUE)
diag_msg("Rows read: ", format(nrow(ch4), big.mark = ","), " from ",
         length(files) - length(read_fail), " files")
if (length(read_fail)) {
  diag_msg("  [WARN] ", length(read_fail), " files failed to read/parse:")
  for (f in read_fail) diag_msg("    - ", f)
} else diag_msg("  [PASS] all files parsed with expected columns")

# ----------------------------------------------------------------
# Types + timezone. Toxics pipeline dates are Local_Time_MST
# (standard time year-round), i.e. fixed UTC-7 = Etc/GMT+7.
# ----------------------------------------------------------------
ch4[, Asset := toupper(trimws(Asset_CAT_EMU))]
ch4[, ch4_ppm := suppressWarnings(as.numeric(Methane_ppmv))]
ch4[, Latitude := suppressWarnings(as.numeric(Latitude))]
ch4[, Longitude := suppressWarnings(as.numeric(Longitude))]
ch4[, date_utc := ymd_hms(UTC_Time, tz = "UTC")]
ch4[, date := with_tz(date_utc, "Etc/GMT+7")]       # MST wall clock (no DST)
# CONVENTION MATCH: the toxics pipeline stores the MST wall clock LABELED as
# UTC (script 02 parses Local_Time_MST with ymd_hms default-UTC). Re-label the
# methane wall clock the same way so second-level joins to the toxics dataset
# align numerically. hour(date)/as.Date(date) are unaffected (wall clock kept).
ch4[, date := force_tz(date, "UTC")]

# ----------------------------------------------------------------
# Study-period boundary: the toxics campaign ends 2025-06-23, but
# methane data extend to Sep 2025. Default: truncate to the study
# period so all manuscript analyses share one window (late-2025
# methane also has no co-located toxics wind). Set METHANE_FULL=1
# to keep everything.
# ----------------------------------------------------------------
if (!nzchar(Sys.getenv("METHANE_FULL"))) {
  n_pre <- nrow(ch4)
  ch4 <- ch4[as.Date(date) <= as.Date("2025-06-30")]
  diag_msg(sprintf("  [STUDY PERIOD] truncated to <= 2025-06-30: dropped %s rows (%.1f%%) from Jul-Sep 2025",
                   format(n_pre - nrow(ch4), big.mark = ","), 100 * (n_pre - nrow(ch4)) / n_pre))
} else diag_msg("  [STUDY PERIOD] METHANE_FULL=1 — keeping all data through Sep 2025")

# DIAG: asset consistency between filename and column
mismatch <- ch4[Asset != meta$Asset_file[match(file, meta$base)], .N]
diag_msg(sprintf("  [%s] Asset column vs filename mismatches: %s rows",
                 ifelse(mismatch == 0, "PASS", "CHECK"), format(mismatch, big.mark = ",")))

# DIAG: time zone sanity — after conversion, sampling should sit in the
# 8 am - 4 pm local window (deployments are ~9 am - 2 pm)
hr_tab <- ch4[, .N, by = .(hr = hour(date))][order(hr)]
in_window <- ch4[hour(date) >= 8 & hour(date) <= 16, .N] / nrow(ch4)
diag_msg(sprintf("  [%s] fraction of measurements between 8am-4pm local: %.1f%% (expect >90%%)",
                 ifelse(in_window > 0.9, "PASS", "CHECK"), 100 * in_window))

# DIAG: CH4 plausibility — ambient baseline ~1.9-2.2 ppm
diag_msg(sprintf("  [SANITY] CH4 ppm: min %.3f | p05 %.3f | median %.3f | p99 %.3f | max %.1f",
                 min(ch4$ch4_ppm, na.rm=TRUE), quantile(ch4$ch4_ppm, .05, na.rm=TRUE),
                 median(ch4$ch4_ppm, na.rm=TRUE), quantile(ch4$ch4_ppm, .99, na.rm=TRUE),
                 max(ch4$ch4_ppm, na.rm=TRUE)))
below_ambient <- mean(ch4$ch4_ppm < 1.7, na.rm = TRUE)
diag_msg(sprintf("  [SANITY] fraction below 1.7 ppm (suspiciously sub-ambient): %.2f%%", 100 * below_ambient))

# ----------------------------------------------------------------
# Apply asset-specific delay (same Picarro inlet as H2S)
# ----------------------------------------------------------------
diag_section("M01: applying delays (CAT 21 s, EMU 17 s)")
n_bad_asset <- ch4[!Asset %in% names(DELAY), .N]
if (n_bad_asset > 0) diag_msg("  [CHECK] rows with Asset not CAT/EMU: ", n_bad_asset, " (treated as EMU)")
ch4[, delay_s := ifelse(Asset == "CAT", DELAY["CAT"], DELAY["EMU"])]
ch4[, date_predelay := date]
ch4[, date := floor_date(date - dseconds(delay_s), "second")]

# DIAG: verify shift on a sample
samp <- ch4[!is.na(date) & !is.na(date_predelay)][sample(.N, min(.N, 10000))]
shift_ok <- samp[, all(abs(as.numeric(date_predelay - date, units = "secs") - delay_s) <= 1)]
diag_msg(sprintf("  [%s] timestamp shift equals per-asset delay (+-1 s flooring) on 10k sample",
                 ifelse(shift_ok, "PASS", "FAIL")))
by_asset <- ch4[, .(mean_shift = mean(as.numeric(date_predelay - date, units = "secs"))), by = Asset]
for (i in seq_len(nrow(by_asset)))
  diag_msg(sprintf("  [SHIFT] %s mean applied shift: %.2f s (expected %s)",
                   by_asset$Asset[i], by_asset$mean_shift[i], DELAY[by_asset$Asset[i]]))

# ----------------------------------------------------------------
# Garage filter + missing-GPS handling
# ----------------------------------------------------------------
diag_section("M01: garage filter (100 m of ATOPs HQ) + missing GPS")
n0 <- nrow(ch4)
n_nogps <- ch4[is.na(Latitude) | is.na(Longitude), .N]
diag_msg(sprintf("  rows without GPS: %s (%.1f%%) — dropped (mostly garage/startup; cannot be mapped)",
                 format(n_nogps, big.mark = ","), 100 * n_nogps / n0))
ch4 <- ch4[!is.na(Latitude) & !is.na(Longitude)]

ch4[, dist_hq_m := distHaversine(cbind(Longitude, Latitude), matrix(ATOPS_HQ, ncol = 2))]
n_garage <- ch4[dist_hq_m <= GARAGE_RADIUS_M, .N]
diag_msg(sprintf("  rows within %d m of ATOPs HQ: %s (%.2f%%) — dropped (indoor/garage air)",
                 GARAGE_RADIUS_M, format(n_garage, big.mark = ","), 100 * n_garage / nrow(ch4)))
# per-file garage fraction (top offenders)
gar_by_file <- ch4[, .(pct_garage = 100 * mean(dist_hq_m <= GARAGE_RADIUS_M)), by = file][order(-pct_garage)]
diag_msg("  files with highest garage fraction (top 5):")
for (i in seq_len(min(5, nrow(gar_by_file))))
  diag_msg(sprintf("    %-45s %.1f%%", gar_by_file$file[i], gar_by_file$pct_garage[i]))
ch4 <- ch4[dist_hq_m > GARAGE_RADIUS_M]

# DIAG: CH4 in garage vs ambient (garage should be same or higher)
diag_msg(sprintf("  rows remaining after filters: %s (%.1f%% of raw)",
                 format(nrow(ch4), big.mark = ","), 100 * nrow(ch4) / n0))

# ----------------------------------------------------------------
# Aggregate to 1-s (mean within second per Asset), finalize, save
# ----------------------------------------------------------------
diag_section("M01: 1-s aggregation + save")
ch4_1s <- ch4[, .(ch4_ppm = mean(ch4_ppm, na.rm = TRUE),
                  Latitude = mean(Latitude), Longitude = mean(Longitude),
                  Route = first(Route)),
              by = .(Asset, date)]
dup_after <- ch4_1s[, .N, by = .(Asset, date)][N > 1, .N]
diag_msg(sprintf("  [%s] duplicate (Asset, second) keys after aggregation: %d",
                 ifelse(dup_after == 0, "PASS", "FAIL"), dup_after))
diag_msg(sprintf("  1-s rows: %s | date range: %s to %s | days: %d",
                 format(nrow(ch4_1s), big.mark = ","), min(ch4_1s$date), max(ch4_1s$date),
                 uniqueN(as.Date(ch4_1s$date))))
diag_msg("  per-asset 1-s rows:")
print(ch4_1s[, .N, by = Asset])
capture.output(print(ch4_1s[, .N, by = Asset]), file = .DIAG_LOG, append = TRUE)

df_ch4 <- as.data.frame(ch4_1s)
save(df_ch4, file = file.path(BASE, "mobile_methane.RData"))
fwrite(ch4_1s, file.path(BASE, "mobile_methane.csv"))
diag_msg("\nSaved: mobile_methane.RData / mobile_methane.csv (delay-corrected, garage-filtered)")
diag_msg("QA caveat to carry into SI: methane not formally calibrated; single informal check")
diag_msg("(2025-06-26) recovered 96.4-97.8% of a 5 ppm standard (within ~5%).")
