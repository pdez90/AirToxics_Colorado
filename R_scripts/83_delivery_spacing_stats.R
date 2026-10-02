# ==============================================================
# 83_delivery_spacing_stats.R
# SI section S1.4 describes the DELIVERED mobile record (before any of our
# processing) in three sentences that no other script reproduces:
#   - "consecutive timestamps are mostly 1 s apart for the EMU laboratory
#     (..% of intervals, by year) and for the CAT laboratory in 2025 (..%), but
#     mostly 2 s apart for the CAT laboratory in 2023 and 2024 (..% and ..% of
#     intervals); a few percent of intervals are longer, and in 2024 about ..%
#     of EMU rows ..."                                           -> (a), (b)
#   - "H2S and HCN are reported in whole ppb, and ..% of consecutive one-second
#     values differ"                                             -> (c)
#   - "A large portion (..%) of the wind data collected by MaxiMet was,
#     therefore, questionable"                                   -> (d)
#
# Input: the 58 monthly CSVs Updated/csv/{Suncor,Terminal}_<Month>_<Year>.csv,
# exactly the files 02_newmobile_data.R reads and R00a_verify_raw_inputs.R
# verifies. The Goodrich route has no file in that set and is never read; the
# Site filter below makes the exclusion explicit, as 03_checks_flags.R does.
# Exact duplicate rows (identical in every column) are dropped first, as in
# 70_table_s31.R. Timestamps are the Local_Time_MST wall clock (fixed MST; the
# -0700 offset is asserted). A "lab-route-day" is Asset x route file x date.
#
# Definitions
#   (a) interval = difference between consecutive timestamps within a
#       lab-route-day, after sorting; share of intervals equal to 0, 1, 2 and
#       > 2 s, by laboratory and year. pct_1s / pct_2s / pct_gt2s are computed
#       over ALL intervals including zero-length ones (repeated timestamps).
#   (b) pct_rows_repeat_prev  = % of rows whose timestamp equals that of the
#       preceding row of the same lab-route-day (= the zero-interval share);
#       pct_rows_share_any    = % of rows whose timestamp is shared with at
#       least one other row of the same lab-route-day.
#   (c) one row per lab-route-day-second (the first row kept where a timestamp
#       repeats); among consecutive pairs exactly 1 s apart with a delivered
#       value at both seconds, % whose values differ. H2S and HCN separately,
#       on the delivered values before QA/QC voiding.
#   (d) MaxiMet QA flag: a row is flagged when trimws(MetData_flag) is
#       non-empty (03_checks_flags.R sets the mobile wind speed and direction
#       to NA unless MetData_flag == ""). Reported over all delivered rows, and
#       over the rows 02_newmobile_data.R keeps (finite Latitude/Longitude and
#       an empty GPS_flag). The full analysis set (after the delay shift and
#       the 300 m screen) lives only in mobile_wswd.RData and is not loaded.
# Pooled rows (year = "all") give the campaign-wide values.
#   -> TABLE_delivery_spacing.csv
#   Rscript R_scripts/83_delivery_spacing_stats.R
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
CSVD <- file.path(SUNCOR_BASE, "Updated", "csv")

files <- list.files(CSVD, pattern = "^(Suncor|Terminal)_[A-Za-z]+_[0-9]{4}\\.csv$", full.names = TRUE)
stopifnot(length(files) == 58)
raw <- rbindlist(lapply(files, function(f) {
  x <- fread(f, showProgress = FALSE, colClasses = c(Local_Time_MST = "character"))
  x[, Site := if (startsWith(basename(f), "Suncor")) "Suncor and Phillips 66 Terminal"
              else "Holly Energy Partners (Sinclair) Terminal"]
  x }), fill = TRUE)
setnames(raw, "Asset (CAT/EMU)", "Asset", skip_absent = TRUE)
raw <- raw[Site != "Goodrich Corporation (Collins Aerospace)"]
n_read <- nrow(raw); raw <- unique(raw)
message(sprintf("rows read %s | exact duplicates removed %s | rows %s",
                format(n_read, big.mark = ","), format(n_read - nrow(raw), big.mark = ","),
                format(nrow(raw), big.mark = ",")))
stopifnot(all(grepl("-0700$", raw$Local_Time_MST)))
raw[, `:=`(Asset = toupper(trimws(Asset)),
           ts    = as.numeric(as.POSIXct(substr(Local_Time_MST, 1, 19), tz = "UTC")),
           day   = substr(Local_Time_MST, 1, 10))]
raw[, year := substr(day, 1, 4)]
stopifnot(!anyNA(raw$ts), all(raw$Asset %in% c("CAT", "EMU")))
setorder(raw, Asset, Site, day, ts)
raw[, dt := ts - shift(ts), by = .(Asset, Site, day)]
raw[, n_same := .N, by = .(Asset, Site, day, ts)]
mf <- trimws(as.character(raw$MetData_flag)); mf[is.na(mf)] <- ""
gf <- trimws(as.character(raw$GPS_flag));     gf[is.na(gf)] <- ""
raw[, `:=`(met_flag = nzchar(mf),
           kept02   = is.finite(Latitude) & is.finite(Longitude) & !nzchar(gf))]

# (c) one row per lab-route-day-second
u <- unique(raw, by = c("Asset", "Site", "day", "ts"))
setorder(u, Asset, Site, day, ts)
u[, `:=`(dt1   = ts - shift(ts),
         h_prv = shift(Hydrogen_Sulfide_ppbV), c_prv = shift(Hydrogen_Cyanide_ppbV)),
  by = .(Asset, Site, day)]

stats <- function(r, uu) {
  iv <- r$dt[!is.na(r$dt)]
  ph <- uu[dt1 == 1 & is.finite(Hydrogen_Sulfide_ppbV) & is.finite(h_prv)]
  pc <- uu[dt1 == 1 & is.finite(Hydrogen_Cyanide_ppbV) & is.finite(c_prv)]
  data.table(n_rows = nrow(r), n_intervals = length(iv),
             pct_0s = 100 * mean(iv == 0), pct_1s = 100 * mean(iv == 1),
             pct_2s = 100 * mean(iv == 2), pct_gt2s = 100 * mean(iv > 2),
             pct_rows_repeat_prev = 100 * sum(r$dt == 0, na.rm = TRUE) / nrow(r),
             pct_rows_share_any   = 100 * mean(r$n_same > 1),
             n_pairs_h2s = nrow(ph), pct_h2s_pairs_differ = 100 * mean(ph$Hydrogen_Sulfide_ppbV != ph$h_prv),
             n_pairs_hcn = nrow(pc), pct_hcn_pairs_differ = if (nrow(pc)) 100 * mean(pc$Hydrogen_Cyanide_ppbV != pc$c_prv) else NA_real_,
             pct_metflag_delivered = 100 * mean(r$met_flag),
             pct_metflag_kept02    = 100 * mean(r$met_flag[r$kept02]))
}
grp <- unique(raw[, .(Asset, year)])[order(Asset, year)]
res <- rbindlist(c(
  lapply(seq_len(nrow(grp)), function(i) {
    a <- grp$Asset[i]; y <- grp$year[i]
    cbind(data.table(lab = a, year = y), stats(raw[Asset == a & year == y], u[Asset == a & year == y])) }),
  lapply(c("CAT", "EMU"), function(a) cbind(data.table(lab = a, year = "all"), stats(raw[Asset == a], u[Asset == a]))),
  list(cbind(data.table(lab = "both", year = "all"), stats(raw, u)))))
fwrite(res, file.path(SUNCOR_BASE, "TABLE_delivery_spacing.csv"))
pr <- copy(res); num <- names(pr)[startsWith(names(pr), "pct_")]
pr[, (num) := lapply(.SD, round, 1), .SDcols = num]
print(pr[, c("lab", "year", "n_intervals", "pct_0s", "pct_1s", "pct_2s", "pct_gt2s",
             "pct_rows_repeat_prev", "pct_rows_share_any"), with = FALSE])
print(pr[, c("lab", "year", "n_pairs_h2s", "pct_h2s_pairs_differ", "n_pairs_hcn",
             "pct_hcn_pairs_differ", "pct_metflag_delivered", "pct_metflag_kept02"), with = FALSE])
message("-> ", file.path(SUNCOR_BASE, "TABLE_delivery_spacing.csv"))
