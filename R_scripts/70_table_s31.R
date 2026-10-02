# ==============================================================
# Table S3.1 — Descriptive statistics of the mobile measurements
#
# Generates EVERY cell of SI Table S3.1 from primary inputs. Nothing in
# the table is typed by hand; the script writes TABLE_S3.1.csv and prints
# the same numbers.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/70_table_s31.R
#
# Inputs
#   Updated/csv/{Suncor,Terminal}_<Month>_<Year>.csv   58 files, as read by
#                                                      02_newmobile_data.R
#   mobile_wswd.RData                                  the analysis set
#
# The Goodrich route is not part of this CSV set and is excluded everywhere.
#
# Row definitions follow the filters the pipeline actually applies. Each row
# applies one further filter to the row above it (cumulative), and the last
# row reproduces the analysis set exactly (asserted below).
#
#   1 Reported by CDPHE          non-missing value in the monthly CSVs, after
#                                dropping exactly-duplicated rows
#   2 Retained after QA/QC       03_checks_flags.R: a value is voided only if a
#                                flag token is a NULL data qualifier. Values
#                                below the MDL (MD) and negative values are KEPT
#   3 With valid GPS             plus finite Latitude and Longitude
#   4 After campaign exclusions  the date exclusions in 03_checks_flags.R (see
#                                EXCLUDE below). The HCN windows follow CDPHE's
#                                own read-me documents.
#   5 Without a GPS qualifier    02_newmobile_data.R removes every row that
#                                carries any GPS_flag token (route deviations and
#                                suspect fixes), with its pollutant values
#   6 One row per second         03: the delay shift moves each pollutant to its
#                                sampling second; rows delivered with the same
#                                laboratory, route and timestamp are averaged
#   7 With a position            03: the delay-corrected second must fall on the
#                                laboratory-route-day position grid and carry a
#                                latitude/longitude (gaps of <= 3 s interpolated,
#                                longer gaps left empty and the row dropped)
#   8 Outside HQ                 03: rows whose delay-corrected position lies
#                                within 300 m of the CDPHE headquarters removed.
#   9 Analysis set               03 section 3d (2026-09-27): H2S and HCN keep ONE
#                                value per 5-s / 2-s acquisition bin (the bin
#                                mean, on the bin's middle delivered second);
#                                the aromatics are unchanged. Equals
#                                mobile_wswd.RData (Goodrich excluded).
#
# (Until 2026-09-27 row 5 was an HQ screen on the DELIVERED position of the raw
# rows, and rows 5-7 were folded silently into the analysis-set row. The
# delivered-position screen described no step the pipeline takes, and the GPS
# qualifier filter of 02, which removes 7-10% of each record, was not shown.)
#
# The bin averaging of H2S/HCN (03, section 3b) changes values, not row counts;
# the one-value-per-bin step (3d) changes counts and is row 9. All summary
# statistics and the below-MDL fraction are computed on the analysis set.
# ==============================================================
suppressPackageStartupMessages({library(data.table)})

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
CSV  <- file.path(BASE, Sys.getenv("SUNCOR_CSV_DIR", "Updated/csv"))
OUT  <- file.path(BASE, "TABLE_S3.1.csv")

POLL <- data.table(
  name = c("Benzene","Toluene","Xylene","Trimethylbenzene","H2S","HCN"),
  raw  = c("Benzene_ppbV","Toluene_ppbV","Xylene_ppbV","Trimethylbenzene_ppbV",
           "Hydrogen_Sulfide_ppbV","Hydrogen_Cyanide_ppbV"),
  flag = c("Benzene_flag","Toluene_flag","Xylene_flag","Trimethylbenzene_flag",
           "Hydrogen_Sulfide_flag","Hydrogen_Cyanide_flag"),
  fin  = c("Benzene_ppb","Toluene_ppb","Xylene_ppb","Trimethylbenzene_ppb",
           "Hydrogen_Sulfide_ppb","Hydrogen_Cyanide_ppb"))

# ---- CDPHE qualifier codebook (verbatim from 03_checks_flags.R) ----
QUAL_NULL <- c("AL","AN","AO","AQ","AT","AX","AY","AZ","BA","BD",
               "BH","BK","BL","BM","BR","EC","MB","XX")
QUAL_KEEP <- c("CD","CG","IH","IL","IR","IT","QG","QP","QT","QW",
               "EH","LJ","MD","NS","QX")

voided <- function(flag) {
  f <- toupper(trimws(as.character(flag))); f[is.na(f)] <- ""
  tk <- strsplit(f, "[,.;[:space:]]+")
  seen <- unique(unlist(tk)); seen <- seen[nzchar(seen)]
  unknown <- setdiff(seen, c(QUAL_NULL, QUAL_KEEP))
  if (length(unknown))
    stop("qualifier code(s) absent from the codebook: ", paste(sort(unknown), collapse = ", "))
  vapply(tk, function(z) any(z %in% QUAL_NULL), logical(1))
}

# ---- audit MDLs, read from the CDPHE packets ----
# 69_cdphe_audit_mdls.R writes CDPHE_audit_MDLs.csv straight from the
# "Quarterly Summary" tab of each quarterly packet, which is where the
# HB21-1189 read-me PDFs say the MDLs live. Nothing is transcribed here.
MDLF <- file.path(BASE, "CDPHE_audit_MDLs.csv")
if (!file.exists(MDLF))
  stop("CDPHE_audit_MDLs.csv not found. Run R_scripts/69_cdphe_audit_mdls.R first.")
mdlraw <- fread(MDLF)
CMAP <- c(Benzene = "Benzene", Toluene = "Toluene", Xylene = "Xylene",
          Trimethylbenzene = "Trimethylbenzene",
          H2S = "Hydrogen sulfide (H2S)", HCN = "Hydrogen cyanide (HCN)")
MDL <- rbindlist(lapply(names(CMAP), function(nm) {
  d <- mdlraw[compound == CMAP[[nm]]][order(from_ym)]
  rbindlist(list(
    data.table(Asset = "CAT", name = nm, from = d$from_ym, mdl = as.numeric(d$cat_mdl)),
    data.table(Asset = "EMU", name = nm, from = d$from_ym, mdl = as.numeric(d$emu_mdl))))
}))
MDL <- MDL[!is.na(mdl)]
message("audit MDLs loaded: ", nrow(MDL), " (lab x quarter x compound) entries, ",
        min(mdlraw$from_ym), " to ", max(mdlraw$from_ym))

# ==============================================================
# stage 1-3 : the monthly CSVs
# ==============================================================
files <- list.files(CSV, pattern = "\\.csv$", full.names = TRUE)
message("Reading ", length(files), " monthly CSVs from ", CSV)
stopifnot(length(files) == 58)
# Local_Time_MST MUST be read as character: fread otherwise parses it to
# POSIXct and silently discards the -0700 offset, which is the one thing this
# pipeline asserts about its timestamps (see REPRODUCIBILITY.md).
# Site is assigned from the file name exactly as 02_newmobile_data.R does, because
# the per-second collapse and the position grid of 03 are keyed on it.
raw <- rbindlist(lapply(files, function(f) {
  x <- fread(f, showProgress = FALSE, colClasses = c(Local_Time_MST = "character"))
  x[, Site := if (startsWith(basename(f), "Suncor")) "Suncor" else "Terminal"]
  x
}), fill = TRUE)
setnames(raw, "Asset (CAT/EMU)", "Asset", skip_absent = TRUE)
message("  rows read              : ", format(nrow(raw), big.mark = ","))
n0 <- nrow(raw); raw <- unique(raw)
message("  exact duplicates removed: ", format(n0 - nrow(raw), big.mark = ","))
message("  rows after dedup       : ", format(nrow(raw), big.mark = ","))

# every raw timestamp must carry the literal -0700 (fixed MST; see REPRODUCIBILITY.md)
stopifnot(is.character(raw$Local_Time_MST))
offs <- unique(sub(".*([+-]\\d{4})$", "\\1", raw$Local_Time_MST))
stopifnot(identical(offs, "-0700"))
message("  all timestamps carry -0700: TRUE")

gps_ok <- is.finite(raw$Latitude) & is.finite(raw$Longitude)
message("  rows with finite GPS   : ", format(sum(gps_ok), big.mark = ","))
# CDPHE headquarters (03_checks_flags.R section 3c): the screen is applied to the
# DELAY-CORRECTED position, so it is evaluated below on the position grid.
HQ_LAT <- 39.785189; HQ_LON <- -105.104411; HQ_RADIUS_M <- 300
.hav_m <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008.8 * asin(pmin(1, sqrt(a)))
}
# GPS qualifier (02_newmobile_data.R): any non-blank GPS_flag removes the row.
gflag <- trimws(as.character(raw$GPS_flag)); gflag[is.na(gflag)] <- ""
gps_flagged <- nzchar(gflag)
message("  rows carrying a GPS flag: ", format(sum(gps_flagged), big.mark = ","))

# ---- campaign exclusions, verbatim from 03_checks_flags.R lines 107-130 ----
# These are hard-coded date windows carrying no comment in the source. They are
# reproduced here so the row is auditable rather than unexplained.
raw[, ts  := as.POSIXct(substr(Local_Time_MST, 1, 19), tz = "MST")]
raw[, day := as.Date(ts)]
EXCLUDE <- function(nm) {
  # 2023 inlet-line contamination, 2023-04-16 .. 2023-09-20. CDPHE names the
  # PTR-ToF-MS and CI-ToF-MS compounds; H2S (Picarro CRDS) is not on that list
  # and is retained, matching EXCLUDE_H2S_2023 <- FALSE in 03_checks_flags.R.
  ex <- raw$day >= as.Date("2023-04-16") & raw$day <= as.Date("2023-09-20")
  if (nm == "H2S") ex <- rep(FALSE, nrow(raw))
  if (nm %in% c("Benzene","Toluene","Xylene","Trimethylbenzene"))
    ex <- ex | raw$day %in% as.Date(c("2024-08-13","2024-08-14"))
  if (nm == "HCN")
    # CDPHE read-me Q1 2025: HCN before 22 Jan 2025 is background-corrected
    # rather than absolute. CDPHE read-me Q2 2025: the 28-30 May 2025 HCN
    # concentrations rest on an inaccurate sensitivity calibration.
    ex <- ex | raw$day %in% as.Date(c("2025-01-02","2025-01-03")) |
          raw$ts <= as.POSIXct("2025-01-22 00:00:00", tz = "MST") |
          raw$day %in% as.Date(c("2025-05-28","2025-05-29","2025-05-30"))
  ex
}

# ---- position grid of 03_checks_flags.R section 1 ----
# built from every row that survives 02 (finite, unflagged GPS): latest row per
# laboratory-route-day-second, a 1-s grid from the first to the last second of
# each laboratory-route-day, and latitude/longitude interpolated linearly across
# gaps of up to 3 s (zoo::na.approx(maxgap = 3)); longer gaps stay empty.
raw[, .sec := as.numeric(ts)]
met <- unique(raw[gps_ok & !gps_flagged, .(Asset, Site, day, .sec, Latitude, Longitude)],
              by = c("Asset", "Site", "day", ".sec"), fromLast = TRUE)
grid <- met[, .(.sec = seq(min(.sec), max(.sec), by = 1)), by = .(Asset, Site, day)]
grid <- merge(grid, met, by = c("Asset", "Site", "day", ".sec"), all.x = TRUE)
setorder(grid, Asset, Site, day, .sec)
grid[, `:=`(Latitude  = zoo::na.approx(Latitude,  x = .sec, maxgap = 3, na.rm = FALSE),
            Longitude = zoo::na.approx(Longitude, x = .sec, maxgap = 3, na.rm = FALSE)),
     by = .(Asset, Site, day)]
grid <- unique(grid, by = c("Asset", "Site", ".sec"))
grid[, has_pos := is.finite(Latitude) & is.finite(Longitude)]
grid[, in_hq := has_pos & .hav_m(Latitude, Longitude, HQ_LAT, HQ_LON) <= HQ_RADIUS_M]
grid <- grid[, .(Asset, Site, .sec, has_pos, in_hq)]
message("  position grid seconds  : ", format(nrow(grid), big.mark = ","))

DELAY <- list(btex = c(CAT = 4, EMU = 5), h2s = c(CAT = 21, EMU = 17), hcn = c(CAT = 6, EMU = 3))
POLL[, delay := c("btex", "btex", "btex", "btex", "h2s", "hcn")]

stage <- POLL[, .(name)]
stage[, `:=`(reported = NA_integer_, after_qc = NA_integer_,
             gps = NA_integer_, after_excl = NA_integer_, no_gps_flag = NA_integer_,
             one_per_second = NA_integer_, with_position = NA_integer_,
             outside_hq = NA_integer_, one_per_bin = NA_integer_)]
for (i in seq_len(nrow(POLL))) {
  v <- raw[[POLL$raw[i]]]; f <- raw[[POLL$flag[i]]]
  keep <- !voided(f); ex <- EXCLUDE(POLL$name[i])
  s5 <- !is.na(v) & keep & gps_ok & !ex & !gps_flagged
  stage$reported[i]    <- sum(!is.na(v))
  stage$after_qc[i]    <- sum(!is.na(v) & keep)
  stage$gps[i]         <- sum(!is.na(v) & keep & gps_ok)
  stage$after_excl[i]  <- sum(!is.na(v) & keep & gps_ok & !ex)
  stage$no_gps_flag[i] <- sum(s5)
  x <- raw[s5, .(Asset, Site, .sec)]
  dl <- DELAY[[POLL$delay[i]]]
  x[, .sec := .sec - ifelse(toupper(Asset) == "CAT", dl[["CAT"]], dl[["EMU"]])]
  x <- unique(x)
  stage$one_per_second[i] <- nrow(x)
  x <- merge(x, grid, by = c("Asset", "Site", ".sec"), all.x = TRUE)
  stage$with_position[i] <- sum(x$has_pos %in% TRUE)
  stage$outside_hq[i]    <- sum(x$has_pos %in% TRUE & !(x$in_hq %in% TRUE))
  # one value per acquisition bin (03, 3d): bins on the delivery clock
  # (delay-corrected second + delay) within laboratory-route-day
  BIN <- c(btex = 1, h2s = 5, hcn = 2)[[POLL$delay[i]]]
  y <- x[has_pos %in% TRUE & !(in_hq %in% TRUE)]
  if (BIN > 1) {
    y[, .day := as.Date(format(as.POSIXct(.sec, origin = "1970-01-01", tz = "MST"), "%Y-%m-%d"))]
    y[, .blk := floor((.sec + ifelse(toupper(Asset) == "CAT", dl[["CAT"]], dl[["EMU"]])) / BIN)]
    stage$one_per_bin[i] <- uniqueN(y, by = c("Asset", "Site", ".day", ".blk"))
  } else stage$one_per_bin[i] <- nrow(y)
}
print(stage)
message("\ncampaign exclusions (03_checks_flags.R):")
message("  aromatics + HCN    : 2023-04-16 .. 2023-09-20 (inlet contamination; H2S retained)")
message("  BTEX               : 2024-08-13, 2024-08-14")
message("  HCN                : 2025-01-02, 2025-01-03, everything <= 2025-01-22,")
message("                       and 2025-05-28..30 (bad sensitivity calibration)")

# ==============================================================
# stage 4 : the analysis set
# ==============================================================
load(file.path(BASE, "mobile_wswd.RData"))   # -> out
d <- as.data.table(out); rm(out)
stopifnot(attr(d$date, "tzone") == "UTC")    # fixed-MST clock, UTC-labelled
d <- d[Site != "Goodrich Corporation (Collins Aerospace)"]
d[, ym := as.integer(format(as.Date(date), "%Y%m"))]
message("\nanalysis set rows        : ", format(nrow(d), big.mark = ","))

fmt  <- function(x, dp = 2) formatC(x, format = "f", digits = dp, big.mark = ",")
res <- data.table(pollutant = POLL$name)
res[, `:=`(reported = stage$reported, after_qc = stage$after_qc,
           gps = stage$gps, after_excl = stage$after_excl,
           no_gps_flag = stage$no_gps_flag, one_per_second = stage$one_per_second,
           with_position = stage$with_position, outside_hq = stage$outside_hq,
           one_per_bin = stage$one_per_bin)]

# Step function on the quarter start month. A measurement before the first
# quarter the packets give for that lab has no audit MDL and is left NA rather
# than back-filled; the count of such rows is reported.
get_mdl <- function(nm, asset, ym) {
  s <- MDL[name == nm & Asset == asset][order(from)]
  if (!nrow(s)) return(rep(NA_real_, length(ym)))
  i <- findInterval(ym, s$from)
  out <- rep(NA_real_, length(ym))
  out[i >= 1L] <- s$mdl[i[i >= 1L]]
  out
}

for (i in seq_len(nrow(POLL))) {
  x  <- d[[POLL$fin[i]]]
  ok <- is.finite(x)
  v  <- x[ok]; a <- d$Asset[ok]; y <- d$ym[ok]
  m  <- rep(NA_real_, length(v))
  for (asset in unique(a)) { j <- a == asset; m[j] <- get_mdl(POLL$name[i], asset, y[j]) }
  q  <- quantile(v, c(0, .05, .25, .5, .75, .95, .99, 1), names = FALSE)
  res[i, `:=`(
    analysis   = length(v),
    n_unique   = uniqueN(v),
    n_no_mdl   = sum(is.na(m)),
    pct_belowMDL = round(100 * mean(v < m, na.rm = TRUE), 1),
    pct_belowMDL_CAT = round(100 * mean(v[a == "CAT"] < m[a == "CAT"], na.rm = TRUE), 1),
    pct_belowMDL_EMU = round(100 * mean(v[a == "EMU"] < m[a == "EMU"], na.rm = TRUE), 1),
    min = q[1], p5 = q[2], p25 = q[3], median = q[4], p75 = q[5],
    p95 = q[6], p99 = q[7], max = q[8],
    mean = mean(v), sd = sd(v),
    first_day = format(min(d$date[ok], na.rm = TRUE), "%Y-%m-%d"),
    last_day  = format(max(d$date[ok], na.rm = TRUE), "%Y-%m-%d"),
    n_days    = uniqueN(as.Date(d$date[ok])))]
  # DELIVERED VALUES (2026-09-27). H2S and HCN are averaged within 5-s / 2-s
  # bins in 03_checks_flags.R, which preserves the mean but lowers peaks and
  # moves the median; the delivered values are kept in *_raw. Report the
  # delivered median / p99 / max beside the bin-mean statistics for those two
  # species. The aromatics are not averaged, so the columns are NA for them.
  rawc <- paste0(POLL$fin[i], "_raw")
  if (rawc %in% names(d)) {
    xr <- d[[rawc]]; xr <- xr[is.finite(xr)]   # every delivered second (not only the one-per-bin rows)
    qr <- quantile(xr, c(.5, .99, 1), names = FALSE)
    res[i, `:=`(n_delivered = length(xr), median_delivered = qr[1],
                p99_delivered = qr[2], max_delivered = qr[3])]
  }
}
for (.c in c("n_delivered", "median_delivered", "p99_delivered", "max_delivered"))
  if (!.c %in% names(res)) res[, (.c) := NA_real_]
if (any(is.finite(res$n_delivered) & res$n_delivered != res$outside_hq))
  stop("delivered seconds differ from the rows outside HQ for some pollutant")

# most common flag tokens actually present, per pollutant
flagtop <- character(nrow(POLL))
for (i in seq_len(nrow(POLL))) {
  f  <- toupper(trimws(as.character(raw[[POLL$flag[i]]]))); f[is.na(f)] <- ""
  tk <- unlist(strsplit(f[nzchar(f)], "[,.;[:space:]]+"))
  tb <- sort(table(tk[nzchar(tk)]), decreasing = TRUE)
  flagtop[i] <- paste(names(tb)[seq_len(min(3, length(tb)))], collapse = ", ")
}
res[, most_common_flags := flagtop]

# the funnel must close: the last stage IS the analysis set
if (!isTRUE(all.equal(res$one_per_bin, res$analysis))) {
  print(res[, .(pollutant, outside_hq, one_per_bin, analysis, diff = analysis - one_per_bin)])
  stop("Table S3.1 funnel does not reproduce the analysis set")
}
message("funnel closes: last stage == analysis set for all six pollutants")
fwrite(res, OUT)
message("\nwrote ", OUT)

cat("\n=================== TABLE S3.1 ===================\n")
lab <- c("Most common flags"                                = "most_common_flags",
         "Reported by CDPHE"                                = "reported",
         "Retained after QA/QC (null qualifiers voided)"    = "after_qc",
         "With valid GPS"                                   = "gps",
         "After campaign date exclusions"                   = "after_excl",
         "Without a GPS qualifier flag"                     = "no_gps_flag",
         "One row per delay-corrected second"               = "one_per_second",
         "With a position at that second"                   = "with_position",
         "Outside 300 m of HQ"                              = "outside_hq",
         "One value per acquisition bin = analysis set"     = "analysis",
         "Sampling days represented"                        = "n_days",
         "  distinct values in the analysis set"            = "n_unique",
         "% of analysis set below audit MDL"                = "pct_belowMDL",
         "  values with no audit MDL published"              = "n_no_mdl",
         "    CAT only"                                     = "pct_belowMDL_CAT",
         "    EMU only"                                     = "pct_belowMDL_EMU",
         "Minimum (ppb)"                                    = "min",
         "5th percentile (ppb)"                             = "p5",
         "25th percentile (ppb)"                            = "p25",
         "Median (ppb)"                                     = "median",
         "75th percentile (ppb)"                            = "p75",
         "95th percentile (ppb)"                            = "p95",
         "99th percentile (ppb)"                            = "p99",
         "Maximum (ppb)"                                    = "max",
         "Median, delivered values (ppb)"                   = "median_delivered",
         "99th percentile, delivered values (ppb)"          = "p99_delivered",
         "Maximum, delivered values (ppb)"                  = "max_delivered")
cat(sprintf("%-46s %12s %12s %12s %16s %12s %12s\n", "", res$pollutant[1], res$pollutant[2],
            res$pollutant[3], res$pollutant[4], res$pollutant[5], res$pollutant[6]))
for (k in names(lab)) {
  col <- lab[[k]]; v <- res[[col]]
  s <- if (is.character(v)) v
       else if (col %in% c("reported","after_qc","gps","after_excl","no_gps_flag","one_per_second","with_position","outside_hq","one_per_bin","analysis","n_unique","n_days","n_no_mdl"))
         format(v, big.mark = ",")
       else if (grepl("^pct", col)) paste0(formatC(v, format = "f", digits = 1), "%")
       else formatC(v, format = "f", digits = 2, big.mark = ",")
  cat(sprintf("%-46s %12s %12s %12s %16s %12s %12s\n", k, s[1], s[2], s[3], s[4], s[5], s[6]))
}
cat(sprintf("%-46s %12s %12s %12s %16s %12s %12s\n", "Mean (sd) (ppb)",
            sprintf("%.2f (%.2f)", res$mean, res$sd)[1], sprintf("%.2f (%.2f)", res$mean, res$sd)[2],
            sprintf("%.2f (%.2f)", res$mean, res$sd)[3], sprintf("%.2f (%.2f)", res$mean, res$sd)[4],
            sprintf("%.2f (%.2f)", res$mean, res$sd)[5], sprintf("%.2f (%.2f)", res$mean, res$sd)[6]))
w <- paste0(res$first_day, " to ", res$last_day)
cat(sprintf("%-46s %12s %12s %12s %16s %12s %12s\n", "Measurement window",
            w[1], w[2], w[3], w[4], w[5], w[6]))
cat("==================================================\n")
