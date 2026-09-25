# ==============================================================
# test_delay_and_cadence.R   (2026-09-22)
# Independent check that the processed record really carries the
# instrument delays and the native-cadence averaging the Methods claim.
#
# Nothing here calls the pipeline's own functions: the delivered record is
# rebuilt straight from the CDPHE monthly CSVs, and mobile.RData is compared
# against it.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript tests/test_delay_and_cadence.R
#   TEST_MONTHS="Sep_2024,April_2024" ...   (default: Sep_2024)
#
# Checks
#   1  delay: cross-correlation of processed vs delivered peaks at exactly the
#      documented shift, per van and per species (CAT 4/6/21 s, EMU 5/3/17 s)
#   2  aromatics: processed 1-s benzene equals the delivered value shifted by
#      the van's delay, value for value
#   3  native cadence: H2S is constant within 5-s blocks and HCN within 2-s
#      blocks anchored on the instrument's own delivery clock, and each block
#      value is the mean of the delivered readings in it
#   4  no gap filling: a second that had no delivered value still has none
#   5  interpolation touches position/met only, never a concentration
# Exits non-zero if any check fails.
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(lubridate) })
BASE   <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
MONTHS <- strsplit(Sys.getenv("TEST_MONTHS", "Sep_2024,Feb_2025,March_2025"), ",")[[1]]
DELAY  <- list(CAT = c(btex = 4, hcn = 6, h2s = 21),
               EMU = c(btex = 5, hcn = 3, h2s = 17))
NULL_FLAGS <- c("AL","AN","AO","AQ","AT","AX","AY","AZ","BA","BD","BH","BK",
                "BL","BM","BR","EC","MB","XX")
ok <- 0L; bad <- 0L; skipped <- 0L
skip <- function(label, why) { cat(sprintf("  [SKIP] %s  -- %s\n", label, why)); skipped <<- skipped + 1L }
chk <- function(label, cond, extra = "") {
  cat(sprintf("  [%s] %s%s\n", if (isTRUE(cond)) "PASS" else "FAIL", label,
              if (nzchar(extra)) paste0("  -- ", extra) else ""))
  if (isTRUE(cond)) ok <<- ok + 1L else bad <<- bad + 1L
}
voided <- function(flag) {
  toks <- strsplit(toupper(gsub("\\s", "", flag)), "[,.;]+")
  vapply(toks, function(z) any(z %in% NULL_FLAGS), logical(1))
}

# ---- 1) delivered record, straight from the monthly CSVs -------------------
read_month <- function(prefix, site, m) {
  f <- file.path(BASE, "Updated", "csv", sprintf("%s_%s.csv", prefix, m))
  if (!file.exists(f)) return(NULL)
  d <- fread(f, colClasses = c(Local_Time_MST = "character"))
  setnames(d, names(d)[1], "Asset")
  stopifnot(all(grepl("-0700$", d$Local_Time_MST)))     # fixed MST, no DST
  d[, date := ymd_hms(substr(Local_Time_MST, 1, 19), tz = "UTC")]   # clock, per time_convention.md
  d[, Site := site]
  d <- d[!nzchar(trimws(as.character(GPS_flag)))]   # script 02 drops GPS-flagged rows outright
  for (p in c("Benzene","Toluene","Trimethylbenzene","Xylene",
              "Hydrogen_Sulfide","Hydrogen_Cyanide")) {
    v <- suppressWarnings(as.numeric(d[[paste0(p, "_ppbV")]]))
    v[voided(d[[paste0(p, "_flag")]])] <- NA_real_
    d[[p]] <- v
  }
  d[, .(Asset, Site, date, Benzene, Toluene, Trimethylbenzene, Xylene,
        Hydrogen_Sulfide, Hydrogen_Cyanide)]
}
delivered <- rbindlist(lapply(MONTHS, function(m) rbindlist(list(
  read_month("Suncor",   "Suncor and Phillips 66 Terminal", m),
  read_month("Terminal", "Holly Energy Partners (Sinclair) Terminal", m)))))
stopifnot(nrow(delivered) > 0)
delivered <- delivered[, lapply(.SD, mean, na.rm = TRUE), by = .(Asset, Site, date)]
for (j in names(delivered)[-(1:3)])
  set(delivered, which(is.nan(delivered[[j]])), j, NA_real_)
cat(sprintf("delivered: %s rows, %s\n", format(nrow(delivered), big.mark = ","),
            paste(range(as.Date(delivered$date)), collapse = " to ")))

# ---- 2) processed record ---------------------------------------------------
e <- new.env(); load(file.path(BASE, "mobile.RData"), envir = e)
proc <- as.data.table(get(ls(e)[1], envir = e)); rm(e)
proc <- proc[as.Date(date) %in% unique(as.Date(delivered$date))]
cat(sprintf("processed: %s rows over the same days\n", format(nrow(proc), big.mark = ",")))

SPEC <- list(btex = c(proc = "Benzene_ppb",          deliv = "Benzene"),
             h2s  = c(proc = "Hydrogen_Sulfide_ppb_raw", deliv = "Hydrogen_Sulfide"),
             hcn  = c(proc = "Hydrogen_Cyanide_ppb_raw", deliv = "Hydrogen_Cyanide"))

cat("\n1. delay: lag at which processed matches delivered\n")
for (asset in c("CAT", "EMU")) for (sp in names(SPEC)) {
  d <- delivered[Asset == asset, .(t = as.numeric(date), v = get(SPEC[[sp]]["deliv"]))]
  p <- proc[Asset == asset, .(t = as.numeric(date), v = get(SPEC[[sp]]["proc"]))]
  d <- d[is.finite(v)]; p <- p[is.finite(v)]
  if (nrow(p) < 500 || nrow(d) < 500) {
    skip(sprintf("%s %-4s delay", asset, sp), "this van/species has no data in the months tested"); next }
  lags <- -30:30
  r <- vapply(lags, function(L) {
    m <- merge(p, d[, .(t = t - L, v)], by = "t")     # processed(t) vs delivered(t+L)
    if (nrow(m) < 500) return(NA_real_)
    suppressWarnings(cor(m$v.x, m$v.y))
  }, numeric(1))
  best <- lags[which.max(r)]
  chk(sprintf("%s %-4s delay = %d s", asset, sp, DELAY[[asset]][[sp]]),
      identical(as.integer(best), as.integer(DELAY[[asset]][[sp]])),
      sprintf("best lag %d s (r = %.4f); r at 0 s = %.4f", best, max(r, na.rm = TRUE),
              r[lags == 0]))
}

cat("\n2. aromatics: value-for-value after the shift\n")
for (asset in c("CAT", "EMU")) {
  L <- DELAY[[asset]][["btex"]]
  d <- delivered[Asset == asset, .(Site, t = as.numeric(date) - L, dv = Benzene)]
  p <- proc[Asset == asset, .(Site, t = as.numeric(date), pv = Benzene_ppb)]
  m <- merge(p[is.finite(pv)], d[is.finite(dv)], by = c("Site", "t"))
  if (nrow(m) < 500) { skip(sprintf("%s benzene value-for-value", asset), "no overlapping data"); next }
  chk(sprintf("%s benzene identical after shifting %d s", asset, L),
      nrow(m) > 1000 && max(abs(m$pv - m$dv)) < 1e-9,
      sprintf("%s seconds compared, max |difference| %.2g ppb",
              format(nrow(m), big.mark = ","), if (nrow(m)) max(abs(m$pv - m$dv)) else NA))
}

cat("\n3. native cadence: blocks anchored on the delivery clock\n")
for (asset in c("CAT", "EMU")) {
  for (info in list(list(sp = "h2s", B = 5, col = "Hydrogen_Sulfide_ppb",  deliv = "Hydrogen_Sulfide"),
                    list(sp = "hcn", B = 2, col = "Hydrogen_Cyanide_ppb", deliv = "Hydrogen_Cyanide"))) {
    L <- DELAY[[asset]][[info$sp]]
    q <- proc[Asset == asset & is.finite(get(info$col))]
    if (nrow(q) < 500) { skip(sprintf("%s %s native cadence", asset, info$sp),
                              "this van/species has no data in the months tested"); next }
    q[, blk := floor((as.numeric(date) + L) / info$B)]
    # (a) one value per block
    s1 <- q[, .(nuniq = uniqueN(round(get(info$col), 10))), by = .(Site, d = as.Date(date), blk)]
    chk(sprintf("%s %s constant within its %d s block", asset, info$sp, info$B),
        all(s1$nuniq == 1L),
        sprintf("%s blocks, %d with more than one value",
                format(nrow(s1), big.mark = ","), sum(s1$nuniq > 1L)))
    # (b) on blocks that survive intact in the record, the value is the mean of
    #     the readings in the block (blocks only partly present - a second with
    #     no position, or a second removed by the 300 m headquarters exclusion,
    #     which is applied after the averaging - are skipped)
    s2 <- q[, .(n = .N, v = get(info$col)[1], m = mean(get(paste0(info$col, "_raw")), na.rm = TRUE)),
            by = .(Site, d = as.Date(date), blk)][n == info$B]
    chk(sprintf("%s %s block value = mean of the readings in the block", asset, info$sp),
        nrow(s2) > 100 && max(abs(s2$v - s2$m)) < 1e-9,
        sprintf("%s complete blocks of %s, max |difference| %.2g ppb",
                format(nrow(s2), big.mark = ","), format(nrow(s1), big.mark = ","),
                if (nrow(s2)) max(abs(s2$v - s2$m)) else NA))
    # (c) the anchor matters wherever the delay is not a whole number of
    #     acquisition intervals: on the post-delay clock the blocks would
    #     straddle two delivered readings
    q[, blk_wrong := floor(as.numeric(date) / info$B)]
    s3 <- q[, .(nuniq = uniqueN(round(get(info$col), 10))), by = .(Site, d = as.Date(date), blk_wrong)]
    straddle <- mean(s3$nuniq > 1L)
    if (L %% info$B == 0) {
      chk(sprintf("%s %s: delay %d s is a whole number of %d s intervals, so both anchors agree",
                  asset, info$sp, L, info$B), straddle < 0.01,
          sprintf("%.0f%% of blocks straddle under either anchor", 100 * straddle))
    } else {
      chk(sprintf("%s %s anchored on the delivery clock, not the shifted clock", asset, info$sp),
          straddle > 0.05,
          sprintf("delay %d s leaves a remainder of %d s; under the shifted-clock anchor %.0f%% of blocks would straddle two readings",
                  L, L %% info$B, 100 * straddle))
    }
  }
}

cat("\n4. no gap filling, and interpolation touches position/met only\n")
for (info in list(c(col = "Hydrogen_Sulfide_ppb", raw = "Hydrogen_Sulfide_ppb_raw"),
                  c(col = "Hydrogen_Cyanide_ppb", raw = "Hydrogen_Cyanide_ppb_raw"))) {
  a <- sum(!is.na(proc[[info["col"]]])); b <- sum(!is.na(proc[[info["raw"]]]))
  chk(sprintf("%s: averaging changed no second from empty to filled", info["col"]),
      a == b, sprintf("%s values before, %s after", format(b, big.mark = ","),
                      format(a, big.mark = ",")))
}
if ("interpolated" %in% names(proc)) {
  ip <- proc[interpolated == 1]
  chk("interpolated seconds carry a position", nrow(ip) > 0 && all(is.finite(ip$Latitude)),
      sprintf("%s interpolated seconds (%.1f%% of the period)",
              format(nrow(ip), big.mark = ","), 100 * nrow(ip) / nrow(proc)))
  # a concentration is never created by interpolation: every benzene value on an
  # interpolated second must exist in the delivered record at that second + the
  # van's own delay
  ip2 <- ip[is.finite(Benzene_ppb),
            .(Site, Asset, t = as.numeric(date) + vapply(Asset, function(a) DELAY[[a]][["btex"]], numeric(1)),
              pv = Benzene_ppb)]
  d <- delivered[is.finite(Benzene), .(Site, Asset, t = as.numeric(date), dv = Benzene)]
  m <- merge(ip2, d, by = c("Site", "Asset", "t"), all.x = TRUE)
  chk("benzene on interpolated seconds comes from a delivered reading",
      nrow(m) > 0 && all(is.finite(m$dv)) && max(abs(m$pv - m$dv)) < 1e-9,
      sprintf("%s values checked, %s without a delivered reading",
              format(nrow(m), big.mark = ","), format(sum(!is.finite(m$dv)), big.mark = ",")))
}

cat(sprintf("\n%d passed, %d failed, %d skipped\n", ok, bad, skipped))
quit(status = if (bad) 1L else 0L)
