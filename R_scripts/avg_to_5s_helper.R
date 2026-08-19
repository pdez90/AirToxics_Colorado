# ==============================================================
# avg_to_5s_helper.R  --  common-cadence averaging for
# MULTI-POLLUTANT, point-level analyses
# ==============================================================
# Use this ONLY where an analysis compares or combines pollutant
# VALUES at the same instant (e.g. pairwise correlations across
# species, pollutant ratios that involve a slow species, or a
# benzene-vs-H2S co-plume check). It averages every listed pollutant
# to the common 5 s (the longest instrument interval) within each
# Asset-Site-day, so no species is compared at a finer resolution
# than it truly has.
#
# Single-pollutant maps/hotspots do NOT need this - they already use
# the species-native cadence set in 03_checks_flags.R (H2S 5 s,
# HCN 2 s, aromatics 1 s).
#
# Usage:
#   source("avg_to_5s_helper.R")
#   df5 <- avg_to_5s(df, cols = c("Benzene_ppb","Hydrogen_Sulfide_ppb"))
# `df` must contain: Asset, Site, date (POSIXct), and the named cols.
# Returns df with those columns replaced by their 5-s block means.
# ==============================================================
avg_to_5s <- function(df, cols, interval_s = 5) {
  stopifnot(all(c("Asset", "Site", "date") %in% names(df)))
  stopifnot(all(cols %in% names(df)))
  is_dt <- inherits(df, "data.table")
  if (is_dt) df <- as.data.frame(df)
  .avg <- function(x) { m <- mean(x, na.rm = TRUE); if (is.nan(m)) NA_real_ else m }
  df$.day <- as.Date(df$date)
  df$.blk <- floor(as.numeric(df$date) / interval_s)
  key <- paste(df$Asset, df$Site, df$.day, df$.blk)
  for (cc in cols) {
    df[[cc]] <- ave(df[[cc]], key, FUN = .avg)
  }
  df$.day <- NULL; df$.blk <- NULL
  if (is_dt) data.table::as.data.table(df) else df
}
