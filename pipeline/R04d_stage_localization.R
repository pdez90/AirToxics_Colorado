# ==============================================================
# R04d_stage_localization.R
# The forensics run (R04c) showed:
#   OLD backup + median metric reproduces the manuscript EXACTLY
#     (1,120 blocks / 83,828 pop / 0.077-0.275 / 0.183-0.650 / 2.37x)
#   NEW rerun gives 1,668 blocks / pw-mean 0.156 ppb / ratio 0.97.
# This script walks the pipeline stage by stage (old backup vs new)
# to find WHERE the mobile benzene values diverge:
#   Stage B: background-corrected 1-s data (sBenzene distribution)
#   Stage D: 500 m segment summaries (matched segment ids)
#   Stage E: census blocks (matched GEOIDs + new-only blocks)
# It also prints file timestamps (provenance: notebook drift check).
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R04d: stage-by-stage localization of the benzene divergence")

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(sf) })

# ----------------------------------------------------------------
# Stage 0: provenance — when were the OLD outputs created?
# ----------------------------------------------------------------
diag_msg("Provenance (mtime of pre-fix backups vs Rmd edit history):")
for (f in c("mobile.RData", "bgcorrected_out_merge.RData", "mobile_corrected.RData",
            "segment500_summaries_clean.RData",
            "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData")) {
  p <- file.path(BACKUP, f)
  if (file.exists(p)) diag_msg(sprintf("  %-55s %s", f, format(file.mtime(p), "%Y-%m-%d %H:%M")))
}
diag_msg("  (Suncor.Rmd itself was last edited 2026-06-14. If old outputs predate later")
diag_msg("   code edits, old-vs-new differences include CODE drift, not only the delay fix.)")

qtiles <- function(x) sprintf("p25 %.3f | p50 %.3f | mean %.3f | p75 %.3f | p95 %.3f | %%NA %.1f",
                              quantile(x, .25, na.rm=TRUE), quantile(x, .5, na.rm=TRUE),
                              mean(x, na.rm=TRUE), quantile(x, .75, na.rm=TRUE),
                              quantile(x, .95, na.rm=TRUE), 100*mean(is.na(x)))

# ----------------------------------------------------------------
# Stage B: background-corrected 1-s sBenzene, old vs new
# ----------------------------------------------------------------
diag_section("Stage B: background-corrected 1-s data (bgcorrected_out_merge)")
for (v in c("OLD", "NEW")) {
  f <- file.path(ifelse(v == "OLD", BACKUP, BASE), "bgcorrected_out_merge.RData")
  e <- new.env(); load(f, envir = e)
  d <- as.data.table(get(ls(e)[1], envir = e)); rm(e)
  scols <- grep("^sBenzene$|^sHydrogen|^sH2S$|^sToluene$", names(d), value = TRUE)
  diag_msg("  [", v, "] rows: ", format(nrow(d), big.mark=","), " | s-columns: ", paste(scols, collapse=", "))
  for (cc in intersect(c("sBenzene", "sToluene", "sH2S"), names(d)))
    diag_msg(sprintf("  [%s] %-10s %s", v, cc, qtiles(d[[cc]])))
  # raw benzene too (should be identical old vs new):
  if ("Benzene_ppb" %in% names(d))
    diag_msg(sprintf("  [%s] %-10s %s", v, "rawBenz", qtiles(d$Benzene_ppb)))
  rm(d); invisible(gc())
}
diag_msg("  VERDICT B: if sBenzene p50/mean differ substantially old vs new while rawBenz")
diag_msg("  matches, the divergence enters at the BACKGROUND-CORRECTION stage (scripts 10-11).")

# ----------------------------------------------------------------
# Stage D: 500 m segment summaries, matched segments
# ----------------------------------------------------------------
diag_section("Stage D: segment summaries (matched segment ids)")
seg_get <- function(v) {
  f <- file.path(ifelse(v == "OLD", BACKUP, BASE), "segment500_summaries_clean.RData")
  e <- new.env(); load(f, envir = e)
  # prefer the long data.table
  nm <- intersect(c("seg_long_dt"), ls(e)); if (!length(nm)) nm <- ls(e)[1]
  d <- as.data.table(get(nm, envir = e))
  d
}
so <- seg_get("OLD"); sn <- seg_get("NEW")
diag_msg("  old cols: ", paste(head(names(so), 12), collapse=", "))
pol_col <- intersect(c("pollutant"), names(so))[1]
if (!is.na(pol_col)) {
  bo <- so[grepl("benzene", get(pol_col), ignore.case=TRUE) & !grepl("toluene|xylene|trimethyl", get(pol_col), ignore.case=TRUE)]
  bn <- sn[grepl("benzene", get(pol_col), ignore.case=TRUE) & !grepl("toluene|xylene|trimethyl", get(pol_col), ignore.case=TRUE)]
  vcol <- intersect(c("median_of_daily_medians", "median", "value"), names(bo))[1]
  idc <- intersect(c("id"), names(bo))[1]
  if (!is.na(vcol) && !is.na(idc)) {
    if ("stat" %in% names(bo)) { bo <- bo[stat == "median_of_daily_medians"]; bn <- bn[stat == "median_of_daily_medians"]; vcol <- "value" }
    diag_msg(sprintf("  segments with benzene: old %d | new %d", uniqueN(bo[[idc]]), uniqueN(bn[[idc]])))
    m <- merge(bo[, .(id = get(idc), old = get(vcol))][, .(old = median(old, na.rm=TRUE)), by=id],
               bn[, .(id = get(idc), new = get(vcol))][, .(new = median(new, na.rm=TRUE)), by=id], by = "id")
    diag_msg(sprintf("  matched segments: %d | cor(old,new) = %.3f | median(new-old) = %.4f ppb",
                     nrow(m), suppressWarnings(cor(m$old, m$new, use="complete.obs")),
                     median(m$new - m$old, na.rm=TRUE)))
    diag_msg(sprintf("  old matched: %s", qtiles(m$old)))
    diag_msg(sprintf("  new matched: %s", qtiles(m$new)))
  } else diag_msg("  [WARN] could not identify value/id columns: ", paste(names(bo), collapse=", "))
} else diag_msg("  [WARN] no 'pollutant' column; cols: ", paste(names(so), collapse=", "))
rm(so, sn); invisible(gc())
diag_msg("  VERDICT D: if matched segments correlate ~1 with median diff ~0, segments are fine")
diag_msg("  and the divergence enters at the BLOCK stage; if they shifted, it is upstream.")

# ----------------------------------------------------------------
# Stage E: census blocks — matched GEOIDs and new-only blocks
# ----------------------------------------------------------------
diag_section("Stage E: blocks (matched GEOIDs)")
blk_get <- function(v) {
  f <- file.path(ifelse(v == "OLD", BACKUP, BASE), "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData")
  e <- new.env(); load(f, envir = e)
  ob <- ls(e)[sapply(ls(e), function(n) inherits(get(n, envir=e), "sf"))][1]
  b <- as.data.table(sf::st_drop_geometry(get(ob, envir=e)))
  b[, GEOID20 := as.character(GEOID20)]
  b
}
bo <- blk_get("OLD"); bn <- blk_get("NEW")
vcol <- "sBenzene_med_of_daily_med"
bo2 <- bo[is.finite(get(vcol)), .(GEOID20, old = get(vcol))]
bn2 <- bn[is.finite(get(vcol)), .(GEOID20, new = get(vcol))]
diag_msg(sprintf("  blocks with finite %s: old %d | new %d", vcol, nrow(bo2), nrow(bn2)))
m <- merge(bo2, bn2, by = "GEOID20")
only_new <- bn2[!GEOID20 %in% bo2$GEOID20]
only_old <- bo2[!GEOID20 %in% bn2$GEOID20]
diag_msg(sprintf("  matched: %d | new-only: %d | old-only: %d", nrow(m), nrow(only_new), nrow(only_old)))
diag_msg(sprintf("  matched blocks OLD values: %s", qtiles(m$old)))
diag_msg(sprintf("  matched blocks NEW values: %s", qtiles(m$new)))
diag_msg(sprintf("  cor(old,new) matched = %.3f | median(new-old) = %.4f ppb | median ratio new/old = %.3f",
                 suppressWarnings(cor(m$old, m$new, use="complete.obs")),
                 median(m$new - m$old, na.rm=TRUE),
                 median(m$new / m$old, na.rm=TRUE)))
diag_msg(sprintf("  NEW-ONLY blocks values:    %s", qtiles(only_new$new)))

diag_section("R04d: overall verdict guide")
diag_msg("Read bottom-up:")
diag_msg(" 1. Stage B differs         -> background correction (or upstream met/join retention)")
diag_msg("    changed between the Feb-2026 run and today's code. Check whether scripts 10-11")
diag_msg("    match what produced the old file (Rmd evolved until 2026-06-14).")
diag_msg(" 2. B same, D shifted       -> segment aggregation changed (scripts 13-14).")
diag_msg(" 3. B+D same, E shifted     -> block stage (script 18: block universe, join, filters).")
diag_msg(" 4. Everything correlates ~1 with tiny diffs, but block COUNT grew a lot")
diag_msg("    -> geography reshuffling from the corrected GPS-time alignment; then the ms")
diag_msg("       risk change is real and delay-driven, and the paper numbers must be updated.")
diag_msg("If (1)-(3): consider a CONTROL RUN — current code with OLD delays (edit .asset_delay")
diag_msg("to 6.5/7.5/9 for both assets, rerun R01->R04b) to cleanly separate code drift from")
diag_msg("the delay effect before rewriting any manuscript numbers.")
