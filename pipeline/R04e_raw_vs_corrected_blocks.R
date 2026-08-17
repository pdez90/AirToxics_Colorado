# ==============================================================
# R04e_raw_vs_corrected_blocks.R
# HYPOTHESIS TEST: the Feb-2026 block file (which reproduces the
# manuscript's 2.4x) aggregated RAW benzene, while current script 18
# feeds background-corrected sBenzene. Evidence so far: matched
# blocks differ by ~ -0.200 ppb (an additive, background-sized
# offset); 1-s data and segments are identical old vs new.
#
# This script, from the NEW (corrected-delay) point data:
#   1. re-aggregates block-level med-of-daily-med using BOTH
#      raw Benzene_ppb and sBenzene
#   2. tests which one reproduces the OLD block values (matched GEOIDs)
#   3. computes the AirToxScreen risk comparison with the RAW metric
#      (scaled 1.149405) and with the sBenzene metric, side by side
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R04e: raw vs background-corrected benzene at the block stage")

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(data.table); library(tigris)
})
options(tigris_use_cache = TRUE)

SCALE_BENZ <- 1.149405   # bin-weighted La Casa scaling factor (new run)

# ----------------------------------------------------------------
# 1) Load NEW corrected points and re-aggregate blocks both ways
#    (mobile_corrected.RData drops raw Benzene_ppb, so build points
#     from bgcorrected_out_merge.RData, which has BOTH raw and s*)
# ----------------------------------------------------------------
e0 <- new.env(); load(file.path(BASE, "bgcorrected_out_merge.RData"), envir = e0)
d0 <- as.data.frame(get(ls(e0)[1], envir = e0)); rm(e0)   # plain df: avoids data.table j-semantics
# raw column is renamed Benzene_ppb -> Benzene by script 10
raw_col <- intersect(c("Benzene", "Benzene_ppb"), names(d0))[1]
diag_msg("bgcorrected rows: ", format(nrow(d0), big.mark = ","),
         " | raw column: ", raw_col,
         " | has sBenzene: ", "sBenzene" %in% names(d0))
stopifnot(!is.na(raw_col), "sBenzene" %in% names(d0),
          all(c("Latitude", "Longitude", "date") %in% names(d0)))
d0 <- d0[!is.na(d0$Latitude) & !is.na(d0$Longitude),
         c("date", "Latitude", "Longitude", raw_col, "sBenzene")]
names(d0)[names(d0) == raw_col] <- "Benzene_ppb"
pts <- st_as_sf(d0, coords = c("Longitude", "Latitude"), crs = 4326)
rm(d0); invisible(gc())
pts$day <- as.Date(pts$date)

blocks <- tigris::blocks(state = "08", year = 2020, class = "sf")
blocks <- st_transform(blocks, 4326)
diag_msg("blocks universe: ", format(nrow(blocks), big.mark = ","))

t0 <- Sys.time()
ji <- st_join(pts[, c("day", "Benzene_ppb", "sBenzene")], blocks[, "GEOID20"], join = st_within)
diag_msg("point->block join done in ", round(difftime(Sys.time(), t0, units = "secs")), " s")
dt <- as.data.table(st_drop_geometry(ji))[!is.na(GEOID20)]

daily <- dt[, .(raw_daily = median(Benzene_ppb, na.rm = TRUE),
                cor_daily = median(sBenzene,   na.rm = TRUE)), by = .(GEOID20, day)]
blk <- daily[, .(raw_mdm = median(raw_daily, na.rm = TRUE),
                 cor_mdm = median(cor_daily, na.rm = TRUE),
                 n_days  = uniqueN(day)), by = GEOID20]
blk <- blk[is.finite(raw_mdm) | is.finite(cor_mdm)]
diag_msg("blocks with any benzene aggregate (new points): ", nrow(blk))

# ----------------------------------------------------------------
# 2) Which metric reproduces the OLD block values?
# ----------------------------------------------------------------
diag_section("R04e-TEST: matched-block comparison vs OLD (Feb-2026) file")
e <- new.env(); load(file.path(BACKUP, "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"), envir = e)
ob <- ls(e)[sapply(ls(e), function(n) inherits(get(n, envir = e), "sf"))][1]
old <- as.data.table(st_drop_geometry(get(ob, envir = e)))[, .(GEOID20 = as.character(GEOID20),
                                                               old_val = sBenzene_med_of_daily_med)]
old <- old[is.finite(old_val)]
m <- merge(old, blk, by = "GEOID20")
diag_msg("matched blocks: ", nrow(m), " of ", nrow(old), " old")
for (cand in c("raw_mdm", "cor_mdm")) {
  cc <- suppressWarnings(cor(m$old_val, m[[cand]], use = "complete.obs"))
  md <- median(m[[cand]] - m$old_val, na.rm = TRUE)
  mr <- median(m[[cand]] / m$old_val, na.rm = TRUE)
  frac_close <- mean(abs(m[[cand]] - m$old_val) <= 0.05, na.rm = TRUE)
  diag_msg(sprintf("  old vs %s : cor %.3f | median diff %+.4f ppb | median ratio %.3f | within +-0.05 ppb: %.0f%%",
                   cand, cc, md, mr, 100 * frac_close))
}
diag_msg("  --> whichever candidate has cor ~1, diff ~0 is what the Feb block run used.")

# ----------------------------------------------------------------
# 3) Risk table both ways (common blocks with AirToxScreen + pop)
# ----------------------------------------------------------------
diag_section("R04e-RISK: risk comparison with raw vs corrected metric (NEW data)")
# airtox benzene per block from the new block file
e2 <- new.env(); load(file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"), envir = e2)
ob2 <- ls(e2)[sapply(ls(e2), function(n) inherits(get(n, envir = e2), "sf"))][1]
atx <- as.data.table(st_drop_geometry(get(ob2, envir = e2)))[, .(GEOID20 = as.character(GEOID20),
                                                                 airtox_ppb = benzene_ppb)]
atx <- unique(atx[is.finite(airtox_ppb)], by = "GEOID20")
# population from airtoxscreen.csv
hdr <- names(fread(file.path(BASE, "airtoxscreen.csv"), nrows = 0))
idc <- grep("^Block$|GEOID", hdr, ignore.case = TRUE, value = TRUE)[1]
pcol <- grep("population", hdr, ignore.case = TRUE, value = TRUE)[1]
pop <- fread(file.path(BASE, "airtoxscreen.csv"), select = c(idc, pcol), colClasses = "character")
setnames(pop, c("GEOID20", "population"))
pop[, GEOID20 := gsub(" ", "0", sprintf("%015s", gsub("\\D", "", GEOID20)))]
pop[, population := suppressWarnings(as.numeric(population))]
pop <- unique(pop, by = "GEOID20")

risk_line <- function(mob_col, label) {
  d <- merge(merge(blk[is.finite(get(mob_col)), .(GEOID20, mob = get(mob_col) * SCALE_BENZ)],
                   atx, by = "GEOID20"), pop, by = "GEOID20")
  d <- d[is.finite(population) & population > 0]
  s_at <- d[, sum(airtox_ppb * population)]; s_mo <- d[, sum(mob * population)]
  diag_msg(sprintf("  %-28s n=%4d | pop=%7s | AT pw %.3f (%.3f-%.3f) | MOB pw %.3f (%.3f-%.3f) | ratio %.2f",
                   label, nrow(d), format(sum(d$population), big.mark = ","),
                   s_at / sum(d$population), 5.75 * s_at / 1e6, 20.40 * s_at / 1e6,
                   s_mo / sum(d$population), 5.75 * s_mo / 1e6, 20.40 * s_mo / 1e6,
                   s_mo / s_at))
}
risk_line("raw_mdm", "RAW (total conc), scaled")
risk_line("cor_mdm", "BG-CORRECTED, scaled")
diag_msg("")
diag_msg("Manuscript targets: AT 0.077-0.275 | mobile 0.183-0.650 | ratio 2.4x (1,120 blocks, 83,828 pop)")
diag_msg("If RAW reproduces old matched values AND its ratio lands near ~2x, conclusion:")
diag_msg("  - Feb-2026 block run (the paper) used RAW total benzene; the current script 18")
diag_msg("    feeds bg-corrected sBenzene (code drift, not the delay fix).")
diag_msg("  - For risk vs AirToxScreen, RAW total concentration is the defensible comparator")
diag_msg("    (AirToxScreen models total ambient conc; IUR applies to total exposure).")
diag_msg("  - Report the RAW-based risk (updated for corrected delays) in the manuscript;")
diag_msg("    report the bg-corrected version as a sensitivity, and state the choice in Methods.")
diag_msg("  - Block-count growth (1,120 -> ~1,668) still needs a decision: consider a minimum")
diag_msg("    n_days per block (e.g., >=2) stated explicitly in Methods.")
