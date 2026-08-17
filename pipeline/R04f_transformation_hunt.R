# ==============================================================
# R04f_transformation_hunt.R
# R04e ruled out both plain raw and plain sBenzene aggregation.
# Since the 1-s data are IDENTICAL old vs new (Stage B), the Feb
# block run must have used a different AGGREGATION/TRANSFORMATION.
# Strategy: apply candidate transformations to the OLD backup 1-s
# data and compare against the OLD block file — same underlying
# data, so the true transformation should match near-exactly.
# Then apply the winner to the NEW data for updated risk numbers.
#
# Candidates (block metric = median over days unless stated):
#   1  sBz_med_dmed        median(daily median sBenzene)            [known: cor .23]
#   2  raw_med_dmed        median(daily median raw)                 [known: cor .34]
#   3  sBz_pos_med_dmed    daily medians over sBenzene > 0 only
#   4  raw_pos_med_dmed    daily medians over raw > 0 only
#   5  sBz_plus_bg         median(daily median of sBenzene + median_bgBenzene)
#   6  sBz_mean_dmed       mean(daily median sBenzene)
#   7  sBz_med_dmean       median(daily mean sBenzene)
#   8  sBz_mean_dmean      mean(daily mean sBenzene)
#   9  raw_mean_dmean      mean(daily mean raw)
#  10  sBz_pos_mean_dmean  mean(daily mean of sBenzene > 0 only)
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R04f: transformation hunt on OLD data (exact-match test)")

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(data.table); library(tigris)
})
options(tigris_use_cache = TRUE)

blocks <- tigris::blocks(state = "08", year = 2020, class = "sf") |> st_transform(4326)

build_candidates <- function(bg_file, label) {
  e0 <- new.env(); load(bg_file, envir = e0)
  d0 <- as.data.frame(get(ls(e0)[1], envir = e0)); rm(e0)
  raw_col <- intersect(c("Benzene", "Benzene_ppb"), names(d0))[1]
  has_bgcol <- "median_bgBenzene" %in% names(d0)
  keep <- c("date", "Latitude", "Longitude", raw_col, "sBenzene",
            if (has_bgcol) "median_bgBenzene")
  d0 <- d0[!is.na(d0$Latitude) & !is.na(d0$Longitude), keep]
  names(d0)[names(d0) == raw_col] <- "raw"
  diag_msg("[", label, "] points: ", format(nrow(d0), big.mark = ","),
           " | median_bgBenzene present: ", has_bgcol)
  pts <- st_as_sf(d0, coords = c("Longitude", "Latitude"), crs = 4326)
  rm(d0); invisible(gc())
  pts$day <- as.Date(pts$date)
  ji <- st_join(pts, blocks[, "GEOID20"], join = st_within)
  dt <- as.data.table(st_drop_geometry(ji))[!is.na(GEOID20)]
  rm(pts, ji); invisible(gc())

  posmed <- function(x) { x <- x[is.finite(x) & x > 0]; if (!length(x)) NA_real_ else median(x) }
  posmean <- function(x) { x <- x[is.finite(x) & x > 0]; if (!length(x)) NA_real_ else mean(x) }

  daily <- dt[, .(
    dmed_s    = median(sBenzene, na.rm = TRUE),
    dmed_r    = median(raw,      na.rm = TRUE),
    dmed_sp   = posmed(sBenzene),
    dmed_rp   = posmed(raw),
    dmed_sbg  = if ("median_bgBenzene" %in% names(dt))
                  median(sBenzene + median_bgBenzene, na.rm = TRUE) else NA_real_,
    dmean_s   = mean(sBenzene, na.rm = TRUE),
    dmean_r   = mean(raw,      na.rm = TRUE),
    dmean_sp  = posmean(sBenzene)
  ), by = .(GEOID20, day)]

  blk <- daily[, .(
    sBz_med_dmed       = median(dmed_s,   na.rm = TRUE),
    raw_med_dmed       = median(dmed_r,   na.rm = TRUE),
    sBz_pos_med_dmed   = median(dmed_sp,  na.rm = TRUE),
    raw_pos_med_dmed   = median(dmed_rp,  na.rm = TRUE),
    sBz_plus_bg        = median(dmed_sbg, na.rm = TRUE),
    sBz_mean_dmed      = mean(dmed_s,     na.rm = TRUE),
    sBz_med_dmean      = median(dmean_s,  na.rm = TRUE),
    sBz_mean_dmean     = mean(dmean_s,    na.rm = TRUE),
    raw_mean_dmean     = mean(dmean_r,    na.rm = TRUE),
    sBz_pos_mean_dmean = mean(dmean_sp,   na.rm = TRUE),
    n_days             = uniqueN(day)
  ), by = GEOID20]
  blk
}

# ---- OLD data -> candidates ----
blk_old <- build_candidates(file.path(BACKUP, "bgcorrected_out_merge.RData"), "OLD")

# ---- OLD block file (target) ----
e <- new.env(); load(file.path(BACKUP, "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"), envir = e)
ob <- ls(e)[sapply(ls(e), function(n) inherits(get(n, envir = e), "sf"))][1]
tgt <- as.data.table(st_drop_geometry(get(ob, envir = e)))[, .(GEOID20 = as.character(GEOID20),
                                                               target = sBenzene_med_of_daily_med)]
tgt <- tgt[is.finite(target)]
diag_msg("target: ", nrow(tgt), " old blocks with finite values")

m <- merge(tgt, blk_old, by = "GEOID20")
diag_section("R04f-RANKING: candidate vs OLD block values (same underlying data)")
cands <- setdiff(names(blk_old), c("GEOID20", "n_days"))
rank_tab <- rbindlist(lapply(cands, function(cc) {
  x <- m[[cc]]
  data.table(candidate = cc,
             n_finite  = sum(is.finite(x)),
             cor       = suppressWarnings(cor(m$target, x, use = "complete.obs")),
             med_diff  = median(x - m$target, na.rm = TRUE),
             med_ratio = median(x / m$target, na.rm = TRUE),
             pct_within_0p05 = 100 * mean(abs(x - m$target) <= 0.05, na.rm = TRUE))
}))[order(-pct_within_0p05)]
txt <- capture.output(print(rank_tab, digits = 3))
for (l in txt) diag_msg("  ", l)
fwrite(rank_tab, file.path(PIPE, "transformation_hunt_ranking.csv"))

best <- rank_tab$candidate[1]
diag_msg("\nBest candidate: ", best,
         "  (exact match expected: cor > 0.98 and >90% within +-0.05 ppb)")
if (rank_tab$pct_within_0p05[1] < 80) {
  diag_msg("[WARN] no candidate matches well — the Feb run's transformation is still")
  diag_msg("       unidentified. Next leads: per-VISIT (run) medians instead of daily;")
  diag_msg("       aggregation before/after scaling; or an entirely different input file.")
}

# ----------------------------------------------------------------
# Apply best candidate to NEW data -> updated risk numbers
# ----------------------------------------------------------------
diag_section("R04f-NEW: best candidate applied to corrected-delay data")
blk_new <- build_candidates(file.path(BASE, "bgcorrected_out_merge.RData"), "NEW")

# airtox + population
e2 <- new.env(); load(file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"), envir = e2)
ob2 <- ls(e2)[sapply(ls(e2), function(n) inherits(get(n, envir = e2), "sf"))][1]
atx <- as.data.table(st_drop_geometry(get(ob2, envir = e2)))[, .(GEOID20 = as.character(GEOID20),
                                                                 airtox_ppb = benzene_ppb)]
atx <- unique(atx[is.finite(airtox_ppb)], by = "GEOID20")
hdr <- names(fread(file.path(BASE, "airtoxscreen.csv"), nrows = 0))
idc <- grep("^Block$|GEOID", hdr, ignore.case = TRUE, value = TRUE)[1]
pcol <- grep("population", hdr, ignore.case = TRUE, value = TRUE)[1]
pop <- fread(file.path(BASE, "airtoxscreen.csv"), select = c(idc, pcol), colClasses = "character")
setnames(pop, c("GEOID20", "population"))
pop[, GEOID20 := gsub(" ", "0", sprintf("%015s", gsub("\\D", "", GEOID20)))]
pop[, population := suppressWarnings(as.numeric(population))]
pop <- unique(pop, by = "GEOID20")

SCALE_BENZ <- 1.149405
risk_line <- function(blk, cc, label) {
  d <- merge(merge(blk[is.finite(get(cc)), .(GEOID20, mob = get(cc) * SCALE_BENZ)],
                   atx, by = "GEOID20"), pop, by = "GEOID20")
  d <- d[is.finite(population) & population > 0]
  s_at <- d[, sum(airtox_ppb * population)]; s_mo <- d[, sum(mob * population)]
  diag_msg(sprintf("  %-24s %-18s n=%4d | pop=%7s | AT %.3f-%.3f | MOB %.3f-%.3f | ratio %.2f",
                   label, cc, nrow(d), format(sum(d$population), big.mark = ","),
                   5.75 * s_at / 1e6, 20.40 * s_at / 1e6,
                   5.75 * s_mo / 1e6, 20.40 * s_mo / 1e6, s_mo / s_at))
}
risk_line(blk_old, best, "OLD data (should ~= ms):")
risk_line(blk_new, best, "NEW data (updated):")
diag_msg("\nManuscript: AT 0.077-0.275 | MOB 0.183-0.650 | ratio 2.4x (1,120 blocks / 83,828 pop)")
diag_msg("If the OLD-data line reproduces the ms, the NEW-data line is the delay-corrected")
diag_msg("update under the SAME definition — but we must then evaluate whether that")
diag_msg("definition (e.g., positive-only medians) is scientifically defensible.")
