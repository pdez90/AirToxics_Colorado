# ==============================================================
# R04c_risk_forensics.R
# Decisive comparison for the benzene-risk discrepancy:
#   manuscript implies pop-weighted mobile mean ~0.38 ppb (ratio 2.4x),
#   new scripted run gives 0.156 ppb (ratio ~1.0).
# This script computes the risk table for BOTH mobile metrics
#   (med_of_daily_med_scaled  vs  mean_of_daily_mean_scaled)
# on BOTH data vintages
#   (old pre-fix backup       vs  new corrected-delay output)
# and for the *_overlap block files as well, to locate exactly
# which combination reproduces the published numbers
# (1,120 blocks; 83,828 residents; 0.077-0.275; 0.183-0.650; 2.4x).
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R04c: risk forensics — metric x vintage")

suppressPackageStartupMessages({ library(sf); library(dplyr); library(data.table) })

# ---- population lookup from AirToxScreen csv (read once) ----
ats_csv <- file.path(BASE, "airtoxscreen.csv")
hdr <- names(fread(ats_csv, nrows = 0))
idc <- grep("^Block$|GEOID", hdr, ignore.case = TRUE, value = TRUE)[1]
pcol <- grep("population", hdr, ignore.case = TRUE, value = TRUE)[1]
pop_tab <- fread(ats_csv, select = c(idc, pcol), colClasses = "character")
setnames(pop_tab, c("GEOID20", "Population_airtox"))
pop_tab[, GEOID20 := gsub(" ", "0", sprintf("%015s", gsub("\\D", "", GEOID20)))]
pop_tab[, Population_airtox := suppressWarnings(as.numeric(Population_airtox))]
pop_tab <- unique(pop_tab, by = "GEOID20")
diag_msg("population lookup: ", format(nrow(pop_tab), big.mark = ","), " GEOIDs")

METRICS <- c(median_metric = "sBenzene_med_of_daily_med_scaled",
             mean_metric   = "sBenzene_mean_of_daily_mean_scaled")

risk_one <- function(f, label) {
  if (!file.exists(f)) { diag_msg("  [SKIP] not found: ", f); return(NULL) }
  e <- new.env(); load(f, envir = e)
  # pick the sf block object (block_sf or block_sf_overlap)
  ob <- ls(e)[sapply(ls(e), function(n) inherits(get(n, envir = e), "sf"))][1]
  if (is.na(ob)) { diag_msg("  [SKIP] no sf object in: ", basename(f)); return(NULL) }
  b <- sf::st_drop_geometry(get(ob, envir = e))
  if (!"benzene_ppb" %in% names(b) && "benzene_ppb_airtox" %in% names(b))
    b$benzene_ppb <- b$benzene_ppb_airtox
  b$GEOID20 <- gsub(" ", "0", sprintf("%015s", gsub("\\D", "", as.character(b$GEOID20))))
  b <- left_join(b, as.data.frame(pop_tab), by = "GEOID20")

  out <- list()
  for (mk in names(METRICS)) {
    mcol <- METRICS[[mk]]
    if (!mcol %in% names(b)) { out[[mk]] <- data.frame(file=basename(f), object=ob, metric=mk, note="metric col missing"); next }
    d <- b %>% filter(is.finite(Population_airtox), Population_airtox > 0,
                      is.finite(benzene_ppb), is.finite(.data[[mcol]]))
    sums_at <- sum(d$benzene_ppb * d$Population_airtox)
    sums_mo <- sum(d[[mcol]]     * d$Population_airtox)
    out[[mk]] <- data.frame(
      vintage = label, object = ob, metric = mk,
      n_common = nrow(d),
      population = sum(d$Population_airtox),
      airtox_pwmean = sums_at / sum(d$Population_airtox),
      mobile_pwmean = sums_mo / sum(d$Population_airtox),
      airtox_risk_lo = 5.75  * sums_at / 1e6, airtox_risk_hi = 20.40 * sums_at / 1e6,
      mobile_risk_lo = 5.75  * sums_mo / 1e6, mobile_risk_hi = 20.40 * sums_mo / 1e6,
      ratio_mobile_over_airtox = sums_mo / sums_at
    )
  }
  do.call(rbind, out)
}

cases <- list(
  c(file.path(BASE,   "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"),         "NEW (corrected delays)"),
  c(file.path(BACKUP, "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"),         "OLD (pre-fix backup)"),
  c(file.path(BASE,   "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData"), "NEW overlap file"),
  c(file.path(BACKUP, "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData"), "OLD overlap file")
)

res <- list()
for (cs in cases) {
  diag_msg("\n--- ", cs[2], " : ", basename(cs[1]), " ---")
  r <- risk_one(cs[1], cs[2])
  if (!is.null(r)) {
    res[[length(res) + 1]] <- r
    txt <- capture.output(print(r, digits = 4, row.names = FALSE))
    for (l in txt) diag_msg("  ", l)
  }
}
tab <- do.call(rbind, res)
out_csv <- file.path(PIPE, "risk_forensics_metric_by_vintage.csv")
write.csv(tab, out_csv, row.names = FALSE)

diag_section("R04c: verdict guide")
diag_msg("Published manuscript targets: n=1,120 blocks | pop=83,828 |")
diag_msg("  AirToxScreen 0.077-0.275 | mobile 0.183-0.650 | ratio 2.4x")
diag_msg("Implied pop-weighted means: AirToxScreen ~0.160 ppb | mobile ~0.380 ppb")
diag_msg("")
diag_msg("How to read the table:")
diag_msg(" - If OLD + mean_metric reproduces ratio ~2.4x  -> manuscript used the MEAN metric;")
diag_msg("   decide which metric the paper should report and state it explicitly in Methods.")
diag_msg(" - If OLD rows match NEW rows closely           -> the delay fix is NOT the cause;")
diag_msg("   the discrepancy predates it (pipeline evolved since the ms numbers were made).")
diag_msg(" - If no combination reproduces n=1,120/pop=83,828 -> the ms block set came from an")
diag_msg("   earlier data cut or extra filters; check FinalFig/benzene_risk_summary_*.csv from")
diag_msg("   the original run and any older _COMMONBLOCKS gpkg for its filter provenance.")
diag_msg("")
diag_msg("Table saved: ", out_csv)
