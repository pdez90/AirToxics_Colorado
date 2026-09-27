# ==============================================================
# audit_all_claims.R - every numeric claim in the manuscript and SI that has a
# pipeline artifact behind it, checked against that artifact.
#
# Built 2026-09-27 after the final vet found statistics in the prose that no
# harness covered (the HRRR-vs-station wind disagreement, the bias-simulation
# percentages, the split-sample and sampling-sufficiency figures). This is the
# union of the existing checks (audit_si_prose.R, audit_manuscript_claims.R,
# the [OK]/[EDIT] blocks in 72/74/77/79) and everything else in the two
# documents that can be re-derived from a file the pipeline writes.
#
# Method, as in audit_si_prose.R: read the .docx, re-derive the number from the
# artifact, round HALF-UP to the precision the prose uses, and require the
# resulting fragment verbatim. A FAIL means the document and the pipeline
# disagree; the pipeline is regenerated, so the document is usually what is
# wrong. Two deliberate tolerances: (i) a value that sits exactly on a .5 tie
# at the quoted precision is accepted under either half-up or R's round()
# (rt() below), because code-generated tables use round(); (ii) table cells
# are compared after collapsing whitespace, so an empty cell reads "| |".
#
# Status 2026-09-27 (night): primary exposure basis for the hazard screen is now the block
# median of daily medians (74/77/79 HAZARD_BASIS); the mean basis is checked as secondary. Claims whose source is a document rather than code (permit records,
# literature values, instrument specifications) are listed at the end so the
# reader can see what this script does NOT vouch for.
#
#   SUNCOR_BASE=~/Downloads/Suncor SI_DOCX=... MS_DOCX=... \
#     Rscript tests/audit_all_claims.R
#
# Heavy inputs (mobile_hrrr.RData, the block and cell surfaces) are used when
# present and SKIPped when not, so the script degrades rather than dies.
# ==============================================================
suppressPackageStartupMessages(library(data.table))

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
SI_DOCX <- path.expand(Sys.getenv("SI_DOCX", file.path(dirname(BASE), "Suncor_v2", "SI_MobileToxics_CDPHE.docx")))
MS_DOCX <- path.expand(Sys.getenv("MS_DOCX", file.path(dirname(BASE), "Suncor_v2", "MobileToxics_CDPHE.docx")))

docx_text <- function(path) {
  if (!file.exists(path)) return(NULL)
  con <- unz(path, "word/document.xml", open = "rb"); on.exit(close(con), add = TRUE)
  x <- rawToChar(readBin(con, "raw", n = file.size(path))); Encoding(x) <- "UTF-8"
  x <- gsub("</w:p>", "\n", x, fixed = TRUE); x <- gsub("</w:tc>", " | ", x, fixed = TRUE)
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE); x <- gsub("&lt;", "<", x, fixed = TRUE); x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&#x27;", "'", x, fixed = TRUE); x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("\u2212", "-", x, fixed = TRUE); x <- gsub("\u2013", "-", x, fixed = TRUE)   # minus / en-dash -> hyphen
  x <- gsub("\u00a0", " ", x, fixed = TRUE)   # no-break space (empty table cells) -> space
  x
}
rh <- function(x, d) sprintf(paste0("%.", d, "f"), sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
# Code-generated tables are written with R's round(), which resolves an exact
# .5 tie (e.g. a stored 0.4805 or a median of 0.385) by float representation,
# not half-up. rt() returns a regex alternative accepting either rounding when
# the value sits on a tie at the precision used; otherwise it is rh().
rt <- function(x, d) { a <- rh(x, d); b <- sprintf(paste0("%.", d, "f"), round(x, d))
  if (a == b) gsub(".", "\\.", a, fixed = TRUE) else sprintf("(%s|%s)", gsub(".", "\\.", a, fixed = TRUE), gsub(".", "\\.", b, fixed = TRUE)) }
rxq <- function(s) gsub("([][{}()|.+*?^$\\\\])", "\\\\\\1", s, perl = TRUE)   # escape a literal for use inside a regex fragment
# Table text: one line, cells separated by " | ", an empty cell rendered "| |".
rows <- function(txt) { x <- gsub("\\s*\\|\\s*", " | ", gsub("\n", " ", txt)); repeat { y <- gsub("\\| +\\|", "| |", x); if (identical(y, x)) break; x <- y }; x }
cm <- function(x) formatC(round(x), format = "d", big.mark = ",")
pc <- function(x, d = 0) rh(x, d)

n_ok <- 0L; n_fail <- 0L; n_skip <- 0L
say <- function(lab, frag, txt, src = "") {
  if (is.null(txt)) { n_skip <<- n_skip + 1L; cat(sprintf("  [SKIP] %-46s (document not found)\n", lab)); return(invisible()) }
  hit <- grepl(frag, txt, fixed = TRUE)
  if (hit) n_ok <<- n_ok + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("  [%s] %-46s \"%s\"%s\n", if (hit) "OK  " else "FAIL", lab, frag,
              if (hit) "" else sprintf("\n         source: %s", src)))
}
sayx <- function(lab, rx, txt, src = "") {   # as say(), but rx is a regex (used with rt())
  if (is.null(txt)) { n_skip <<- n_skip + 1L; cat(sprintf("  [SKIP] %-46s (document not found)\n", lab)); return(invisible()) }
  hit <- grepl(rx, txt, perl = TRUE)
  if (hit) n_ok <<- n_ok + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("  [%s] %-46s /%s/%s\n", if (hit) "OK  " else "FAIL", lab, rx, if (hit) "" else sprintf("\n         source: %s", src)))
}
skip <- function(lab, why) { n_skip <<- n_skip + 1L; cat(sprintf("  [SKIP] %-46s %s\n", lab, why)) }
need <- function(p) { f <- file.path(BASE, p); if (!file.exists(f)) stop("missing pipeline output: ", f); fread(f) }
have <- function(p) file.exists(file.path(BASE, p))
hdr  <- function(s) cat("\n== ", s, " ==\n", sep = "")
ang  <- function(a, b) abs(((a - b + 180) %% 360) - 180)

SI <- docx_text(SI_DOCX); MS <- docx_text(MS_DOCX)
cat("SI  : ", if (is.null(SI)) paste("NOT FOUND ->", SI_DOCX) else SI_DOCX, "\n", sep = "")
cat("MS  : ", if (is.null(MS)) paste("NOT FOUND ->", MS_DOCX) else MS_DOCX, "\n", sep = "")
if (!is.null(SI)) SI <- gsub(" ", " ", SI); if (!is.null(MS)) MS <- gsub(" ", " ", MS)
BOTH <- paste(MS, SI, sep = "\n")

# ==========================================================================
hdr("A. Blocks, risk, abstract  <- benzene_risk_summary + TABLE_S4.1 + block file")
rs <- need("FinalFig/benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS.csv")
ats <- rs[grepl("AirToxScreen", metric)]; mob <- rs[grepl("Mobile", metric)]
say("abstract: mobile vs AirToxScreen cases",
    sprintf("%s-%s excess cases among %s residents, versus %s-%s", rh(mob$risk_5_75, 3), rh(mob$risk_20_40, 3),
            cm(mob$total_population_used), rh(ats$risk_5_75, 3), rh(ats$risk_20_40, 3)), MS)
say("2.1: common blocks and residents", sprintf("%s blocks housing %s residents", cm(mob$n_blocks), cm(mob$total_population_used)), MS)
say("3.3: pop-weighted means", sprintf("were %s and %s ppb, respectively", rh(mob$pop_weighted_mean_ppb, 3), rh(ats$pop_weighted_mean_ppb, 3)), MS)
say("3.3: residents across common blocks", sprintf("there were %s residents across the %s census blocks", cm(mob$total_population_used), cm(mob$n_blocks)), MS)
say("3.3: cancer cases", sprintf("was %s-%s cases", rh(mob$risk_5_75, 3), rh(mob$risk_20_40, 3)), MS)
say("4: risk ratio", sprintf("risk ratio %s", rh(mob$pop_weighted_mean_ppb / ats$pop_weighted_mean_ppb, 2)), MS)

BLK <- file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData")
if (file.exists(BLK)) {
  suppressPackageStartupMessages(library(sf))
  e <- new.env(); suppressWarnings(load(BLK, envir = e)); d <- sf::st_drop_geometry(get(ls(e)[1], envir = e))
  m <- d$sBenzene_med_of_daily_med_scaled; a <- d$benzene_ppb; k <- is.finite(m) & is.finite(a); r <- m[k] / a[k]
  say("3.3: Pearson / Spearman", sprintf("(Pearson r = %s, Spearman r = %s)", rh(cor(m[k], a[k]), 2), rh(cor(m[k], a[k], method = "spearman"), 2)), MS)
  say("3.3: AirToxScreen block range", sprintf("span only %s-%s ppb across the entire domain - a %s-fold range", rh(min(a[k]), 3), rh(max(a[k]), 3), rh(max(a[k]) / min(a[k]), 1)), MS)
  say("3.3: mobile block maximum", sprintf("reaching %s ppb", rh(max(m[k]), 1)), MS)
  say("3.3: blocks > 2x / 5x / max ratio", sprintf("In %d of the %s common blocks (%s%%), mobile-derived concentrations exceed AirToxScreen by more than a factor of two, in %d blocks by more than a factor of five, and in the most extreme block by a factor of %d",
      sum(r > 2), cm(sum(k)), rh(100 * mean(r > 2), 1), sum(r > 5), round(max(r))), MS)
  say("3.3: blocks below AirToxScreen", sprintf("lower than AirToxScreen in %s%% of blocks (median ratio %s)", pc(100 * mean(r < 1)), rh(median(r), 2)), MS)
  ar <- d$area_km2[k]
  say("2.5.1: block area", sprintf("median of %s km2 (IQR: %s-%s km2), a mean of %s km2, and a range from %s to %s km2",
      rh(median(ar), 4), rh(quantile(ar, .25), 4), rh(quantile(ar, .75), 4), rh(mean(ar), 4), rh(min(ar), 4), rh(max(ar), 2)), MS)
  # neurological most-exposed block (within block) - Table S7.1 / S7.1 prose.
  # PRIMARY BASIS (2026-09-27): median of daily medians, as in section 3.3; the
  # mean of daily means is the secondary basis quoted alongside it.
  MV <- 8.314 * 298.15 / 83000 * 1000
  hqb <- function(b) cbind(d[[paste0("sToluene_", b)]] * 92.14 / MV / 5000, d[[paste0("sXylene_", b)]] * 106.16 / MV / 100,
                           d[[paste0("sTrimethylbenzene_", b)]] * 120.19 / MV / 60)
  hq <- hqb("med_of_daily_med"); ok <- rowSums(!is.finite(hq)) == 0 & is.finite(d$POP20)
  hqm <- hqb("mean_of_daily_mean"); okm <- rowSums(!is.finite(hqm)) == 0 & is.finite(d$POP20)
  say("S7.1: neurological within-block maximum", sprintf("the largest index within any single block is %s (block %s)", rh(max(rowSums(hq[ok, ])), 3), d$GEOID20[ok][which.max(rowSums(hq[ok, ]))]), SI)
  say("S7.1: eligible blocks (neurological)", sprintf("(%s blocks for the neurological system)", cm(sum(ok))), SI)
  say("S7.1: sum of separate maxima on the mean basis (not used)", sprintf("their sum, %s, exceeds the largest within-block index of %s", rh(sum(apply(hqm, 2, max, na.rm = TRUE)), 3), rh(max(rowSums(hqm[okm, ])), 3)), SI)
  say("S7.2: neurological most-exposed block", sprintf("and %s at the most-exposed block", rh(max(rowSums(hq[ok, ])), 2)), SI)
  # both bases, benzene comparison (section 3.3 / S4.3) - from the block file itself
  a <- d$benzene_ppb; pw <- function(x) { k <- is.finite(x) & is.finite(a) & is.finite(d$POP20) & d$POP20 > 0; sum(x[k] * d$POP20[k]) / sum(d$POP20[k]) }
  mm <- d$sBenzene_mean_of_daily_mean_scaled; md <- d$sBenzene_med_of_daily_med_scaled; k2 <- is.finite(mm) & is.finite(md) & is.finite(a) & is.finite(d$POP20) & d$POP20 > 0
  say("3.3 / S4.3: mean-basis benzene", sprintf("a population-weighted mobile benzene of %s ppb, %s-%s excess cases, a mobile-to-AirToxScreen ratio of %s and %d blocks above twice", rh(pw(mm), 3),
      rh(5.75 * sum(mm[k2] * d$POP20[k2]) / 1e6, 3), rh(20.40 * sum(mm[k2] * d$POP20[k2]) / 1e6, 3), rh(pw(mm) / pw(a), 2), sum(mm[k2] / a[k2] > 2)), MS)
  say("S4.3: mean-basis benzene", sprintf("the population-weighted mobile benzene is %s ppb (%s-%s excess cases), the ratio to AirToxScreen is %s, and %d rather than %d blocks exceed twice", rh(pw(mm), 3),
      rh(5.75 * sum(mm[k2] * d$POP20[k2]) / 1e6, 3), rh(20.40 * sum(mm[k2] * d$POP20[k2]) / 1e6, 3), rh(pw(mm) / pw(a), 2), sum(mm[k2] / a[k2] > 2), sum(md[k2] / a[k2] > 2)), SI)
  say("S7.3: mean > median share and factor", sprintf("The block means exceed the block medians in %s%% of blocks, by a median factor of %s for benzene", pc(100 * mean(mm[k2] > md[k2])), rh(median((mm / md)[k2 & md > 0]), 1)), SI)
} else skip("block-file checks (3.3, 2.5.1, S7.1)", "censusblocks_..._overlap.RData not present")

# ==========================================================================
hdr("B. Figure 2 cell surface  <- segment500_summaries_acrossSites.RData")
SEG <- file.path(BASE, "segment500_summaries_acrossSites.RData")
if (file.exists(SEG)) {
  suppressPackageStartupMessages(library(sf))
  e <- new.env(); suppressWarnings(load(SEG, envir = e)); pd <- sf::st_drop_geometry(e$seg_wide_sf)
  cell <- function(p, md = 3) { x <- suppressWarnings(as.numeric(pd[[paste0("bgcorr_", p, "_median_of_daily_medians")]]))
    n <- suppressWarnings(as.numeric(pd[[paste0("bgcorr_", p, "_n_days_any")]])); x[is.finite(x) & is.finite(n) & n >= md] }
  b <- cell("Benzene")
  say("3.3: benzene cell range and count", sprintf("ranged from %s to %s ppb across the %d cells sampled on at least three days", rh(min(b), 2), rh(max(b), 2), length(b)), MS)
  av <- unlist(lapply(c("Benzene", "Toluene", "Trimethylbenzene", "Xylene"), cell))
  say("3.3: shared aromatic colour scale", sprintf("shared color scale spanning %s to %s ppb", rh(quantile(av, .02), 2), rh(quantile(av, .98), 2)), MS)
  say("3.3: benzene <= 0.15 ppb share", sprintf("at or below the relatively small value of 0.15 ppb in %s%% of mapped cells", pc(100 * mean(b <= 0.15))), MS)
  top <- sort(b, decreasing = TRUE)[1:3]
  say("3.3: three highest benzene cells", sprintf("The three highest cells (%s, %s and %s ppb)", rh(top[3], 2), rh(top[2], 2), rh(top[1], 2)), MS)
  say("3.3: toluene maximum", sprintf("(maximum %s ppb)", rh(max(cell("Toluene")), 2)), MS)
  say("3.3: TMB and xylene maxima", sprintf("Trimethylbenzene (maximum %s ppb) and xylene (maximum %s ppb)", rh(max(cell("Trimethylbenzene")), 2), rh(max(cell("Xylene")), 2)), MS)
  h <- cell("H2S")
  say("3.3: H2S cell range and count", sprintf("ranged from %s to %s ppb across the %d cells", rh(min(h), 2), rh(max(h), 2), length(h)), MS)
  say("3.3: H2S share >= 1 ppb", sprintf("the %s%% of mapped cells at or above 1 ppb", pc(100 * mean(h >= 1))), MS)
  say("Figure 2 caption: H2S / benzene minima", sprintf("extend to %s ppb, well below the aromatics' minimum of %s ppb", rh(min(h), 2), rh(min(b), 2)), MS)
  hc <- cell("HCN")
  say("3.3: HCN median, count, max, >= 1.2 ppb", sprintf("median value of %s ppb across %d cells", rh(median(hc), 2), length(hc)), MS)
  say("3.3: HCN maximum and top cells", sprintf("(maximum %s ppb; %d of %d cells, under 1%%, at or above 1.2 ppb)", rh(max(hc), 2), sum(hc >= 1.2), length(hc)), MS)
  # Table S3.2 sustained maxima: >= 10 sampled days, 24-h scaled for benzene/toluene/xylene
  sfF <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")
  if (file.exists(sfF)) {
    e2 <- new.env(); load(sfF, envir = e2); o <- get(ls(e2)[1], envir = e2)
    fac <- setNames(as.numeric(o$ratio_all_over_mobilelike), tolower(o$pollutant))
    mx <- function(p, f = 1) max(cell(p, 10)) * f
    SIrow <- rows(SI)
    say("Table S3.2: benzene sustained max (>=10 d, scaled)", sprintf("| %s |", rh(mx("Benzene", fac["benzene"]), 2)), SIrow)
    say("Table S3.2: toluene sustained max", sprintf("| %s |", rh(mx("Toluene", fac["toluene"]), 2)), SIrow)
    sayx("Table S3.2: TMB sustained max (unscaled)", sprintf("\\| %s \\(unscaled\\) \\|", rt(mx("Trimethylbenzene"), 2)), SIrow)
    say("Table S3.2: xylene sustained max", sprintf("| %s |", rh(mx("Xylene", fac["xylene"]), 2)), SIrow)
    say("Table S3.2: H2S sustained max (unscaled)", sprintf("| %s (unscaled) |", rh(mx("H2S"), 2)), SIrow)
    say("Table S3.2: HCN sustained max (unscaled)", sprintf("| %s (unscaled) |", rh(mx("HCN"), 1)), SIrow)
  }
} else skip("Figure 2 cell checks (3.3, Table S3.2)", "segment500_summaries_acrossSites.RData not present")

# ==========================================================================
hdr("C. Two-laboratory comparison  <- TABLE_cat_emu_comparison.csv")
ce <- need("TABLE_cat_emu_comparison.csv"); g <- function(p, c) ce[pollutant == p][[c]]
say("2.1.1: cell-days", sprintf("(%d cell-days)", g("Benzene", "n_celldays")), MS)
say("2.1.1: EMU/CAT median ratios", sprintf("were %s for benzene and %s for toluene, with larger divergence for trimethylbenzene (%s) and xylene (%s)",
    rh(g("Benzene", "median_ratio_EMU_over_CAT"), 2), rh(g("Toluene", "median_ratio_EMU_over_CAT"), 2),
    rh(g("Trimethylbenzene", "median_ratio_EMU_over_CAT"), 2), rh(g("Xylene", "median_ratio_EMU_over_CAT"), 2)), MS)
say("S1.5: H2S / HCN median abs difference", sprintf("median absolute differences (%s and %s ppb)", rh(g("H2S", "median_abs_diff_ppb"), 1), rh(g("HCN", "median_abs_diff_ppb"), 1)), SI)

# ==========================================================================
hdr("D. Measurement record  <- TABLE_S3.1.csv, TABLE_S1.4_qaqc_checks.csv")
s31 <- need("TABLE_S3.1.csv"); q14 <- need("TABLE_S1.4_qaqc_checks.csv")
say("3.1: HCN record", sprintf("with %s values confined to %d days", cm(s31[pollutant == "HCN", analysis]), s31[pollutant == "HCN", n_days]), MS)
say("3.1 / 3.8: below-MDL shares", sprintf("%s%% of benzene, %s%% of H2S and %s%% of HCN", pc(s31[pollutant == "Benzene", pct_belowMDL]), pc(s31[pollutant == "H2S", pct_belowMDL]), pc(s31[pollutant == "HCN", pct_belowMDL])), MS)
say("S1.4: negatives retained", sprintf("%s of them for benzene and %s for H2S", cm(q14[pollutant == "Benzene", n_negative_after_qc]), cm(q14[pollutant == "H2S", n_negative_after_qc])), SI)
say("Table S3.1: analysis-set counts", sprintf("| %s | %s | %s | %s | %s | %s |", cm(s31$analysis[1]), cm(s31$analysis[2]), cm(s31$analysis[3]), cm(s31$analysis[4]), cm(s31$analysis[5]), cm(s31$analysis[6])),
    rows(SI))
say("Table S3.1: sampling days", sprintf("| %d | %d | %d | %d | %d | %d |", s31$n_days[1], s31$n_days[2], s31$n_days[3], s31$n_days[4], s31$n_days[5], s31$n_days[6]),
    rows(SI))
say("Table S3.1: below-MDL row", sprintf("| %s%% | %s%% | %s%% | %s%% | %s%% | %s%% |", rh(s31$pct_belowMDL[1], 1), rh(s31$pct_belowMDL[2], 1), rh(s31$pct_belowMDL[3], 1), rh(s31$pct_belowMDL[4], 1), rh(s31$pct_belowMDL[5], 1), rh(s31$pct_belowMDL[6], 1)),
    rows(SI))
say("2.4: ppb -> ug/m3 for benzene", sprintf("1 ppb = %s μg/m3", rh(78.11 / (8.314 * 298.15 / 83000 * 1000), 2)), MS)

# ==========================================================================
hdr("E. MDL substitution  <- TABLE_mdl_sensitivity_summary/blocks.csv")
ms_ <- need("TABLE_mdl_sensitivity_summary.csv"); mb <- need("TABLE_mdl_sensitivity_blocks.csv")
bz <- function(cs, col) ms_[pollutant == "Benzene" & case == cs][[col]]
say("S3.1: below-MDL fractions", sprintf("flags %s%% of benzene, %s%% of toluene, %s%% of trimethylbenzene, %s%% of xylene, %s%% of H",
    rh(ms_[pollutant == "Benzene" & case == "raw", pct_substituted], 1), rh(ms_[pollutant == "Toluene" & case == "raw", pct_substituted], 1),
    rh(ms_[pollutant == "Trimethylbenzene" & case == "raw", pct_substituted], 1), rh(ms_[pollutant == "Xylene" & case == "raw", pct_substituted], 1),
    rh(ms_[pollutant == "H2S" & case == "raw", pct_substituted], 1)), SI)
say("S3.1: HCN fraction", sprintf("S, and %s%% of HCN observations", rh(ms_[pollutant == "HCN" & case == "raw", pct_substituted], 1)), SI)
say("S3.1: benzene medians by case", sprintf("the campaign median is %s, %s, %s, and %s ppb under the 0, raw, MDL/2, and MDL cases", rh(bz("zero", "median"), 0), rh(bz("raw", "median"), 2), rh(bz("half", "median"), 2), rh(bz("full", "median"), 2)), SI)
say("S3.1: full-MDL p95", sprintf("the 95th percentile (%s ppb)", rh(bz("full", "p95"), 1)), SI)
say("S3.1: block count", sprintf("for the %s blocks that retain a valid value", cm(mb$blocks[1])), SI)
say("S3.1: correlations by case", sprintf("falls to %s for substitution with 0, %s for MDL/2, and %s for full-MDL substitution", rh(mb[case == "zero", r_vs_raw], 2), rh(mb[case == "half", r_vs_raw], 2), rh(mb[case == "full", r_vs_raw], 2)), SI)
say("S3.1: block/ATS ratios by case", sprintf("from %s for the raw, unscaled values to %s for MDL/2 substitution and %s for MDL substitution", rh(mb[case == "raw", median_ratio_vs_ATS], 2), rh(mb[case == "half", median_ratio_vs_ATS], 2), rh(mb[case == "full", median_ratio_vs_ATS], 2)), SI)
say("3.8: correlations after substitution", sprintf("falls to %s and %s, respectively", rh(mb[case == "half", r_vs_raw], 2), rh(mb[case == "full", r_vs_raw], 2)), MS)

# ==========================================================================
hdr("F. La Casa  <- lacasa metrics, TABLE_lacasa_daynight_ratios.csv, scaling factors")
lm <- need("FinalFig/lacasa_mobile_exact_minute_metrics_100m_500m.csv")
n100_23 <- lm[threshold_m == 100 & dataset == "2023_ELF" & pollutant == "Toluene", n]; n100_24 <- lm[threshold_m == 100 & dataset == "2024_2R" & pollutant == "Toluene", n]
n500_24 <- lm[threshold_m == 500 & dataset == "2024_2R" & pollutant == "Toluene", n]
say("3.2: 100 m overlaps", sprintf("Within 100 m, %s overlap was observed in 2023 and only %d minutes in 2024", if (n100_23 == 0) "no" else n100_23, n100_24), MS)
say("3.2: 500 m overlaps", sprintf("increased the number of paired observations to %d minutes in 2024", n500_24), MS)
t5 <- lm[threshold_m == 500 & dataset == "2024_2R" & pollutant == "Toluene"]; x5 <- lm[threshold_m == 500 & dataset == "2024_2R" & pollutant == "Xylene"]
say("3.2: toluene agreement", sprintf("toluene r = %s (RMSE = %s ppb;", rh(t5$pearson_r, 2), rh(t5$rmse, 2)), MS)
say("3.2: toluene ODR slope", sprintf("ODR slope = %s) and xylene r = %s (RMSE = %s ppb;", rh(t5$odr_slope, 2), rh(x5$pearson_r, 2), rh(x5$rmse, 2)), MS)
say("3.2: xylene ODR slope", sprintf("ODR slope = %s).", rh(x5$odr_slope, 2)), MS)
dn <- need("TABLE_lacasa_daynight_ratios.csv")
say("S4.4: night/window ratios", sprintf("by factors of %s (benzene), %s (toluene), and %s (xylene) (median-based ratios %s, %s, and %s)",
    rh(dn[pollutant == "benzene", ratio_night_over_window_mean], 2), rh(dn[pollutant == "toluene", ratio_night_over_window_mean], 2), rh(dn[pollutant == "xylene", ratio_night_over_window_mean], 2),
    rh(dn[pollutant == "benzene", ratio_night_over_window_median], 2), rh(dn[pollutant == "toluene", ratio_night_over_window_median], 2), rh(dn[pollutant == "xylene", ratio_night_over_window_median], 2)), SI)
wk <- dn$ratio_weekend_over_window_mean
say("S4.4: weekend ratios", sprintf("%s-%s%% lower than weekday daytimes (ratios %s, %s, and %s)", pc(100 * (1 - max(wk))), pc(100 * (1 - min(wk))), rh(wk[1], 2), rh(wk[2], 2), rh(wk[3], 2)), SI)
nt <- dn$ratio_night_over_window_mean
say("3.8: La Casa contrasts", sprintf("by factors of %s-%s, and weekend daytimes are %s-%s%% lower", rh(min(nt), 2), rh(max(nt), 2), pc(100 * (1 - max(wk))), pc(100 * (1 - min(wk)))), MS)
sfF <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")
if (file.exists(sfF)) { e2 <- new.env(); load(sfF, envir = e2); o <- get(ls(e2)[1], envir = e2); f <- as.numeric(o$ratio_all_over_mobilelike)
  say("3.8 / 4: scaling factors as percentages", sprintf("%s-%s%% depending on pollutant", pc(100 * (min(f) - 1)), pc(100 * (max(f) - 1))), MS) }

# ==========================================================================
hdr("G. Hotspot chain  <- hotspot_thresholds_summary, MASTER index, TABLE_S5.1, summary_stats")
th <- need("hotspot_thresholds_summary.csv"); tv <- function(p) th[pollutant == p]
say("2.5.3: persistence thresholds", sprintf("(n = %s and %d days for benzene; n = %s and %d days for toluene; n = %s and %d days for trimethylbenzene; n = %s and %d days for xylene; n = %s and %d days for H",
    rh(tv("benzene")$n_cutoff_p90, 1), tv("benzene")$n_days_cutoff_p90, rh(tv("toluene")$n_cutoff_p90, 1), tv("toluene")$n_days_cutoff_p90,
    rh(tv("trimethylbenzene")$n_cutoff_p90, 1), tv("trimethylbenzene")$n_days_cutoff_p90, rh(tv("xylene")$n_cutoff_p90, 0), tv("xylene")$n_days_cutoff_p90,
    rh(tv("hydrogen_sulfide")$n_cutoff_p90, 0), tv("hydrogen_sulfide")$n_days_cutoff_p90), MS)
say("2.5.3: HCN threshold", sprintf("S; n = %s and %d days for HCN)", rh(tv("hydrogen_cyanide")$n_cutoff_p90, 1), tv("hydrogen_cyanide")$n_days_cutoff_p90), MS)
say("2.5.3: initial clusters", sprintf("yielded %s initial clusters across the six pollutants (%d benzene, %d toluene, %d trimethylbenzene, %d xylene, %d H",
    cm(sum(th$n_clusters_all)), tv("benzene")$n_clusters_all, tv("toluene")$n_clusters_all, tv("trimethylbenzene")$n_clusters_all, tv("xylene")$n_clusters_all, tv("hydrogen_sulfide")$n_clusters_all), MS)
say("2.5.3: HCN clusters", sprintf("S and %d HCN)", tv("hydrogen_cyanide")$n_clusters_all), MS)
say("2.5.3: kernel weight at 15 km", sprintf("(~%s%% of near-field weight at 15 km)", pc(100 * exp(-15000 / 12000))), MS)
ss <- need("summary_stats_persistent.csv")
say("3.4: 14 persistent groups", sprintf("Regional map of %d persistent hotspot groups", ss$n_groups_3plus_pollutants), MS)
M <- need("MASTER_hotspot_group_index.csv"); S5 <- need("TABLE_S5.1_group_exceedance_days.csv")
tok <- strsplit(M$pollutants, "+", fixed = TRUE)
say("3.4.2: persistence range", sprintf("ranged from %d to %d days", min(M$max_n_days), max(M$max_n_days)), MS)
pp <- apply(S5[, .(benzene, toluene, trimethylbenzene, xylene, H2S, HCN)], 1, max)
say("3.4.2: per-pollutant day range", sprintf("(maximum per-pollutant counts ranged from %d to %d days)", min(pp), max(pp)), MS)
say("3.4.2: composition counts", sprintf("Benzene was present in %d of the %d groups", sum(sapply(tok, function(t) "benzene" %in% t)), nrow(M)), MS)
say("3.4.2: H2S / HCN membership", sprintf("with H2S in %s and HCN in %s", c("four","five","six")[sum(sapply(tok, function(t) "h2s" %in% t)) - 3], c("three","four","five")[sum(sapply(tok, function(t) "hcn" %in% t)) - 2]), MS)
gm <- function(id) M[group_id == id]; g5 <- function(id) S5[group_id == id]
say("3.4.2: group 4", sprintf("was above the campaign 99th percentile within 100 m on %d days, and its toluene cluster registered exceedances on %d distinct days", g5(4)$toluene, gm(4)$max_n_days), MS)
say("3.4.2: group 4 TRI distance", sprintf("It lies %s km from the Phillips 66 terminal", rh(gm(4)$tri_dist_km, 2)), MS)
say("3.4.2: groups 40 and 43", sprintf("at %s km (Owens Corning Roofing and Asphalt) and %s km (KBP Coil Coaters)", rh(gm(40)$tri_dist_km, 2), rh(gm(43)$tri_dist_km, 2)), MS)
say("3.4.2: groups 22 and 29", sprintf("with %d and %d measurements within 100 m", g5(22)$n_rows_100m, g5(29)$n_rows_100m), MS)
say("3.4.2: group 13 TRI distance", sprintf("Group 13, %s km from a glass-container plant", rh(gm(13)$tri_dist_km, 2)), MS)
say("3.4.2: group 11", sprintf("with %s mobile measurements within 100 m and exceedances on %d distinct days", cm(g5(11)$n_rows_100m), gm(11)$max_n_days), MS)
say("3.4.2: group 60", sprintf("%s km from the Sinclair Denver products terminal, and is persistent for HCN, trimethylbenzene and xylene for up to %d days", rh(gm(60)$tri_dist_km, 2), gm(60)$max_n_days), MS)
# SI Table S5.1 rows: per-group days and TRI distance
SIrow <- rows(SI)
for (i in seq_len(nrow(S5))) { r <- S5[i]
  say(sprintf("Table S5.1: group %s days", r$group_id), sprintf("benzene %d; toluene %d; trimethylbenzene %d; xylene %d; H", r$benzene, r$toluene, r$trimethylbenzene, r$xylene), SI)
  say(sprintf("Table S5.1: group %s TRI distance", r$group_id), sprintf("(%s km)", rh(r$tri_dist_km, 2)), SI) }

# DBSCAN sensitivity
ds <- need("TABLE_dbscan_sensitivity.csv"); b <- ds[baseline == TRUE]
one <- ds[(thr_pctl != b$thr_pctl) + (eps_m != b$eps_m) + (pers_pctl != b$pers_pctl) == 1]
say("2.5.3 / S5.3: single-step recovery range", sprintf("%s-%s%% of the %d baseline group locations", pc(100 * min(one$recovery_of_baseline)), pc(100 * max(one$recovery_of_baseline)), ss$n_groups_3plus_pollutants), MS)
lo <- one[recovery_of_baseline == min(recovery_of_baseline)]
say("2.5.3: minimum single-step recovery", sprintf("falling to %s%% when either the clustering radius is raised from 100 to 200 m or the persistence percentile is raised from the 90th to the 95th", pc(100 * min(one$recovery_of_baseline))), MS)
say("S5.3: groups range", sprintf("ranges from %d (coarsest eps with strictest persistence) to %d (finest eps with loosest persistence)", min(ds$groups_3plus), max(ds$groups_3plus)), SI)
mod <- ds[eps_m %in% c(50, 100) & pers_pctl %in% c(0.85, 0.90)]
say("S5.3: moderate-perturbation recovery", sprintf("%s-%s%% of the 14 baseline locations are recovered for eps of 50-100 m with persistence p85-p90", pc(100 * min(mod$recovery_of_baseline)), pc(100 * max(mod$recovery_of_baseline))), SI)
say("S5.3: single-step minimum", sprintf("recovery falls to %s%% (%d of 14)", pc(100 * min(one$recovery_of_baseline)), round(14 * min(one$recovery_of_baseline))), SI)
say("S5.3: baseline reproduces 14", sprintf("the same %d groups persistent in three or more pollutants", b$groups_3plus), SI)
# split-sample and sufficiency
sp <- need("TABLE_split_sample_hotspots.csv"); oe <- sp[split == "odd_even"]; ca <- sp[split == "calendar"]
say("S5.5: odd/even groups and agreement", sprintf("identified %d and %d groups persistent in three or more pollutants, respectively; %s%% and %s%% of one half's groups lie within 300 m of the other's, and the halves recover %s%% and %s%%",
    oe$groups3_A, oe$groups3_B, pc(100 * oe$frac_A_near_B), pc(100 * oe$frac_B_near_A), pc(100 * oe$frac_base_near_A), pc(100 * oe$frac_base_near_B)), SI)
say("S5.5: calendar halves", sprintf("identified %d (2023-2024) and %d (2025) groups, recovering %s%% and %s%% of the full-campaign locations, with cross-half agreement of %s%% and %s%%",
    ca$groups3_A, ca$groups3_B, pc(100 * ca$frac_base_near_A), pc(100 * ca$frac_base_near_B), pc(100 * ca$frac_A_near_B), pc(100 * ca$frac_B_near_A)), SI)
su <- need("TABLE_sampling_sufficiency.csv"); sk <- function(k, c) su[k_days == k][[c]]
say("S5.6: map correlations", sprintf("reached %s with 40 sampling days, %s with 80, %s with 160, and %s with 200", rh(sk(40, "map_cor_median"), 2), rh(sk(80, "map_cor_median"), 2), rh(sk(160, "map_cor_median"), 2), rh(sk(200, "map_cor_median"), 2)), SI)
say("S5.6: hotspot recovery", sprintf("from %s%% at 10 days to %s-%s%% at 60-80 days, %s%% at 120 days, %s%% at 160 days, and %s%% at 200 days",
    pc(100 * sk(10, "recovery_median")), pc(100 * min(sk(60, "recovery_median"), sk(80, "recovery_median"))), pc(100 * max(sk(60, "recovery_median"), sk(80, "recovery_median"))),
    pc(100 * sk(120, "recovery_median")), pc(100 * sk(160, "recovery_median")), pc(100 * sk(200, "recovery_median"))), SI)

# ==========================================================================
hdr("H. Plume funnel, inversion, detectability, attribution")
fc <- need("WWTP_H2S_plume_step_counts.csv"); ret <- need("WWTP_H2S_retained_plumes.csv")
say("2.5.5 / 3.6: candidate events", sprintf("yielded %d candidate", fc$n_plumes_remaining[1]), MS)
say("2.5.5: wind test evaluable", sprintf("genuinely evaluated for %d of the %d candidate events", 3L, fc$n_plumes_remaining[2]), MS)
say("S6.3: funnel counts", sprintf("This step yielded %d candidate plume events", fc$n_plumes_remaining[1]), SI)
say("S6.3: >= 3 points", sprintf("reduced this set to %d events", fc$n_plumes_remaining[2]), SI)
say("S6.3: retained", sprintf("In total, %d plume events passed all filters", fc$n_plumes_remaining[nrow(fc)]), SI)
p <- need("TABLE_min_detectable_rate_plumes.csv"); stopifnot(setequal(p$plume_id, ret$plume_id))
rates <- sort(p$inferred_tpy)
say("3.6: four rates and mean", sprintf("gave %s, %s, %s and %s metric tons/yr, a range spanning a factor of four around a mean of %s metric tons/yr", cm(rates[1]), cm(rates[2]), cm(rates[3]), cm(rates[4]), cm(mean(p$inferred_tpy))), MS)
say("3.6: Qmin range and SNR", sprintf("ranged from %s to %s metric tons/yr across the four retained intercepts; observed peak H2S enhancements were %s-%s times the detection limit", cm(min(p$qmin_mdl5)), cm(max(p$qmin_mdl5)), rh(min(p$snr_vs_mdl5), 1), rh(max(p$snr_vs_mdl5), 1)), MS)
say("S6.6: per-plume Qmin", sprintf("was %s, %s, %s, and %s t/yr", cm(sort(p$qmin_mdl5)[1]), cm(sort(p$qmin_mdl5)[2]), cm(sort(p$qmin_mdl5)[3]), cm(sort(p$qmin_mdl5)[4])), SI)
say("S6.5.2 / S6.6: rate range", sprintf("(%s-%s metric t/yr)", cm(min(rates)), cm(max(rates))), SI)
sc <- need("FinalFig/WWTP_H2S_inversion_summary_mean_ci_METRIC_TPY.csv"); us <- sc[usable == TRUE]; nr <- us[scenario != "reflections_FALSE"]
say("3.6: scenario ranges", sprintf("Across the %d scenarios that remain once the ill-conditioned cases are excluded, scenario means ranged between %s and %s metric tons/yr, and between %s and %s across the %d",
    nrow(us), cm(min(us$metric_mean)), cm(max(us$metric_mean)), cm(min(nr$metric_mean)), cm(max(nr$metric_mean)), nrow(nr)), MS)
say("S6.5.2: no-reflection mean", sprintf("the mean estimate increased to %s metric tons/yr, compared with %s", cm(sc[scenario == "reflections_FALSE", metric_mean]), cm(sc[sens_group == "baseline", metric_mean])), SI)
say("S6.5.2: scenario range", sprintf("mean estimates range from %s to %s metric tons/yr, and from %s to %s across the %d", cm(min(us$metric_mean)), cm(max(us$metric_mean)), cm(min(nr$metric_mean)), cm(max(nr$metric_mean)), nrow(nr)), SI)
at <- need("WWTP_H2S_source_attribution.csv"); at <- at[plume_id %in% p$plume_id]
say("S6.8: all four not evaluable", if (all(is.na(at$co_any))) "For none of the four plumes could the gate be evaluated" else "<<< some plume WAS evaluable - the S6.8 wording is now wrong >>>", SI)
say("3.6 / S6.8: methane max enhancement", sprintf("(at most approximately %s ppm", rh(max(at$dCH4_peak, na.rm = TRUE), 2)), MS)
say("Table S6.2: methane column", sprintf("| %s |", rh(max(at$dCH4_peak, na.rm = TRUE), 3)), rows(SI))
# bias simulations
sh <- need("error_vs_true_stack_height_WWTP_0p5to5km_summary.csv"); se <- function(d, h) sh[distance_km == d & H_true_m == h, mean_error]
say("S6.5.1: stack height at 0.5 km (low)", sprintf("overestimated by only %s%% for a 1 m source and %s%% for a 10 m source", rh(se(0.5, 1), 1), rh(se(0.5, 10), 1)), SI)
say("S6.5.1: stack height at 0.5 km (high)", sprintf("underestimated by %s%% at 15 m, %s%% at 20 m, %s%% at 30 m and %s%% at 50 m", rh(-se(0.5, 15), 1), rh(-se(0.5, 20), 1), rh(-se(0.5, 30), 1), rh(-se(0.5, 50), 1)), SI)
say("S6.5.1: stack height at 1 km", sprintf("At 1 km, mean error ranged from +%s%% for a 1 m source to %s%% for a 50 m source", rh(se(1, 1), 1), rh(se(1, 50), 1)), SI)
say("S6.5.1: stack height at 1.5 km", sprintf("narrowed to +%s%% to %s%%", rh(se(1.5, 1), 1), rh(se(1.5, 50), 1)), SI)
say("S6.5.1: stack height at 2 km", sprintf("the 50 m case reached only %s%%", rh(se(2, 50), 1)), SI)
say("S6.5.1: 50 m case at 3 and 5 km", sprintf("fell from %s%% at 3 km to %s%% at 5 km", rh(se(3, 50), 1), rh(se(5, 50), 1)), SI)
say("S6.5.1: maximum source-height bias", sprintf("at most %s%% and only for an extreme 50 m release sampled at 0.5 km", pc(-se(0.5, 50))), SI)
cw <- need("error_vs_distance_crosswind_mismatch_WWTP_summary.csv"); ce_ <- function(d, y) cw[distance_km == d & y_true_m == y, mean_error]
say("S6.5.1: crosswind at 0.5 km", sprintf("from %s%% for a 25 m offset to %s%% for 50 m, %s%% for 100 m, %s%% for 200 m, %s%% for 300 m and %s%% for 500 m", rh(ce_(0.5,25),1), rh(ce_(0.5,50),1), rh(ce_(0.5,100),1), rh(ce_(0.5,200),1), rh(ce_(0.5,300),1), rh(ce_(0.5,500),1)), SI)
say("S6.5.1: crosswind at 1 km", sprintf("from %s%% (25 m) to %s%% (50 m), %s%% (100 m), %s%% (200 m), %s%% (300 m) and %s%% (500 m)", rh(ce_(1,25),1), rh(ce_(1,50),1), rh(ce_(1,100),1), rh(ce_(1,200),1), rh(ce_(1,300),1), rh(ce_(1,500),1)), SI)
say("S6.5.1: crosswind at 2 km", sprintf("from %s%% for a 25 m offset to %s%% for 50 m, %s%% for 100 m, %s%% for 200 m, %s%% for 300 m and %s%% for 500 m", rh(ce_(2,25),1), rh(ce_(2,50),1), rh(ce_(2,100),1), rh(ce_(2,200),1), rh(ce_(2,300),1), rh(ce_(2,500),1)), SI)
say("S6.5.1: crosswind at 5 km", sprintf("was %s%% for 25 m, %s%% for 50 m, %s%% for 100 m, %s%% for 200 m, %s%% for 300 m and %s%% for 500 m", rh(ce_(5,25),1), rh(ce_(5,50),1), rh(ce_(5,100),1), rh(ce_(5,200),1), rh(ce_(5,300),1), rh(ce_(5,500),1)), SI)
qg <- need("TABLE_min_detectable_rate_grid.csv"); qq <- function(x, c, u, m = 5) qg[abs(x_km - x) < 1e-9 & CAT == c & abs(u_ms - u) < 1e-9 & abs(mdl_ppb - m) < 1e-9, qmin_tpy]
if (have("WWTP_H2S_plume_bearing_check.csv")) {
  pb <- need("WWTP_H2S_plume_bearing_check.csv")
  say("S6.8 / 3.6: source separation", sprintf("bearing %s-%s degrees from the wastewater facility", rh(min(pb$source_separation_deg), 0), rh(max(pb$source_separation_deg), 0)), SI)
  say("S6.8: off-axis to WWTP and refinery", sprintf("within %s-%s degrees of the wastewater-facility bearing and within %s-%s degrees of the refinery bearing", rh(min(pb$offaxis_WWTP_deg), 0), rh(max(pb$offaxis_WWTP_deg), 0), rh(min(pb$offaxis_refinery_deg), 0), rh(max(pb$offaxis_refinery_deg), 0)), SI)
  p10 <- pb[which.min(pb$dist_refinery_km)]
  say("S6.8: refinery between van and WWTP (plume 10)", sprintf("the refinery lies %s km along the same bearing, between the van and the wastewater facility at %s km", rh(p10$dist_refinery_km, 1), rh(p10$dist_WWTP_km, 2)), SI)
  say("3.6: refinery between van and WWTP (plume 10)", sprintf("the refinery lies %s km along the same bearing, between the van and the wastewater facility %s km away", rh(p10$dist_refinery_km, 1), rh(p10$dist_WWTP_km, 2)), MS)
  say("3.6: off-axis ranges in the manuscript", sprintf("within %s-%s degrees of the wastewater-facility bearing and within %s-%s degrees of the refinery bearing", rh(min(pb$offaxis_WWTP_deg), 0), rh(max(pb$offaxis_WWTP_deg), 0), rh(min(pb$offaxis_refinery_deg), 0), rh(max(pb$offaxis_refinery_deg), 0)), MS)
} else skip("S6.8 bearing comparison", "WWTP_H2S_plume_bearing_check.csv not present (run plume_scripts/P11_plume_geometry_checks.R)")
if (have("WWTP_H2S_receptor_height_check.csv")) {
  rz <- need("WWTP_H2S_receptor_height_check.csv")
  say("S6.4: receptor-height effect", sprintf("changes the inferred rates by at most %s%%", rh(max(abs(rz$pct_change_in_Q)), 3)), SI)
  say("S6.4: sigma_z and mixing-depth ranges", sprintf("(σz of %s-%s m at %s-%s km; mixing depths of %s-%s m)", cm(min(rz$sigma_z_m)), cm(max(rz$sigma_z_m)), rh(min(rz$x_km), 2), rh(max(rz$x_km), 2), cm(min(rz$hpbl_m)), cm(max(rz$hpbl_m))), SI)
} else skip("S6.4 receptor-height check", "WWTP_H2S_receptor_height_check.csv not present (run plume_scripts/P11_plume_geometry_checks.R)")
say("S6.6: Qmin under D at 3.2 m/s", sprintf("is approximately %s t/yr at 0.5 km, %s t/yr at 1 km, and %s t/yr at 2 km", cm(qq(0.5,"D",3.2)), cm(qq(1,"D",3.2)), cm(qq(2,"D",3.2))), SI)
say("S6.6: Qmin under B at 2 km", sprintf("to approximately %s t/yr at 2 km", cm(round(qq(2,"B",3.2), -1))), SI)
say("S6.6: Qmin at 0.5 km across speeds", sprintf("roughly %s-%s t/yr at 0.5 km under D stability", cm(qq(0.5,"D",2)), cm(qq(0.5,"D",5))), SI)

# ==========================================================================
hdr("I. Winds  <- mobile_hrrr.RData (heavy; skipped if absent)")
HR <- file.path(BASE, "mobile_hrrr.RData")
if (file.exists(HR)) {
  e <- new.env(); suppressWarnings(load(HR, envir = e)); r <- e$res
  d0 <- ang(r$winddir, r$wd); k <- is.finite(d0)
  say("3.8: wind-direction disagreement", sprintf("differ by a median of %s degrees, with a 95th percentile of %s degrees, and their angular offsets", pc(median(d0[k])), pc(quantile(d0[k], .95))), MS)
  lat0 <- 39.81000446758592; lon0 <- -104.95562509611672; p_ <- pi / 180
  dlon <- (r$Longitude - lon0) * p_; y <- sin(dlon) * cos(r$Latitude * p_); x <- cos(lat0 * p_) * sin(r$Latitude * p_) - sin(lat0 * p_) * cos(r$Latitude * p_) * cos(dlon)
  brg <- (atan2(y, x) / p_) %% 360; d1 <- abs(ang(r$winddir, brg) - ang(r$wd, brg)); k1 <- is.finite(d1)
  say("3.8: off-axis disagreement (P05 [GEOM])", sprintf("differ by a median of %s degrees, with a 95th percentile of %s degrees.", pc(median(d1[k1])), pc(quantile(d1[k1], .95))), MS)
  rm(r, e); invisible(gc())
} else skip("3.8 wind disagreement", "mobile_hrrr.RData not present")
WS <- file.path(BASE, "mobile_wswd.RData")
if (file.exists(WS)) { e <- new.env(); suppressWarnings(load(WS, envir = e)); o <- get(ls(e)[1], envir = e)
  say("2.3: median station distance", sprintf("nearest meteorological monitoring station used was %s km", rh(median(o$dist_km, na.rm = TRUE), 1)), MS); rm(o, e); invisible(gc())
} else skip("2.3 station distance", "mobile_wswd.RData not present")

# ==========================================================================
hdr("J. Methane  <- methane_hotspot_summary, cent_out_methane_persistent, methane_at_toxics_hotspots, methane_sourceprob")
mh <- need("methane_hotspot_summary.csv"); mc <- need("cent_out_methane_persistent.csv"); ma <- need("methane_at_toxics_hotspots.csv")
say("3.7: p99, clusters", sprintf("(≥ 99th percentile, %s ppm) into %d spatial clusters, of which two", rh(mh$p99, 2), mh$n_clusters), MS)
say("S8: p99 / p95 / events / days / clusters", sprintf("campaign-wide 99th percentile (%s ppm; the 95th percentile was %s ppm). The %s high-methane events (observed on %d of 193 days) grouped into %d spatial clusters", rh(mh$p99, 3), rh(mh$p95, 3), cm(mh$n_high_events), mh$n_days_high, mh$n_clusters), SI)
c1 <- mc[cluster == 1]; c2 <- mc[cluster == 2]
say("3.7: largest methane hotspot", sprintf("(%s° N, %s° W; events on %d of 193 days, maximum %s ppm)", rh(c1$lat, 3), rh(-c1$lon, 3), c1$n_days, rh(c1$ch4_max, 1)), MS)
say("3.7: second methane hotspot", sprintf("(%s° N, %s° W; %d days)", rh(c2$lat, 3), rh(-c2$lon, 3), c2$n_days), MS)
say("S8: largest methane hotspot", sprintf("(%s° N, %s° W; %s events on %d days; maximum %s ppm)", rh(c1$lat, 4), rh(-c1$lon, 4), cm(c1$n_events), c1$n_days, rh(c1$ch4_max, 1)), SI)
hav <- function(la, lo, LA, LO) { R <- 6371008.8; p_ <- pi/180; a <- sin((LA-la)*p_/2)^2 + cos(la*p_)*cos(LA*p_)*sin((LO-lo)*p_/2)^2; 2*R*asin(pmin(1, sqrt(a))) }
M <- need("MASTER_hotspot_group_index.csv")
d40 <- hav(c2$lat, c2$lon, M[group_id == 40, Latitude], M[group_id == 40, Longitude]) / 1000; d29 <- hav(c1$lat, c1$lon, M[group_id == 29, Latitude], M[group_id == 29, Longitude]) / 1000
say("3.7: distances to groups 40 and 29", sprintf("The second lies %s km from air-toxics Group 40 and the largest %s km from Group 29", rh(d40, 1), rh(d29, 1)), MS)
say("3.7: co-elevation range", sprintf("ranged from %s%% to %s%%", rh(min(ma$pct_ge_p95), 1), rh(max(ma$pct_ge_p95), 1)), MS)
t34 <- ma[group_id == 34]; t40 <- ma[group_id == 40]
say("3.7: groups 34 and 40", sprintf("strongest at Group 34 (%s%%) and Group 40 (%s%%, with high-methane events on %s sampling days)", rh(t34$pct_ge_p95, 1), rh(t40$pct_ge_p95, 1), c("one","two","three","four","five","six","seven")[t40$days_with_high]), MS)
say("3.7: groups below 5%", sprintf("whereas %d of the %d groups", sum(ma$pct_ge_p95 < 5), nrow(ma)), MS)
MP <- file.path(BASE, "methane_sourceprob.RData")
if (file.exists(MP)) { e <- new.env(); load(MP, envir = e); s <- e$sourceprob_ch4; M_ <- s$M; i <- which(M_ == max(M_), arr.ind = TRUE)[1, ]
  xs <- seq(s$xr[1], s$xr[2], length.out = ncol(M_)); ys <- seq(s$yr[1], s$yr[2], length.out = nrow(M_))
  lat <- s$center["lat"] + ys[i[1]] / 111320; lon <- s$center["lon"] + xs[i[2]] / (111320 * cos(s$center["lat"] * pi / 180))
  say("3.7 / S8: methane source-probability maximum", sprintf("(maximum at %s° N, %s° W", rh(lat, 3), rh(-lon, 3)), BOTH)
  say("S8: same maximum in the SI", sprintf("(maximum at %s° N, %s° W)", rh(lat, 3), rh(-lon, 3)), SI)
} else skip("3.7 methane source maximum", "methane_sourceprob.RData not present")

# ==========================================================================
hdr("K. Smoke and season  <- TABLE_smoke_comparison.csv, TABLE_seasonal.csv")
sm <- need("TABLE_smoke_comparison.csv"); sv <- function(p, c) sm[pollutant == p & class == "none"][[c]]
say("S3.2: benzene / toluene / xylene ratios", sprintf("benzene daily medians were unchanged (ratio %s), toluene and xylene were modestly lower on smoke days (ratios %s and %s; Wilcoxon p = %s and %s)",
    rh(sv("Benzene","ratio_smoke_over_none"), 2), rh(sv("Toluene","ratio_smoke_over_none"), 2), rh(sv("Xylene","ratio_smoke_over_none"), 2), rh(sv("Toluene","p_wilcoxon_smoke_vs_none"), 3), rh(sv("Xylene","p_wilcoxon_smoke_vs_none"), 3)), SI)
say("S3.2: H2S under light overlay", sprintf("(%s vs %s ppb) but not significantly so (p = %s)", rh(sm[pollutant == "H2S" & class == "light", median_of_day_medians], 1), rh(sv("H2S","median_of_day_medians"), 1), rh(sv("H2S","p_wilcoxon_smoke_vs_none"), 2)), SI)
se_ <- need("TABLE_seasonal.csv"); sz <- function(p, s) se_[pollutant == p & season == s, med]
say("S3.3: benzene by season", sprintf("benzene %s ppb in DJF vs %s ppb in other seasons", rh(sz("Benzene","DJF"), 2), rh(sz("Benzene","MAM"), 2)), SI)
tl <- c(sz("Toluene","MAM"), sz("Toluene","JJA"), sz("Toluene","SON"))
say("S3.3: toluene by season", sprintf("toluene %s vs %s-%s ppb", rh(sz("Toluene","DJF"), 2), rh(min(tl), 2), rh(max(tl), 2)), SI)
say("S3.3: H2S by season", sprintf("(%s ppb in DJF, %s in MAM and JJA, %s in SON)", rh(sz("H2S","DJF"), 2), rh(sz("H2S","MAM"), 2), rh(sz("H2S","SON"), 2)), SI)
say("S3.3: HCN by season", sprintf("a winter median of %s ppb falling to %s-%s ppb", rh(sz("HCN","DJF"), 2), rh(min(sz("HCN","MAM"), sz("HCN","JJA")), 1), rh(max(sz("HCN","MAM"), sz("HCN","JJA")), 0)), SI)

# ==========================================================================
hdr("L. Bootstrap  <- TABLE_bootstrap_ratio.csv, TABLE_bootstrap_blocks.csv")
br <- need("TABLE_bootstrap_ratio.csv"); bb <- need("TABLE_bootstrap_blocks.csv")
# Table S4.1 hazard-index columns (median basis after the 79 re-run)
if (have("TABLE_S4.1_background_sensitivity.csv")) {
  t41 <- need("TABLE_S4.1_background_sensitivity.csv")
  for (r in seq_len(nrow(t41))) say(sprintf("Table S4.1 HI columns, %s", t41$arm[r]),
    sprintf("| %s | %s | %s | %s |", rh(t41$HI_pwmean_Endocrine[r], 3), rh(t41$HI_pwmean_Respiratory[r], 3), rh(t41$HI_pwmean_Neurological[r], 3), rh(t41$HI_pwmean_Hematological[r], 3)), rows(SI))
  say("S4.6: max-block ranges", sprintf("ranging from %s to %s and from %s to %s, respectively", rh(min(t41$HI_maxblock_Endocrine), 2), rh(max(t41$HI_maxblock_Endocrine), 2), rh(min(t41$HI_maxblock_Respiratory), 2), rh(max(t41$HI_maxblock_Respiratory), 2)), SI)
}
say("S4.5: aggregate ratio and CI", sprintf("The aggregate ratio is %s with a 95%% bootstrap interval of %s-%s", rh(br$ratio_point, 2), rh(br$ci_lo, 2), rh(br$ci_hi, 2)), SI)
say("4: CI in the manuscript", sprintf("(risk ratio %s, 95%% CI: %s, %s)", rh(br$ratio_point, 2), rh(br$ci_lo, 2), rh(br$ci_hi, 2)), MS)
say("S4.5: block counts", sprintf("of the %d blocks whose point estimate exceeds twice the AirToxScreen value in this construction, %d remain above 2x in at least 80%% of bootstrap replicates and %d in at least 95%%", nrow(bb), sum(bb$pr_gt2 >= 0.8), sum(bb$pr_gt2 >= 0.95)), SI)

# The bootstrap (58) must re-aggregate the SAME block surface as section 3.3 (18/20).
# Until 2026-09-27 it rounded coordinates to 5 dp before the point-in-block join;
# roads are block boundaries, so 1.4% of points moved block and 100 -> 95.
if (exists("d") && "sBenzene_med_of_daily_med_scaled" %in% names(d) && have("TABLE_bootstrap_blocks.csv")) {
  bb <- fread(file.path(BASE, "TABLE_bootstrap_blocks.csv"), colClasses = list(character = "block"))
  m <- d$sBenzene_med_of_daily_med_scaled; a <- d$benzene_ppb; k <- is.finite(m) & is.finite(a)
  gt2 <- as.character(d$GEOID20[k][m[k] / a[k] > 2])
  same <- length(gt2) == nrow(bb) && setequal(gt2, bb$block)
  if (same) n_ok <<- n_ok + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("  [%s] %-46s bootstrap >2x set (%d blocks) %s section 3.3's (%d)\n", if (same) "OK  " else "FAIL",
              "S4.5: bootstrap reproduces the >2x block set", nrow(bb), if (same) "==" else "!=", length(gt2)))
  if (!same) cat("         re-run R_scripts/58_bootstrap_blocks.R (exact st_join, 2026-09-27), then update S4.5 / section 4 CI text\n")
}

# ==========================================================================
hdr("M. Hazard screen  <- TABLE_S7.1(b), S7.2, S7.3, S7.4")
h1 <- need("TABLE_S7.1b_hazard_index_by_organ.csv"); h7 <- need("TABLE_S7.1_chronic_hazard.csv"); h3 <- need("TABLE_S7.3_scaling_scenarios.csv"); h4 <- need("TABLE_S7.4_breakeven_factors.csv"); h2 <- need("TABLE_S7.2_acute_screen.csv")
SIrow <- rows(SI); ho <- function(o, c) h1[target_organ == o][[c]]
say("Table S7.1: organ HI rows", sprintf("| Endocrine (HCN) | %s | | %s | Respiratory (H2S) | %s | | %s | Neurological (toluene + xylenes + 1,2,4-TMB) | %s | | %s | Hematological (benzene) | %s | | %s |",
    rh(ho("Endocrine","HI_pwmean"), 3), rh(ho("Endocrine","HI_maxblock"), 2), rh(ho("Respiratory","HI_pwmean"), 3), rh(ho("Respiratory","HI_maxblock"), 2),
    rh(ho("Neurological","HI_pwmean"), 3), rh(ho("Neurological","HI_maxblock"), 3), rh(ho("Hematological","HI_pwmean"), 3), rh(ho("Hematological","HI_maxblock"), 3)), SIrow)
s3 <- function(sc, o, c) h3[scenario == sc & organ == o][[c]]
sayx("Table S7.3: pop-weighted rows", sprintf(rxq("| Endocrine (HCN) | %s | %s | %s | %s | Respiratory (H2S) | %s | %s | %s | %s | Neurological (toluene + xylenes + TMB) | %s | %s | %s | %s | Hematological (benzene) | %s | %s | %s | %s |"),
    rt(s3("A_none","Endocrine","HI_pwmean"),3), rt(s3("B_aromatics","Endocrine","HI_pwmean"),3), rt(s3("C_borrowed","Endocrine","HI_pwmean"),3), rt(s3("D_upper","Endocrine","HI_pwmean"),3),
    rt(s3("A_none","Respiratory","HI_pwmean"),3), rt(s3("B_aromatics","Respiratory","HI_pwmean"),3), rt(s3("C_borrowed","Respiratory","HI_pwmean"),3), rt(s3("D_upper","Respiratory","HI_pwmean"),3),
    rt(s3("A_none","Neurological","HI_pwmean"),3), rt(s3("B_aromatics","Neurological","HI_pwmean"),3), rt(s3("C_borrowed","Neurological","HI_pwmean"),3), rt(s3("D_upper","Neurological","HI_pwmean"),3),
    rt(s3("A_none","Hematological","HI_pwmean"),3), rt(s3("B_aromatics","Hematological","HI_pwmean"),3), rt(s3("C_borrowed","Hematological","HI_pwmean"),3), rt(s3("D_upper","Hematological","HI_pwmean"),3)), SIrow)
sayx("Table S7.3: most-exposed-block rows", sprintf(rxq("| Endocrine | %s | %s | %s | %s | Respiratory | %s | %s | %s | %s | Neurological | %s | %s | %s | %s | Hematological | %s | %s | %s | %s |"),
    rt(s3("A_none","Endocrine","HI_maxblock"),2), rt(s3("B_aromatics","Endocrine","HI_maxblock"),2), rt(s3("C_borrowed","Endocrine","HI_maxblock"),2), rt(s3("D_upper","Endocrine","HI_maxblock"),2),
    rt(s3("A_none","Respiratory","HI_maxblock"),2), rt(s3("B_aromatics","Respiratory","HI_maxblock"),2), rt(s3("C_borrowed","Respiratory","HI_maxblock"),2), rt(s3("D_upper","Respiratory","HI_maxblock"),2),
    rt(s3("A_none","Neurological","HI_maxblock"),2), rt(s3("B_aromatics","Neurological","HI_maxblock"),2), rt(s3("C_borrowed","Neurological","HI_maxblock"),2), rt(s3("D_upper","Neurological","HI_maxblock"),2),
    rt(s3("A_none","Hematological","HI_maxblock"),2), rt(s3("B_aromatics","Hematological","HI_maxblock"),2), rt(s3("C_borrowed","Hematological","HI_maxblock"),2), rt(s3("D_upper","Hematological","HI_maxblock"),2)), SIrow)
say("S7.1: aromatic scaling effect", sprintf("raises the population-weighted neurological index from %s to %s and the hematological index from %s to %s, raises the most-exposed-block neurological index from %s to %s",
    rh(s3("A_none","Neurological","HI_pwmean"),3), rh(s3("B_aromatics","Neurological","HI_pwmean"),3), rh(s3("A_none","Hematological","HI_pwmean"),3), rh(s3("B_aromatics","Hematological","HI_pwmean"),3),
    rh(s3("A_none","Neurological","HI_maxblock"),3), rh(s3("B_aromatics","Neurological","HI_maxblock"),3)), SI)
# both bases side by side (TABLE_S7.1c) -> S7.2 / S7.3 / 3.3 prose
if (have("TABLE_S7.1c_basis_comparison.csv")) {
  hc <- need("TABLE_S7.1c_basis_comparison.csv"); g <- function(b, o, c) hc[basis == b & target_organ == o][[c]]
  say("S7.2: mean basis stated", sprintf("the community endocrine index is %s and the respiratory index %s, with most-exposed-block values of %s and %s", rh(g("mean_of_daily_means","Endocrine","HI_pwmean"), 2), rh(g("mean_of_daily_means","Respiratory","HI_pwmean"), 2), rh(g("mean_of_daily_means","Endocrine","HI_maxblock"), 2), rh(g("mean_of_daily_means","Respiratory","HI_maxblock"), 2)), SI)
  say("S7.3: community indices on both bases", sprintf("the community indices are %s (endocrine), %s (respiratory), %s (neurological) and %s (hematological), against %s, %s, %s and %s on the median basis", rh(g("mean_of_daily_means","Endocrine","HI_pwmean"), 2), rh(g("mean_of_daily_means","Respiratory","HI_pwmean"), 2), rh(g("mean_of_daily_means","Neurological","HI_pwmean"), 3), rh(g("mean_of_daily_means","Hematological","HI_pwmean"), 3), rh(g("median_of_daily_medians","Endocrine","HI_pwmean"), 2), rh(g("median_of_daily_medians","Respiratory","HI_pwmean"), 2), rh(g("median_of_daily_medians","Neurological","HI_pwmean"), 3), rh(g("median_of_daily_medians","Hematological","HI_pwmean"), 3)), SI)
  say("S7.3: most-exposed-block indices on both bases", sprintf("the most-exposed-block values are %s, %s, %s and %s against %s, %s, %s and %s", rh(g("mean_of_daily_means","Endocrine","HI_maxblock"), 2), rh(g("mean_of_daily_means","Respiratory","HI_maxblock"), 2), rh(g("mean_of_daily_means","Neurological","HI_maxblock"), 2), rh(g("mean_of_daily_means","Hematological","HI_maxblock"), 2), rh(g("median_of_daily_medians","Endocrine","HI_maxblock"), 2), rh(g("median_of_daily_medians","Respiratory","HI_maxblock"), 2), rh(g("median_of_daily_medians","Neurological","HI_maxblock"), 2), rh(g("median_of_daily_medians","Hematological","HI_maxblock"), 2)), SI)
  say("3.3: hazard indices, median basis", sprintf("population-weighted hazard indices of %s for endocrine effects (driven by HCN) and %s for respiratory effects", rh(g("median_of_daily_medians","Endocrine","HI_pwmean"), 2), rh(g("median_of_daily_medians","Respiratory","HI_pwmean"), 2)), MS)
  say("3.3: hazard indices, mean basis", sprintf("On the block mean of daily means the community endocrine index is %s and the respiratory index %s, with most-exposed-block values of %s and %s", rh(g("mean_of_daily_means","Endocrine","HI_pwmean"), 2), rh(g("mean_of_daily_means","Respiratory","HI_pwmean"), 2), rh(g("mean_of_daily_means","Endocrine","HI_maxblock"), 2), rh(g("mean_of_daily_means","Respiratory","HI_maxblock"), 2)), MS)
  say("4: hazard conclusion on both bases", sprintf("at the community average the endocrine index is %s on the median-of-daily-medians basis used for the benzene comparison and %s on the mean-of-daily-means basis", rh(g("median_of_daily_medians","Endocrine","HI_pwmean"), 2), rh(g("mean_of_daily_means","Endocrine","HI_pwmean"), 2)), MS)
} else skip("S7 basis comparison", "TABLE_S7.1c_basis_comparison.csv not present (run 74)")
if (have("TABLE_cumulative_HI_summary.csv")) {
  cu <- need("TABLE_cumulative_HI_summary.csv"); cg <- function(t, m, c) cu[tos == t & metric == m][[c]]
  say("S7.3: cell-level median metric", sprintf("on the cell median the endocrine index exceeds 1 in %s%% of cells (maximum %s) and the respiratory index in one cell (maximum %s)", pc(cg("Endocrine","median","pct_cells_HI_gt1")), rh(cg("Endocrine","median","HI_max"), 2), rh(cg("Respiratory","median","HI_max"), 2)), SI)
  say("S7.3: cell-level mean metric", sprintf("on the cell mean the endocrine index exceeds 1 in %s%% of cells (maximum %s) and the respiratory index in the most-exposed cells (maximum %s)", pc(cg("Endocrine","mean","pct_cells_HI_gt1")), rh(cg("Endocrine","mean","HI_max"), 2), rh(cg("Respiratory","mean","HI_max"), 2)), SI)
}
say("S7.4: community endocrine under C and D", sprintf("from %s (A, B) to %s and %s (C, D)", rh(s3("A_none","Endocrine","HI_pwmean"), 2), rh(s3("C_borrowed","Endocrine","HI_pwmean"), 2), rh(s3("D_upper","Endocrine","HI_pwmean"), 2)), SI)
b4 <- function(o, c) h4[organ == o][[c]]
say("S7.4: break-even factors (pw)", sprintf("the endocrine index reaches 1 at an HCN factor of %s, and the respiratory index at an H", rh(b4("Endocrine","f_breakeven_pwmean"), 2)), SI)
say("S7.4: break-even respiratory (pw)", sprintf("S factor of %s.", rh(b4("Respiratory","f_breakeven_pwmean"), 2)), SI)
say("S7.4: neurological break-even (max block)", sprintf("for the neurological system this gives %s, and it need not be", rh(b4("Neurological","f_breakeven_maxblock"), 2)), SI)
say("S7.3: OEHHA chronic re-anchoring", sprintf("raise the most-exposed-block hazard quotients to %s (benzene, hematological) and %s (1,2,4-trimethylbenzene, neurological)", rh(h7[pollutant == "Benzene", maxblock_ugm3] / 3, 2), rh(h7[pollutant == "1,2,4-Trimethylbenzene", maxblock_ugm3] / 4, 2)), SI)
say("S7.4: break-even (max block)", sprintf("the corresponding thresholds are %s for HCN and %s for H", rh(b4("Endocrine","f_breakeven_maxblock"), 2), rh(b4("Respiratory","f_breakeven_maxblock"), 2)), SI)
say("Table S7.4: neurological row", sprintf("| %s / %s | %s / %s |", rh(b4("Neurological","HI_pwmean_unscaled"), 3), rh(b4("Neurological","HI_maxblock_unscaled"), 2), rh(b4("Neurological","f_breakeven_pwmean"), 2), rh(b4("Neurological","f_breakeven_maxblock"), 2)), SIrow)
say("Table S7.4: endocrine row", sprintf("| %s / %s | %s / %s |", rh(b4("Endocrine","HI_pwmean_unscaled"), 3), rh(b4("Endocrine","HI_maxblock_unscaled"), 2), rh(b4("Endocrine","f_breakeven_pwmean"), 2), rh(b4("Endocrine","f_breakeven_maxblock"), 2)), SIrow)
ac <- function(p, c) h2[pollutant == p][[c]]
say("S7.2: acute HQ at campaign max", sprintf("benzene (HQ ~ %s), H2S (HQ ~ %s) and toluene (HQ ~ %s) exceed the 1-hour REL", rh(ac("Benzene","HQ_max"), 0), rh(ac("H2S","HQ_max"), 1), rh(ac("Toluene","HQ_max"), 1)), SI)
say("S7.2: acute TMB peak", sprintf("the trimethylbenzene peak reaches %s%% of its REL (HQ ~ %s)", rh(100 * ac("1,2,4-Trimethylbenzene","HQ_max"), 0), rh(ac("1,2,4-Trimethylbenzene","HQ_max"), 2)), SI)
say("S7.2: acute p99", sprintf("(largest HQ %s, benzene)", rh(max(h2$HQ_p99), 2)), SI)

# ==========================================================================
hdr("N. Claims this script does NOT vouch for (sourced from documents, not code)")
cat("  - Permit and TRI quantities in S6.1 / S6.7 / Table S6.1 (119.01 and 2.38 t/yr; 340 lb/yr; 8 t/yr digester gas; 5,819 lb and 22,373 lb TRI)\n")
cat("  - Literature values in S6.4 (>= 15 transects; >= 10 transects; ~95% within +/-70%; slope 0.96; 266 plumes; ~4%)\n")
cat("  - Instrument specifications in S1 / S2 (cadences, calibration ranges, tubing, flow rates, MaxiMet 92.2%)\n")
cat("  - AirToxScreen / IRIS / OEHHA reference values (RfCs, unit risks, RELs, MRLs)\n")
cat("  - The 2.3 wind-station fallback statistics (218,527; 8.6%; 0.9 km) - reconstructed 2026-09-23 from 06_merge_with_wind.R, no artifact written\n")
cat("  - HQ-screen record counts (47,643 / 2,602,928 etc.) - checked by 72_check_s14_qaqc.R, not here\n")
cat("  - Hour-of-day, weekday, correlation and speed statistics - checked by tests/audit_manuscript_claims.R\n")

cat(sprintf("\n%d OK, %d FAIL, %d SKIP\n", n_ok, n_fail, n_skip))
if (n_fail > 0) { cat("\nFAIL means the document and the pipeline outputs disagree.\n"); quit(status = 1) }
