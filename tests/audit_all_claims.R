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
# Status 2026-09-30: the PRIMARY statistic for the maps (Figure 2), the benzene
# block comparison and the hazard screen is the MEAN of daily means (script
# defaults EXPOSURE_BASIS / HAZARD_BASIS = mean); the median of daily medians is
# the supplementary analysis (SI section S4.7, S7.3; *_medianbasis outputs). Claims whose source is a document rather than code (permit records,
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
say("3.3: residents across common blocks", sprintf("The %s census blocks in which both AirToxScreen and mobile monitoring benzene concentrations were available house %s residents", cm(mob$n_blocks), cm(mob$total_population_used)), MS)
say("3.3: cancer cases", sprintf("was %s-%s cases", rh(mob$risk_5_75, 3), rh(mob$risk_20_40, 3)), MS)
say("4: risk ratio", sprintf("risk ratio %s", rh(mob$pop_weighted_mean_ppb / ats$pop_weighted_mean_ppb, 2)), MS)

BLK <- file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData")
if (file.exists(BLK)) {
  suppressPackageStartupMessages(library(sf))
  e <- new.env(); suppressWarnings(load(BLK, envir = e)); d <- sf::st_drop_geometry(get(ls(e)[1], envir = e))
  m <- d$sBenzene_mean_of_daily_mean_scaled; a <- d$benzene_ppb; k <- is.finite(m) & is.finite(a); r <- m[k] / a[k]
  # supplementary basis (S4.7): the same statistics on the block median of daily medians
  .m2 <- d$sBenzene_med_of_daily_med_scaled; .k2 <- is.finite(.m2) & is.finite(a); .r2 <- .m2[.k2] / a[.k2]
  say("S4.7: Pearson / Spearman (median basis)", sprintf("(Pearson r = %s, Spearman r = %s)", rh(cor(.m2[.k2], a[.k2]), 2), rh(cor(.m2[.k2], a[.k2], method = "spearman"), 2)), SI)
  say("S4.7: mobile block maximum (median basis)", sprintf("the mobile blocks reach %s ppb", rh(max(.m2[.k2]), 1)), SI)
  say("S4.7: blocks > 2x / 5x / max (median basis)", sprintf("exceed AirToxScreen by more than a factor of two in %d of the %s blocks (%s%%), by more than a factor of five in %d and by a factor of %d",
      sum(.r2 > 2), cm(sum(.k2)), rh(100 * mean(.r2 > 2), 1), sum(.r2 > 5), round(max(.r2))), SI)
  say("S4.7: blocks below AirToxScreen (median basis)", sprintf("lower than AirToxScreen in %s%% of blocks (median ratio %s)", pc(100 * mean(.r2 < 1)), rh(median(.r2), 2)), SI)
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
  # PRIMARY BASIS (2026-09-30): mean of daily means, as in section 3.3; the
  # median of daily medians is the supplementary basis.
  MV <- 8.314 * 298.15 / 83000 * 1000
  hqb <- function(b) cbind(d[[paste0("sToluene_", b)]] * 92.14 / MV / 5000, d[[paste0("sXylene_", b)]] * 106.16 / MV / 100,
                           d[[paste0("sTrimethylbenzene_", b)]] * 120.19 / MV / 60)
  hq <- hqb("mean_of_daily_mean"); ok <- rowSums(!is.finite(hq)) == 0 & is.finite(d$POP20)
  hqm <- hqb("mean_of_daily_mean"); okm <- rowSums(!is.finite(hqm)) == 0 & is.finite(d$POP20)
  say("S7.1: neurological within-block maximum", sprintf("the largest index within any single block is %s (block %s)", rh(max(rowSums(hq[ok, ])), 3), d$GEOID20[ok][which.max(rowSums(hq[ok, ]))]), SI)
  say("S7.1: eligible blocks (neurological)", sprintf("(%s blocks for the neurological system)", cm(sum(ok))), SI)
  say("S7.1: sum of separate maxima on the mean basis (not used)", sprintf("their sum, %s, exceeds the largest within-block index of %s", rh(sum(apply(hqm, 2, max, na.rm = TRUE)), 3), rh(max(rowSums(hqm[okm, ])), 3)), SI)
  say("S7.2: neurological most-exposed block", sprintf("and %s at the most-exposed block", rh(max(rowSums(hq[ok, ])), 2)), SI)
  # both bases, benzene comparison (section 3.3 / S4.3) - from the block file itself
  a <- d$benzene_ppb; pw <- function(x) { k <- is.finite(x) & is.finite(a) & is.finite(d$POP20) & d$POP20 > 0; sum(x[k] * d$POP20[k]) / sum(d$POP20[k]) }
  mm <- d$sBenzene_mean_of_daily_mean_scaled; md <- d$sBenzene_med_of_daily_med_scaled; k2 <- is.finite(mm) & is.finite(md) & is.finite(a) & is.finite(d$POP20) & d$POP20 > 0
  say("3.3: median-basis benzene (supplementary)", sprintf("the population-weighted mobile benzene is %s ppb, %s-%s excess cases, a mobile-to-AirToxScreen ratio of %s and %d blocks above twice", rh(pw(md), 3),
      rh(5.75 * sum(md[k2] * d$POP20[k2]) / 1e6, 3), rh(20.40 * sum(md[k2] * d$POP20[k2]) / 1e6, 3), rh(pw(md) / pw(a), 2), sum(md[k2] / a[k2] > 2)), MS)
  say("S4.3: median-basis benzene", sprintf("the population-weighted mobile benzene is %s ppb (%s-%s excess cases), the ratio to AirToxScreen is %s, and %d rather than %d blocks exceed twice", rh(pw(md), 3),
      rh(5.75 * sum(md[k2] * d$POP20[k2]) / 1e6, 3), rh(20.40 * sum(md[k2] * d$POP20[k2]) / 1e6, 3), rh(pw(md) / pw(a), 2), sum(md[k2] / a[k2] > 2), sum(mm[k2] / a[k2] > 2)), SI)
  say("S4.7: median-basis benzene and risk", sprintf("the population-weighted mobile benzene over the %s common blocks is %s ppb against %s ppb for AirToxScreen (ratio %s), and the excess lifetime cancer risk is %s-%s cases against %s-%s cases",
      cm(sum(k2)), rh(pw(md), 3), rh(pw(a), 3), rh(pw(md) / pw(a), 2), rh(5.75 * sum(md[k2] * d$POP20[k2]) / 1e6, 3), rh(20.40 * sum(md[k2] * d$POP20[k2]) / 1e6, 3),
      rh(5.75 * sum(a[k2] * d$POP20[k2]) / 1e6, 3), rh(20.40 * sum(a[k2] * d$POP20[k2]) / 1e6, 3)), SI)
  say("S4.7: block mean > median share and factor", sprintf("for benzene the block means exceed the block medians in %s%% of blocks, by a median factor of %s", pc(100 * mean(mm[k2] > md[k2])), rh(median((mm / md)[k2 & md > 0]), 1)), SI)
  say("S7.3: mean > median share and factor", sprintf("The block means exceed the block medians in %s%% of blocks, by a median factor of %s for benzene", pc(100 * mean(mm[k2] > md[k2])), rh(median((mm / md)[k2 & md > 0]), 1)), SI)
} else skip("block-file checks (3.3, 2.5.1, S7.1)", "censusblocks_..._overlap.RData not present")

# ==========================================================================
hdr("B. Figure 2 cell surface  <- segment500_summaries_acrossSites.RData")
SEG <- file.path(BASE, "segment500_summaries_acrossSites.RData")
if (file.exists(SEG)) {
  suppressPackageStartupMessages(library(sf))
  e <- new.env(); suppressWarnings(load(SEG, envir = e)); pd <- sf::st_drop_geometry(e$seg_wide_sf)
  cell <- function(p, md = 3, st = "mean_of_daily_means") { x <- suppressWarnings(as.numeric(pd[[paste0("bgcorr_", p, "_", st)]]))
    n <- suppressWarnings(as.numeric(pd[[paste0("bgcorr_", p, "_n_days_any")]])); x[is.finite(x) & is.finite(n) & n >= md] }
  ARO <- c("Benzene", "Toluene", "Trimethylbenzene", "Xylene")
  # primary: Figure 2 / section 3.3 on the mean of daily means; supplementary: Figure S4.13 / S4.7 on the median of daily medians
  for (.b in list(list(st = "mean_of_daily_means", doc = MS, tag = "3.3"), list(st = "median_of_daily_medians", doc = SI, tag = "S4.7"))) {
    cl <- function(p, md = 3) cell(p, md, .b$st); T <- .b$tag; D <- .b$doc
    b <- cl("Benzene"); av <- unlist(lapply(ARO, cl)); h <- cl("H2S"); hc <- cl("HCN")
    say(paste0(T, ": benzene cell range and count"), sprintf("from %s to %s ppb across the %d cells sampled on at least three days", rh(min(b), 2), rh(max(b), 2), length(b)), D)
    say(paste0(T, ": shared aromatic colour scale"), sprintf("color scale spanning %s to %s ppb", rh(quantile(av, .02), 2), rh(quantile(av, .98), 2)), D)
    top <- sort(b, decreasing = TRUE)[1:3]
    say(paste0(T, ": three highest benzene cells"), sprintf("three highest cells (%s, %s and %s ppb)", rh(top[3], 2), rh(top[2], 2), rh(top[1], 2)), D)
    say(paste0(T, ": H2S cell range and count"), sprintf("from %s to %s ppb across the %d cells", rh(min(h), 2), rh(max(h), 2), length(h)), D)
    say(paste0(T, ": H2S display scale"), sprintf("%s to %s ppb", rh(quantile(h, .02), 2), rh(quantile(h, .98), 2)), D)
    say(paste0(T, ": HCN median and count"), sprintf("median value of %s ppb across %d cells", rh(median(hc), 2), length(hc)), D)
    say(paste0(T, ": HCN display scale"), sprintf("%s to %s ppb", rh(quantile(hc, .02), 2), rh(quantile(hc, .98), 2)), D)
    .hs <- 100 * sum(hc >= 1.2 - 1e-9) / length(hc)
    say(paste0(T, ": HCN maximum and >= 1.2 ppb"), sprintf("(maximum %s ppb; %d of %d cells, %s, at or above 1.2 ppb)", rh(max(hc), 2), sum(hc >= 1.2 - 1e-9), length(hc),
        if (.hs < 1) "under 1%" else paste0(rh(.hs, 1), "%")), D)
    say(paste0(if (T == "3.3") "Figure 2" else "Figure S4.13", " caption: H2S / benzene minima"), sprintf("extend to %s ppb, well below the aromatics' minimum of %s ppb", rh(min(h), 2), rh(min(b), 2)), D)
    if (T == "3.3") {
      say("3.3: benzene <= 0.15 ppb share", sprintf("at or below the relatively small value of 0.15 ppb in %s%% of mapped cells", pc(100 * mean(b <= 0.15 + 1e-9))), MS)
      say("3.3: toluene maximum", sprintf("(maximum %s ppb)", rh(max(cl("Toluene")), 2)), MS)
      say("3.3: TMB and xylene maxima", sprintf("Trimethylbenzene (maximum %s ppb) and xylene (maximum %s ppb)", rh(max(cl("Trimethylbenzene")), 2), rh(max(cl("Xylene")), 2)), MS)
      say("3.3: cells > 0.8 ppb (T, TMB, X)", sprintf("(%d, %d and %d cells for toluene, trimethylbenzene and xylene)", sum(cl("Toluene") > 0.8 + 1e-9), sum(cl("Trimethylbenzene") > 0.8 + 1e-9), sum(cl("Xylene") > 0.8 + 1e-9)), MS)
      say("3.3: H2S share >= 1 ppb", sprintf("the %s%% of mapped cells at or above 1 ppb", pc(100 * mean(h >= 1 - 1e-9))), MS)
      say("3.3: HCN cells >= 2 ppb", sprintf("the %d cells at or above 2 ppb", sum(hc >= 2 - 1e-9)), MS)
    } else {
      say("S4.7: benzene <= 0.15 ppb share", sprintf("%s%% of cells are at or below 0.15 ppb", pc(100 * mean(b <= 0.15 + 1e-9))), SI)
      say("S4.7: aromatic maxima", sprintf("The maxima are %s ppb for toluene, %s ppb for trimethylbenzene and %s ppb for xylene", rh(max(cl("Toluene")), 2), rh(max(cl("Trimethylbenzene")), 2), rh(max(cl("Xylene")), 2)), SI)
      say("S4.7: H2S share >= 1 ppb", sprintf("with %s%% of cells at or above 1 ppb", pc(100 * mean(h >= 1 - 1e-9))), SI)
    }
  }
  # Table S3.2 sustained maxima: >= 10 sampled days, 24-h scaled for benzene/toluene/xylene
  sfF <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")
  if (file.exists(sfF)) {
    e2 <- new.env(); load(sfF, envir = e2); o <- get(ls(e2)[1], envir = e2)
    fac <- setNames(as.numeric(o$ratio_all_over_mobilelike), tolower(o$pollutant))
    mx <- function(p, f = 1, st = "mean_of_daily_means") max(cell(p, 10, st)) * f
    SIrow <- rows(SI)
    say("Table S3.2: benzene sustained max (>=10 d, scaled)", sprintf("| %s |", rh(mx("Benzene", fac["benzene"]), 2)), SIrow)
    say("Table S3.2: toluene sustained max", sprintf("| %s |", rh(mx("Toluene", fac["toluene"]), 2)), SIrow)
    sayx("Table S3.2: TMB sustained max (unscaled)", sprintf("\\| %s \\(unscaled\\) \\|", rt(mx("Trimethylbenzene"), 2)), SIrow)
    say("Table S3.2: xylene sustained max", sprintf("| %s |", rh(mx("Xylene", fac["xylene"]), 2)), SIrow)
    say("Table S3.2: H2S sustained max (unscaled)", sprintf("| %s (unscaled) |", rh(mx("H2S"), 2)), SIrow)
    say("Table S3.2: HCN sustained max (unscaled)", sprintf("| %s (unscaled) |", rh(mx("HCN"), 1)), SIrow)
    md2 <- function(p, f = 1) mx(p, f, "median_of_daily_medians")
    sayx("S4.7: Table S3.2 sustained maxima (median basis)", sprintf(rxq("are %s ppb for benzene, %s ppb for toluene, %s ppb for trimethylbenzene, %s ppb for xylene, %s ppb for H2S and %s ppb for HCN"),
        rt(md2("Benzene", fac["benzene"]), 2), rt(md2("Toluene", fac["toluene"]), 2), rt(md2("Trimethylbenzene"), 2), rt(md2("Xylene", fac["xylene"]), 2), rt(md2("H2S"), 2), rt(md2("HCN"), 1)), SI)
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
# Table S3.1 funnel (round 6): each stage row in processing order, and the funnel must close
for (.st in c("no_gps_flag", "one_per_second", "with_position", "outside_hq")) if (.st %in% names(s31))
  say(sprintf("Table S3.1: stage %s", .st), paste0("| ", paste(cm(s31[[.st]]), collapse = " | "), " |"), rows(SI))
if ("outside_hq" %in% names(s31)) cat(sprintf("  [%s] %-46s last funnel stage == analysis set\n",
    if (all((if ("one_per_bin" %in% names(s31)) s31$one_per_bin else s31$outside_hq) == s31$analysis)) "OK  " else "FAIL", "Table S3.1: funnel closes"))
say("2.1.1: HCN funnel", sprintf("the HCN record comprises %s delivered rows, of which %s remain in the analysis set", cm(s31[pollutant == "HCN", after_excl]), cm(s31[pollutant == "HCN", analysis])), MS)
if ("no_gps_flag" %in% names(s31)) { .gp <- 100 * (1 - s31$no_gps_flag / s31$after_excl)
  say("S1.4: GPS-flag share", sprintf("which removes %s-%s%% of each pollutant's record after the campaign exclusions", rh(min(.gp), 1), rh(max(.gp), 1)), SI) }
# delivered (un-averaged) H2S / HCN statistics beside the bin means (2026-09-27)
if (all(c("median_delivered", "p99_delivered", "max_delivered") %in% names(s31))) {
  .dv <- function(col) { v <- s31[[col]]; ifelse(is.finite(v), formatC(v, format = "f", digits = 2, big.mark = ","), "-") }   # docx_text() maps en-dash to hyphen
  for (.c in c("median_delivered", "p99_delivered", "max_delivered"))
    say(sprintf("Table S3.1: %s row", .c), paste0("| ", paste(.dv(.c), collapse = " | "), " |"), rows(SI))
  say("S7.1: H2S maximum on both series", sprintf("the hydrogen sulfide maximum is %s ppb as a bin mean and %s ppb as delivered",
      formatC(s31[pollutant == "H2S", max], format = "fg"), formatC(s31[pollutant == "H2S", max_delivered], format = "fg")), SI)
}
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

if (have("TABLE_mdl_sensitivity_blocks_medianbasis.csv")) { mbm <- need("TABLE_mdl_sensitivity_blocks_medianbasis.csv")
  say("S4.7: MDL substitution on the median basis", sprintf("to %s (zero), %s (MDL/2) and %s (MDL), and raises the median block-to-AirToxScreen ratio from %s to %s and %s",
      rh(mbm[case == "zero", r_vs_raw], 2), rh(mbm[case == "half", r_vs_raw], 2), rh(mbm[case == "full", r_vs_raw], 2),
      rh(mbm[case == "raw", median_ratio_vs_ATS], 2), rh(mbm[case == "half", median_ratio_vs_ATS], 2), rh(mbm[case == "full", median_ratio_vs_ATS], 2)), SI)
} else skip("S4.7: MDL substitution (median basis)", "TABLE_mdl_sensitivity_blocks_medianbasis.csv not present")
if (have("TABLE_S4.1_background_sensitivity_medianbasis.csv") && have("TABLE_S4.1b_background_sensitivity_cells_medianbasis.csv")) {
  t41m <- need("TABLE_S4.1_background_sensitivity_medianbasis.csv"); t41bm <- need("TABLE_S4.1b_background_sensitivity_cells_medianbasis.csv")
  .en <- t41bm[pollutant == "Endocrine" & is.finite(pct_HI_gt1), pct_HI_gt1]
  say("S4.7: background settings on the median basis", sprintf("the mobile-to-AirToxScreen ratio ranges from %s to %s and the community endocrine index from %s to %s; and the share of 500 m cells with an endocrine hazard index above 1 ranges from %s%% to %s%%",
      rh(min(t41m$ratio_mobile_over_airtox), 2), rh(max(t41m$ratio_mobile_over_airtox), 2), rh(min(t41m$HI_pwmean_Endocrine), 3), rh(max(t41m$HI_pwmean_Endocrine), 3), rh(min(.en), 0), rh(max(.en), 0)), SI)
  say("S4.6: median-basis ratio and endocrine ranges", sprintf("On the supplementary median basis (section S4.7) the ratio ranged from %s to %s.", rh(min(t41m$ratio_mobile_over_airtox), 2), rh(max(t41m$ratio_mobile_over_airtox), 2)), SI)
  say("S4.6: median-basis endocrine range", sprintf("the community endocrine range was %s-%s, below 1 throughout", rh(min(t41m$HI_pwmean_Endocrine), 3), rh(max(t41m$HI_pwmean_Endocrine), 3)), SI)
  .nb <- t41m[!(percentile == 20 & window_min == 20)]
  say("S4.6: median-basis identical cells", sprintf("%s-%s%% of cells retained exactly the same value", rh(min(.nb$cells_identical_pct), 0), rh(max(.nb$cells_identical_pct), 0)), SI)
  say("S4.6: median-basis cell endocrine share", sprintf("On the supplementary median basis it varied substantially, from %s%% to %s%%", rh(min(.en), 0), rh(max(.en), 0)), SI)
}

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
say("S4.4: weekend ratios", sprintf("%s-%s%% lower than the driving-window means (ratios %s, %s, and %s)", pc(100 * (1 - max(wk))), pc(100 * (1 - min(wk))), rh(wk[1], 2), rh(wk[2], 2), rh(wk[3], 2)), SI)
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
say("2.5.3: HCN threshold", sprintf("S; n = %s and %d days for HCN)", rh(tv("hydrogen_cyanide")$n_cutoff_p90, 0), tv("hydrogen_cyanide")$n_days_cutoff_p90), MS)
say("2.5.3: initial clusters", sprintf("yielded %s initial clusters across the six pollutants (%d benzene, %d toluene, %d trimethylbenzene, %d xylene, %d H",
    cm(sum(th$n_clusters_all)), tv("benzene")$n_clusters_all, tv("toluene")$n_clusters_all, tv("trimethylbenzene")$n_clusters_all, tv("xylene")$n_clusters_all, tv("hydrogen_sulfide")$n_clusters_all), MS)
say("2.5.3: HCN clusters", sprintf("S and %d HCN)", tv("hydrogen_cyanide")$n_clusters_all), MS)
# (2026-09-30) H2S event threshold on the 5-s bin values, retained clusters, grouping counts
say("2.5.3: H2S event threshold", sprintf("; %s ppb for H", rh(tv("hydrogen_sulfide")$b99_ppb, 1)), MS)
say("2.5.3: retained clusters", sprintf("retaining %s clusters", paste(paste(th$n_persistent_clusters[1:5], collapse = ", "), "and", th$n_persistent_clusters[6])), MS)
.ssg <- need("summary_stats_persistent.csv")
say("2.5.3: grouping counts", sprintf("These %d persistent single-pollutant clusters were then spatially grouped across pollutants into %d candidate hotspot groups. Of these, %d contained at least two pollutants, %d contained three or more, and %d contained four or more",
    sum(th$n_persistent_clusters), .ssg$n_groups, .ssg$n_groups_2plus_pollutants, .ssg$n_groups_3plus_pollutants, .ssg$n_groups_4plus_pollutants), MS)
say("2.5.3: largest mixture", sprintf("within a single group was %s", c("three","four","five","six")[.ssg$max_pollutants_in_group - 2]), MS)
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
say("3.4.2: group 4 TRI distance", sprintf("lies %s km from the Phillips 66 terminal", rh(gm(4)$tri_dist_km, 2)), MS)
say("3.4.2: groups 40 and 43", sprintf("at %s km (Owens Corning Roofing and Asphalt) and %s km (KBP Coil Coaters)", rh(gm(40)$tri_dist_km, 2), rh(gm(43)$tri_dist_km, 2)), MS)
say("3.4.2: groups 22 and 29", sprintf("with %d and %d measurements within 100 m", g5(22)$n_rows_100m, g5(29)$n_rows_100m), MS)
say("3.4.2: group 13 TRI distance", sprintf("Group 13, %s km from a glass-container plant", rh(gm(13)$tri_dist_km, 2)), MS)
say("3.4.2: group 11", sprintf("with %s mobile measurements within 100 m, and its benzene cluster registered exceedances on %d distinct days (the group persistence metric", cm(g5(11)$n_rows_100m), gm(11)$max_n_days), MS)
say("3.4.2: group 60", sprintf("%s km from the Sinclair Denver products terminal, and is persistent for HCN, trimethylbenzene and xylene; its xylene cluster registered exceedances on %d distinct days", rh(gm(60)$tri_dist_km, 2), gm(60)$max_n_days), MS)
say("3.4.2: groups 12 and 28", sprintf("their xylene clusters registering exceedances on %d and %d distinct days (the group persistence metric)", gm(12)$max_n_days, gm(28)$max_n_days), MS)
say("3.4.2: group 9", sprintf("Group 9 lies closer to a TRI facility than any other group in the study, %s km from the Sinclair Denver products terminal", rh(gm(9)$tri_dist_km, 2)), MS)
# Figure 4D: median ratios within 100 m (34_fancy_plots_of_hotspots.R) and the composition classes
RA <- dcast(need("hotspot_source_fingerprint_outputs/hotspot_ratio_summary_ALL.csv"), group_id ~ ratio, value.var = "median")
o_tb <- RA[order(-T_B)]$group_id; o_tmb <- RA[order(-TMB_B)]$group_id
stopifnot(o_tb[1] == 4, o_tmb[1] == 13, setequal(o_tb[2:3], c(40, 43)), setequal(o_tmb[2:3], c(40, 43)))
say("3.4.1: Figure 4D ratio leaders", "the aromatics-only Groups 40 and 43 have the second- and third-highest toluene/benzene and trimethylbenzene/benzene ratios, behind Group 4 for toluene/benzene and Group 13 for trimethylbenzene/benzene", MS)
rest <- RA[!group_id %in% c(4, 13, 40, 43)]
say("3.4.1: Figure 4D remaining range", sprintf("the remaining %s groups lie within toluene/benzene ratios of %s to %s and trimethylbenzene/benzene ratios of %s to %s",
    c("ten","eleven","twelve")[nrow(rest) - 9], rh(min(rest$T_B), 1), rh(max(rest$T_B), 1), rh(min(rest$TMB_B), 2), rh(max(rest$TMB_B), 2)), MS)
say("3.4.2: group 13 TMB/B", sprintf("highest trimethylbenzene/benzene ratio of any group (%s; Figure 4D)", rh(RA[group_id == 13]$TMB_B, 2)), MS)
cls <- ifelse(grepl("h2s|hcn", M$pollutants), "R", ifelse(grepl("trimethylbenzene", M$pollutants), "P", "B"))
say("3.4.2: petroleum VOC class", sprintf("Petroleum VOC hotspots (Groups %s)", paste(sort(M$group_id[cls == "P"]), collapse = ", ")), MS)
say("3.4.2: reduced-species class", sprintf("Reduced-species hotspots (Groups %s)", paste(sort(M$group_id[cls == "R"]), collapse = ", ")), MS)
say("3.4.2: BTEX class", sprintf("BTEX-dominated hotspots (Groups %s)", paste(sort(M$group_id[cls == "B"]), collapse = ", ")), MS)
# Group 13 headquarters diagnostic (81_group13_hq_annulus.R)
G13 <- need("TABLE_group13_hq_annulus.csv"); ar <- G13[aromatic == TRUE]
say("3.4.2: group 13 HQ distance/bearing", sprintf("Its centroid lies %d m from the CDPHE mobile-laboratory headquarters whose 300 m surround was excluded (section 2.1.1), and the headquarters sits at a bearing of %d degrees from the group",
    round(G13$group13_dist_to_hq_m[1]), round(G13$bearing_group13_to_hq_deg[1])), MS)
hc <- G13[pollutant == "HCN"]
say("3.4.2: group 13 annuli", sprintf("Among retained measurements of the four aromatics, the fraction exceeding the pollutant-specific 99th percentile is %s to %s%% in the 300 to 400 m annulus, which is nearer the headquarters, against %s to %s%% in the 400 to 500 m annulus that contains the group (HCN, for which the group is also persistent: %s%% and %s%%)",
    rh(min(ar$pct_above_300_400), 1), rh(max(ar$pct_above_300_400), 1), rh(min(ar$pct_above_400_500), 1), rh(max(ar$pct_above_400_500), 1),
    rh(hc$pct_above_300_400, 1), rh(hc$pct_above_400_500, 1)), MS)
say("3.4.2: campaign-wide rate", sprintf("a campaign-wide rate of about %s%%", rh(max(G13$pct_above_campaign), 0)), MS)
if (gm(9)$tri_dist_km != min(M$tri_dist_km, na.rm = TRUE)) { n_fail <<- n_fail + 1L; cat("  [FAIL] 3.4.2: Group 9 is no longer the group nearest a TRI facility\n") }
# SI Table S5.1 rows: per-group days and TRI distance
SIrow <- rows(SI)
for (i in seq_len(nrow(S5))) { r <- S5[i]
  # the full day list; pollutants with no day are omitted from the caption (2026-09-30)
  .dd <- c(benzene = r$benzene, toluene = r$toluene, trimethylbenzene = r$trimethylbenzene, xylene = r$xylene, H2S = r$H2S, HCN = r$HCN)
  .dd <- .dd[.dd > 0]
  say(sprintf("Table S5.1: group %s days", r$group_id), paste0("Days above the campaign 99th percentile within 100 m: ",
      paste(sprintf("%s %d", sub("H2S", "H2S", names(.dd)), .dd), collapse = "; "), "."), SI)
  say(sprintf("Table S5.1: group %s TRI distance", r$group_id), sprintf("(%s km)", rh(r$tri_dist_km, 2)), SI) }

# DBSCAN sensitivity
ds <- need("TABLE_dbscan_sensitivity.csv"); b <- ds[baseline == TRUE]
one <- ds[(thr_pctl != b$thr_pctl) + (eps_m != b$eps_m) + (pers_pctl != b$pers_pctl) == 1]
say("2.5.3 / S5.3: single-step recovery range", sprintf("%s-%s%% of the %d baseline group locations", pc(100 * min(one$recovery_of_baseline)), pc(100 * max(one$recovery_of_baseline)), ss$n_groups_3plus_pollutants), MS)
lo <- one[recovery_of_baseline == min(recovery_of_baseline)]
.r1 <- function(th, ep, pe) pc(100 * ds[thr_pctl == th & eps_m == ep & pers_pctl == pe, recovery_of_baseline])
say("2.5.3: single-step recoveries", sprintf("(%s%% and %s%% for the lower and higher event thresholds, %s%% and %s%% for the smaller and larger clustering radii, and %s%% and %s%% for the lower and higher persistence percentiles)",
    .r1(0.985, 100, 0.9), .r1(0.995, 100, 0.9), .r1(0.99, 50, 0.9), .r1(0.99, 200, 0.9), .r1(0.99, 100, 0.85), .r1(0.99, 100, 0.95)), MS)
say("2.5.3: combined radius + persistence", sprintf("recovery falls to %s%% when the larger radius is combined with the stricter persistence percentile", .r1(0.99, 200, 0.95)), MS)
say("S5.3: single-step and combined", sprintf("but recovery is %s%% for the larger clustering radius or the stricter persistence percentile and falls to %s%% when those two are combined",
    pc(100 * min(one$recovery_of_baseline)), .r1(0.99, 200, 0.95)), SI)
say("S5.3: groups range", sprintf("ranges from %d (coarsest eps with strictest persistence) to %d (finest eps with loosest persistence)", min(ds$groups_3plus), max(ds$groups_3plus)), SI)
mod <- ds[eps_m %in% c(50, 100) & pers_pctl %in% c(0.85, 0.90)]
say("S5.3: moderate-perturbation recovery", sprintf("%s-%s%% of the %d baseline locations are recovered for eps of 50-100 m with persistence p85-p90", pc(100 * min(mod$recovery_of_baseline)), pc(100 * max(mod$recovery_of_baseline)), b$groups_3plus), SI)
say("S5.3: single-step minimum", sprintf("recovery falls to %s%% (%d of %d)", pc(100 * min(one$recovery_of_baseline)), round(b$groups_3plus * min(one$recovery_of_baseline)), b$groups_3plus), SI)
say("S5.3: baseline reproduces 14", sprintf("the same %d groups persistent in three or more pollutants", b$groups_3plus), SI)
# split-sample and sufficiency
sp <- need("TABLE_split_sample_hotspots.csv"); oe <- sp[split == "odd_even"]; ca <- sp[split == "calendar"]
say("S5.5: odd/even groups and agreement", sprintf("%s groups persistent in three or more pollutants%s; %s%% and %s%% of one half's groups lie within 300 m of the other's, and the halves recover %s%% and %s%%",
    if (oe$groups3_A == oe$groups3_B) paste("each identified", oe$groups3_A) else sprintf("identified %d and %d", oe$groups3_A, oe$groups3_B),
    if (oe$groups3_A == oe$groups3_B) "" else ", respectively", pc(100 * oe$frac_A_near_B), pc(100 * oe$frac_B_near_A), pc(100 * oe$frac_base_near_A), pc(100 * oe$frac_base_near_B)), SI)
say("S5.5: calendar halves", sprintf("identified %d (2023-2024) and %d (2025) groups, recovering %s%% and %s%% of the full-campaign locations, with cross-half agreement of %s%% and %s%%",
    ca$groups3_A, ca$groups3_B, pc(100 * ca$frac_base_near_A), pc(100 * ca$frac_base_near_B), pc(100 * ca$frac_A_near_B), pc(100 * ca$frac_B_near_A)), SI)
su <- need("TABLE_sampling_sufficiency.csv"); sk <- function(k, c) su[k_days == k][[c]]
say("S5.6: map correlations", sprintf("reached %s with 40 sampling days, %s with 80, %s with 160, and %s with 200", rh(sk(40, "map_cor_median"), 2), rh(sk(80, "map_cor_median"), 2), rh(sk(160, "map_cor_median"), 2), rh(sk(200, "map_cor_median"), 2)), SI)
.r6080 <- unique(c(pc(100 * min(sk(60, "recovery_median"), sk(80, "recovery_median"))), pc(100 * max(sk(60, "recovery_median"), sk(80, "recovery_median")))))
say("S5.6: hotspot recovery", sprintf("from %s%% at 10 days to %s%% at 60-80 days, %s%% at 120 days, %s%% at 160 days, and %s%% at 200 days",
    pc(100 * sk(10, "recovery_median")), paste(.r6080, collapse = "-"),
    pc(100 * sk(120, "recovery_median")), pc(100 * sk(160, "recovery_median")), pc(100 * sk(200, "recovery_median"))), SI)

# ==========================================================================
hdr("H. Plume funnel, inversion, detectability, attribution")
fc <- need("WWTP_H2S_plume_step_counts.csv"); ret <- need("WWTP_H2S_retained_plumes.csv")
say("2.5.5 / 3.6: candidate events", sprintf("yielded %d candidate", fc$n_plumes_remaining[1]), MS)
say("2.5.5: wind test evaluable", sprintf("genuinely evaluated for %d of the %d events with at least three plume-flagged observations", 3L, fc$n_plumes_remaining[2]), MS)
say("S6.3: funnel counts", sprintf("This step yielded %d candidate plume events", fc$n_plumes_remaining[1]), SI)
say("S6.3: >= 3 points", sprintf("reduced this set to %d events", fc$n_plumes_remaining[2]), SI)
say("S6.3: retained", sprintf("In total, %d plume events passed all filters", fc$n_plumes_remaining[nrow(fc)]), SI)
p <- need("TABLE_min_detectable_rate_plumes.csv"); stopifnot(setequal(p$plume_id, ret$plume_id))
rates <- sort(p$inferred_tpy)
say("3.6: four rates and mean", sprintf("gave %s, %s, %s and %s metric tons/yr, a range spanning a factor of four around a mean of %s metric tons/yr", cm(rates[1]), cm(rates[2]), cm(rates[3]), cm(rates[4]), cm(mean(p$inferred_tpy))), MS)
say("3.6: Qmin range and SNR", sprintf("ranged from %s to %s metric tons/yr across the four retained intercepts at the 5 ppb audited H2S detection limit of the CAT laboratory, which recorded all four; observed peak H2S enhancements were %s-%s times that limit", cm(min(p$qmin_mdl5)), cm(max(p$qmin_mdl5)), rh(min(p$snr_vs_mdl5), 1), rh(max(p$snr_vs_mdl5), 1)), MS)
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
  say("S6.4: sigma_z and mixing-depth ranges", sprintf("(σz of %s-%s m at %s-%s km, within 1%% of the inversion's along-wind values; mixing depths of %s-%s m)", cm(min(rz$sigma_z_m)), cm(max(rz$sigma_z_m)), rh(min(rz$x_km), 2), rh(max(rz$x_km), 2), cm(min(rz$hpbl_m)), cm(max(rz$hpbl_m))), SI)
} else skip("S6.4 receptor-height check", "WWTP_H2S_receptor_height_check.csv not present (run plume_scripts/P11_plume_geometry_checks.R)")
MV_site <- 83000 / (8.314 * 298.15); MV_std <- 101325 / (8.314 * 273.15)   # mol/m3 at 25 C / 830 hPa and at 0 C / 1013 hPa
.inv <- need("FinalFig/WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv")[sens_group == "baseline"]
say("S6.4: measured air density at the intercepts", sprintf("(%s-%s mol/m3 for the four retained plumes; %s mol/m3", rh(min(.inv$mol_m3), 1), rh(max(.inv$mol_m3), 1), rh(MV_site, 2)), SI)
say("S6.4: standard-conditions ratio", sprintf("(%s mol/m3) would give rates %s-%s%% higher", rh(MV_std, 2), rh(100 * (MV_std / max(.inv$mol_m3) - 1), 0), rh(100 * (MV_std / min(.inv$mol_m3) - 1), 0)), SI)
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
  say("2.3: median station distance", sprintf("The median distance from that hourly position to the station used was %s km", rh(median(o$dist_km, na.rm = TRUE), 1)), MS)
  # (2026-09-30) one value per acquisition bin: counts, per-lab weighting, bin vs delivered statistics
  if ("Hydrogen_Sulfide_ppb_raw" %in% names(o)) { o <- data.table::as.data.table(o)[Site != "Goodrich Corporation (Collins Aerospace)"]
    hb <- o[is.finite(Hydrogen_Sulfide_ppb)]; hr_ <- o[is.finite(Hydrogen_Sulfide_ppb_raw)]
    cb <- o[is.finite(Hydrogen_Cyanide_ppb)]; cr <- o[is.finite(Hydrogen_Cyanide_ppb_raw)]
    frag <- sprintf("the %s delivered H2S seconds that pass the screens described below form %s 5-s bins (%s seconds per bin), the %s HCN seconds form %s 2-s bins (%s)",
      cm(nrow(hr_)), cm(nrow(hb)), rh(nrow(hr_) / nrow(hb), 2), cm(nrow(cr)), cm(nrow(cb)), rh(nrow(cr) / nrow(cb), 2))
    say("2.1.1: bins", gsub("H2S", "H2S", frag), MS)
    say("S1.4: bins per lab", sprintf("form %s 5-s bins (%s seconds per bin: %s for the CAT laboratory and %s for the EMU", cm(nrow(hb)), rh(nrow(hr_) / nrow(hb), 2),
      rh(nrow(hr_[Asset == "CAT"]) / nrow(hb[Asset == "CAT"]), 2), rh(nrow(hr_[Asset == "EMU"]) / nrow(hb[Asset == "EMU"]), 2)), SI)
    .cat <- sprintf("the CAT laboratory supplied %s%% of the H2S values, against %s%% of the bins", pc(100 * mean(hr_$Asset == "CAT")), pc(100 * mean(hb$Asset == "CAT")))
    say("2.1.1: CAT weighting", .cat, MS)
    say("S1.4: CAT weighting", sprintf("the CAT laboratory supplies %s%% of the delivered H2S seconds but %s%% of the bins", pc(100 * mean(hr_$Asset == "CAT")), pc(100 * mean(hb$Asset == "CAT"))), SI)
    say("S1.4: bin vs delivered H2S", sprintf("For H2S the median is %s ppb against %s ppb for the delivered seconds, the mean %s against %s ppb",
      rh(median(hb$Hydrogen_Sulfide_ppb), 2), pc(median(hr_$Hydrogen_Sulfide_ppb_raw)), rh(mean(hb$Hydrogen_Sulfide_ppb), 2), rh(mean(hr_$Hydrogen_Sulfide_ppb_raw), 2)), SI)
    if (all(c("Hydrogen_Sulfide_ppb_rep", "Hydrogen_Cyanide_ppb_rep") %in% names(o))) cat("  [OK  ] mobile_wswd carries the repeated 1-s bin means (*_rep) for the correlations\n") else
      { n_fail <<- n_fail + 1L; cat("  [FAIL] mobile_wswd lacks *_rep: the correlation figures fell back to the one-per-bin columns (re-run R02)\n") }
  }
  rm(o, e); invisible(gc())
} else skip("2.3 station distance", "mobile_wswd.RData not present")

# ==========================================================================
hdr("J. Methane  <- methane_hotspot_summary, cent_out_methane_persistent, methane_at_toxics_hotspots, methane_sourceprob")
mh <- need("methane_hotspot_summary.csv"); mc <- need("cent_out_methane_persistent.csv"); ma <- need("methane_at_toxics_hotspots.csv")
say("3.7: p99, clusters", sprintf("(≥ 99th percentile, %s ppm) into %d spatial clusters, of which two", rh(mh$p99, 2), mh$n_clusters), MS)
say("S8: p99 / p95 / events / days / clusters", sprintf("campaign-wide 99th percentile (%s ppm; the 95th percentile was %s ppm). The %s high-methane events (observed on %d of 193 days) grouped into %d spatial clusters", rh(mh$p99, 3), rh(mh$p95, 3), cm(mh$n_high_events), mh$n_days_high, mh$n_clusters), SI)
c1 <- mc[cluster == 1]; c2 <- mc[cluster == 2]
say("3.7: largest methane hotspot", sprintf("(%s° N, %s° W; %s events on %d of 193 days, maximum %s ppm)", rh(c1$lat, 3), rh(-c1$lon, 3), cm(c1$n_events), c1$n_days, rh(c1$ch4_max, 1)), MS)
say("3.7: second methane hotspot", sprintf("(%s° N, %s° W; %s events on %d days)", rh(c2$lat, 3), rh(-c2$lon, 3), cm(c2$n_events), c2$n_days), MS)
say("S8: largest methane hotspot", sprintf("(%s° N, %s° W; %s events on %d days; maximum %s ppm)", rh(c1$lat, 4), rh(-c1$lon, 4), cm(c1$n_events), c1$n_days, rh(c1$ch4_max, 1)), SI)
hav <- function(la, lo, LA, LO) { R <- 6371008.8; p_ <- pi/180; a <- sin((LA-la)*p_/2)^2 + cos(la*p_)*cos(LA*p_)*sin((LO-lo)*p_/2)^2; 2*R*asin(pmin(1, sqrt(a))) }
M <- need("MASTER_hotspot_group_index.csv")
d40 <- hav(c2$lat, c2$lon, M[group_id == 40, Latitude], M[group_id == 40, Longitude]) / 1000; d29 <- hav(c1$lat, c1$lon, M[group_id == 29, Latitude], M[group_id == 29, Longitude]) / 1000
say("3.7: distances to groups 40 and 29", sprintf("The second lies %s km from air-toxics Group 40 and the largest %s km from Group 29", rh(d40, 1), rh(d29, 1)), MS)
say("3.7: co-elevation range", sprintf("ranged from %s%% to %s%%", rh(min(ma$pct_ge_p95), 1), rh(max(ma$pct_ge_p95), 1)), MS)
t34 <- ma[group_id == 34]; t40 <- ma[group_id == 40]
.nw <- c("one","two","three","four","five","six","seven")
say("3.7: groups 34 and 40", sprintf("strongest at Group 34 (%s%%, with high-methane events on %s sampling day%s) and Group 40 (%s%%, on %s days)", rh(t34$pct_ge_p95, 1), .nw[t34$days_with_high], if (t34$days_with_high == 1) "" else "s", rh(t40$pct_ge_p95, 1), .nw[t40$days_with_high]), MS)
say("3.7: groups below 5%", sprintf("whereas %d of the %d groups", sum(ma$pct_ge_p95 < 5), nrow(ma)), MS)
MP <- file.path(BASE, "methane_sourceprob.RData")
if (file.exists(MP)) { e <- new.env(); load(MP, envir = e); s <- e$sourceprob_ch4; M_ <- s$M; i <- which(M_ == max(M_), arr.ind = TRUE)[1, ]
  xs <- seq(s$xr[1], s$xr[2], length.out = ncol(M_)); ys <- seq(s$yr[1], s$yr[2], length.out = nrow(M_))
  lat <- s$center["lat"] + ys[i[1]] / 111320; lon <- s$center["lon"] + xs[i[2]] / (111320 * cos(s$center["lat"] * pi / 180))
  say("3.7 / S8: methane source-probability maximum", sprintf("(maximum at %s° N, %s° W", rh(lat, 3), rh(-lon, 3)), BOTH)
  say("S8: same maximum in the SI", sprintf("(maximum at %s° N, %s° W, about", rh(lat, 3), rh(-lon, 3)), SI)
} else skip("3.7 methane source maximum", "methane_sourceprob.RData not present")

# ==========================================================================
hdr("K. Smoke and season  <- TABLE_smoke_comparison.csv, TABLE_seasonal.csv")
sm <- need("TABLE_smoke_comparison.csv"); sv <- function(p, c) sm[pollutant == p & class == "none"][[c]]
say("S3.2: benzene / toluene / xylene ratios", sprintf("benzene daily medians had the same median (ratio %s) but a lower distribution on smoke days (Wilcoxon p = %s), and toluene and xylene were modestly lower on smoke days (ratios %s and %s; Wilcoxon p = %s and %s)",
    rh(sv("Benzene","ratio_smoke_over_none"), 2), rh(sv("Benzene","p_wilcoxon_smoke_vs_none"), 3), rh(sv("Toluene","ratio_smoke_over_none"), 2), rh(sv("Xylene","ratio_smoke_over_none"), 2), rh(sv("Toluene","p_wilcoxon_smoke_vs_none"), 3), rh(sv("Xylene","p_wilcoxon_smoke_vs_none"), 3)), SI)
say("S3.2: H2S under light overlay", sprintf("(%s vs %s ppb) but not significantly so (p = %s)", rh(sm[pollutant == "H2S" & class == "light", median_of_day_medians], 1), rh(sv("H2S","median_of_day_medians"), 1), rh(sv("H2S","p_wilcoxon_smoke_vs_none"), 2)), SI)
se_ <- need("TABLE_seasonal.csv"); sz <- function(p, s) se_[pollutant == p & season == s, med]
say("S3.3: benzene by season", sprintf("benzene %s ppb in DJF vs %s ppb in other seasons", rh(sz("Benzene","DJF"), 2), rh(sz("Benzene","MAM"), 2)), SI)
tl <- c(sz("Toluene","MAM"), sz("Toluene","JJA"), sz("Toluene","SON"))
say("S3.3: toluene by season", sprintf("toluene %s vs %s-%s ppb", rh(sz("Toluene","DJF"), 2), rh(min(tl), 2), rh(max(tl), 2)), SI)
say("S3.3: H2S by season", sprintf("(%s ppb in DJF, %s in MAM, %s in JJA and SON)", rh(sz("H2S","DJF"), 2), rh(sz("H2S","MAM"), 2), rh(sz("H2S","JJA"), 2)), SI)
if (rh(sz("H2S","JJA"), 2) != rh(sz("H2S","SON"), 2)) { n_fail <<- n_fail + 1L; cat("  [FAIL] S3.3: H2S JJA and SON medians differ; the sentence pairs them\n") }
say("S3.3: HCN by season", sprintf("a winter median of %s ppb falling to %s-%s ppb", rh(sz("HCN","DJF"), 2), rh(min(sz("HCN","MAM"), sz("HCN","JJA")), 1), rh(max(sz("HCN","MAM"), sz("HCN","JJA")), 0)), SI)

# ==========================================================================
hdr("L. Bootstrap  <- TABLE_bootstrap_ratio.csv, TABLE_bootstrap_blocks.csv")
# Table S4.1 hazard-index columns (median basis after the 79 re-run)
if (have("TABLE_S4.1_background_sensitivity.csv")) {
  t41 <- need("TABLE_S4.1_background_sensitivity.csv")
  for (r in seq_len(nrow(t41))) say(sprintf("Table S4.1 HI columns, %s", t41$arm[r]),
    sprintf("| %s | %s | %s | %s |", rh(t41$HI_pwmean_Endocrine[r], 3), rh(t41$HI_pwmean_Respiratory[r], 3), rh(t41$HI_pwmean_Neurological[r], 3), rh(t41$HI_pwmean_Hematological[r], 3)), rows(SI))
  say("S4.6: max-block ranges", sprintf("ranging from %s to %s and from %s to %s, respectively", rh(min(t41$HI_maxblock_Endocrine), 2), rh(max(t41$HI_maxblock_Endocrine), 2), rh(min(t41$HI_maxblock_Respiratory), 2), rh(max(t41$HI_maxblock_Respiratory), 2)), SI)
}
for (.bs in list(list(f = "", doc = SI, tag = "S4.5", col = "sBenzene_mean_of_daily_mean_scaled"), list(f = "_medianbasis", doc = SI, tag = "S4.7", col = "sBenzene_med_of_daily_med_scaled"))) {
  if (!have(paste0("TABLE_bootstrap_ratio", .bs$f, ".csv"))) { skip(paste0(.bs$tag, ": bootstrap"), paste0("TABLE_bootstrap_ratio", .bs$f, ".csv not present (run 58)")); next }
  br <- need(paste0("TABLE_bootstrap_ratio", .bs$f, ".csv")); bb <- fread(file.path(BASE, paste0("TABLE_bootstrap_blocks", .bs$f, ".csv")), colClasses = list(character = "block"))
  if (.bs$tag == "S4.5") {
    say("S4.5: aggregate ratio and CI", sprintf("The aggregate ratio is %s with a 95%% bootstrap interval of %s-%s", rh(br$ratio_point, 2), rh(br$ci_lo, 2), rh(br$ci_hi, 2)), SI)
    say("4: CI in the manuscript", sprintf("(risk ratio %s, 95%% CI: %s, %s)", rh(br$ratio_point, 2), rh(br$ci_lo, 2), rh(br$ci_hi, 2)), MS)
    if ("n_gt2_min" %in% names(br)) say("S4.5: upper tail per replicate", sprintf("every one of the %d replicates has at least %d blocks above twice AirToxScreen (median %s; 95%% interval %s-%s)",
        br$B, br$n_gt2_min, rh(br$n_gt2_med, 0), rh(br$n_gt2_lo, 0), rh(br$n_gt2_hi, 0)), SI)
    say("S4.5: block counts", sprintf("of the %d blocks whose point estimate exceeds twice the AirToxScreen value in this construction, %d remain above 2x in at least 80%% of bootstrap replicates and %d in at least 95%%", nrow(bb), sum(bb$pr_gt2 >= 0.8), sum(bb$pr_gt2 >= 0.95)), SI)
  } else {
    say("S4.7: bootstrap ratio and CI (median basis)", sprintf("gives an aggregate ratio of %s with a 95%% bootstrap interval of %s-%s", rh(br$ratio_point, 2), rh(br$ci_lo, 2), rh(br$ci_hi, 2)), SI)
    say("S4.7: bootstrap block counts (median basis)", sprintf("of the %d blocks above twice AirToxScreen, %d remain so in at least 80%% of replicates and %d in at least 95%%",
        nrow(bb), sum(bb$pr_gt2 >= 0.8), sum(bb$pr_gt2 >= 0.95)), SI)
    if ("n_gt2_min" %in% names(br)) say("S4.7: bootstrap upper tail (median basis)", sprintf("every replicate has at least %d such blocks (median %s; 95%% interval %s-%s)",
        br$n_gt2_min, rh(br$n_gt2_med, 0), rh(br$n_gt2_lo, 0), rh(br$n_gt2_hi, 0)), SI)
  }
  # the bootstrap (58) must re-aggregate the SAME block surface as section 3.3 / S4.7
  if (exists("d") && .bs$col %in% names(d)) {
    m <- d[[.bs$col]]; a <- d$benzene_ppb; k <- is.finite(m) & is.finite(a)
    gt2 <- as.character(d$GEOID20[k][m[k] / a[k] > 2])
    same <- length(gt2) == nrow(bb) && setequal(gt2, bb$block)
    if (same) n_ok <<- n_ok + 1L else n_fail <<- n_fail + 1L
    cat(sprintf("  [%s] %-46s bootstrap >2x set (%d blocks) %s the block file's (%d)\n", if (same) "OK  " else "FAIL",
                paste0(.bs$tag, ": bootstrap reproduces the >2x block set"), nrow(bb), if (same) "==" else "!=", length(gt2)))
  }
}
# ==========================================================================
if (have("TABLE_scaling_sensitivity_risk.csv")) {
  ss <- need("TABLE_scaling_sensitivity_risk.csv")
  say("S4.3: scaling ratio range (mean basis)", sprintf("the resulting aggregate risk ratio ranges from %s (median-based construction) to %s (baseline), with mobile risk ranges of %s-%s excess cases",
      rh(min(ss$ratio_vs_ATS), 2), rh(ss[construction == "A_binweighted", ratio_vs_ATS], 2), sprintf("%.3f", min(ss$risk_lo)), sprintf("%.3f", max(ss$risk_hi))), SI)
  if (min(ss$ratio_vs_ATS) <= 1) { n_fail <<- n_fail + 1L; cat("  [FAIL] S4.3: a scaling construction puts the mean-basis ratio at or below 1; the text says every one exceeds AirToxScreen\n") }
}

if (have("TABLE_bin_location_error.csv")) {
  bl <- need("TABLE_bin_location_error.csv"); g_ <- function(p, s, c) bl[pollutant == p & subset == s][[c]]; H_ <- "bins > campaign p99 (hotspot events)"
  .nh <- g_("H2S", H_, "n_max_off_gt100"); .nc <- g_("HCN", H_, "n_max_off_gt100")
  say("S5: binned-event positional uncertainty", sprintf("lies a median of %s m and a 95th percentile of %s m from the assigned position for H2S (%s and %s m for HCN); only %d of the %s H2S events, and %s of the %s HCN events, %s beyond 100 m, and in 95%% of events the assigned position is within %s m (H2S) and %s m (HCN)",
      rh(g_("H2S", H_, "max_off_p50"), 0), rh(g_("H2S", H_, "max_off_p95"), 0), rh(g_("HCN", H_, "max_off_p50"), 0), rh(g_("HCN", H_, "max_off_p95"), 0),
      .nh, cm(g_("H2S", H_, "n_bins")), if (.nc == 0) "none" else .nc, cm(g_("HCN", H_, "n_bins")), if (.nh == 1) "extends" else "extend",
      rh(g_("H2S", H_, "cen_off_p95"), 0), rh(g_("HCN", H_, "cen_off_p95"), 0)), SI)
  say("S5: median driving speed in the bins", sprintf("about %s m per second at the median driving speed (%s km/h)", rh(g_("H2S", "all bins", "speed_kmh_p50") / 3.6, 0), rh(g_("H2S", "all bins", "speed_kmh_p50"), 0)), SI)
  say("S5: event counts match the hotspot events", sprintf("of the %s H2S events", cm(g_("H2S", H_, "n_bins"))), SI)
} else skip("S5: binned-event positional uncertainty", "TABLE_bin_location_error.csv not present (run 80)")

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
  M_ <- "mean_of_daily_means"; D_ <- "median_of_daily_medians"
  say("S7.2 / 3.3 / S4.7: median basis stated", sprintf("the community endocrine index is %s and the respiratory index %s, with most-exposed-block values of %s and %s", rh(g(D_,"Endocrine","HI_pwmean"), 2), rh(g(D_,"Respiratory","HI_pwmean"), 2), rh(g(D_,"Endocrine","HI_maxblock"), 2), rh(g(D_,"Respiratory","HI_maxblock"), 2)), SI)
  say("S7.3: community indices on both bases", sprintf("the community indices are %s (endocrine), %s (respiratory), %s (neurological) and %s (hematological), against %s, %s, %s and %s on the mean basis", rh(g(D_,"Endocrine","HI_pwmean"), 2), rh(g(D_,"Respiratory","HI_pwmean"), 2), rh(g(D_,"Neurological","HI_pwmean"), 3), rh(g(D_,"Hematological","HI_pwmean"), 3), rh(g(M_,"Endocrine","HI_pwmean"), 2), rh(g(M_,"Respiratory","HI_pwmean"), 2), rh(g(M_,"Neurological","HI_pwmean"), 3), rh(g(M_,"Hematological","HI_pwmean"), 3)), SI)
  say("S7.3: most-exposed-block indices on both bases", sprintf("the most-exposed-block values are %s, %s, %s and %s against %s, %s, %s and %s", rh(g(D_,"Endocrine","HI_maxblock"), 2), rh(g(D_,"Respiratory","HI_maxblock"), 2), rh(g(D_,"Neurological","HI_maxblock"), 2), rh(g(D_,"Hematological","HI_maxblock"), 2), rh(g(M_,"Endocrine","HI_maxblock"), 2), rh(g(M_,"Respiratory","HI_maxblock"), 2), rh(g(M_,"Neurological","HI_maxblock"), 2), rh(g(M_,"Hematological","HI_maxblock"), 2)), SI)
  .pc <- function(o) { a <- g(M_, o, "HI_maxblock"); b <- g(D_, o, "HI_maxblock"); v <- round(100 * (b / a - 1)); paste0(if (v > 0) "+" else "", v, "%") }
  say("S7.3: most-exposed-block changes, mean -> median", sprintf("endocrine %s to %s, %s; respiratory %s to %s, %s; neurological %s to %s, %s; hematological %s to %s, %s",
      rh(g(M_,"Endocrine","HI_maxblock"), 2), rh(g(D_,"Endocrine","HI_maxblock"), 2), .pc("Endocrine"), rh(g(M_,"Respiratory","HI_maxblock"), 2), rh(g(D_,"Respiratory","HI_maxblock"), 2), .pc("Respiratory"),
      rh(g(M_,"Neurological","HI_maxblock"), 3), rh(g(D_,"Neurological","HI_maxblock"), 3), .pc("Neurological"), rh(g(M_,"Hematological","HI_maxblock"), 3), rh(g(D_,"Hematological","HI_maxblock"), 3), .pc("Hematological")), SI)
  say("3.3: hazard indices, mean basis (primary)", sprintf("population-weighted hazard indices of %s for endocrine effects (driven by HCN) and %s for respiratory effects (driven by H2S), rising to %s and %s in the most-exposed block, whereas neurological (%s) and hematological (%s)",
      rh(g(M_,"Endocrine","HI_pwmean"), 2), rh(g(M_,"Respiratory","HI_pwmean"), 2), rh(g(M_,"Endocrine","HI_maxblock"), 2), rh(g(M_,"Respiratory","HI_maxblock"), 2), rh(g(M_,"Neurological","HI_pwmean"), 2), rh(g(M_,"Hematological","HI_pwmean"), 2)), MS)
  say("3.3: hazard indices, median basis", sprintf("On the supplementary block median of daily medians the community endocrine index is %s and the respiratory index %s, with most-exposed-block values of %s and %s", rh(g(D_,"Endocrine","HI_pwmean"), 2), rh(g(D_,"Respiratory","HI_pwmean"), 2), rh(g(D_,"Endocrine","HI_maxblock"), 2), rh(g(D_,"Respiratory","HI_maxblock"), 2)), MS)
  say("S7.2: mean-basis results", sprintf("hazard index of %s at the community average and %s in the most-exposed block. Hydrogen sulfide gives a respiratory hazard index of %s at the community average but %s in the most-exposed block",
      rh(g(M_,"Endocrine","HI_pwmean"), 2), rh(g(M_,"Endocrine","HI_maxblock"), 2), rh(g(M_,"Respiratory","HI_pwmean"), 2), rh(g(M_,"Respiratory","HI_maxblock"), 2)), SI)
  say("S7.2: neurological / hematological", sprintf("reaches only %s at the community average and %s at the most-exposed block, and the hematological index for benzene reaches %s and %s respectively",
      rh(g(M_,"Neurological","HI_pwmean"), 3), rh(g(M_,"Neurological","HI_maxblock"), 2), rh(g(M_,"Hematological","HI_pwmean"), 3), rh(g(M_,"Hematological","HI_maxblock"), 2)), SI)
  say("4: hazard conclusion on both bases", sprintf("at the community average the endocrine index is %s on the mean-of-daily-means basis used for the benzene comparison and %s on the supplementary median-of-daily-medians basis", rh(g(M_,"Endocrine","HI_pwmean"), 2), rh(g(D_,"Endocrine","HI_pwmean"), 2)), MS)
} else skip("S7 basis comparison", "TABLE_S7.1c_basis_comparison.csv not present (run 74)")
if (have("TABLE_cumulative_HI_summary.csv")) {
  cu <- need("TABLE_cumulative_HI_summary.csv"); cg <- function(t, m, c) cu[tos == t & metric == m][[c]]
  say("S7.3: cell-level median metric", sprintf("on the cell median the endocrine index exceeds 1 in %s%% of cells (maximum %s) and the respiratory index in %s (maximum %s)", pc(cg("Endocrine","median","pct_cells_HI_gt1")), rh(cg("Endocrine","median","HI_max"), 2),
      c("no cell", "one cell", "two cells", "three cells", "four cells")[min(cg("Respiratory","median","n_cells_HI_gt1"), 4) + 1], rh(cg("Respiratory","median","HI_max"), 2)), SI)
  say("S7.3: cell-level mean metric", sprintf("on the cell mean the endocrine index exceeds 1 in %s%% of cells (maximum %s) and the respiratory index in the most-exposed cells (maximum %s)", pc(cg("Endocrine","mean","pct_cells_HI_gt1")), rh(cg("Endocrine","mean","HI_max"), 2), rh(cg("Respiratory","mean","HI_max"), 2)), SI)
}
say("S7.4: community endocrine under C and D", sprintf("from %s (A, B) to %s and %s (C, D)", rh(s3("A_none","Endocrine","HI_pwmean"), 2), rh(s3("C_borrowed","Endocrine","HI_pwmean"), 2), rh(s3("D_upper","Endocrine","HI_pwmean"), 2)), SI)
b4 <- function(o, c) h4[organ == o][[c]]
say("S7.4: break-even factors (pw)", sprintf("would fall to 1 only at an HCN factor of %s, and the respiratory index reaches 1 at an H", rh(b4("Endocrine","f_breakeven_pwmean"), 2)), SI)
say("S7.4: break-even respiratory (pw)", sprintf("the respiratory index reaches 1 at an H2S factor of %s.", rh(b4("Respiratory","f_breakeven_pwmean"), 2)), SI)
if (have("TABLE_S7.4_breakeven_factors_medianbasis.csv") && have("TABLE_S7.3_scaling_scenarios_medianbasis.csv")) {
  h4m <- need("TABLE_S7.4_breakeven_factors_medianbasis.csv"); b4m <- function(o, c) h4m[organ == o][[c]]
  h3m <- need("TABLE_S7.3_scaling_scenarios_medianbasis.csv"); s3m <- function(sc, o, c) h3m[scenario == sc & organ == o][[c]]
  say("S7.4: neurological break-even (max block), both", sprintf("for the neurological system this gives %s (%s on the median basis), and it need not be", rh(b4("Neurological","f_breakeven_maxblock"), 2), rh(b4m("Neurological","f_breakeven_maxblock"), 2)), SI)
  say("S7.4: median-basis endocrine break-even", sprintf("On the supplementary median basis the endocrine index reaches 1 at an HCN factor of %s", rh(b4m("Endocrine","f_breakeven_pwmean"), 2)), SI)
  say("S7.4: median-basis respiratory break-even", sprintf("and the respiratory break-even factor is %s.", rh(b4m("Respiratory","f_breakeven_pwmean"), 2)), SI)
  say("S7.4: median-basis max-block thresholds", sprintf("(%s and %s on the median basis)", rh(b4m("Endocrine","f_breakeven_maxblock"), 2), rh(b4m("Respiratory","f_breakeven_maxblock"), 2)), SI)
  say("S7.3: median-basis break-even factors", sprintf("on the median basis the community break-even factors are %s for HCN and %s for H", rh(b4m("Endocrine","f_breakeven_pwmean"), 2), rh(b4m("Respiratory","f_breakeven_pwmean"), 2)), SI)
  say("S7.4: median-basis community endocrine under C and D", sprintf("from %s (A, B) to %s and %s (C, D)", rh(s3m("A_none","Endocrine","HI_pwmean"), 2), rh(s3m("C_borrowed","Endocrine","HI_pwmean"), 2), rh(s3m("D_upper","Endocrine","HI_pwmean"), 2)), SI)
} else skip("S7.4 median basis", "*_medianbasis S7.3/S7.4 tables not present (run 77 with HAZARD_BASIS=med_of_daily_med)")
say("S7.3: OEHHA chronic re-anchoring", sprintf("raise the most-exposed-block hazard quotients to %s (benzene, hematological) and %s (1,2,4-trimethylbenzene, neurological)", rh(h7[pollutant == "Benzene", maxblock_ugm3] / 3, 2), rh(h7[pollutant == "1,2,4-Trimethylbenzene", maxblock_ugm3] / 4, 2)), SI)
say("S7.4: break-even (max block)", sprintf("the corresponding thresholds are %s for HCN and %s for H", rh(b4("Endocrine","f_breakeven_maxblock"), 2), rh(b4("Respiratory","f_breakeven_maxblock"), 2)), SI)
say("Table S7.4: neurological row", sprintf("| %s / %s | %s / %s |", rh(b4("Neurological","HI_pwmean_unscaled"), 3), rh(b4("Neurological","HI_maxblock_unscaled"), 2), rh(b4("Neurological","f_breakeven_pwmean"), 2), rh(b4("Neurological","f_breakeven_maxblock"), 2)), SIrow)
say("Table S7.4: endocrine row", sprintf("| %s / %s | %s / %s |", rh(b4("Endocrine","HI_pwmean_unscaled"), 3), rh(b4("Endocrine","HI_maxblock_unscaled"), 2), rh(b4("Endocrine","f_breakeven_pwmean"), 2), rh(b4("Endocrine","f_breakeven_maxblock"), 2)), SIrow)
ac <- function(p, c) h2[pollutant == p][[c]]
say("S7.2: acute HQ at campaign max", sprintf("benzene (HQ ~ %s), H2S (HQ ~ %s on the 5-s bin means", rh(ac("Benzene","HQ_max"), 0), rh(ac("H2S","HQ_max"), 1)), SI)
say("S7.2: acute HQ toluene", sprintf("and toluene (HQ ~ %s) exceed the 1-hour REL", rh(ac("Toluene","HQ_max"), 1)), SI)
say("S7.2: acute TMB peak", sprintf("the trimethylbenzene peak reaches %s%% of its REL (HQ ~ %s)", rh(100 * ac("1,2,4-Trimethylbenzene","HQ_max"), 0), rh(ac("1,2,4-Trimethylbenzene","HQ_max"), 2)), SI)
say("S7.2: acute p99", sprintf("(largest HQ %s, benzene)", rh(max(h2$HQ_p99), 2)), SI)
if (have("TABLE_S7.2_acute_screen.csv")) { .ac <- need("TABLE_S7.2_acute_screen.csv")
  if ("HQ_max_delivered" %in% names(.ac)) {
    .h <- .ac[pollutant == "H2S"]; .n <- .ac[pollutant == "HCN"]
    say("S7.2: H2S acute HQ on both series", sprintf("S (HQ ~ %s on the 5-s bin means; ~%s at the highest delivered reading, %s ppb)",
        rh(.h$HQ_max, 1), rh(.h$HQ_max_delivered, 1), formatC(.h$max_ppb_delivered, format = "fg")), SI)
    say("Table S7.2: H2S row with delivered values", sprintf("| %s (%s) | %s (%s) | %s (%s) | %s (%s) |",
        formatC(.h$p99_ugm3, format = "f", digits = 2), formatC(.h$p99_ugm3_delivered, format = "f", digits = 2),
        formatC(.h$HQ_p99, format = "f", digits = 3), formatC(.h$HQ_p99_delivered, format = "f", digits = 3),
        formatC(.h$max_ugm3, format = "f", digits = 1), formatC(.h$max_ugm3_delivered, format = "f", digits = 1),
        formatC(.h$HQ_max, format = "f", digits = 2), formatC(.h$HQ_max_delivered, format = "f", digits = 1)), rows(SI))
    say("Table S7.2: HCN row with delivered values", sprintf("| %s (%s) | %s (%s) | %s (%s) | %s (%s) |",
        formatC(.n$p99_ugm3, format = "f", digits = 2), formatC(.n$p99_ugm3_delivered, format = "f", digits = 2),
        formatC(.n$HQ_p99, format = "f", digits = 4), formatC(.n$HQ_p99_delivered, format = "f", digits = 4),
        formatC(.n$max_ugm3, format = "f", digits = 1), formatC(.n$max_ugm3_delivered, format = "f", digits = 1),
        formatC(.n$HQ_max, format = "f", digits = 3), formatC(.n$HQ_max_delivered, format = "f", digits = 3)), rows(SI))
  } }

# ==========================================================================
hdr("M2. TRI inside/outside and La Casa CPF  <- tri_inside_outside_1km_stats.csv, TABLE_lacasa_cpf.csv")
if (have("tri_inside_outside_1km_stats.csv")) { ti <- need("tri_inside_outside_1km_stats.csv"); tm <- function(pol_, col_) ti[Pollutant == pol_][[col_]]
  say("3.3: TRI medians (aromatics)", sprintf("toluene (%s vs %s ppb), xylene (%s vs %s ppb), trimethylbenzene (%s vs %s ppb)",
      rh(tm("Toluene","med_in"),2), rh(tm("Toluene","med_out"),2), rh(tm("Xylene","med_in"),2), rh(tm("Xylene","med_out"),2),
      rh(tm("Trimethylbenzene","med_in"),2), rh(tm("Trimethylbenzene","med_out"),2)), MS)
  # equal medians are stated as "equal for ... (x ppb in both" (2026-09-30)
  if (rh(tm("HCN","med_in"),2) == rh(tm("HCN","med_out"),2)) {
    say("3.3: TRI medians (H2S)", sprintf("S (%s vs %s ppb), and equal for benzene", rh(tm("H2S","med_in"),2), rh(tm("H2S","med_out"),2)), MS)
    say("3.3: TRI medians (HCN equal)", sprintf("and HCN (%s ppb in both", rh(tm("HCN","med_in"),2)), MS)
  } else
  say("3.3: TRI medians (H2S)", sprintf("S (%s vs %s ppb) and HCN (%s vs %s ppb)", rh(tm("H2S","med_in"),2), rh(tm("H2S","med_out"),2), rh(tm("HCN","med_in"),2), rh(tm("HCN","med_out"),2)), MS)
  say("3.3: TRI benzene medians equal", sprintf("and equal for benzene (%s ppb in both", rh(tm("Benzene","med_in"),2)), MS)
  if (tm("Benzene","med_in") != tm("Benzene","med_out")) { n_fail <<- n_fail + 1L; cat("  [FAIL] benzene inside/outside medians differ\n") } }
if (have("TABLE_lacasa_cpf.csv")) { cp <- need("TABLE_lacasa_cpf.csv")
  ne <- cp[sector %in% c(2, 3)]; base <- cp[, sum(n_high) / sum(n), by = pollutant]$V1
  say("S5.4: CPF NE sectors and base rate", sprintf("the CPF over all sectors together is %s by construction; for the northeasterly sectors containing the industrial corridor (bearings 45-70 degrees; Figure S5.7) it is %s-%s",
      rh(mean(base), 2), rh(min(ne$cpf), 2), rh(max(ne$cpf), 2)), SI)
  say("S5.4: CPF SW-W peak", sprintf("(CPF %s-%s)", rh(min(cp[sector %in% 9:12, cpf]), 2), rh(max(cp[sector %in% 9:12, cpf]), 2)), SI) }

hdr("T. Text diagnostics  <- TABLE_delivery_spacing.csv (83), TABLE_background_sign_changes.csv (82), TABLE_wind_station_distance.csv (84)")
if (have("TABLE_delivery_spacing.csv")) { ds <- need("TABLE_delivery_spacing.csv")
  g <- function(l, y, v) ds[lab == l & year == y][[v]]
  emu <- ds[lab == "EMU" & year %in% c("2023", "2024", "2025")]
  say("S1.4: delivery spacing", sprintf("mostly 1 s apart for the EMU laboratory (%s-%s%% of intervals, by year) and for the CAT laboratory in 2025 (%s%%), but mostly 2 s apart for the CAT laboratory in 2023 and 2024 (%s%% and %s%% of intervals)",
      rh(min(emu$pct_1s), 0), rh(max(emu$pct_1s), 0), rh(g("CAT", "2025", "pct_1s"), 0), rh(g("CAT", "2023", "pct_2s"), 0), rh(g("CAT", "2024", "pct_2s"), 0)), SI)
  say("S1.4: EMU 2024 repeated timestamps", sprintf("in 2024 about %s%% of EMU rows repeat the timestamp of the preceding row", rh(g("EMU", "2024", "pct_rows_repeat_prev"), 0)), SI)
  lb <- ds[lab %in% c("CAT", "EMU") & year == "all"]
  say("S1.4: consecutive values differ", sprintf("and %s-%s%% of consecutive one-second values differ", rh(min(lb$pct_h2s_pairs_differ, lb$pct_hcn_pairs_differ), 0), rh(max(lb$pct_h2s_pairs_differ, lb$pct_hcn_pairs_differ), 0)), SI)
  say("S1.4: MaxiMet flag share", sprintf("A large portion (%s%% of the records retained after the GPS screen)", rh(g("both", "all", "pct_metflag_kept02"), 1)), SI)
} else skip("S1.4 delivery statistics", "TABLE_delivery_spacing.csv not found (run 83)")
if (have("TABLE_background_sign_changes.csv")) { bs <- need("TABLE_background_sign_changes.csv"); b <- bs[pollutant == "Benzene"]; h <- bs[pollutant == "H2S"]
  say("S4.1.1: sign changes", sprintf("in this record %s%% of negative benzene readings and %s%% of negative H2S values become positive, and %s%% and %s%% of positive readings become negative",
      rh(b$pct_neg_to_pos, 1), rh(h$pct_neg_to_pos, 1), rh(b$pct_pos_to_neg, 1), rh(h$pct_pos_to_neg, 1)), SI)
  say("S4.1.1: negative run medians", sprintf("the run-median background is itself negative in %s%% of benzene rows and %s%% of H2S values; Equation S2 applies with a negative run median in %s benzene and %s H2S values (%s%% and %s%% of each record), and in %s and %s of them",
      rh(b$pct_run_median_negative, 0), rh(h$pct_run_median_negative, 0), cm(b$n_eq3_negative_median), cm(h$n_eq3_negative_median),
      rh(b$pct_eq3_negative_median, 1), rh(h$pct_eq3_negative_median, 2), cm(b$n_eq3_pos_to_neg), cm(h$n_eq3_pos_to_neg)), SI)
} else skip("S4.1.1 sign changes", "TABLE_background_sign_changes.csv not found (run 82)")
if (have("TABLE_wind_station_distance.csv")) { wsd <- need("TABLE_wind_station_distance.csv")
  say("2.3: wind-station distances", sprintf("The median distance from that hourly position to the station used was %s km (%s km from the individual measurements; for %s%% of measurements",
      rh(wsd$median_dist_hourly_position_km, 1), rh(wsd$median_dist_measurement_km, 1), rh(wsd$pct_other_station_nearer, 1)), MS)
  say("2.3: wind-station fallback", sprintf("For %s measurements (%s%%), the nearest station reported no wind for that hour and the next-closest reporting station was used, a median of %s km farther away",
      cm(wsd$n_fallback), rh(wsd$pct_fallback, 1), rh(wsd$median_extra_km_fallback, 1)), MS)
} else skip("2.3 wind-station distances", "TABLE_wind_station_distance.csv not found (run 84)")
if (have("TABLE_lacasa_cpf.csv")) { cp2 <- need("TABLE_lacasa_cpf.csv")
  say("S5.4: CPF rows", sprintf("(winds > 1 m/s; %s rows with valid concentrations", cm(cp2[pollutant == "benzene", sum(n)])), SI) }
dm <- need("hotspot_source_fingerprint_outputs/hotspot_source_directional_metrics.csv")[group_id == 13]
b13 <- need("TABLE_group13_hq_annulus.csv")$bearing_group13_to_hq_deg[1]
inw <- dm[abs(((source_bearing_deg - b13 + 180) %% 360) - 180) <= 30]
lbl <- function(nm) round(inw[source_name == nm, source_bearing_deg])
say("3.4.2: Group 13 acceptance window", sprintf("as WWTF2 (%d degrees), Sinclair (%d), WWTF1 (%d), Suncor (%d), Phillips 66 (%d), the woodshop (%d) and three refuelling locations (%s)",
    lbl("WWTF2"), lbl("Sinclair"), lbl("WWTF1"), lbl("Suncor"), lbl("Phillips 66"), lbl("Woodshop"),
    sub(", (\\d+)$", " and \\1", paste(sort(round(inw[source_type == "Refuel", source_bearing_deg])), collapse = ", "))), MS)
if (nrow(inw) != 9) { n_fail <- n_fail + 1L; cat(sprintf("  [FAIL] 3.4.2: %d candidate sources in the Group 13 window; the text lists 9\n", nrow(inw))) }
sm <- need("FinalFig/WWTP_H2S_inversion_summary_mean_ci_METRIC_TPY.csv")
say("S6.5.2: averaging-time range", sprintf("(%s-%s metric tons/yr for 60 s to 3,600 s)", cm(sm[scenario == "avg_60s", metric_mean]), cm(sm[scenario == "avg_3600s", metric_mean])), SI)
if (have("TABLE_block_assignment_sensitivity.csv")) { ba <- need("TABLE_block_assignment_sensitivity.csv")
  say("3.8: block assignment sensitivity", sprintf("rounding the coordinates to five decimal places (about 1 m) moves %s%% of the observations in the common blocks into a different block", rh(ba$pct_changed, 1)), MS)
  say("S4.5: block assignment sensitivity", sprintf("rounding the measurement coordinates to five decimal places (about 1 m) moves %s%% of the observations in the common blocks into a different block", rh(ba$pct_changed, 1)), SI)
} else skip("3.8 / S4.5 block assignment", "TABLE_block_assignment_sensitivity.csv not found (run 85)")
if (have("TABLE_wind_source_agreement.csv")) { wa <- need("TABLE_wind_source_agreement.csv")
  say("3.8: wind-source agreement (86)", sprintf("differ by a median of %s degrees, with a 95th percentile of %s degrees, and their angular offsets", pc(wa$median_wd_difference_deg), pc(wa$p95_wd_difference_deg)), MS)
  say("3.8: offset agreement (86)", sprintf("differ by a median of %s degrees, with a 95th percentile of %s degrees.", pc(wa$median_offset_difference_deg), pc(wa$p95_offset_difference_deg)), MS)
} else skip("3.8 wind-source agreement", "TABLE_wind_source_agreement.csv not found (run 86)")
hdr("N. Claims this script does NOT vouch for (sourced from documents, not code)")
cat("  - Permit and TRI quantities in S6.1 / S6.7 / Table S6.1 (119.01 and 2.38 t/yr; 340 lb/yr; 8 t/yr digester gas; 5,819 lb and 22,373 lb TRI)\n")
cat("  - Literature values in S6.4 (>= 15 transects; >= 10 transects; ~95% within +/-70%; slope 0.96; 266 plumes; ~4%)\n")
cat("  - Instrument specifications in S1 / S2 (cadences, calibration ranges, tubing, flow rates)\n")
cat("  - AirToxScreen / IRIS / OEHHA reference values (RfCs, unit risks, RELs, MRLs)\n")
cat("  - HQ-screen record counts (47,643 / 2,602,928 etc.) - checked by 72_check_s14_qaqc.R, not here\n")
cat("  - Hour-of-day, weekday, correlation and speed statistics - checked by tests/audit_manuscript_claims.R\n")

cat(sprintf("\n%d OK, %d FAIL, %d SKIP\n", n_ok, n_fail, n_skip))
if (n_fail > 0) { cat("\nFAIL means the document and the pipeline outputs disagree.\n"); quit(status = 1) }
