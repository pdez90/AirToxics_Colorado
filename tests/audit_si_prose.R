# ==============================================================
# Audit the PROSE of the manuscript and SI against the pipeline's own outputs.
#
# WHY THIS EXISTS (2026-09-26). Every other harness in this project checks the
# RUN against a hardcoded claim string. Nothing checked the DOCUMENT against the
# run, and that gap let two rounding slips reach the SI: section S4.6 read
# "0.154" where the source is 0.1534957 (rounds to 0.153) and "0.107" where the
# source is 0.1064980 (rounds to 0.106) - the second contradicting Table S4.1 in
# the same section. Script 79's own [OK] lines passed throughout, because its
# claims carry the source values at four decimals and the error was made in
# transcribing them to three.
#
# This reads the .docx directly, re-derives each quoted number from the CSV the
# pipeline wrote, rounds it HALF-UP to the precision the prose uses, and requires
# the resulting sentence fragment to appear verbatim. A FAIL therefore means the
# document and the outputs have drifted apart, in either direction.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript tests/audit_si_prose.R
#
# The .docx files are deliberately kept OUT of the repository, so a missing
# document is a SKIP, not a failure. Override either location with SI_DOCX /
# MS_DOCX if they live somewhere else.
# ==============================================================
suppressPackageStartupMessages(library(data.table))

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
SI_DOCX <- path.expand(Sys.getenv("SI_DOCX",
             file.path(dirname(BASE), "Suncor_manuscript", "SI_MobileToxics_CDPHE.docx")))
MS_DOCX <- path.expand(Sys.getenv("MS_DOCX",
             file.path(dirname(BASE), "Suncor_manuscript", "MobileToxics_CDPHE.docx")))

# ---- read a .docx as plain text ------------------------------------------
docx_text <- function(path) {
  if (!file.exists(path)) return(NULL)
  con <- unz(path, "word/document.xml", open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, "raw", n = file.size(path))
  x <- rawToChar(raw); Encoding(x) <- "UTF-8"
  # paragraph and table-cell boundaries become separators so that text from
  # adjacent cells cannot concatenate into a match that is not really there
  x <- gsub("</w:p>", "\n", x, fixed = TRUE)
  x <- gsub("</w:tc>", " | ", x, fixed = TRUE)
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x
}

# ---- round HALF-UP, the way a person rounds ------------------------------
# R's round() is banker's rounding: round(0.1065, 3) gives 0.106 on some values
# and 0.107 on others depending on the binary representation. Prose is written
# by a person, so the check has to use the convention a person uses.
rh <- function(x, d) sprintf(paste0("%.", d, "f"), sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
cm <- function(x) formatC(round(x), format = "d", big.mark = ",")

n_ok <- 0L; n_fail <- 0L; n_skip <- 0L
say <- function(lab, frag, txt, src) {
  if (is.null(txt)) { n_skip <<- n_skip + 1L
    cat(sprintf("  [SKIP] %-44s (document not found)\n", lab)); return(invisible()) }
  hit <- grepl(frag, txt, fixed = TRUE)
  if (hit) n_ok <<- n_ok + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("  [%s] %-44s \"%s\"%s\n", if (hit) "OK  " else "FAIL", lab, frag,
              if (hit) "" else sprintf("\n         source: %s", src)))
}
need <- function(p) { f <- file.path(BASE, p)
  if (!file.exists(f)) stop("missing pipeline output: ", f); fread(f) }

SI <- docx_text(SI_DOCX); MS <- docx_text(MS_DOCX)
cat("SI  : ", if (is.null(SI)) paste("NOT FOUND ->", SI_DOCX) else SI_DOCX, "\n", sep = "")
cat("MS  : ", if (is.null(MS)) paste("NOT FOUND ->", MS_DOCX) else MS_DOCX, "\n", sep = "")

# ==========================================================================
cat("\n== SI section S4.6 vs TABLE_S4.1_background_sensitivity.csv ==\n")
t  <- need("TABLE_S4.1_background_sensitivity.csv")
bs <- t[percentile == 20 & window_min == 20]; nb <- t[!(percentile == 20 & window_min == 20)]
g  <- function(col, f) f(t[[col]])

say("benzene range",
    sprintf("ranges from %s to %s ppb", rh(g("mobile_pw_ppb", min), 3), rh(g("mobile_pw_ppb", max), 3)),
    SI, sprintf("%.7f - %.7f", g("mobile_pw_ppb", min), g("mobile_pw_ppb", max)))
say("AirToxScreen benzene",
    sprintf("against %s ppb for AirToxScreen", rh(t$airtox_pw_ppb[1], 3)), SI, t$airtox_pw_ppb[1])
say("ratio range + published",
    sprintf("ranges from %s to %s, against %s at the published setting",
            rh(g("ratio_mobile_over_airtox", min), 2), rh(g("ratio_mobile_over_airtox", max), 2),
            rh(bs$ratio_mobile_over_airtox, 2)), SI,
    sprintf("%.4f - %.4f, base %.4f", g("ratio_mobile_over_airtox", min),
            g("ratio_mobile_over_airtox", max), bs$ratio_mobile_over_airtox))
say("excess-case range",
    sprintf("%s-%s cases at the lower unit risk and %s-%s at the upper",
            rh(g("mobile_cases_low", min), 3), rh(g("mobile_cases_low", max), 3),
            rh(g("mobile_cases_high", min), 3), rh(g("mobile_cases_high", max), 3)), SI,
    sprintf("%.7f/%.7f  %.7f/%.7f", g("mobile_cases_low", min), g("mobile_cases_low", max),
            g("mobile_cases_high", min), g("mobile_cases_high", max)))
say("AirToxScreen cases",
    sprintf("against %s and %s for AirToxScreen", rh(t$airtox_cases_low[1], 3), rh(t$airtox_cases_high[1], 3)),
    SI, sprintf("%.6f / %.6f", t$airtox_cases_low[1], t$airtox_cases_high[1]))
say("endocrine HI range",
    sprintf("spans %s to %s across the nine settings", rh(g("HI_pwmean_Endocrine", min), 3),
            rh(g("HI_pwmean_Endocrine", max), 3)), SI,
    sprintf("%.5f - %.5f", g("HI_pwmean_Endocrine", min), g("HI_pwmean_Endocrine", max)))
say("respiratory HI range",
    sprintf("spans %s to %s;", rh(g("HI_pwmean_Respiratory", min), 3), rh(g("HI_pwmean_Respiratory", max), 3)),
    SI, sprintf("%.5f - %.5f", g("HI_pwmean_Respiratory", min), g("HI_pwmean_Respiratory", max)))
say("neuro + haem HI ranges",
    sprintf("index spans %s to %s and the hematological index %s to %s",
            rh(g("HI_pwmean_Neurological", min), 3), rh(g("HI_pwmean_Neurological", max), 3),
            rh(g("HI_pwmean_Hematological", min), 3), rh(g("HI_pwmean_Hematological", max), 3)), SI, "")
say("most-exposed-block HI ranges",
    sprintf("endocrine index spans %s to %s and the respiratory index %s to %s",
            rh(g("HI_maxblock_Endocrine", min), 2), rh(g("HI_maxblock_Endocrine", max), 2),
            rh(g("HI_maxblock_Respiratory", min), 2), rh(g("HI_maxblock_Respiratory", max), 2)), SI, "")
say("cells identical",
    sprintf("between %s%% and %s%% of the %d mapped benzene cells",
            rh(min(nb$cells_identical_pct), 0), rh(max(nb$cells_identical_pct), 0), t$n_cells[1]), SI,
    sprintf("%.2f - %.2f", min(nb$cells_identical_pct), max(nb$cells_identical_pct)))
say("cells within one step",
    sprintf("between %s%% and %s%% lie within one 0.05 ppb reporting step",
            rh(min(nb$cells_within_one_step_pct), 0), rh(max(nb$cells_within_one_step_pct), 0)), SI,
    sprintf("%.2f - %.2f", min(nb$cells_within_one_step_pct), max(nb$cells_within_one_step_pct)))
say("largest single-cell change",
    sprintf("is %s ppb on the 24-hour basis", rh(max(t$max_abs_diff_ppb), 2)), SI, max(t$max_abs_diff_ppb))

tb <- need("TABLE_S4.1b_background_sensitivity_cells.csv")
en <- tb[pollutant == "Endocrine" & is.finite(pct_HI_gt1)]
say("cell endocrine exceedance",
    sprintf("ranges from %s%% to %s%%", rh(min(en$pct_HI_gt1), 0), rh(max(en$pct_HI_gt1), 0)), SI,
    sprintf("%.2f - %.2f", min(en$pct_HI_gt1), max(en$pct_HI_gt1)))

# ==========================================================================
cat("\n== SI section S6.5.2 vs the retained-plume outputs ==\n")
p   <- need("TABLE_min_detectable_rate_plumes.csv")
pts <- need("WWTP_H2S_source_attribution_points.csv")
att <- need("WWTP_H2S_source_attribution.csv")
setnames(pts, ".dH2S", "dH2S", skip_absent = TRUE)
mn  <- pts[, .(mean_dH2S = mean(dH2S)), by = plume_id]
r   <- merge(p[, .(plume_id, dH2S_ppb, inferred_tpy)], mn, by = "plume_id")
r[, ratio := dH2S_ppb / mean_dH2S][, q_mean := inferred_tpy / ratio]
dur <- sort(att[plume_id %in% p$plume_id, duration_s])
np  <- range(pts[plume_id %in% p$plume_id, .N, by = plume_id]$N)

say("traverse durations",
    sprintf("last %s and %s s", paste(head(dur, -1), collapse = ", "), tail(dur, 1)), SI, paste(dur, collapse = ","))
say("retained points per traverse",
    sprintf("carry %d to %d retained points each", np[1], np[2]), SI, paste(np, collapse = "-"))
say("peak/mean ratios",
    sprintf("is %s, %s, %s and %s, averaging %s",
            rh(sort(r$ratio)[1], 2), rh(sort(r$ratio)[2], 2), rh(sort(r$ratio)[3], 2),
            rh(sort(r$ratio)[4], 2), rh(mean(r$ratio), 2)), SI,
    paste(sprintf("%.4f", sort(r$ratio)), collapse = " "))
say("traverse-mean vs peak mean",
    sprintf("a mean of %s against %s metric tons/yr", cm(mean(r$q_mean)), cm(mean(p$inferred_tpy))), SI,
    sprintf("%.1f vs %.1f", mean(r$q_mean), mean(p$inferred_tpy)))
say("reduction",
    sprintf("a reduction of %s%%", rh(100 * (1 - mean(r$q_mean) / mean(p$inferred_tpy)), 0)), SI,
    sprintf("%.2f%%", 100 * (1 - mean(r$q_mean) / mean(p$inferred_tpy))))

# ==========================================================================
cat("\n== manuscript ==\n")
say("MS emission caveat",
    sprintf("by %s%%, from %s to %s metric tons/yr",
            rh(100 * (1 - mean(r$q_mean) / mean(p$inferred_tpy)), 0),
            cm(mean(p$inferred_tpy)), cm(mean(r$q_mean))), MS, "")
.dev <- max(abs(t$mobile_pw_ppb - bs$mobile_pw_ppb) / bs$mobile_pw_ppb) * 100
say("MS S4.6 pointer bound",
    sprintf("moves by at most %d%%", ceiling(.dev)), MS, sprintf("max deviation %.2f%%", .dev))

cat(sprintf("\n%d OK, %d FAIL, %d SKIP\n", n_ok, n_fail, n_skip))
if (n_fail > 0) {
  cat("\nFAIL means the document and the pipeline outputs disagree. Fix whichever\n")
  cat("is wrong - usually the document, since the outputs are regenerated.\n")
  quit(status = 1)
}
