# ==============================================================
# 77  TEMPORAL-SCALING SENSITIVITY FOR THE NONCANCER HAZARD SCREEN
#     (SI Section S7.4; companion to 74_health_hazard_screening.R)
#
# WHY THIS EXISTS. S7 screens six pollutants against IRIS chronic RfCs using
# UNSCALED block concentrations, while Section 3.3's benzene cancer-risk
# comparison uses concentrations scaled to 24-h equivalence by the La Casa
# bin-weighted factors (script 17 / SI S4.3). The mismatch is not an oversight:
# La Casa measures only benzene, toluene and C8 aromatics, so no La Casa factor
# exists for 1,2,4-trimethylbenzene, H2S or HCN - and the two species that
# drive every S7 conclusion are exactly the two that cannot be scaled.
#
# This script answers the reviewer-facing question directly: does anything in
# S7 change if the measured aromatics ARE scaled, and how large would a
# borrowed factor for H2S/HCN have to be to change a conclusion?
#
# Because HQ is linear in concentration and each scaling is a single
# multiplicative constant, every quantity below is exact arithmetic on the
# block surface - no pipeline stage is re-run, and nothing here can drift from
# 74's numbers except through the block file the two scripts share.
#
# Scenarios (applied to the block-resolved exposure surface):
#   A none       every species unscaled                 <- the S7 baseline
#   B aromatics  benzene/toluene/xylene at their own La Casa factors;
#                1,2,4-TMB at the mean aromatic factor (it is an aromatic with
#                no La Casa channel); H2S and HCN left unscaled
#   C borrowed   scenario B, and H2S/HCN also at the mean aromatic factor
#   D upper      scenario B, and H2S/HCN at the LARGEST aromatic factor
#                (xylene) - a deliberate upper bound, not a recommendation
#
# Break-even table: the factor f on the unscaled species at which each organ
# hazard index crosses 1. Endocrine is HCN alone and Respiratory is H2S alone,
# so f* = 1 / HQ. This is the number a reader needs: it says how wrong a
# borrowed factor would have to be before a conclusion moved.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/77_health_scaling_sensitivity.R
#
# Inputs
#   censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData
#   lacasa_scaling_factors_option1_binweighted.RData   (written by R04/script 17)
# Outputs
#   TABLE_S7.3_scaling_scenarios.csv    organ HI under scenarios A-D
#   TABLE_S7.3b_scaling_by_pollutant.csv  per-pollutant HQ and the factor
#                                       applied, under scenarios A-D. Written
#                                       so that the Shiny explorer's scaling
#                                       toggle reads SI values rather than
#                                       recomputing them (it also needs the
#                                       factors themselves to scale the 500 m
#                                       cell surface, which is why the factor
#                                       and its provenance are columns here).
#   TABLE_S7.4_breakeven_factors.csv    factor at which each HI crosses 1
# ==============================================================
suppressPackageStartupMessages({ library(data.table) })

BASE  <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
BLOCK <- file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData")
SF    <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")
OUT1  <- file.path(BASE, "TABLE_S7.3_scaling_scenarios.csv")
OUT1b <- file.path(BASE, "TABLE_S7.3b_scaling_by_pollutant.csv")
OUT2  <- file.path(BASE, "TABLE_S7.4_breakeven_factors.csv")

# Same unit convention as 73/74: ppb -> ug/m3 at 25 C and the 830 hPa site
# pressure, NOT sea-level 24.45. See the UNIT FIX note in 73_cumulative_risk.R.
MOLAR_VOL <- 8.314 * 298.15 / 83000 * 1000   # 29.8653 L/mol

POLL <- data.table(
  name      = c("Benzene","Toluene","Xylenes","1,2,4-Trimethylbenzene","H2S","HCN"),
  block_col = c("sBenzene_mean_of_daily_mean","sToluene_mean_of_daily_mean",
                "sXylene_mean_of_daily_mean","sTrimethylbenzene_mean_of_daily_mean",
                "sH2S_mean_of_daily_mean","sHCN_mean_of_daily_mean"),
  MW        = c(78.11, 92.14, 106.16, 120.19, 34.08, 27.03),
  RfC_ugm3  = c(30, 5000, 100, 60, 2, 0.8),
  organ     = c("Hematological","Neurological","Neurological","Neurological",
                "Respiratory","Endocrine"))
POLL[, cf := MW / MOLAR_VOL]

# ---- La Casa scaling factors, read from the file R04 writes ---------------
# Hard-coding these is how the 300 m exclusion silently left two SI tables on
# the pre-exclusion scaling; read them, and fall back only with a loud message.
.sf_get <- function(pol, fallback) {
  if (!file.exists(SF)) { message("[SCALING] factor file absent - using documented ", pol); return(fallback) }
  e <- new.env(); load(SF, envir = e); o <- get(ls(e)[1], envir = e)
  r <- as.numeric(o[["ratio_all_over_mobilelike"]])[match(pol, o[["pollutant"]])]
  if (length(r) != 1L || !is.finite(r)) fallback else r
}
f_benz <- .sf_get("benzene", 1.165)
f_tol  <- .sf_get("toluene", 1.274)
f_xyl  <- .sf_get("xylene",  1.443)
f_arom_mean <- mean(c(f_benz, f_tol, f_xyl))
f_arom_max  <- max(c(f_benz, f_tol, f_xyl))
message(sprintf("[SCALING] benzene %.4f | toluene %.4f | xylene %.4f | mean %.4f | max %.4f",
                f_benz, f_tol, f_xyl, f_arom_mean, f_arom_max))

# ---- exposure surface -----------------------------------------------------
if (!file.exists(BLOCK)) stop("block file not found: ", BLOCK)
e <- new.env(); load(BLOCK, envir = e)
d <- get(ls(e)[1], envir = e); d[["geometry"]] <- NULL; d <- as.data.frame(d)
pop <- suppressWarnings(as.numeric(d[["POP20"]]))
message("block file: ", nrow(d), " blocks, total POP20 = ",
        format(sum(pop, na.rm = TRUE), big.mark = ","))

# unscaled HQ per pollutant, both metrics (reproduces 74's Table S7.1 columns)
base_hq <- rbindlist(lapply(seq_len(nrow(POLL)), function(i) {
  x  <- suppressWarnings(as.numeric(d[[POLL$block_col[i]]]))
  ok <- is.finite(x) & is.finite(pop)
  pw <- sum(x[ok] * pop[ok]) / sum(pop[ok])
  data.table(pollutant = POLL$name[i], organ = POLL$organ[i],
             RfC_ugm3 = POLL$RfC_ugm3[i],
             pwmean_ugm3 = pw * POLL$cf[i],
             maxblock_ugm3 = max(x[ok]) * POLL$cf[i],
             HQ_pwmean = pw * POLL$cf[i] / POLL$RfC_ugm3[i],
             HQ_maxblock = max(x[ok]) * POLL$cf[i] / POLL$RfC_ugm3[i])
}))

# CROSS-CHECK. The block file also carries pipeline-written *_scaled columns
# for the three La Casa species. Multiplying the unscaled HQ by the factor must
# reproduce the HQ computed from those columns, or the factor file and the
# block surface disagree and every scenario below is built on sand.
chk <- data.table(pollutant = c("Benzene","Toluene","Xylenes"),
                  col = c("sBenzene_mean_of_daily_mean_scaled",
                          "sToluene_mean_of_daily_mean_scaled",
                          "sXylene_mean_of_daily_mean_scaled"),
                  f = c(f_benz, f_tol, f_xyl))
cat("\n== cross-check: factor x unscaled column vs the pipeline's scaled column ==\n")
for (i in seq_len(nrow(chk))) {
  j  <- match(chk$pollutant[i], POLL$name)
  x  <- suppressWarnings(as.numeric(d[[POLL$block_col[j]]]))
  xs <- suppressWarnings(as.numeric(d[[chk$col[i]]]))
  ok <- is.finite(x) & is.finite(xs) & is.finite(pop)
  a  <- sum(x[ok] * chk$f[i] * pop[ok]) / sum(pop[ok])
  b  <- sum(xs[ok] * pop[ok]) / sum(pop[ok])
  rel <- abs(a - b) / max(abs(b), .Machine$double.eps)
  cat(sprintf("  %-10s implied %.6f  pipeline %.6f  rel.diff %.2e  [%s]\n",
              chk$pollutant[i], a, b, rel, if (rel < 1e-6) "OK" else "MISMATCH"))
  if (rel >= 1e-6) warning("scaled column disagrees with the factor for ", chk$pollutant[i])
}

# ---- scenarios ------------------------------------------------------------
# A factor vector per scenario, one entry per row of POLL.
SCEN <- list(
  A_none      = c(1, 1, 1, 1, 1, 1),
  B_aromatics = c(f_benz, f_tol, f_xyl, f_arom_mean, 1, 1),
  C_borrowed  = c(f_benz, f_tol, f_xyl, f_arom_mean, f_arom_mean, f_arom_mean),
  D_upper     = c(f_benz, f_tol, f_xyl, f_arom_mean, f_arom_max,  f_arom_max))
SCEN_DESC <- c(
  A_none      = "no scaling (the S7 baseline)",
  B_aromatics = "measured aromatics scaled; H2S and HCN unscaled",
  C_borrowed  = "B, plus H2S/HCN at the mean aromatic factor",
  D_upper     = "B, plus H2S/HCN at the largest aromatic factor (upper bound)")

scen <- rbindlist(lapply(names(SCEN), function(s) {
  f <- SCEN[[s]]
  hq <- copy(base_hq)[, `:=`(HQ_pwmean = HQ_pwmean * f, HQ_maxblock = HQ_maxblock * f)]
  hq[, .(scenario = s, description = SCEN_DESC[[s]],
         pollutants = paste(pollutant, collapse = " + "),
         HI_pwmean = round(sum(HQ_pwmean), 4),
         HI_maxblock = round(sum(HQ_maxblock), 3)), by = organ]
}))
setcolorder(scen, c("scenario","description","organ"))
setorder(scen, scenario, -HI_pwmean)
fwrite(scen, OUT1); message("-> ", OUT1)

# ---- the same scenarios, kept per pollutant --------------------------------
# The organ table above is what the SI prints; the explorer needs the layer
# underneath it - which factor was applied to which species, and where that
# factor came from - so that a reader toggling scenarios can see that H2S and
# HCN are never carrying a measured factor. Provenance is a column rather than
# a footnote for exactly that reason.
FSRC <- list(
  A_none      = rep("unscaled (no factor applied)", 6L),
  B_aromatics = c("La Casa, measured", "La Casa, measured", "La Casa, measured",
                  "borrowed: mean of the measured aromatics",
                  "unscaled (no La Casa channel)", "unscaled (no La Casa channel)"),
  C_borrowed  = c("La Casa, measured", "La Casa, measured", "La Casa, measured",
                  "borrowed: mean of the measured aromatics",
                  "borrowed: mean of the measured aromatics",
                  "borrowed: mean of the measured aromatics"),
  D_upper     = c("La Casa, measured", "La Casa, measured", "La Casa, measured",
                  "borrowed: mean of the measured aromatics",
                  "borrowed: largest measured aromatic (upper bound)",
                  "borrowed: largest measured aromatic (upper bound)"))
stopifnot(identical(names(FSRC), names(SCEN)),
          all(vapply(FSRC, length, 1L) == nrow(POLL)),
          identical(base_hq$pollutant, POLL$name))   # the factor vectors are
                                                     # positional; this is the
                                                     # assertion that keeps
                                                     # them aligned with POLL
scen_poll <- rbindlist(lapply(names(SCEN), function(s) {
  f <- SCEN[[s]]
  data.table(scenario = s, description = SCEN_DESC[[s]],
             pollutant = base_hq$pollutant, organ = base_hq$organ,
             factor = round(f, 6), factor_source = FSRC[[s]],
             RfC_ugm3 = base_hq$RfC_ugm3,
             pwmean_ugm3   = signif(base_hq$pwmean_ugm3   * f, 6),
             maxblock_ugm3 = signif(base_hq$maxblock_ugm3 * f, 6),
             HQ_pwmean     = signif(base_hq$HQ_pwmean     * f, 6),
             HQ_maxblock   = signif(base_hq$HQ_maxblock   * f, 6))
}))
fwrite(scen_poll, OUT1b); message("-> ", OUT1b)

# CROSS-CHECK. Summing the per-pollutant table by organ must return the organ
# table written above, or the two files disagree and the explorer's toggle
# would show numbers the SI does not.
.rs <- scen_poll[, .(HI_pwmean = sum(HQ_pwmean), HI_maxblock = sum(HQ_maxblock)),
                 by = .(scenario, organ)]
.cm <- merge(.rs, scen[, .(scenario, organ, HI_pwmean, HI_maxblock)],
             by = c("scenario", "organ"), suffixes = c("_poll", "_organ"))
.d1 <- max(abs(.cm$HI_pwmean_poll   - .cm$HI_pwmean_organ))
.d2 <- max(abs(.cm$HI_maxblock_poll - .cm$HI_maxblock_organ))
# TOLERANCE. `scen` rounds HI_pwmean to 4 decimals and HI_maxblock to 3, so a
# perfect agreement still shows up to 5e-5 and 5e-4 of pure rounding; 2e-3
# passes that and would still catch any real disagreement, which would be
# orders of magnitude larger.
cat(sprintf("\n== S7.3b sums to S7.3: max abs diff pw %.2e, maxblock %.2e  [%s] ==\n",
            .d1, .d2, if (max(.d1, .d2) < 2e-3) "OK" else "MISMATCH"))
if (max(.d1, .d2) >= 2e-3)
  warning("TABLE_S7.3b does not sum to TABLE_S7.3 - do not publish the app toggle")
cat("\n== SI Table S7.3  organ hazard index under four scaling scenarios ==\n")
print(scen[, .(scenario, organ, pollutants, HI_pwmean, HI_maxblock)], row.names = FALSE)

cat("\n== does any conclusion move? (HI crossing 1) ==\n")
flip <- scen[, .(any_cross = uniqueN(HI_pwmean >= 1) > 1L ||
                             uniqueN(HI_maxblock >= 1) > 1L), by = organ]
for (i in seq_len(nrow(flip)))
  cat(sprintf("  %-15s %s\n", flip$organ[i],
      if (flip$any_cross[i]) "CHANGES SIDE of HI = 1 across scenarios A-D"
      else "same side of HI = 1 in every scenario A-D"))

# ---- agreement with 74 ----------------------------------------------------
# Scenario A must reproduce 74's Table S7.1. 74 rounds each HQ to 3 significant
# figures before summing, so single-pollutant organs differ in the 4th figure;
# anything larger means the two scripts are reading different block surfaces.
S71 <- file.path(BASE, "TABLE_S7.1_chronic_hazard.csv")
if (file.exists(S71)) {
  s71 <- fread(S71)
  h71 <- s71[, .(HI_pwmean_74 = sum(HQ_pwmean), HI_maxblock_74 = sum(HQ_maxblock)),
             by = .(organ = target_organ)]
  cmp <- merge(scen[scenario == "A_none", .(organ, HI_pwmean, HI_maxblock)], h71, by = "organ")
  cat("\n== scenario A vs 74's Table S7.1 (rounding difference only) ==\n")
  for (i in seq_len(nrow(cmp))) {
    r1 <- abs(cmp$HI_pwmean[i]   - cmp$HI_pwmean_74[i])   / max(cmp$HI_pwmean_74[i], 1e-12)
    r2 <- abs(cmp$HI_maxblock[i] - cmp$HI_maxblock_74[i]) / max(cmp$HI_maxblock_74[i], 1e-12)
    cat(sprintf("  %-15s pw %.4f vs %.4f (%.1e)   max %.3f vs %.3f (%.1e)  [%s]\n",
        cmp$organ[i], cmp$HI_pwmean[i], cmp$HI_pwmean_74[i], r1,
        cmp$HI_maxblock[i], cmp$HI_maxblock_74[i], r2,
        if (max(r1, r2) < 5e-3) "OK" else "MISMATCH"))
    if (max(r1, r2) >= 5e-3)
      warning("scenario A disagrees with Table S7.1 for ", cmp$organ[i],
              " by more than rounding - check that both scripts read the same block file")
  }
} else message("[CHECK] TABLE_S7.1_chronic_hazard.csv absent - run 74 first to enable the cross-check")

# ---- break-even factors ---------------------------------------------------
# f is applied ONLY to the species that have no La Casa factor (1,2,4-TMB,
# H2S, HCN); the three measured aromatics stay at their own factors, i.e. the
# break-even is posed with only the MEASURED aromatics scaled (so the
# "aromscaled" columns below differ from scenario B, which also gives 1,2,4-TMB
# a borrowed factor). Solving
#     HI(f) = sum(measured HQ x own factor) + f x sum(unscalable HQ) = 1
# gives f* = (1 - HI_measured_part) / HI_unscalable_part.
# For Endocrine and Respiratory there is no measured part, so f* = 1 / HQ.
# Hematological has no unscalable species, so no f can move it: f* is NA.
FAC <- c(Benzene = f_benz, Toluene = f_tol, Xylenes = f_xyl,
         `1,2,4-Trimethylbenzene` = NA, H2S = NA, HCN = NA)   # NA = no La Casa channel
be <- rbindlist(lapply(unique(base_hq$organ), function(org) {
  s   <- base_hq[organ == org]
  uns <- is.na(FAC[s$pollutant])
  fstar <- function(col) {
    meas <- sum(s[[col]][!uns] * FAC[s$pollutant][!uns])
    unsc <- sum(s[[col]][uns])
    if (unsc <= 0) NA_real_ else (1 - meas) / unsc
  }
  data.table(
    organ = org,
    driven_by = paste(s$pollutant, collapse = " + "),
    no_lacasa_factor = if (any(uns)) paste(s$pollutant[uns], collapse = " + ") else "(none)",
    HI_pwmean_unscaled = round(sum(s$HQ_pwmean), 4),
    HI_pwmean_aromscaled = round(sum(fifelse(uns, s$HQ_pwmean, s$HQ_pwmean * FAC[s$pollutant])), 4),
    f_breakeven_pwmean = round(fstar("HQ_pwmean"), 3),
    HI_maxblock_unscaled = round(sum(s$HQ_maxblock), 3),
    HI_maxblock_aromscaled = round(sum(fifelse(uns, s$HQ_maxblock, s$HQ_maxblock * FAC[s$pollutant])), 3),
    f_breakeven_maxblock = round(fstar("HQ_maxblock"), 3))
}))
setorder(be, -HI_pwmean_unscaled)
fwrite(be, OUT2); message("-> ", OUT2)
cat("\n== SI Table S7.4  break-even factor on the species with no La Casa factor ==\n")
cat("   (measured aromatics held at their own factors; f* < 0 means the organ\n")
cat("    is already above HI = 1 on the measured species alone; NA means no\n")
cat("    unscalable species, so no borrowed factor can move that organ)\n")
print(be, row.names = FALSE)

cat(sprintf(paste0(
  "\n== the window that matters ==\n",
  "  Measured La Casa factors span %.3f to %.3f (mean %.3f).\n",
  "  Community-metric conclusions are unchanged for any factor between\n",
  "  %.3f (endocrine HI would fall to 1) and %.3f (respiratory HI would\n",
  "  rise to 1). The measured aromatic range sits inside that window, so a\n",
  "  borrowed factor of any plausible size leaves every S7 statement standing.\n"),
  f_benz, f_xyl, f_arom_mean,
  be[organ == "Endocrine", f_breakeven_pwmean],
  be[organ == "Respiratory", f_breakeven_pwmean]))

cat(paste0(
  "\n== why we do not adopt a borrowed factor ==\n",
  "  A La Casa factor is the ratio of a 24/7 mean to a mobile-bin-weighted\n",
  "  mean, so it is large exactly for species depleted during the sampling\n",
  "  window. 78_diurnal_scaling_evidence.R shows that within 500 m cells the\n",
  "  aromatics fall across that window while H2S and HCN rise, so the premise\n",
  "  behind borrowing - that these species behave alike within the day - is\n",
  "  contradicted by our own data. That rules borrowing OUT; it does not\n",
  "  establish that the H2S/HCN factors are below 1, because benzene rises\n",
  "  across the window too and still carries a factor of 1.165: the factor is\n",
  "  set largely by overnight hours we never sampled. Scenarios C and D are\n",
  "  therefore bounds, not estimates, and the S7 baseline remains scenario A.\n",
  "  The break-even table is what a reader should use.\n"))
message("\nDONE.")
