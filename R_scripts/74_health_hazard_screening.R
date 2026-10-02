# ==============================================================
# 74  Screening-level cumulative noncancer hazard assessment  (SI Section S7)
#
# Extends the manuscript's benzene cancer-risk analysis (Section 3.3) to a
# screening-level, cumulative NONCANCER hazard assessment for the six measured
# pollutants, following the hazard-quotient / hazard-index framework used for
# fenceline-community mobile data by Chiger et al. 2025 (Environ. Health
# Perspect. 133:057004) and, for cancer risk, by Robinson et al. 2025 (PNAS
# 122:e2504770122).
#
# NUMBERED 74 because 73_cumulative_risk.R already exists: that script is the
# 500 m grid-CELL analysis of the same question (background-corrected s*
# columns, median-of-daily-medians primary, >=10 visit-days), run by
# MAKE_FIGURES group T. THIS script is the census-BLOCK analysis quoted in SI
# Section S7 (population-weighted, same exposure surface as the Section 3.3
# cancer-risk comparison); S7.3 cites the cell analysis as the sensitivity
# check. The two agree on every qualitative conclusion.
#
#   HQ_i        = EC_i / RfC_i                         (Chiger Eq. 1)
#   HI_organ    = sum over pollutants i of HQ_i        (Chiger Eq. 2)
#                 that act on that target organ system
#
# HQ or HI >= 1 flags an exposure above the level judged to be without
# appreciable risk of that effect. EC is the exposure concentration; RfC is the
# EPA IRIS chronic inhalation reference concentration. Each pollutant is placed
# under the target organ system of its IRIS critical effect ("traditional"
# approach of Chiger et al.; see NOTE on the multi-effects extension below).
#
# Two exposure metrics per pollutant, both from block-resolved concentrations
# (pipeline output censusblocks_..._BINWEIGHTED_AB_overlap.RData):
#   - population-weighted mean of the block mean-of-daily-mean concentration
#     (a community-representative long-term average), and
#   - the single most-exposed block (worst-case).
# Negative and below-MDL values are RETAINED in the block means, exactly as in
# every other pipeline stage (see REPRODUCIBILITY.md); population weighting and
# averaging make the community metric unbiased.
#
# A companion ACUTE screen compares the campaign 99th-percentile and maximum
# short-term concentrations (SI Table S3.1) with OEHHA 1-hour acute Reference
# Exposure Levels. Our peaks are sub-minute and the REL averaging time is one
# hour, so both are short-duration comparisons, not bounds on hourly exposure.
# For H2S and HCN the comparison is also made on the DELIVERED values
# (Table S3.1 *_delivered columns), because the 5-s / 2-s bin averaging of
# 03_checks_flags.R lowers peaks (H2S maximum 345.6 ppb as a bin mean, 481 ppb
# as delivered).
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/74_health_hazard_screening.R
#
# Inputs
#   censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData   block means (ppb)
#   TABLE_S3.1.csv                                              written by 70_table_s31.R
# Outputs
#   TABLE_S7.1_chronic_hazard.csv     per-pollutant chronic HQ + per-organ HI
#   TABLE_S7.2_acute_screen.csv       per-pollutant acute HQ vs OEHHA acute REL
#
# Reference values are external toxicity constants, not derived from the data;
# each is hard-coded with its source and verified against the primary document
# on 2026-08-23:
#   IRIS chronic RfC (mg/m3), critical effect -> target organ system
#     Benzene   3e-2   decreased lymphocyte count      Hematological  (IRIS 2003)
#     Toluene   5e+0   neurological (CNS/PNS)          Neurological   (IRIS 2005)
#     Xylenes   1e-1   impaired motor coordination     Neurological   (IRIS 2003)
#     1,2,4-TMB 6e-2   decreased pain sensitivity      Neurological   (IRIS 2016;
#                      the TMB RfC applies to any TMB isomer or mixture)
#     H2S       2e-3   olfactory-mucosa nasal lesions  Respiratory    (IRIS 2003)
#     HCN       8e-4   thyroid enlargement             Endocrine      (IRIS 2010;
#                      secondary CNS concern noted by IRIS)
#   OEHHA acute 1-h REL (ug/m3), current REL summary table (accessed 2026-08-23;
#   table last modified 2026-05-21): Benzene 27 (2014), Toluene 5000 (2020
#   revision -- NOT the older 37000), Xylenes 22000, Trimethylbenzenes 2400
#   (adopted after the 2023 SRP review), H2S 42, HCN 340.
#
# NOTE (multi-effects extension). Chiger et al. also pilot an expanded
# "multi-effects toxicity database" (METDB) that assigns each chemical to every
# target organ system with a toxicity value, not only its most-sensitive one.
# With only six pollutants, each carrying a single IRIS RfC, the traditional and
# expanded assignments coincide here except that benzene's effect is also
# immunological and HCN's is secondarily neurological. We therefore report the
# traditional (most-sensitive-organ) HIs and flag the METDB extension as a
# limitation in S7, rather than deriving new toxicity values.
# ==============================================================
suppressPackageStartupMessages({library(data.table)})

BASE  <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
BLOCK <- file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData")
S31   <- file.path(BASE, "TABLE_S3.1.csv")
OUT1  <- file.path(BASE, "TABLE_S7.1_chronic_hazard.csv")
OUT2  <- file.path(BASE, "TABLE_S7.2_acute_screen.csv")

# EXPOSURE BASIS (2026-09-27). The chronic screen used the block MEAN of daily
# means while the benzene cancer comparison of section 3.3 used the block
# MEDIAN of daily medians, and a reviewer pointed out that the two headline
# conclusions were each conditional on the statistic chosen for them. The
# primary basis for BOTH is now the median of daily medians (the section 3.3
# statistic); the mean basis is computed alongside and written to *_meanbasis
# files and to TABLE_S7.1c_basis_comparison.csv, and is discussed in SI S7 as
# the exposure-relevant (but outlier-sensitive) alternative. On the mean basis
# the community endocrine index is 1.61; on the median basis it is 0.90.
# Override with HAZARD_BASIS=mean_of_daily_mean to reproduce the old primary.
# (2026-09-30) The primary basis is the block MEAN of daily means again, now for
# the whole paper (hazard screen, benzene cancer comparison and the maps); the
# median of daily medians is the supplementary analysis (_medianbasis files and
# TABLE_S7.1c). HAZARD_BASIS=med_of_daily_med swaps them.
HAZARD_BASIS <- Sys.getenv("HAZARD_BASIS", "mean_of_daily_mean")
stopifnot(HAZARD_BASIS %in% c("med_of_daily_med", "mean_of_daily_mean"))
OTHER_BASIS  <- setdiff(c("med_of_daily_med", "mean_of_daily_mean"), HAZARD_BASIS)
message("[BASIS] primary block statistic: ", HAZARD_BASIS, "  (secondary: ", OTHER_BASIS, ")")

# ppb -> ug/m3 at 25 C and the 830 hPa SITE pressure: ug/m3 = ppb * MW / 29.8653.
# NOT the sea-level 24.45: every other ppb<->ug/m3 conversion in this paper is
# at site pressure - the benzene IURs in Section 2.4 (5.75 and 20.40 per
# million per ppb) are IRIS's 2.2e-6 and 7.8e-6 per ug/m3 converted at this
# same 29.87 L/mol, and 18_*census_block* converts AirToxScreen the same way.
# See the UNIT FIX note in 73_cumulative_risk.R. Exposure happens in Denver air
# at ~1600 m; using 24.45 here would overstate every HQ by 22%.
MOLAR_VOL <- 8.314 * 298.15 / 83000 * 1000   # 29.8653 L/mol

# ---- reference table -----------------------------------------------------
POLL <- data.table(
  name       = c("Benzene","Toluene","Xylenes","1,2,4-Trimethylbenzene","H2S","HCN"),
  poll       = c("Benzene","Toluene","Xylene","Trimethylbenzene","H2S","HCN"),
  s31_row    = c("Benzene","Toluene","Xylene","Trimethylbenzene","H2S","HCN"),
  MW         = c(78.11, 92.14, 106.16, 120.19, 34.08, 27.03),
  RfC_ugm3   = c(30, 5000, 100, 60, 2, 0.8),          # IRIS chronic RfC, mg/m3 -> ug/m3
  organ      = c("Hematological","Neurological","Neurological","Neurological",
                 "Respiratory","Endocrine"),
  acuteREL   = c(27, 5000, 22000, 2400, 42, 340)      # OEHHA 1-h acute REL, ug/m3
)
POLL[, cf := MW / MOLAR_VOL]
POLL[, block_col := paste0("s", poll, "_", HAZARD_BASIS)]

# ==============================================================
# chronic hazard: block-resolved exposure
# ==============================================================
if (!file.exists(BLOCK)) stop("block file not found: ", BLOCK)
e <- new.env(); load(BLOCK, envir = e)
d <- get(ls(e)[1], envir = e)
d[["geometry"]] <- NULL
d <- as.data.frame(d)
pop <- suppressWarnings(as.numeric(d[["POP20"]]))
message("block file: ", nrow(d), " blocks, total POP20 = ",
        format(sum(pop, na.rm = TRUE), big.mark = ","))

hazard_on <- function(basis) {
  cols <- paste0("s", POLL$poll, "_", basis)
chronic <- rbindlist(lapply(seq_len(nrow(POLL)), function(i) {
    x  <- suppressWarnings(as.numeric(d[[cols[i]]]))     # ppb
    ok <- is.finite(x) & is.finite(pop)
    pop_cov  <- sum(pop[ok])
    pw_ppb   <- sum(x[ok] * pop[ok]) / pop_cov                    # pop-weighted mean, ppb
    max_ppb  <- max(x[ok])
    cf       <- POLL$cf[i]
    data.table(
      pollutant       = POLL$name[i],
      target_organ    = POLL$organ[i],
      RfC_ugm3        = POLL$RfC_ugm3[i],
      n_blocks        = sum(ok),
      pop_covered     = round(pop_cov),
      pwmean_ppb      = round(pw_ppb, 4),
      pwmean_ugm3     = round(pw_ppb * cf, 4),
      HQ_pwmean       = signif(pw_ppb  * cf / POLL$RfC_ugm3[i], 4),   # 4 s.f. (2026-09-27): 3 s.f. double-rounded 0.8947 -> 0.895 -> "0.90"
      maxblock_ppb    = round(max_ppb, 3),
      maxblock_ugm3   = round(max_ppb * cf, 3),
      HQ_maxblock     = signif(max_ppb * cf / POLL$RfC_ugm3[i], 4))
  }))
  
  # WITHIN-BLOCK MAXIMUM (2026-09-27). HI_maxblock was sum(HQ_maxblock): the sum
  # of each pollutant's own most-exposed block. For the three single-pollutant
  # organ systems that is the same thing, but the neurological index combines
  # toluene, xylenes and 1,2,4-trimethylbenzene, and their maxima do not fall in
  # one block (toluene and xylenes peak in 080310036011000, trimethylbenzene in
  # 080310041032013). Summing them gave 0.555, an index no block experiences. The
  # index is now computed block by block, over blocks where every organ pollutant
  # is finite, and its maximum taken. The old sum is kept in a separate column so
  # the two can be told apart; it is an upper bound, not an exposure.
  .hq_block <- lapply(seq_len(nrow(POLL)), function(i) {
    x <- suppressWarnings(as.numeric(d[[cols[i]]]))
    x * POLL$cf[i] / POLL$RfC_ugm3[i] })
  HI <- rbindlist(lapply(unique(POLL$organ), function(og) {
    idx <- which(POLL$organ == og)
    M <- do.call(cbind, .hq_block[idx])
    ok <- is.finite(pop) & rowSums(!is.finite(M)) == 0L
    hi_b <- rowSums(M[ok, , drop = FALSE])
    j <- which.max(hi_b)
    data.table(target_organ = og,
               pollutants = paste(POLL$name[idx], collapse = " + "),
               HI_pwmean = round(sum(chronic[target_organ == og, HQ_pwmean]), 5),   # 5 dp (2026-09-27: 4 dp double-rounded 0.89473 to 0.90); 77 cross-checks this file at 5e-3
               HI_maxblock = round(max(hi_b), 3),
               HI_maxblock_block = as.character(d[["GEOID20"]][ok][j]),
               n_blocks_all_pollutants = sum(ok),
               HI_maxblock_sum_of_maxima = round(sum(chronic[target_organ == og, HQ_maxblock]), 3))
  }))[order(-HI_pwmean)]
  
  
  list(chronic = chronic, HI = HI)
}
.pri <- hazard_on(HAZARD_BASIS); chronic <- .pri$chronic; HI <- .pri$HI
.sec <- hazard_on(OTHER_BASIS)
OUT1s <- sub("\\.csv$", if (OTHER_BASIS == "mean_of_daily_mean") "_meanbasis.csv" else "_medianbasis.csv", OUT1)
fwrite(.sec$chronic, OUT1s); message("-> ", OUT1s, "  (secondary basis: ", OTHER_BASIS, ")")
fwrite(chronic, OUT1)
message("-> ", OUT1, "  (basis: ", HAZARD_BASIS, ")")
OUT1b <- file.path(BASE, "TABLE_S7.1b_hazard_index_by_organ.csv")
fwrite(HI, OUT1b)
# both bases side by side, for SI S7 and the basis discussion
.tag <- function(b) if (b == "med_of_daily_med") "median_of_daily_medians" else "mean_of_daily_means"
cmpb <- rbindlist(list(
  cbind(basis = .tag(HAZARD_BASIS), role = "primary",   .pri$HI[, .(target_organ, pollutants, HI_pwmean, HI_maxblock, HI_maxblock_block, n_blocks_all_pollutants)]),
  cbind(basis = .tag(OTHER_BASIS),  role = "secondary", .sec$HI[, .(target_organ, pollutants, HI_pwmean, HI_maxblock, HI_maxblock_block, n_blocks_all_pollutants)])))
OUT1c <- file.path(BASE, "TABLE_S7.1c_basis_comparison.csv")
fwrite(cmpb, OUT1c); message("-> ", OUT1c)
cat("\n== hazard index by organ, both exposure bases ==\n"); print(cmpb, row.names = FALSE)
message("-> ", OUT1b, "  (HI_maxblock is the within-block maximum; the sum of",
        " separate maxima is kept alongside it)")
cat("\n== SI Table S7.1  chronic hazard quotients (block-resolved) ==\n")
print(chronic, row.names = FALSE)
cat("\n== chronic hazard INDEX by target organ system ==\n")
print(HI, row.names = FALSE)

# ==============================================================
# acute screen: peak concentration vs OEHHA 1-h acute REL
# ==============================================================
if (!file.exists(S31)) stop("Table S3.1 not found. Run 70_table_s31.R first.")
s <- fread(S31)
acute <- rbindlist(lapply(seq_len(nrow(POLL)), function(i) {
  r   <- s[pollutant == POLL$s31_row[i]]
  cf  <- POLL$cf[i]; rel <- POLL$acuteREL[i]
  p99 <- as.numeric(r$p99); mx <- as.numeric(r$max)
  data.table(
    pollutant   = POLL$name[i],
    acuteREL_ugm3 = rel,
    p99_ppb     = p99,
    p99_ugm3    = round(p99 * cf, 2),
    HQ_p99      = if (is.na(rel)) NA_real_ else signif(p99 * cf / rel, 3),
    max_ppb     = mx,
    max_ugm3    = round(mx * cf, 1),
    HQ_max      = if (is.na(rel)) NA_real_ else signif(mx * cf / rel, 3),
    p99_ppb_delivered  = if ("p99_delivered" %in% names(r)) as.numeric(r$p99_delivered) else NA_real_,
    max_ppb_delivered  = if ("max_delivered" %in% names(r)) as.numeric(r$max_delivered) else NA_real_)
}))
acute[, `:=`(p99_ugm3_delivered = round(p99_ppb_delivered * POLL$cf, 2),
             HQ_p99_delivered   = signif(p99_ppb_delivered * POLL$cf / acuteREL_ugm3, 3),
             max_ugm3_delivered = round(max_ppb_delivered * POLL$cf, 1),
             HQ_max_delivered   = signif(max_ppb_delivered * POLL$cf / acuteREL_ugm3, 3))]
fwrite(acute, OUT2)
message("-> ", OUT2)
cat("\n== SI Table S7.2  acute screen vs OEHHA 1-h acute REL ==\n")
print(acute, row.names = FALSE)

# ==============================================================
# claim-by-claim check of the numbers quoted in SI Section S7
# ==============================================================
cat("\n== SI S7, claim by claim (mean-of-daily-means basis unless HAZARD_BASIS is set) ==\n")
ck <- function(lab, got, claim, tol = 0.01) {
  agree <- is.finite(got) && is.finite(claim) && abs(got - claim) <= tol * max(1, abs(claim))
  cat(sprintf("  [%s] %-46s run %-10s S7 says %s\n",
              if (agree) "OK  " else "EDIT", lab,
              format(signif(got, 4)), format(claim)))
}
gHI <- function(org, which) HI[target_organ == org][[which]]
ck("endocrine HI, pop-weighted mean (HCN)",  gHI("Endocrine","HI_pwmean"),   1.619)
ck("endocrine HI, most-exposed block",       gHI("Endocrine","HI_maxblock"), 9.24)
ck("respiratory HI, pop-weighted mean (H2S)",gHI("Respiratory","HI_pwmean"), 0.356)
ck("respiratory HI, most-exposed block",     gHI("Respiratory","HI_maxblock"),4.82)
ck("neurological HI, pop-weighted mean",     gHI("Neurological","HI_pwmean"), 0.031)
ck("neurological HI, most-exposed block",    gHI("Neurological","HI_maxblock"),0.509)  # within-block (mean basis; 0.282 on the median basis)
ck("hematological HI, pop-weighted mean",    gHI("Hematological","HI_pwmean"),0.015)
ck("hematological HI, most-exposed block",   gHI("Hematological","HI_maxblock"),0.237)
ck("acute HQ, benzene at campaign max",      acute[pollutant=="Benzene", HQ_max], 55.0, 0.02)
ck("acute HQ, H2S at campaign max",          acute[pollutant=="H2S",     HQ_max], 9.39, 0.02)
ck("acute HQ, toluene at campaign max",      acute[pollutant=="Toluene", HQ_max], 1.77, 0.02)
ck("acute HQ, 1,2,4-TMB at campaign max",    acute[pollutant=="1,2,4-Trimethylbenzene", HQ_max], 0.404, 0.02)
ck("acute HQ, benzene at p99",               acute[pollutant=="Benzene", HQ_p99], 0.174, 0.02)
ck("acute HQ, H2S at delivered maximum",     acute[pollutant=="H2S",     HQ_max_delivered], 13.1, 0.02)

# ---- sensitivity noted in S7.3: OEHHA chronic RELs sit far below the IRIS ----
# RfC for two species (benzene 3 vs 30 ug/m3; trimethylbenzenes 4 vs 60 ug/m3).
# Most-exposed-block HQs re-anchored to the OEHHA chronic RELs:
oehha_chr <- c(Benzene = 3, `1,2,4-Trimethylbenzene` = 4)
for (nm in names(oehha_chr)) {
  hq <- chronic[pollutant == nm, maxblock_ugm3] / oehha_chr[[nm]]
  cat(sprintf("  [SENS] %-46s run %-10s (S7.3 says %s)\n",
              paste0(nm, " max-block HQ vs OEHHA chronic REL"),
              format(signif(hq, 3)),
              if (nm == "Benzene") "1.74" else "2.85"))
}

cat("\n  EDIT means the run disagrees with the sentence in S7 and the SI\n")
cat("  should carry the run's number instead.\n")
message("\nDONE.")
