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
# Exposure Levels. Because our peaks are sub-minute and the REL averaging time
# is one hour, the acute HQs are conservative upper bounds.
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
  block_col  = c("sBenzene_mean_of_daily_mean","sToluene_mean_of_daily_mean",
                 "sXylene_mean_of_daily_mean","sTrimethylbenzene_mean_of_daily_mean",
                 "sH2S_mean_of_daily_mean","sHCN_mean_of_daily_mean"),
  s31_row    = c("Benzene","Toluene","Xylene","Trimethylbenzene","H2S","HCN"),
  MW         = c(78.11, 92.14, 106.16, 120.19, 34.08, 27.03),
  RfC_ugm3   = c(30, 5000, 100, 60, 2, 0.8),          # IRIS chronic RfC, mg/m3 -> ug/m3
  organ      = c("Hematological","Neurological","Neurological","Neurological",
                 "Respiratory","Endocrine"),
  acuteREL   = c(27, 5000, 22000, 2400, 42, 340)      # OEHHA 1-h acute REL, ug/m3
)
POLL[, cf := MW / MOLAR_VOL]

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

chronic <- rbindlist(lapply(seq_len(nrow(POLL)), function(i) {
  x  <- suppressWarnings(as.numeric(d[[POLL$block_col[i]]]))     # ppb
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
    HQ_pwmean       = signif(pw_ppb  * cf / POLL$RfC_ugm3[i], 3),
    maxblock_ppb    = round(max_ppb, 3),
    maxblock_ugm3   = round(max_ppb * cf, 3),
    HQ_maxblock     = signif(max_ppb * cf / POLL$RfC_ugm3[i], 3))
}))

HI <- chronic[, .(pollutants = paste(pollutant, collapse = " + "),
                  HI_pwmean   = round(sum(HQ_pwmean), 3),
                  HI_maxblock = round(sum(HQ_maxblock), 3)),
              by = target_organ][order(-HI_pwmean)]

fwrite(chronic, OUT1)
message("-> ", OUT1)
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
    HQ_max      = if (is.na(rel)) NA_real_ else signif(mx * cf / rel, 3))
}))
fwrite(acute, OUT2)
message("-> ", OUT2)
cat("\n== SI Table S7.2  acute screen vs OEHHA 1-h acute REL ==\n")
print(acute, row.names = FALSE)

# ==============================================================
# claim-by-claim check of the numbers quoted in SI Section S7
# ==============================================================
cat("\n== SI S7, claim by claim ==\n")
ck <- function(lab, got, claim, tol = 0.01) {
  agree <- is.finite(got) && is.finite(claim) && abs(got - claim) <= tol * max(1, abs(claim))
  cat(sprintf("  [%s] %-46s run %-10s S7 says %s\n",
              if (agree) "OK  " else "EDIT", lab,
              format(signif(got, 4)), format(claim)))
}
gHI <- function(org, which) HI[target_organ == org][[which]]
ck("endocrine HI, pop-weighted mean (HCN)",  gHI("Endocrine","HI_pwmean"),   1.60)
ck("endocrine HI, most-exposed block",       gHI("Endocrine","HI_maxblock"), 8.71)
ck("respiratory HI, pop-weighted mean (H2S)",gHI("Respiratory","HI_pwmean"), 0.371)
ck("respiratory HI, most-exposed block",     gHI("Respiratory","HI_maxblock"),4.97)
ck("neurological HI, pop-weighted mean",     gHI("Neurological","HI_pwmean"), 0.031)
ck("neurological HI, most-exposed block",    gHI("Neurological","HI_maxblock"),0.555)
ck("hematological HI, pop-weighted mean",    gHI("Hematological","HI_pwmean"),0.015)
ck("hematological HI, most-exposed block",   gHI("Hematological","HI_maxblock"),0.237)
ck("acute HQ, benzene at campaign max",      acute[pollutant=="Benzene", HQ_max], 55.0, 0.02)
ck("acute HQ, H2S at campaign max",          acute[pollutant=="H2S",     HQ_max], 9.39, 0.02)
ck("acute HQ, toluene at campaign max",      acute[pollutant=="Toluene", HQ_max], 1.77, 0.02)
ck("acute HQ, 1,2,4-TMB at campaign max",    acute[pollutant=="1,2,4-Trimethylbenzene", HQ_max], 0.404, 0.02)
ck("acute HQ, benzene at p99",               acute[pollutant=="Benzene", HQ_p99], 0.174, 0.02)

# ---- sensitivity noted in S7.3: OEHHA chronic RELs sit far below the IRIS ----
# RfC for two species (benzene 3 vs 30 ug/m3; trimethylbenzenes 4 vs 60 ug/m3).
# Most-exposed-block HQs re-anchored to the OEHHA chronic RELs:
oehha_chr <- c(Benzene = 3, `1,2,4-Trimethylbenzene` = 4)
for (nm in names(oehha_chr)) {
  hq <- chronic[pollutant == nm, maxblock_ugm3] / oehha_chr[[nm]]
  cat(sprintf("  [SENS] %-46s run %-10s (S7.3 says %s)\n",
              paste0(nm, " max-block HQ vs OEHHA chronic REL"),
              format(signif(hq, 3)),
              if (nm == "Benzene") "2.37" else "3.24"))
}

cat("\n  EDIT means the run disagrees with the sentence in S7 and the SI\n")
cat("  should carry the run's number instead.\n")
message("\nDONE.")
