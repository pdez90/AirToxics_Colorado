# ==============================================================
# 73  CUMULATIVE RISK: non-cancer hazard indices + total cancer risk
# --------------------------------------------------------------
# Extends the health interpretation beyond the AirToxScreen comparison,
# following two DeCarlo-group papers:
#
#  (A) NON-CANCER - Chiger et al. 2025, Environ Health Perspect 133(5):057004
#      Eq 1:  HQ_i   = EC_i / RfC_i
#      Eq 2:  HI_TOS = sum_i HQ_i,TOS   (summed WITHIN a target organ system)
#      HQ or HI > 1 => potential concern.
#
#  (B) CANCER - Robinson et al. 2025, PNAS 122(41):e2504770122
#      Risk_i = EC_i * IUR_i ; total = sum_i Risk_i, expressed as excess cancer
#      cases per one million, benchmarked against USEPA's 100-in-one-million.
#      CAVEAT: benzene is the ONLY species we measure with an IRIS IUR, so our
#      "total" is a strict LOWER BOUND. Robinson et al. summed 17 carcinogens
#      dominated by ethylene oxide, chloroprene and formaldehyde.
#
# EXPOSURE DATASET - deliberately IDENTICAL to the maps and census-block work
# so that Figure 2, the block-level risk analysis and these health metrics all
# rest on one exposure basis:
#   * background-corrected concentrations (s* columns, script 11)
#   * MEDIAN of daily medians per 500 m cell            [PRIMARY]
#   * 24-h temporal scaling (SI S4.1) where a factor exists
#   * MEAN of daily means per cell                      [SENSITIVITY]
#
# Note on convention: EPA risk guidance (RAGS Part A) and Chiger et al. use the
# 95% UCL of the arithmetic mean, since chronic dose is proportional to the
# time-integrated (mean) concentration. The median of daily medians is more
# robust to transient on-road plumes and is the metric used throughout this
# study; because it suppresses episodic peaks that do contribute to long-term
# dose, it is the more CONSERVATIVE (lower) exposure estimate. Both are
# reported so the difference is explicit.
#
# Outputs:
#   TABLE_cumulative_HQ_by_cell.csv       per-cell EC, HQ, HI by target organ
#   TABLE_cumulative_HQ_summary.csv       per-pollutant summary
#   TABLE_cumulative_HI_summary.csv       HI by target organ system, both metrics
#   TABLE_cumulative_cancer_risk.csv      per-cell benzene cancer risk
#   TABLE_cumulative_cancer_summary.csv   domain cancer summary
#   FinalFig/FIG_cumulative_HI.png        HI by target organ system
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2)
})

BASE       <- "/Users/priyanka/Downloads/Suncor"
MIN_VISITS <- 10        # cells must have >= this many visit-days (cf. Chiger et al.)
hr <- function(s) cat("\n========== ", s, " ==========\n", sep = "")

# ---------------- toxicity values ----------------
mw <- c(Benzene = 78.11, Toluene = 92.14, Trimethylbenzene = 120.19,
        Xylene = 106.17, H2S = 34.08, HCN = 27.03)
rfc_mgm3 <- c(Benzene = 0.03, Toluene = 5, Trimethylbenzene = 0.06,
              Xylene = 0.1, H2S = 0.002, HCN = 0.0008)
# UNIT FIX (2026-08-20): this used 24.45 L/mol (25 C, 1 atm). Every other
# concentration conversion in this paper is at the SITE's pressure, not at sea
# level: 18_...census_block_level_stats.R converts AirToxScreen ug/m3 -> ppb at
# 25 C and 830 hPa, and the benzene IURs used below (5.75 and 20.40 per million
# per ppb) are the IRIS 2.2e-6 and 7.8e-6 per ug/m3 converted at that same
# 29.87 L/mol. A mixing ratio has no meaning without a stated air density, and
# the exposure occurs in Denver air at ~1600 m, not at sea level.
# Effect: every RfC in ppb rises by 29.8653/24.45 = 1.2215, so every HQ and
# every HI falls by 18.13% (previously reported values x 0.81868). H2S HQ
# 3.3 -> 2.70. Rankings are unchanged, but any cell whose reported HI fell in
# [1.000, 1.2215) drops below 1.
VM_L_PER_MOL <- 8.314 * 298.15 / 83000 * 1000   # 29.8653 L/mol at 25 C, 830 hPa
rfc_ppb  <- rfc_mgm3 * 1000 / mw * VM_L_PER_MOL

# Target organ system = critical-effect organ underlying each IRIS RfC:
#   Benzene   decreased lymphocyte count   -> Hematological/Immunological
#   Toluene   neurological effects         -> Neurological
#   1,2,4-TMB decreased pain sensitivity   -> Neurological
#   Xylenes   impaired motor coordination  -> Neurological
#   H2S       olfactory epithelial lesions -> Respiratory
#   HCN       thyroid enlargement/iodide   -> Endocrine
TOS <- c(Benzene = "Hematological/Immunological", Toluene = "Neurological",
         Trimethylbenzene = "Neurological", Xylene = "Neurological",
         H2S = "Respiratory", HCN = "Endocrine")

# PROVENANCE FIX (2026-08-20): these were hardcoded copies of the La Casa
# scaling ratios. 18_...census_block_level_stats.R READS them from
# lacasa_scaling_factors_option1_binweighted.RData, so re-running script 17
# updated the census-block surface while leaving the health tables on stale
# constants. Read the same file; fall back to the documented values only if it
# is absent, and say so.
.sf_file <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")
.sf_get <- function(pol, fallback) {
  if (!file.exists(.sf_file)) {
    message("[SCALING] ", basename(.sf_file), " not found - using the documented value for ", pol)
    return(fallback)
  }
  e <- new.env(); load(.sf_file, envir = e)
  o <- get(ls(e)[1], envir = e)
  if (!all(c("pollutant", "ratio_all_over_mobilelike") %in% names(o))) return(fallback)
  r <- as.numeric(o[["ratio_all_over_mobilelike"]])[match(pol, o[["pollutant"]])]
  if (length(r) != 1L || !is.finite(r)) fallback else r
}
scale_f <- c(Benzene          = .sf_get("benzene", 1.149),
             Toluene          = .sf_get("toluene", 1.228),
             Trimethylbenzene = NA_real_,
             Xylene           = .sf_get("xylene",  1.377),
             H2S              = NA_real_, HCN = NA_real_)
message(sprintf("[SCALING] benzene %.4f | toluene %.4f | xylene %.4f  (TMB, H2S, HCN unscaled - not measured at La Casa)",
                scale_f[["Benzene"]], scale_f[["Toluene"]], scale_f[["Xylene"]]))
# BUGFIX (2026-08-20): these were a single van-and-period slice of the audited
# CDPHE detection limits, not the campaign values. 45_mdl_sensitivity.R holds
# the full audited table, which is resolved by vehicle and period and spans
# benzene 0.5-3.2, toluene 0.18-0.27, TMB 0.22-0.45, xylene 0.19-0.29,
# H2S 2-6 and HCN 0.18-13 ppb. The two species where this matters are exactly
# the two whose reference concentrations sit below detection: the note printed
# below claimed "audited MDLs (4 and 5 ppb)" for H2S and HCN, where the
# conservative audited values are 6 and 13. Use the most conservative audited
# value per species, so the measurement-capability statement cannot understate
# the gap, and report the range.
mdl_audited <- list(Benzene = c(0.5, 3.2), Toluene = c(0.18, 0.27),
                    Trimethylbenzene = c(0.22, 0.45), Xylene = c(0.19, 0.29),
                    H2S = c(2, 6), HCN = c(0.18, 13))
mdl_ppb <- vapply(mdl_audited, max, numeric(1))
message("[MDL] using the most conservative audited value per species: ",
        paste(sprintf("%s %.2f", names(mdl_ppb), mdl_ppb), collapse = " | "))
message("[MDL] audited ranges across vehicles and periods (45_mdl_sensitivity.R): ",
        paste(sprintf("%s %.2f-%.2f", names(mdl_audited),
                      vapply(mdl_audited, min, numeric(1)),
                      vapply(mdl_audited, max, numeric(1))), collapse = " | "))
IUR_LOW <- 5.75e-6; IUR_HIGH <- 20.40e-6      # benzene, cases per ppb

# background-corrected columns (script 11), same as census-block script 18
POLLS <- c(Benzene = "sBenzene", Toluene = "sToluene",
           Trimethylbenzene = "sTrimethylbenzene", Xylene = "sXylene",
           H2S = "sH2S", HCN = "sHCN")

hr("Toxicity values and target organ systems")
print(data.table(pollutant = names(rfc_ppb), rfc_ppb = round(rfc_ppb, 3),
                 target_organ_system = TOS[names(rfc_ppb)],
                 mdl_ppb = mdl_ppb[names(rfc_ppb)],
                 rfc_below_mdl = rfc_ppb < mdl_ppb[names(rfc_ppb)]))

# ---------------- load background-corrected data + grid ----------------
hr("Load background-corrected data + 500 m grid")
load(file.path(BASE, "bgcorrected_out_merge.RData"))     # -> df
setDT(df)
missing <- setdiff(unname(POLLS), names(df))
if (length(missing)) stop("missing bg-corrected columns: ", paste(missing, collapse = ", "))
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]

grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"), quiet = TRUE)
st_crs(grid) <- 26913
cent_m <- st_centroid(st_geometry(grid))
pts <- st_transform(st_as_sf(df[, .(Longitude, Latitude)],
                             coords = c("Longitude", "Latitude"), crs = 4326), 26913)
df[, cell := grid$id[st_nearest_feature(pts, cent_m)]]
rm(pts); gc()
cat("rows:", format(nrow(df), big.mark = ","), " cells:", uniqueN(df$cell), "\n")

# ---------------- per-cell exposure concentrations ----------------
hr("Exposure concentrations per 500 m cell (same construction as the maps)")
# one-sided UCL of the mean of VISIT-DAY MEANS at confidence level `p`
ucl_of_mean <- function(x, p) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 2) return(NA_real_)
  mean(x) + qt(p, df = n - 1) * sd(x) / sqrt(n)
}

exposure <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  v <- df[[col]]; fin <- is.finite(v)
  if (!any(fin)) return(NULL)
  daily <- df[fin, .(dmed = median(get(col)), dmean = mean(get(col))),
              by = .(cell, day)]
  per <- daily[, .(n_days      = .N,
                   median_ppb  = median(dmed),          # PRIMARY: median of daily medians
                   mean_ppb    = mean(dmean),           # mean of daily means
                   ucl85_ppb   = ucl_of_mean(dmean, 0.85),  # 85% UCL of visit-day means
                   ucl95_ppb   = ucl_of_mean(dmean, 0.95)), # 95% UCL (EPA/Chiger)
               by = cell]
  per[, pollutant := pn]
  per
}), use.names = TRUE)

exposure <- exposure[n_days >= MIN_VISITS]
cat("cells retained (>=", MIN_VISITS, "visit-days):\n")
print(exposure[, .(n_cells = uniqueN(cell)), by = pollutant])

exposure[, scale_factor := scale_f[pollutant]]
METRICS <- c("median", "mean", "ucl85", "ucl95")
for (m in METRICS) {
  src <- paste0(m, "_ppb"); dst <- paste0("EC_", m)
  exposure[, (dst) := fifelse(is.na(scale_factor), get(src), get(src) * scale_factor)]
  exposure[, (dst) := pmax(get(dst), 0)]     # negatives are noise about zero
}

hr("Effect of the exposure metric")
cmp <- exposure[, .(EC_median = round(mean(EC_median), 3),
                    EC_mean    = round(mean(EC_mean), 3),
                    EC_ucl85   = round(mean(EC_ucl85, na.rm = TRUE), 3),
                    EC_ucl95   = round(mean(EC_ucl95, na.rm = TRUE), 3),
                    EC_median_max = round(max(EC_median), 3),
                    EC_ucl95_max  = round(max(EC_ucl95, na.rm = TRUE), 3)),
                by = pollutant]
cmp[, mean_over_median  := round(EC_mean  / pmax(EC_median, 1e-9), 2)]
cmp[, ucl85_over_median := round(EC_ucl85 / pmax(EC_median, 1e-9), 2)]
cmp[, ucl95_over_median := round(EC_ucl95 / pmax(EC_median, 1e-9), 2)]
print(cmp)
cat("\n[READ] mean_over_median > 1 quantifies how much the plume-suppressing map\n")
cat("       metric lowers the exposure estimate relative to a mean-based one.\n")

# ---------------- HQ / HI ----------------
hr("Hazard quotients (Chiger Eq 1) and hazard indices (Chiger Eq 2)")
exposure[, rfc := rfc_ppb[pollutant]]
exposure[, tos := TOS[pollutant]]
for (m in METRICS) {
  exposure[, (paste0("HQ_", m)) := get(paste0("EC_", m)) / rfc]
}

hi_all <- rbindlist(lapply(METRICS, function(m) {
  x <- exposure[, .(HI = sum(get(paste0("HQ_", m)), na.rm = TRUE),
                    chems = paste(sort(unique(pollutant)), collapse = "+")),
                by = .(cell, tos)]
  x[, metric := m]; x
}))

# BUGFIX (2026-08-20): each pollutant is filtered to >= MIN_VISITS visit-days
# INDEPENDENTLY, so a Neurological cell may carry toluene alone, toluene and
# xylene, or all three; sum(..., na.rm = TRUE) then values an absent species at
# zero rather than as missing. The `chems` label recording that composition was
# computed and never used, so n_cells pooled 1-, 2- and 3-chemical cells and
# pct_cells_HI_gt1 had a different denominator for every organ system. A
# hazard index summed over a cell-dependent chemical set is not a well-defined
# quantity. Report three things instead of one: the pooled summary (kept for
# continuity, now labelled), the same summary broken down by composition, and a
# COMPLETE-SET summary restricted to cells carrying every species assigned to
# that organ system - the last being the one that is directly comparable
# across cells and the one to quote.
.hi_stats <- function(d, by_cols) {
  s <- d[, .(n_cells = .N,
             HI_mean_across_cells = round(mean(HI, na.rm = TRUE), 3),
             HI_median_across_cells = round(median(HI, na.rm = TRUE), 3),
             HI_max = round(max(HI, na.rm = TRUE), 3),
             n_cells_HI_gt1 = sum(HI > 1, na.rm = TRUE)), by = by_cols]
  s[, pct_cells_HI_gt1 := round(100 * n_cells_HI_gt1 / n_cells, 1)]
  s[]
}

# how many species each organ system SHOULD carry
.tos_n <- table(TOS)
hi_all[, n_chems := lengths(strsplit(chems, "+", fixed = TRUE))]
hi_all[, complete_set := n_chems == as.integer(.tos_n[tos])]

hi_by_chems <- .hi_stats(hi_all, c("tos", "metric", "chems"))
fwrite(hi_by_chems, file.path(BASE, "TABLE_cumulative_HI_by_composition.csv"))

hi_complete <- .hi_stats(hi_all[complete_set == TRUE], c("tos", "metric"))
fwrite(hi_complete, file.path(BASE, "TABLE_cumulative_HI_completeset.csv"))

cat("\nHazard index restricted to cells carrying the COMPLETE species set for the organ system:\n")
print(hi_complete)
cat("\nComposition breakdown (how many cells carry which species):\n")
print(hi_by_chems[metric == METRICS[1]][order(tos, -n_cells)])

hi_summary <- hi_all[, .(n_cells = .N,
                         HI_mean_across_cells = round(mean(HI, na.rm = TRUE), 3),
                         HI_median_across_cells = round(median(HI, na.rm = TRUE), 3),
                         HI_max = round(max(HI, na.rm = TRUE), 3),
                         n_cells_HI_gt1 = sum(HI > 1, na.rm = TRUE)),
                     by = .(tos, metric)]
hi_summary[, pct_cells_HI_gt1 := round(100 * n_cells_HI_gt1 / n_cells, 1)]
setorder(hi_summary, metric, -HI_max)

hq_summary <- exposure[, .(n_cells = .N,
                           EC_median_mean = round(mean(EC_median), 3),
                           EC_median_max  = round(max(EC_median), 3),
                           HQ_median_mean = round(mean(HQ_median), 3),
                           HQ_median_max  = round(max(HQ_median), 3),
                           HQ_mean_max    = round(max(HQ_mean), 3),
                           HQ_ucl85_mean  = round(mean(HQ_ucl85, na.rm = TRUE), 3),
                           HQ_ucl85_max   = round(max(HQ_ucl85, na.rm = TRUE), 3),
                           HQ_ucl95_mean  = round(mean(HQ_ucl95, na.rm = TRUE), 3),
                           HQ_ucl95_max   = round(max(HQ_ucl95, na.rm = TRUE), 3),
                           n_cells_HQ_gt1 = sum(HQ_median > 1, na.rm = TRUE)),
                       by = .(pollutant, tos)]
hq_summary[, rfc_below_mdl := rfc_ppb[pollutant] < mdl_ppb[pollutant]]
setorder(hq_summary, -HQ_median_max)

hr("HQ by pollutant (PRIMARY = median of daily medians, scaled)")
print(hq_summary)
hr("HI by target organ system (both exposure metrics)")
print(hi_summary)
cat("\n[NOTE] HQ/HI for H2S and HCN are governed by values at or below the method\n")
cat("       detection limit: audited MDLs (4 and 5 ppb) exceed the IRIS RfCs\n")
cat("       (1.43 and 0.72 ppb). These are measurement-capability artefacts,\n")
cat("       not demonstrated exceedances (SI Table S3.2).\n")

# ---------------- cancer risk (Robinson framing) ----------------
hr("Total cancer risk (Robinson framing): benzene only = LOWER BOUND")
benz <- exposure[pollutant == "Benzene"]
benz[, risk_low_per_million       := EC_median * IUR_LOW  * 1e6]
benz[, risk_high_per_million      := EC_median * IUR_HIGH * 1e6]
benz[, risk_high_meanmetric_permil:= EC_mean   * IUR_HIGH * 1e6]
benz[, risk_high_ucl85_permil     := EC_ucl85  * IUR_HIGH * 1e6]
benz[, risk_high_ucl95_permil     := EC_ucl95  * IUR_HIGH * 1e6]
cancer <- data.table(
  metric = c("cells assessed", "mean EC (ppb)", "max EC (ppb)",
             "mean risk per million (IUR low)",  "mean risk per million (IUR high)",
             "max risk per million (IUR low)",   "max risk per million (IUR high)",
             "cells above 100-in-a-million (IUR high)",
             "mean risk per million (IUR high, MEAN-of-means metric)",
             "mean risk per million (IUR high, 85% UCL metric)",
             "mean risk per million (IUR high, 95% UCL metric)"),
  value  = c(nrow(benz), round(mean(benz$EC_median),3), round(max(benz$EC_median),3),
             round(mean(benz$risk_low_per_million),2),
             round(mean(benz$risk_high_per_million),2),
             round(max(benz$risk_low_per_million),2),
             round(max(benz$risk_high_per_million),2),
             sum(benz$risk_high_per_million > 100),
             round(mean(benz$risk_high_meanmetric_permil),2),
             round(mean(benz$risk_high_ucl85_permil, na.rm = TRUE),2),
             round(mean(benz$risk_high_ucl95_permil, na.rm = TRUE),2)))
print(cancer)
cat("\n[CONTEXT] USEPA Superfund upper acceptable level = 100-in-one-million.\n")
cat("          Robinson et al. (2025) report a maximum tract-level TOTAL cancer\n")
cat("          risk of 560-in-one-million in the Baton Rouge-New Orleans corridor,\n")
cat("          summing 17 carcinogens dominated by ethylene oxide. Our estimate\n")
cat("          covers benzene alone and bounds the measured contribution below.\n")

# ---------------- outputs ----------------
fwrite(exposure,   file.path(BASE, "TABLE_cumulative_HQ_by_cell.csv"))
fwrite(cmp,        file.path(BASE, "TABLE_cumulative_exposure_metric_comparison.csv"))
fwrite(hq_summary, file.path(BASE, "TABLE_cumulative_HQ_summary.csv"))
fwrite(hi_summary, file.path(BASE, "TABLE_cumulative_HI_summary.csv"))
fwrite(benz[, .(cell, n_days, EC_median, EC_mean, EC_ucl85, EC_ucl95,
                risk_low_per_million, risk_high_per_million,
                risk_high_meanmetric_permil, risk_high_ucl85_permil,
                risk_high_ucl95_permil)],
       file.path(BASE, "TABLE_cumulative_cancer_risk.csv"))
fwrite(cancer,     file.path(BASE, "TABLE_cumulative_cancer_summary.csv"))

pl <- hi_summary[metric == "median", .(tos, HI_mean_across_cells, HI_max)]
pl <- data.table::melt(pl, id.vars = "tos", variable.name = "stat", value.name = "HI")
p <- ggplot2::ggplot(pl, ggplot2::aes(x = stats::reorder(tos, HI), y = pmax(HI, 1e-4),
                                      fill = stat)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  ggplot2::coord_flip() + ggplot2::scale_y_log10() +
  ggplot2::labs(x = NULL, y = "Hazard index (log scale)",
                title = "Cumulative non-cancer hazard index by target organ system",
                subtitle = "Chiger et al. (2025) Eq 1-2; exposure = median of daily medians, 24-h scaled; dashed line HI = 1",
                fill = NULL) +
  ggplot2::theme_bw(base_size = 11)
dir.create(file.path(BASE, "FinalFig"), showWarnings = FALSE)
ggplot2::ggsave(file.path(BASE, "FinalFig", "FIG_cumulative_HI.png"), p,
                width = 9, height = 5, dpi = 300, bg = "white")

cat("\n[Saved] TABLE_cumulative_* (6 files) and FinalFig/FIG_cumulative_HI.png\n")
