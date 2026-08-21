# ==============================================================
# 74  Is hotspot Group 9 an artefact of the mobile-lab garage?
# --------------------------------------------------------------
# Group 9's centroid (39.784898 N, -105.103168 W) lies 112 m from the ATOPs
# headquarters (39.785359 N, -105.104331 W), where the mobile laboratories are
# garaged and idled. The next-nearest hotspot group is 465 m away and all others
# are >1.6 km. This matters because Group 9 carries three headline claims:
#   (i)  the most extreme trimethylbenzene/benzene ratio in the study (~3.2),
#   (ii) NO directional enrichment toward any candidate facility (all <=1.0),
#   (iii) the strongest methane co-elevation of any group (34.7% >= p95).
# All three are equally consistent with emissions from the vehicles' own depot:
# a source CO-LOCATED with the measurement point produces no upwind directional
# signal, and vehicle/garage air is both methane- and aromatic-rich.
#
# IMPORTANT ASYMMETRY: the METHANE pipeline (M01) removes observations within
# 100 m of the garage, but the TOXICS pipeline applies NO such filter, so the
# aromatics and H2S at Group 9 include near-garage air.
#
# This script quantifies the dependence by recomputing Group 9's statistics
# while progressively excluding observations near the garage.
#
# Output: TABLE_group9_garage_sensitivity.csv
# ==============================================================

suppressPackageStartupMessages({ library(data.table); library(geosphere) })

BASE <- "/Users/priyanka/Downloads/Suncor"
HQ   <- c(lon = -105.104331, lat = 39.785359)   # ATOPs HQ / garage
G9   <- c(lon = -105.10316782735, lat = 39.784898289519)
BUFFER_M <- 100                                  # group analysis buffer
EXCL <- c(0, 100, 150, 200, 250, 300)            # garage exclusion radii to test

hr <- function(s) cat("\n========== ", s, " ==========\n", sep = "")

hr("Load background-corrected toxics")
load(file.path(BASE, "bgcorrected_out_merge.RData")); setDT(df)
df <- df[is.finite(Latitude) & is.finite(Longitude)]
df[, dist_g9_m := distHaversine(cbind(Longitude, Latitude), matrix(G9, ncol = 2))]
df[, dist_hq_m := distHaversine(cbind(Longitude, Latitude), matrix(HQ, ncol = 2))]

g9 <- df[dist_g9_m <= BUFFER_M]
cat("rows within", BUFFER_M, "m of Group 9 centroid:", format(nrow(g9), big.mark = ","), "\n")
cat("of which within 100 m of the garage:", format(g9[dist_hq_m <= 100, .N], big.mark = ","),
    sprintf("(%.1f%%)\n", 100 * mean(g9$dist_hq_m <= 100)))
cat("distance-to-garage quantiles for Group 9 observations (m):\n")
print(round(quantile(g9$dist_hq_m, c(0, .05, .25, .5, .75, .95, 1)), 0))

# REWRITE (2026-08-20). Two defects in the previous version:
#
#  (1) It did not reproduce the quantities it was testing. Group 9's published
#      statistics come from 33_final_hotspot_plots.R, which uses the RAW
#      *_ppb columns, a campaign 95th-percentile threshold per pollutant, and
#      TMB/benzene from the raw columns. This script used the BACKGROUND-
#      CORRECTED s* columns and the Section 2.5.3 event thresholds (p99), so
#      the excl_radius_m = 0 row was not the published baseline and a
#      "collapses" or "is stable" reading was against the wrong reference.
#  (2) The exceedance numerators were per-pollutant (finite values only) while
#      the denominators n_obs / n_days pooled every row in the radius. HCN
#      exists only from 2025-01-22 and H2S/HCN have different missingness from
#      the aromatics, so a rate formed as nhigh_HCN / n_obs used a denominator
#      many times too large - and it shrank with radius on a different
#      schedule than its numerator, which is exactly what would fake a trend.
#
# Both bases are now reported side by side, each with its own per-pollutant
# denominator and an explicit exceedance RATE.

POLL_RAW <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
              Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
              H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")
POLL_BG  <- c(Benzene = "sBenzene", Toluene = "sToluene",
              Trimethylbenzene = "sTrimethylbenzene",
              Xylene = "sXylene", H2S = "sH2S", HCN = "sHCN")

# BUGFIX (2026-08-20): this applied the Section 2.5.3 event thresholds
# (1.8 / 4.31 / 2.59 / 3.19 / 4.6 / 11) to the BACKGROUND-CORRECTED s* columns.
# Those six numbers are the 99th percentiles of the RAW *_ppb columns
# (28_hotspot_analysis...R:59), not of the s* columns, so the "bgcorr_p99" half
# of the table counted exceedances of an arbitrary cut rather than a 1% cut.
# Compute the percentile on the same columns it is applied to.
THR_P99 <- vapply(POLL_BG, function(cc) {
  if (!cc %in% names(df)) return(NA_real_)
  as.numeric(stats::quantile(df[[cc]], 0.99, na.rm = TRUE))
}, numeric(1))
names(THR_P99) <- names(POLL_BG)

# Script 33's basis: campaign p95 of the RAW column, computed here from the
# same full record so the excl_radius_m = 0 row reconciles with the published
# group statistics rather than approximating them.
THR_P95 <- vapply(POLL_RAW, function(cc) {
  if (!cc %in% names(df)) return(NA_real_)
  as.numeric(stats::quantile(df[[cc]], 0.95, na.rm = TRUE))
}, numeric(1))
cat("\ncampaign p95 thresholds from the RAW columns (script 33 basis):\n")
print(round(THR_P95, 3))
cat("Section 2.5.3 p99 event thresholds (bg-corrected basis):\n")
print(THR_P99)

group9_table <- function(cols, thr, basis, ratio_num, ratio_den) {
  rbindlist(lapply(EXCL, function(rad) {
    sub <- g9[dist_hq_m > rad]
    if (!nrow(sub)) return(NULL)
    out <- data.table(basis = basis, excl_radius_m = rad,
                      n_obs_any = nrow(sub),
                      n_days_any = uniqueN(as.Date(sub$date)))
    for (pn in names(cols)) {
      col <- cols[[pn]]
      if (!col %in% names(sub) || !is.finite(thr[[pn]])) next
      ok  <- is.finite(sub[[col]])
      # PER-POLLUTANT denominator: only rows where this pollutant was measured
      n_p <- sum(ok)
      d_p <- uniqueN(as.Date(sub$date[ok]))
      hi  <- sub[ok & sub[[col]] >= thr[[pn]]]
      out[[paste0("n_obs_",   pn)]] <- n_p
      out[[paste0("n_days_",  pn)]] <- d_p
      out[[paste0("nhigh_",   pn)]] <- nrow(hi)
      out[[paste0("ndayshi_", pn)]] <- uniqueN(as.Date(hi$date))
      out[[paste0("rate_",    pn)]] <- if (n_p > 0) round(100 * nrow(hi) / n_p, 3) else NA_real_
      v <- sub[[col]][ok]
      out[[paste0("med_",     pn)]] <- if (length(v)) round(median(v), 4) else NA_real_
    }
    vb <- suppressWarnings(as.numeric(sub[[ratio_den]]))
    vt <- suppressWarnings(as.numeric(sub[[ratio_num]]))
    okr <- is.finite(vb) & is.finite(vt) & vb > 0
    # [[<- throughout rather than := ; mixing them makes data.table emit an
    # "Invalid .internal.selfref" warning on every row of the table.
    out[["n_ratio"]] <- sum(okr)
    out[["TMB_over_benzene_median"]] <- if (any(okr)) round(median(vt[okr] / vb[okr]), 3) else NA_real_
    out
  }), fill = TRUE)
}

hr("Group 9 vs garage-exclusion radius - RAW basis (reconciles with script 33)")
res_raw <- group9_table(POLL_RAW, THR_P95, "raw_p95",
                        "Trimethylbenzene_ppb", "Benzene_ppb")
print(as.data.frame(res_raw), row.names = FALSE)

hr("Group 9 vs garage-exclusion radius - BACKGROUND-CORRECTED basis (p99 events)")
res_bg <- group9_table(POLL_BG, THR_P99, "bgcorr_p99",
                       "sTrimethylbenzene", "sBenzene")
print(as.data.frame(res_bg), row.names = FALSE)

res <- rbindlist(list(res_raw, res_bg), fill = TRUE)
fwrite(res, file.path(BASE, "TABLE_group9_garage_sensitivity.csv"))

hr("INTERPRETATION")
cat("If Group 9's exceedance counts and TMB/benzene ratio collapse as the garage\n")
cat("exclusion radius grows, the hotspot is substantially a self-sampling artefact.\n")
cat("If they are stable, the depot is not the explanation and the attribution to a\n")
cat("nearby industrial source stands.\n")
cat("\n[Saved] TABLE_group9_garage_sensitivity.csv\n")
