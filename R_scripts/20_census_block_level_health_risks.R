# ==============================================================
# 20  Census block-level health risks
# Auto-split from Suncor.Rmd  (section 20 of 40)
# ==============================================================

#Census block-level health risks

# ============================================================
# ADD-ON: Benzene risk using ONLY COMMON blocks (AirTox + Mobile)
# - Uses AirTox population weights
# - Reports # common blocks explicitly
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# choose mobile benzene metric
# EXPOSURE BASIS (2026-09-30): the block MEAN of daily means is the primary
# statistic (it is what a lifetime-average unit risk and AirToxScreen's annual
# mean presume); the median of daily medians is the supplementary one.
# EXPOSURE_BASIS=med_of_daily_med restores the 2026-09-27 primary.
EXPOSURE_BASIS <- Sys.getenv("EXPOSURE_BASIS", "mean_of_daily_mean")
stopifnot(EXPOSURE_BASIS %in% c("mean_of_daily_mean", "med_of_daily_med"))
mobile_benzene_col <- paste0("sBenzene_", EXPOSURE_BASIS, "_scaled")

stopifnot(exists("block_sf_risk"))
stopifnot(all(c("benzene_ppb_airtox", "Population_airtox", "GEOID20") %in% names(block_sf_risk)))
stopifnot(mobile_benzene_col %in% names(block_sf_risk))

# helpers
pw_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

risk_calc <- function(ppb, pop, factor) {
  ok <- is.finite(ppb) & is.finite(pop) & pop > 0
  if (!any(ok)) return(NA_real_)
  factor * sum(ppb[ok] * pop[ok], na.rm = TRUE) / 1e6
}

# drop geometry for speed
df_all <- sf::st_drop_geometry(block_sf_risk)

# ---- define "common blocks" = BOTH benzene metrics present + valid population
df_common <- df_all %>%
  filter(
    is.finite(Population_airtox) & Population_airtox > 0,
    is.finite(benzene_ppb_airtox),
    is.finite(.data[[mobile_benzene_col]])
  )

n_common <- nrow(df_common)
common_geoid <- df_common$GEOID20

message("COMMON blocks (AirTox benzene + Mobile benzene): ", n_common)

# optional extra diagnostics
message("Total blocks with AirTox benzene: ",
        sum(is.finite(df_all$benzene_ppb_airtox) &
              is.finite(df_all$Population_airtox) & df_all$Population_airtox > 0))

message("Total blocks with Mobile benzene: ",
        sum(is.finite(df_all[[mobile_benzene_col]]) &
              is.finite(df_all$Population_airtox) & df_all$Population_airtox > 0))

message("Example common GEOIDs: ")
print(head(common_geoid, 10))

# ---- compute risks using SAME blocks
results_risk_common <- data.frame(
  metric = c("AirToxScreen benzene_ppb (COMMON blocks)",
             paste0("Mobile ", mobile_benzene_col, " (COMMON blocks)")),
  n_blocks = c(n_common, n_common),
  total_population_used = c(
    sum(df_common$Population_airtox, na.rm = TRUE),
    sum(df_common$Population_airtox, na.rm = TRUE)
  ),
  pop_weighted_mean_ppb = c(
    pw_mean(df_common$benzene_ppb_airtox, df_common$Population_airtox),
    pw_mean(df_common[[mobile_benzene_col]], df_common$Population_airtox)
  ),
  risk_5_75 = c(
    risk_calc(df_common$benzene_ppb_airtox, df_common$Population_airtox, 5.75),
    risk_calc(df_common[[mobile_benzene_col]], df_common$Population_airtox, 5.75)
  ),
  risk_20_40 = c(
    risk_calc(df_common$benzene_ppb_airtox, df_common$Population_airtox, 20.40),
    risk_calc(df_common[[mobile_benzene_col]], df_common$Population_airtox, 20.40)
  )
)

print(results_risk_common)

# ---- save CSV
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
out_dir_fig <- file.path(SUNCOR_BASE, "FinalFig")
out_csv_common <- file.path(out_dir_fig, "benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS.csv")
utils::write.csv(results_risk_common, out_csv_common, row.names = FALSE)
message("Saved COMMON-BLOCKS risk summary CSV: ", out_csv_common)

# ---- SECONDARY BASIS (2026-09-27): the same comparison on the block MEAN of
# daily means, written to a separate file so the primary file keeps one mobile
# row. The primary statistic is the median of daily medians (robust to the few
# high days a block may have been sampled on); the mean is the exposure-
# relevant statistic and AirToxScreen is itself an annual mean, so the SI
# reports both and discusses the difference (section S4.7 / S7). On the mean
# basis mobile benzene exceeds AirToxScreen (ratio ~1.26) rather than sitting
# just below it (0.92); the difference is the skewed upper tail the median
# discards. 74/77/79 carry the same primary/secondary pair for the hazard screen.
# (2026-09-30) `mean_col` is now simply the OTHER basis (the median when the
# primary is the mean); the name is kept so the block below is unchanged.
mean_col <- if (EXPOSURE_BASIS == "mean_of_daily_mean") "sBenzene_med_of_daily_med_scaled" else "sBenzene_mean_of_daily_mean_scaled"
if (mean_col %in% names(df_common)) {
  ok2 <- is.finite(df_common[[mean_col]])
  d2  <- df_common[ok2, ]
  gt2 <- function(x) sum(x / d2$benzene_ppb_airtox > 2, na.rm = TRUE)
  results_meanbasis <- data.frame(
    metric = c("AirToxScreen benzene_ppb (COMMON blocks)",
               paste0("Mobile ", mobile_benzene_col, " (COMMON blocks) [primary basis]"),
               paste0("Mobile ", mean_col, " (COMMON blocks) [supplementary basis]")),
    n_blocks = nrow(d2),
    total_population_used = sum(d2$Population_airtox, na.rm = TRUE),
    pop_weighted_mean_ppb = c(pw_mean(d2$benzene_ppb_airtox, d2$Population_airtox),
                              pw_mean(d2[[mobile_benzene_col]], d2$Population_airtox),
                              pw_mean(d2[[mean_col]], d2$Population_airtox)),
    risk_5_75  = c(risk_calc(d2$benzene_ppb_airtox, d2$Population_airtox, 5.75),
                   risk_calc(d2[[mobile_benzene_col]], d2$Population_airtox, 5.75),
                   risk_calc(d2[[mean_col]], d2$Population_airtox, 5.75)),
    risk_20_40 = c(risk_calc(d2$benzene_ppb_airtox, d2$Population_airtox, 20.40),
                   risk_calc(d2[[mobile_benzene_col]], d2$Population_airtox, 20.40),
                   risk_calc(d2[[mean_col]], d2$Population_airtox, 20.40)),
    blocks_gt2x_airtox = c(NA, gt2(d2[[mobile_benzene_col]]), gt2(d2[[mean_col]])))
  results_meanbasis$ratio_vs_airtox <- results_meanbasis$pop_weighted_mean_ppb / results_meanbasis$pop_weighted_mean_ppb[1]
  print(results_meanbasis)
  out_csv_basis <- file.path(out_dir_fig, "benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS_basis_comparison.csv")
  utils::write.csv(results_meanbasis, out_csv_basis, row.names = FALSE)
  message("Saved basis-comparison CSV: ", out_csv_basis)
} else message("[BASIS] ", mean_col, " not in block_sf_risk - secondary-basis comparison skipped")

# ---- OPTIONAL: write the common-block subset as a GPKG for mapping
# (does not overwrite anything)
block_sf_common <- block_sf_risk %>% filter(GEOID20 %in% common_geoid)
out_gpkg_common <- file.path(SUNCOR_BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg")
sf::st_write(block_sf_common, out_gpkg_common, append = FALSE, quiet = TRUE)
message("Wrote COMMON-BLOCKS gpkg: ", out_gpkg_common)

cor(df_common$benzene_ppb_airtox, df_common[[mobile_benzene_col]], use="complete.obs")
