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
mobile_benzene_col <- "sBenzene_med_of_daily_med_scaled"
# mobile_benzene_col <- "sBenzene_mean_of_daily_mean_scaled"

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
out_dir_fig <- "/Users/priyanka/Downloads/Suncor/FinalFig"
out_csv_common <- file.path(out_dir_fig, "benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS.csv")
utils::write.csv(results_risk_common, out_csv_common, row.names = FALSE)
message("Saved COMMON-BLOCKS risk summary CSV: ", out_csv_common)

# ---- OPTIONAL: write the common-block subset as a GPKG for mapping
# (does not overwrite anything)
block_sf_common <- block_sf_risk %>% filter(GEOID20 %in% common_geoid)
out_gpkg_common <- "/Users/priyanka/Downloads/Suncor/censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"
sf::st_write(block_sf_common, out_gpkg_common, append = FALSE, quiet = TRUE)
message("Wrote COMMON-BLOCKS gpkg: ", out_gpkg_common)

cor(df_common$benzene_ppb_airtox, df_common[[mobile_benzene_col]], use="complete.obs")
