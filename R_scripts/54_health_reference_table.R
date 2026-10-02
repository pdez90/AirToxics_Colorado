# ==============================================================
# 54  HEALTH-BASED REFERENCE VALUES + HAZARD QUOTIENTS (SI Table S3.2)
# Compares campaign concentrations with health-based reference
# values and computes chronic hazard quotients (HQ) for the species
# with EPA IRIS non-cancer reference concentrations (RfCs).
# Exposure metrics computed from the raw data:
#   - campaign median and p99 of 1-s values (the p99 = event threshold)
#   - highest 500 m cell median-of-daily-medians ("max sustained cell"),
#     scaled to 24-h equivalence for benzene/toluene/xylene (S4.1 factors)
# Reference values (hardcoded, sources in comments):
#   IRIS RfCs: benzene 0.03, toluene 5, xylenes 0.1, 1,2,4-TMB 0.06,
#     H2S 0.002, HCN 0.0008 mg/m3 (EPA IRIS)
#   ATSDR inhalation MRLs (ppm): benzene 0.009/0.007/0.002 (draft 2024);
#     toluene 2/-/1; xylenes 2/0.6/0.05; H2S 0.07/0.02/- ; HCN none
#   H2S odor detection threshold ~0.5-8 ppb (ATSDR)
# Output: TABLE_health_reference_HQ.csv
# ==============================================================

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({ library(data.table); library(sf) })
# EXPOSURE BASIS (2026-09-30): the MEAN of daily means is the primary statistic
# for concentrations, exposure and health (what a long-term reference value and
# AirToxScreen's annual mean presume); the median of daily medians is the
# supplementary analysis. EXPOSURE_BASIS=med_of_daily_med runs the supplementary
# version, written with a _medianbasis suffix so it never overwrites the primary.
EXPOSURE_BASIS <- Sys.getenv("EXPOSURE_BASIS", "mean_of_daily_mean")
stopifnot(EXPOSURE_BASIS %in% c("mean_of_daily_mean", "med_of_daily_med"))
.agg <- if (EXPOSURE_BASIS == "mean_of_daily_mean") mean else stats::median
.sfx <- if (EXPOSURE_BASIS == "mean_of_daily_mean") "" else "_medianbasis"
message("[BASIS] ", EXPOSURE_BASIS)

BASE <- SUNCOR_BASE
message("Loading mobile data + grid...")
# BUGFIX (2026-08-20): this loaded mobile_wswd.RData, which is 06_merge_with_
# wind.R's output and carries only the RAW *_ppb columns. Every hazard quotient
# in TABLE_health_reference_HQ.csv was therefore computed on a different
# exposure basis from 73_cumulative_risk.R, which uses the background-corrected
# s* columns, and from the census-block surface in 18_...R, which also uses
# them. Two hazard-quotient tables in one paper on two different bases cannot
# be reconciled by a reader. Load the background-corrected record instead - the
# same file 73 reads.
load(file.path(BASE, "bgcorrected_out_merge.RData"))
df <- as.data.table(df); gc()

# TWO BASES (2026-09-25). The exposure columns above are BACKGROUND-CORRECTED,
# which is required for the hazard quotients to reconcile with 73/74. But the
# campaign median and 99th-percentile "event threshold" columns of SI Table S3.2
# are descriptive statistics of the analysis set, and SI Table S3.1, the
# manuscript text and R99 all report them on the RAW record. Reporting one basis
# in S3.1 and the other in S3.2, under the same column names, is not
# reconcilable by a reader. Both are therefore computed and written, and the SI
# table shows raw first with the background-corrected value in parentheses.
.rawenv <- new.env()
load(file.path(BASE, "mobile_wswd.RData"), envir = .rawenv)   # -> out
raw <- as.data.table(get("out", envir = .rawenv)); rm(.rawenv); gc()
# Filter EXACTLY as 70_table_s31.R does for its "analysis set" (Goodrich route
# dropped, no GPS filter) so these columns equal Table S3.1 cell for cell.
raw <- raw[Site != "Goodrich Corporation (Collins Aerospace)"]
RAWCOL <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
            Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
            H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")
stopifnot(all(RAWCOL %in% names(raw)))
message(sprintf("[BASIS] raw record: %s rows | background-corrected: %s rows",
                format(nrow(raw), big.mark = ","), format(nrow(df), big.mark = ",")))
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

# ppb = (mg/m3)*1000/MW*V_m. UNIT FIX (2026-08-23): V_m is the molar volume at
# the 830 hPa SITE pressure (29.8653 L/mol at 25 C), NOT sea-level 24.45 - the
# same convention as the Section 2.4 benzene IURs, 73_cumulative_risk.R and
# 74_health_hazard_screening.R. Expected HQ_chronic after this fix:
#   benzene 0.040 | toluene 0.001 | TMB 0.025 | xylene 0.027
#   H2S 1.398 | HCN 1.697   (rfc_ppb: 11.47/1620.7/14.91/28.13/1.753/0.884)
VM_L_PER_MOL <- 8.314 * 298.15 / 83000 * 1000   # 29.8653
mw <- c(Benzene = 78.11, Toluene = 92.14, Trimethylbenzene = 120.19,
        Xylene = 106.17, H2S = 34.08, HCN = 27.03)
rfc_mgm3 <- c(Benzene = 0.03, Toluene = 5, Trimethylbenzene = 0.06,
              Xylene = 0.1, H2S = 0.002, HCN = 0.0008)
rfc_ppb <- rfc_mgm3 * 1000 / mw * VM_L_PER_MOL

# SCALING FACTORS (2026-09-23): read them from the file R04 writes instead of
# hard-coding. The 300 m headquarters exclusion moved the factors from
# 1.149/1.228/1.377 to 1.165/1.274/1.443, and a hard-coded constant would have
# left this table on the old scaling while the block surface used the new one.
.sf_file <- file.path(SUNCOR_BASE, "lacasa_scaling_factors_option1_binweighted.RData")
.sf_get <- function(pol, fallback = NULL) {
  # The factor is read from the La Casa output of script 17; a missing or
  # malformed file stops the script rather than falling back to a typed-in
  # constant that could be stale.
  if (!file.exists(.sf_file))
    stop("[SCALING] ", basename(.sf_file), " not found - run R_scripts/17 (pipeline R04) first.")
  e <- new.env(); load(.sf_file, envir = e)
  o <- get(ls(e)[1], envir = e)
  if (!all(c("pollutant", "ratio_all_over_mobilelike") %in% names(o)))
    stop("[SCALING] ", basename(.sf_file), " lacks pollutant / ratio_all_over_mobilelike columns.")
  r <- as.numeric(o[["ratio_all_over_mobilelike"]])[match(pol, o[["pollutant"]])]
  if (length(r) != 1L || !is.finite(r)) stop("[SCALING] no finite factor for ", pol, " in ", basename(.sf_file))
  r
}
scale_f <- c(Benzene = .sf_get("benzene", 1.149), Toluene = .sf_get("toluene", 1.228),
             Trimethylbenzene = NA, Xylene = .sf_get("xylene", 1.377), H2S = NA, HCN = NA)
message(sprintf("[SCALING] benzene %.4f | toluene %.4f | xylene %.4f", scale_f[["Benzene"]], scale_f[["Toluene"]], scale_f[["Xylene"]]))
mrl_ppb <- data.table(
  pollutant = c("Benzene", "Toluene", "Xylene", "H2S", "HCN", "Trimethylbenzene"),
  mrl_acute = c(9, 2000, 2000, 70, NA, NA),
  mrl_intermediate = c(7, NA, 600, 20, NA, NA),
  mrl_chronic = c(2, 1000, 50, NA, NA, NA))

# background-corrected columns (script 11), matching 73_cumulative_risk.R
POLLS <- c(Benzene = "sBenzene", Toluene = "sToluene",
           Trimethylbenzene = "sTrimethylbenzene", Xylene = "sXylene",
           H2S = "sH2S", HCN = "sHCN")
stopifnot(all(POLLS %in% names(df)))

res <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  v <- df[[col]]; fin <- is.finite(v)
  daily <- df[fin, .(dmed = .agg(get(col))), by = .(cell, day)]
  cellmed <- daily[, .(m = .agg(dmed), n_days = .N), by = cell]
  # (2026-08-20) The "maximum sustained cell" previously took max() over EVERY
  # cell, including cells visited on a single day, which is not a sustained
  # concentration in any sense. 73_cumulative_risk.R requires >= 10 visit-days
  # for the same quantity over the same domain; apply the same rule here so the
  # two tables describe the same population, and report what it removes.
  MIN_VISITS_54 <- 10
  .n_all <- nrow(cellmed)
  cellmed <- cellmed[n_days >= MIN_VISITS_54]
  message(sprintf("  %-18s cells: %d -> %d after the >=%d visit-day filter",
                  pn, .n_all, nrow(cellmed), MIN_VISITS_54))
  stopifnot(nrow(cellmed) > 0)
  mx <- max(cellmed$m)
  sf_ <- scale_f[[pn]]
  mx_scaled <- if (is.na(sf_)) mx else mx * sf_
  rv <- raw[[RAWCOL[[pn]]]]; rfin <- is.finite(rv)
  data.table(pollutant = pn,
             median_1s_raw = round(median(rv[rfin]), 3),
             p99_1s_raw = round(quantile(rv[rfin], 0.99), 2),
             median_1s_bgcorr = round(median(v[fin]), 3),
             p99_1s_bgcorr = round(quantile(v[fin], 0.99), 2),
             median_1s = round(median(rv[rfin]), 3),   # kept: raw, = Table S3.1
             p99_1s = round(quantile(rv[rfin], 0.99), 2),
             max_cell_median = round(mx, 3),
             scale_factor = sf_,
             max_cell_median_24h = round(mx_scaled, 3),
             rfc_ppb = round(rfc_ppb[[pn]], 2),
             HQ_chronic = round(mx_scaled / rfc_ppb[[pn]], 3))
}))
res <- merge(res, mrl_ppb, by = "pollutant", sort = FALSE)
res[, exceeds_chronic_mrl := fifelse(is.na(mrl_chronic), NA,
                                     max_cell_median_24h > mrl_chronic)]
fwrite(res, file.path(BASE, paste0("TABLE_health_reference_HQ", .sfx, ".csv")))
print(res)
message("\nHQ_chronic = (highest sustained 500 m cell median, 24-h scaled where ",
        "factors exist) / IRIS RfC. HQ < 1 indicates the sustained ",
        "concentration is below the non-cancer reference level.")
message("H2S odor detection threshold ~0.5-8 ppb for comparison; campaign p99 = ",
        res[pollutant == "H2S", p99_1s], " ppb.")
message("DONE.")
