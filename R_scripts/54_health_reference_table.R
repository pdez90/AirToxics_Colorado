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

suppressPackageStartupMessages({ library(data.table); library(sf) })

BASE <- "/Users/priyanka/Downloads/Suncor"
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

# molar volumes at 25C: ppb = (mg/m3)*1000/MW*24.45
mw <- c(Benzene = 78.11, Toluene = 92.14, Trimethylbenzene = 120.19,
        Xylene = 106.17, H2S = 34.08, HCN = 27.03)
rfc_mgm3 <- c(Benzene = 0.03, Toluene = 5, Trimethylbenzene = 0.06,
              Xylene = 0.1, H2S = 0.002, HCN = 0.0008)
rfc_ppb <- rfc_mgm3 * 1000 / mw * 24.45
scale_f <- c(Benzene = 1.149, Toluene = 1.228, Trimethylbenzene = NA,
             Xylene = 1.377, H2S = NA, HCN = NA)
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
  daily <- df[fin, .(dmed = median(get(col))), by = .(cell, day)]
  cellmed <- daily[, .(m = median(dmed), n_days = .N), by = cell]
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
  data.table(pollutant = pn,
             median_1s = round(median(v[fin]), 3),
             p99_1s = round(quantile(v[fin], 0.99), 2),
             max_cell_median = round(mx, 3),
             scale_factor = sf_,
             max_cell_median_24h = round(mx_scaled, 3),
             rfc_ppb = round(rfc_ppb[[pn]], 2),
             HQ_chronic = round(mx_scaled / rfc_ppb[[pn]], 3))
}))
res <- merge(res, mrl_ppb, by = "pollutant", sort = FALSE)
res[, exceeds_chronic_mrl := fifelse(is.na(mrl_chronic), NA,
                                     max_cell_median_24h > mrl_chronic)]
fwrite(res, file.path(BASE, "TABLE_health_reference_HQ.csv"))
print(res)
message("\nHQ_chronic = (highest sustained 500 m cell median, 24-h scaled where ",
        "factors exist) / IRIS RfC. HQ < 1 indicates the sustained ",
        "concentration is below the non-cancer reference level.")
message("H2S odor detection threshold ~0.5-8 ppb for comparison; campaign p99 = ",
        res[pollutant == "H2S", p99_1s], " ppb.")
message("DONE.")
