# ==============================================================
# 18  Suncor + Terminal: Calculating census block level stats
# Auto-split from Suncor.Rmd  (section 18 of 40)
# ==============================================================

#Suncor + Terminal: Calculating census block level stats

# ============================================================
# ADD-ON CHUNK (builds on your existing BIN-WEIGHTED scaling chunk)
# Outputs:
#   (a) block median-of-daily-median  (bg-corrected) + scaled
#   (b) block mean-of-daily-means     (bg-corrected) + scaled
# Uses: READ scale_factors from disk (does NOT recalc ratios here)
# FIX: AirToxScreen join uses LEFT_JOIN (no merge .x/.y issues)
# ADDED: block area in m2, ha, and km2
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(tigris)
  library(ggplot2)
  library(cowplot)
})

options(tigris_use_cache = TRUE)

# ----------------------------
# 0) Load mobile (sf points) + scale_factors from prior chunk outputs
# ----------------------------
load("/Users/priyanka/Downloads/Suncor/mobile_corrected.RData")  # loads out_sf (from script 12)
if (!exists("out_merge") && exists("out_sf")) out_merge <- out_sf   # normalize object name

sf_path <- "/Users/priyanka/Downloads/Suncor/lacasa_scaling_factors_option1_binweighted.RData"
load(sf_path)  # loads `scale_factors`

stopifnot(exists("out_merge"), inherits(out_merge, "sf"))
stopifnot(exists("scale_factors"))

get_ratio <- function(pol) {
  r <- scale_factors[pollutant == pol, ratio_all_over_mobilelike]
  stopifnot(length(r) == 1L, is.finite(r))
  as.numeric(r)
}
benzene_ratio <- get_ratio("benzene")
toluene_ratio <- get_ratio("toluene")
xylene_ratio  <- get_ratio("xylene")

message("Using BIN-WEIGHTED La Casa scaling ratios (loaded):")
print(data.table(
  pollutant = c("benzene","toluene","xylene"),
  ratio_all_over_mobilelike = c(benzene_ratio, toluene_ratio, xylene_ratio)
))

# ----------------------------
# 1) Prep mobile points + pollutant columns
# ----------------------------
out_merge$day <- as.Date(out_merge$date)

bg_vars <- c("sBenzene","sToluene","sTrimethylbenzene","sXylene","sH2S","sHCN")
bg_vars <- intersect(bg_vars, names(out_merge))
stopifnot(length(bg_vars) > 0)

# ----------------------------
# 2) Census blocks + point-in-polygon join
# ----------------------------
# BUGFIX (2026-08-20): this dropped POP20, the 2020 decennial block
# population that tigris already returns. With no population column on
# block_sf, R04b_build_block_sf_risk.R falls through to its fallback branch and
# reads a population column out of airtoxscreen.csv - a DIFFERENT file from the
# airtoxscreen.xlsx this script reads for the concentrations. The published
# 126,607-resident figure therefore came from a file the concentration chain
# never touches, and could not be checked against the geography it is attached
# to. Keep the census population on the block geometry so R04b uses it.
blocks <- tigris::blocks(state = "08", year = 2020, class = "sf")
.pop20 <- intersect(c("POP20", "POP"), names(blocks))
if (!length(.pop20)) {
  message("[POP] tigris returned no POP20 column; R04b will fall back to airtoxscreen.csv")
  blocks <- dplyr::select(blocks, GEOID20, geometry)
} else {
  message(sprintf("[POP] carrying %s from the 2020 census blocks: %s residents across %s blocks statewide",
                  .pop20[1],
                  format(sum(as.numeric(blocks[[.pop20[1]]]), na.rm = TRUE), big.mark = ","),
                  format(nrow(blocks), big.mark = ",")))
  blocks <- dplyr::select(blocks, GEOID20, dplyr::all_of(.pop20[1]), geometry)
}

if (!is.na(st_crs(blocks))) out_merge <- st_transform(out_merge, st_crs(blocks))

message("Joining points to census blocks (within)...")
t0 <- Sys.time()
pts_joined <- st_join(out_merge, blocks, join = st_within, left = TRUE)
message("Done. Elapsed: ", round(difftime(Sys.time(), t0, units = "secs"), 1), " sec")

DTp <- as.data.table(st_drop_geometry(pts_joined))
DTp <- DTp[!is.na(GEOID20) & !is.na(day)]
DTp[, GEOID20 := as.character(GEOID20)]  # key as character

# ----------------------------
# 3) A) Median-of-daily-median (then median across days)
# ----------------------------
daily_med <- DTp[, lapply(.SD, median, na.rm = TRUE),
                 by = .(GEOID20, day),
                 .SDcols = bg_vars]

block_med_of_daily_med <- daily_med[, lapply(.SD, median, na.rm = TRUE),
                                    by = .(GEOID20),
                                    .SDcols = bg_vars]

setnames(block_med_of_daily_med,
         old = bg_vars,
         new = paste0(bg_vars, "_med_of_daily_med"))

# ----------------------------
# 4) B) Mean-of-daily-means (DeCarlo-style)
# ----------------------------
daily_mean <- DTp[, lapply(.SD, mean, na.rm = TRUE),
                  by = .(GEOID20, day),
                  .SDcols = bg_vars]

block_mean_of_daily_mean <- daily_mean[, lapply(.SD, mean, na.rm = TRUE),
                                       by = .(GEOID20),
                                       .SDcols = bg_vars]

setnames(block_mean_of_daily_mean,
         old = bg_vars,
         new = paste0(bg_vars, "_mean_of_daily_mean"))

# ----------------------------
# 5) Counts (support / coverage)
# ----------------------------
block_counts <- DTp[, .(
  n_points = .N,
  n_days   = uniqueN(day)
), by = .(GEOID20)]

# ----------------------------
# 6) Merge A + B + counts into one block table
# ----------------------------
block_dt <- Reduce(function(x, y) merge(x, y, by = "GEOID20", all = TRUE),
                   list(block_med_of_daily_med, block_mean_of_daily_mean, block_counts))

block_dt <- block_dt[!is.na(n_points)]

# (2026-08-20) The block surface keeps every block with at least one
# observation, so a block visited for a single second on a single day carries
# the same weight in the published risk comparison as one visited on a hundred
# days. 73_cumulative_risk.R requires >= 10 visit-days for the same quantity
# over the same domain. The primary block set is NOT filtered here, because the
# 1,668-block / 126,607-resident figures are reported from it - but the
# sensitivity is now quantified rather than invisible, so the decision can be
# made on evidence.
if ("n_days" %in% names(block_dt)) {
  .thr <- 10
  .lo  <- block_dt[n_days < .thr]
  message(sprintf("[BLOCKS] %s of %s blocks have < %d visit-days (%.1f%%); median visit-days = %.0f, 10th pct = %.0f",
                  format(nrow(.lo), big.mark = ","), format(nrow(block_dt), big.mark = ","),
                  .thr, 100 * nrow(.lo) / nrow(block_dt),
                  stats::median(block_dt$n_days), stats::quantile(block_dt$n_days, 0.10)))
  message(sprintf("[BLOCKS] those blocks hold %s of %s mobile observations (%.1f%%)",
                  format(sum(.lo$n_points), big.mark = ","),
                  format(sum(block_dt$n_points), big.mark = ","),
                  100 * sum(.lo$n_points) / sum(block_dt$n_points)))
}

# ----------------------------
# 7) Apply scaling to BOTH A and B versions (Benzene/Toluene/Xylene)
# ----------------------------
scale_col <- function(dt, src, ratio, out) {
  if (src %in% names(dt)) dt[, (out) := ratio * get(src)]
  dt
}

# A scaled
block_dt <- scale_col(block_dt, "sBenzene_med_of_daily_med", benzene_ratio,
                      "sBenzene_med_of_daily_med_scaled")
block_dt <- scale_col(block_dt, "sToluene_med_of_daily_med", toluene_ratio,
                      "sToluene_med_of_daily_med_scaled")
block_dt <- scale_col(block_dt, "sXylene_med_of_daily_med",  xylene_ratio,
                      "sXylene_med_of_daily_med_scaled")

# B scaled
block_dt <- scale_col(block_dt, "sBenzene_mean_of_daily_mean", benzene_ratio,
                      "sBenzene_mean_of_daily_mean_scaled")
block_dt <- scale_col(block_dt, "sToluene_mean_of_daily_mean", toluene_ratio,
                      "sToluene_mean_of_daily_mean_scaled")
block_dt <- scale_col(block_dt, "sXylene_mean_of_daily_mean",  xylene_ratio,
                      "sXylene_mean_of_daily_mean_scaled")

# ----------------------------
# 8) Attach geometry + calculate area + centroids for mapping
# ----------------------------
blocks$GEOID20 <- as.character(blocks$GEOID20)

block_sf <- blocks |>
  left_join(as.data.frame(block_dt), by = "GEOID20") |>
  st_as_sf()

# ---- calculate block area in square meters
# if layer is long/lat, project first to a CRS with meter units
if (sf::st_is_longlat(block_sf)) {
  block_sf <- sf::st_transform(block_sf, 26913)  # NAD83 / UTM zone 13N
}

block_sf$area_m2  <- as.numeric(sf::st_area(block_sf))
block_sf$area_ha  <- block_sf$area_m2 / 10000
block_sf$area_km2 <- block_sf$area_m2 / 1e6

message("Block area summary (m2):")
print(summary(block_sf$area_m2))

cent <- st_centroid(block_sf)
cc <- st_coordinates(cent)
block_sf$Lon_block <- cc[,1]
block_sf$Lat_block <- cc[,2]

# ----------------------------
# 9) Join AirToxScreen + scatterplots (A and B)
#   FIX: use LEFT_JOIN to avoid merge() suffix issues
# ----------------------------
# ============================================================
# Read AirToxScreen from XLSX WITHOUT mangling block GEOIDs
# - Forces Block/GEOID column to TEXT
# - Pads to 15 digits (census block GEOID length)
# - Converts pollutant columns safely to numeric
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

xlsx_path <- "/Users/priyanka/Downloads/Suncor/airtoxscreen.xlsx"

# 1) Peek at sheet names (optional)
# print(excel_sheets(xlsx_path))

# 2) Read the sheet, forcing the ID column to TEXT
# If your ID column is named "Block" in the Excel file, this will work as-is.
# If it's named differently (e.g., GEOID, GEOID20), adjust `id_col`.
id_col <- "Block"   # <-- change if needed

airtox_xlsx <- read_excel(
  xlsx_path,
  sheet = 1,
  col_types = "text"
) %>%
  dplyr::rename(Block = all_of(id_col)) %>%
  dplyr::mutate(
    Block = gsub("\\D", "", Block),
    Block = sprintf("%015s", Block),
    Block = gsub(" ", "0", Block)
  )

# 3) Convert pollutant columns to numeric (only these)
to_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

airtox_xlsx <- airtox_xlsx %>%
  dplyr::mutate(
    BENZENE = to_num(`BENZENE`),
    TOLUENE = to_num(`TOLUENE`),
    `XYLENES (MIXED ISOMERS)` = to_num(`XYLENES (MIXED ISOMERS)`)
  )

# 4) Convert ug/m3 -> ppb (same as your function)
MW <- c(benzene = 78.11, toluene = 92.14, xylene = 106.17)
ugm3_to_ppb <- function(ugm3, T_C, P_hPa, MW) {
  R <- 8.314
  T_K <- T_C + 273.15
  P_Pa <- P_hPa * 100
  (ugm3 * R * T_K * 1e3) / (MW * P_Pa)
}

airtox_keep <- airtox_xlsx %>%
  dplyr::transmute(
    GEOID20     = Block,
    benzene_ppb = ugm3_to_ppb(BENZENE, 25, 830, MW["benzene"]),
    toluene_ppb = ugm3_to_ppb(TOLUENE, 25, 830, MW["toluene"]),
    xylene_ppb  = ugm3_to_ppb(`XYLENES (MIXED ISOMERS)`, 25, 830, MW["xylene"])
  )

# 5) Sanity checks
message("AirTox XLSX GEOID length range: ",
        paste(range(nchar(airtox_keep$GEOID20)), collapse = " to "))
message("Unique GEOIDs in XLSX: ", length(unique(airtox_keep$GEOID20)))
print(head(airtox_keep$GEOID20, 10))

block_sf$GEOID20 <- as.character(block_sf$GEOID20)

block_sf <- block_sf %>%
  left_join(airtox_keep, by = "GEOID20") %>%
  st_as_sf()

df_plot <- st_drop_geometry(block_sf)
message("AirToxScreen blocks with finite benzene_ppb AFTER join: ",
        sum(is.finite(df_plot$benzene_ppb)))

# plot helper
add_stats_plot <- function(df, xvar, yvar, xlabel, ylabel, min_n = 10) {
  ok <- is.finite(df[[xvar]]) & is.finite(df[[yvar]])
  n_ok <- sum(ok)
  if (n_ok < min_n) {
    message("Skipping plot (too few complete cases): ", yvar, " vs ", xvar, " | n=", n_ok)
    return(NULL)
  }

  d2 <- df[ok, , drop = FALSE]
  r_val <- suppressWarnings(stats::cor(d2[[xvar]], d2[[yvar]], use = "complete.obs"))
  rmse_val <- sqrt(mean((d2[[yvar]] - d2[[xvar]])^2, na.rm = TRUE))
  lab <- sprintf("n = %d\nR = %.2f\nRMSE = %.2f", n_ok, r_val, rmse_val)

  xpos <- max(d2[[xvar]], na.rm = TRUE)
  ypos <- min(d2[[yvar]], na.rm = TRUE)

  ggplot(d2, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.6, size = 1.2) +
    xlab(xlabel) + ylab(ylabel) +
    theme_bw() +
    geom_text(
      data = data.frame(x = xpos, y = ypos, lab = lab),
      aes(x = x, y = y, label = lab),
      inherit.aes = FALSE,
      hjust = 1, vjust = 0, size = 4
    )
}

out_dir_fig <- "/Users/priyanka/Downloads/Suncor/FinalFig"

# ---- A: median-of-daily-median scaled
pA1 <- add_stats_plot(df_plot, "benzene_ppb", "sBenzene_med_of_daily_med_scaled",
                      "AirToxScreen Benzene (ppb)", "Mobile Benzene (A scaled; ppb)")
pA2 <- add_stats_plot(df_plot, "toluene_ppb", "sToluene_med_of_daily_med_scaled",
                      "AirToxScreen Toluene (ppb)", "Mobile Toluene (A scaled; ppb)")
pA3 <- add_stats_plot(df_plot, "xylene_ppb",  "sXylene_med_of_daily_med_scaled",
                      "AirToxScreen Xylene (ppb)", "Mobile Xylene (A scaled; ppb)")

plotsA <- Filter(Negate(is.null), list(pA1, pA2, pA3))
if (length(plotsA) > 0) {
  out_A <- file.path(out_dir_fig, "scatterplot_blocks_BINWEIGHTED_A_medofdailymed.jpeg")
  jpeg(out_A, res = 800, width = 9000, height = 9000)
  print(cowplot::plot_grid(plotlist = plotsA, labels = LETTERS[1:length(plotsA)]))
  dev.off()
  message("Saved: ", out_A)
} else {
  message("No A-plots created (likely no overlap / complete cases).")
}

# ---- B: mean-of-daily-means scaled
pB1 <- add_stats_plot(df_plot, "benzene_ppb", "sBenzene_mean_of_daily_mean_scaled",
                      "AirToxScreen Benzene (ppb)", "Mobile Benzene (B scaled; ppb)")
pB2 <- add_stats_plot(df_plot, "toluene_ppb", "sToluene_mean_of_daily_mean_scaled",
                      "AirToxScreen Toluene (ppb)", "Mobile Toluene (B scaled; ppb)")
pB3 <- add_stats_plot(df_plot, "xylene_ppb",  "sXylene_mean_of_daily_mean_scaled",
                      "AirToxScreen Xylene (ppb)", "Mobile Xylene (B scaled; ppb)")

plotsB <- Filter(Negate(is.null), list(pB1, pB2, pB3))
if (length(plotsB) > 0) {
  out_B <- file.path(out_dir_fig, "scatterplot_blocks_BINWEIGHTED_B_meanofdailymean.jpeg")
  jpeg(out_B, res = 800, width = 9000, height = 9000)
  print(cowplot::plot_grid(plotlist = plotsB, labels = LETTERS[1:length(plotsB)]))
  dev.off()
  message("Saved: ", out_B)
} else {
  message("No B-plots created (likely no overlap / complete cases).")
}

# ----------------------------
# 10) Save block outputs
# ----------------------------
save(block_sf, block_dt,
     file = "/Users/priyanka/Downloads/Suncor/censusblocks_suncor_terminal_BINWEIGHTED_AB.RData")

out_gpkg <- "/Users/priyanka/Downloads/Suncor/censusblocks_suncor_terminal_BINWEIGHTED_AB.gpkg"
st_write(block_sf, out_gpkg, append = FALSE, quiet = TRUE)
message("Wrote: ", out_gpkg)

# --------------------------------------------------
# 9b) KEEP ONLY blocks with BOTH:
#      - mobile data (n_points > 0)
#      - AirToxScreen data (finite pollutant)
# --------------------------------------------------
block_sf_overlap <- block_sf %>%
  filter(
    !is.na(n_points) & n_points > 0,
    is.finite(benzene_ppb) |
      is.finite(toluene_ppb) |
      is.finite(xylene_ppb)
  )

message("Blocks with mobile data: ",
        sum(!is.na(block_sf$n_points) & block_sf$n_points > 0))

message("Blocks with AirToxScreen data: ",
        sum(is.finite(block_sf$benzene_ppb) |
              is.finite(block_sf$toluene_ppb) |
              is.finite(block_sf$xylene_ppb)))

message("Blocks with BOTH mobile + AirToxScreen: ",
        nrow(block_sf_overlap))
summary(block_sf_overlap$area_km2)
# ----------------------------
# 11) Save ONLY overlap blocks
# ----------------------------
save(block_sf_overlap,
     file = "/Users/priyanka/Downloads/Suncor/censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData")

out_gpkg <- "/Users/priyanka/Downloads/Suncor/censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.gpkg"
st_write(block_sf_overlap, out_gpkg, append = FALSE, quiet = TRUE)

message("Wrote OVERLAP file: ", out_gpkg)
