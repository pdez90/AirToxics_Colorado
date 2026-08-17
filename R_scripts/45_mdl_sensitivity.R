# ==============================================================
# 45  MDL-SUBSTITUTION SENSITIVITY (SI)
# Four cases for handling observations below the method detection
# limit (MDL):
#   raw   - values as reported by CDPHE (baseline used in the paper)
#   zero  - below-MDL values replaced with 0
#   half  - below-MDL values replaced with MDL/2
#   full  - below-MDL values replaced with MDL
#
# MDLs are the CDPHE audit values published in the quarterly README
# files of the official air-toxics repository
# (https://www.colorado.gov/airquality/air_toxics_repo.aspx),
# van- (CAT/EMU) and period-specific. Where a quarter has no
# published audit value, the last audited value is carried forward.
#
# Outputs (BASE):
#   TABLE_mdl_sensitivity_summary.csv   - campaign stats by case
#   TABLE_mdl_sensitivity_blocks.csv    - benzene census-block metric by case
#   FinalFig/FIG_mdl_sensitivity.png    - two-panel SI figure
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]
stopifnot("date" %in% names(df))
message("  rows: ", format(nrow(df), big.mark = ","))
message("  columns: ", paste(names(df), collapse = ", "))

# van (asset) column, if it survived the pipeline
asset_col <- grep("asset", names(df), ignore.case = TRUE, value = TRUE)[1]
if (!is.na(asset_col)) {
  df[, van := toupper(trimws(gsub("[^A-Za-z]", "", as.character(get(asset_col)))))]
  df[!van %in% c("CAT", "EMU"), van := NA_character_]
  message("  van column '", asset_col, "': ",
          paste(capture.output(print(table(df$van, useNA = "always"))), collapse = " "))
} else {
  df[, van := NA_character_]
  message("  NOTE: no Asset (CAT/EMU) column found - using the more ",
          "conservative (larger) of the two vans' MDLs per period.")
}

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")
stopifnot(all(unlist(POLLS) %in% names(df)))

# ---- MDL lookup (CDPHE quarterly README audit values) ---------
# columns: van, pollutant, start, end, mdl (ppbV)
L <- function(van, poll, start, end, mdl)
  data.table(van = van, pollutant = poll,
             start = as.Date(start), end = as.Date(end), mdl = mdl)
mdl_tab <- rbindlist(list(
  # Benzene
  L("CAT", "Benzene", "2023-01-01", "2024-09-30", 0.5),
  L("CAT", "Benzene", "2024-10-01", "2025-06-30", 1.0),
  L("EMU", "Benzene", "2023-10-01", "2024-03-31", 1.2),
  L("EMU", "Benzene", "2024-04-01", "2024-06-30", 3.2),
  L("EMU", "Benzene", "2024-07-01", "2024-09-30", 1.2),
  L("EMU", "Benzene", "2024-10-01", "2024-12-31", 0.7),
  L("EMU", "Benzene", "2025-01-01", "2025-06-30", 1.5),
  # Toluene
  L("CAT", "Toluene", "2023-01-01", "2025-06-30", 0.18),
  L("EMU", "Toluene", "2023-10-01", "2024-12-31", 0.27),
  L("EMU", "Toluene", "2025-01-01", "2025-06-30", 0.21),
  # Xylene
  L("CAT", "Xylene", "2023-01-01", "2025-06-30", 0.19),
  L("EMU", "Xylene", "2023-10-01", "2024-12-31", 0.24),
  L("EMU", "Xylene", "2025-01-01", "2025-06-30", 0.29),
  # Trimethylbenzene
  L("CAT", "Trimethylbenzene", "2023-01-01", "2025-06-30", 0.22),
  L("EMU", "Trimethylbenzene", "2023-10-01", "2024-12-31", 0.44),  # 0.44 carried Oct-Dec 24
  L("EMU", "Trimethylbenzene", "2025-01-01", "2025-06-30", 0.45),
  # H2S
  L("CAT", "H2S", "2023-01-01", "2024-09-30", 5),
  L("CAT", "H2S", "2024-10-01", "2025-06-30", 6),
  L("EMU", "H2S", "2023-10-01", "2024-03-31", 4),
  L("EMU", "H2S", "2024-04-01", "2024-06-30", 2),
  L("EMU", "H2S", "2024-07-01", "2024-09-30", 4),
  L("EMU", "H2S", "2024-10-01", "2024-12-31", 5),
  L("EMU", "H2S", "2025-01-01", "2025-06-30", 4),
  # HCN
  L("CAT", "HCN", "2023-01-01", "2024-09-30", 13),
  L("CAT", "HCN", "2024-10-01", "2025-06-30", 5),
  L("EMU", "HCN", "2025-01-01", "2025-06-30", 0.18)
))
message("MDL lookup: ", nrow(mdl_tab), " van x pollutant x period rows")

df[, day := as.Date(date)]
mdl_for <- function(poll) {
  # per-row MDL; if van unknown, use the max (conservative) across vans
  out <- rep(NA_real_, nrow(df))
  for (v in c("CAT", "EMU")) {
    tab <- mdl_tab[van == v & pollutant == poll]
    for (r in seq_len(nrow(tab))) {
      sel <- df$day >= tab$start[r] & df$day <= tab$end[r] &
             (is.na(df$van) | df$van == v)
      out[sel] <- ifelse(is.na(out[sel]), tab$mdl[r],
                         pmax(out[sel], tab$mdl[r]))
    }
    if (!all(is.na(df$van))) {  # van known: overwrite exactly for this van
      tabv <- mdl_tab[van == v & pollutant == poll]
      for (r in seq_len(nrow(tabv))) {
        sel <- !is.na(df$van) & df$van == v &
               df$day >= tabv$start[r] & df$day <= tabv$end[r]
        out[sel] <- tabv$mdl[r]
      }
    }
  }
  out
}

CASES <- c("raw", "zero", "half", "full")
substitute_case <- function(v, below, mdl, case)
  switch(case,
         raw  = v,
         zero = ifelse(below, 0, v),
         half = ifelse(below, mdl / 2, v),
         full = ifelse(below, mdl, v))

# flag columns, if retained, define below-MDL via CDPHE "MD" qualifier
flag_col <- function(poll) {
  fc <- paste0(names(POLLS)[match(poll, names(POLLS))], "_flag")
  fc2 <- c(Benzene = "Benzene_flag", Toluene = "Toluene_flag",
           Trimethylbenzene = "Trimethylbenzene_flag", Xylene = "Xylene_flag",
           H2S = "Hydrogen_Sulfide_flag", HCN = "Hydrogen_Cyanide_flag")[poll]
  if (fc2 %in% names(df)) fc2 else NA_character_
}

# ---- 1) campaign summary stats by case ------------------------
summ <- list(); below_store <- list()
for (poll in names(POLLS)) {
  col <- POLLS[[poll]]
  v <- df[[col]]
  fin <- is.finite(v)
  mdl <- mdl_for(poll)
  fc <- flag_col(poll)
  below_flag <- if (!is.na(fc)) grepl("MD", df[[fc]]) & fin else NULL
  below_val  <- fin & is.finite(mdl) & v < mdl
  below <- if (!is.null(below_flag)) below_flag else below_val
  message(sprintf(
    "%-17s finite %s | below-MDL: %s (%.1f%%) [%s]%s", poll,
    format(sum(fin), big.mark = ","), format(sum(below), big.mark = ","),
    100 * sum(below) / sum(fin),
    if (!is.null(below_flag)) "flag-based MD" else "value < MDL",
    if (!is.null(below_flag))
      sprintf(" | value-based would give %.1f%%", 100 * sum(below_val) / sum(fin))
    else ""))
  below_store[[poll]] <- below
  for (cs in CASES) {
    x <- substitute_case(v, below, mdl, cs)[fin]
    summ[[paste(poll, cs)]] <- data.table(
      pollutant = poll, case = cs, n = length(x),
      pct_substituted = round(100 * sum(below) / sum(fin), 1),
      median = round(median(x), 3), p95 = round(quantile(x, .95), 3),
      p99 = round(quantile(x, .99), 3), mean = round(mean(x), 3))
  }
}
summ <- rbindlist(summ)
fwrite(summ, file.path(BASE, "TABLE_mdl_sensitivity_summary.csv"))
print(summ)

# ---- 2) benzene census-block metric by case -------------------
message("Assigning benzene observations to census blocks...")
g <- st_read(file.path(BASE,
       "censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"),
       quiet = TRUE)
gll <- st_transform(g, 4326)
idcol <- grep("GEOID", names(gll), value = TRUE)[1]
if (is.na(idcol)) stop("no GEOID column found in COMMONBLOCKS gpkg; columns: ",
                       paste(names(gll), collapse = ", "))
message("  block id column: ", idcol)
df[, `:=`(bz_below = below_store[["Benzene"]], bz_mdl = mdl_for("Benzene"))]
bz <- df[is.finite(Benzene_ppb) & is.finite(Latitude) & is.finite(Longitude),
         .(Benzene_ppb, Latitude, Longitude, day,
           below = bz_below, mdl = bz_mdl)]
# unique rounded locations -> one spatial join, then map back
bz[, `:=`(rlon = round(Longitude, 5), rlat = round(Latitude, 5))]
uloc <- unique(bz[, .(rlon, rlat)])
message("  ", format(nrow(bz), big.mark = ","), " obs at ",
        format(nrow(uloc), big.mark = ","), " unique locations")
up <- st_as_sf(uloc, coords = c("rlon", "rlat"), crs = 4326, remove = FALSE)
w <- st_within(up, gll)   # points on shared boundaries can hit 2 polygons
nmulti <- sum(lengths(w) > 1)
if (nmulti > 0) message("  ", nmulti,
  " boundary points fell in >1 block; keeping the first match")
first <- vapply(w, function(z) if (length(z)) z[1] else NA_integer_, 1L)
uloc[, block := st_drop_geometry(gll)[[idcol]][first]]
bz <- merge(bz, uloc, by = c("rlon", "rlat"))
bz <- bz[!is.na(block)]
message("  obs inside common blocks: ", format(nrow(bz), big.mark = ","),
        " across ", uniqueN(bz$block), " blocks")

blk <- list()
for (cs in CASES) {
  bz[, val := substitute_case(Benzene_ppb, below, mdl, cs)]
  daily <- bz[, .(dmed = median(val)), by = .(block, day)]
  bmed <- daily[, .(bval = median(dmed)), by = block]
  blk[[cs]] <- bmed[, .(block, bval, case = cs)]
}
blk <- rbindlist(blk)
wide <- dcast(blk, block ~ case, value.var = "bval")
ats <- as.data.table(st_drop_geometry(gll))
ats <- ats[, .(block = get(idcol), ats = benzene_ppb_airtox)]
wide <- merge(wide, ats, by = "block")
res <- rbindlist(lapply(CASES, function(cs) {
  x <- wide[[cs]]
  data.table(case = cs,
    blocks = length(x),
    min = round(min(x), 3), median = round(median(x), 3),
    max = round(max(x), 2),
    r_vs_raw = round(cor(x, wide$raw), 3),
    median_ratio_vs_ATS = round(median(x / wide$ats, na.rm = TRUE), 2),
    blocks_gt2x_ATS = sum(x / wide$ats > 2, na.rm = TRUE))
}))
fwrite(res, file.path(BASE, "TABLE_mdl_sensitivity_blocks.csv"))
print(res)

# ---- 3) figure ------------------------------------------------
case_lab <- c(raw = "Raw (as reported)", zero = "Substitute 0",
              half = "Substitute MDL/2", full = "Substitute MDL")
summ[, case_f := factor(case_lab[case], levels = case_lab)]
pA <- ggplot(summ, aes(pollutant, median, fill = case_f)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "grey20",
           linewidth = 0.2) +
  geom_point(aes(y = p95, group = case_f), position = position_dodge(0.8),
             shape = 21, size = 1.6, fill = "white", stroke = 0.5) +
  scale_fill_brewer(palette = "Blues", name = NULL) +
  labs(x = NULL, y = "Concentration (ppb)",
       title = "A) Campaign median (bars) and 95th percentile (points) by substitution case") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")
blk[, case_f := factor(case_lab[case], levels = case_lab)]
pB <- ggplot(blk, aes(case_f, bval, fill = case_f)) +
  geom_boxplot(outlier.size = 0.4, linewidth = 0.3, show.legend = FALSE) +
  geom_hline(yintercept = median(wide$ats), linetype = 2, color = "red") +
  annotate("text", x = 0.6, y = median(wide$ats), vjust = -0.6, hjust = 0,
           label = "median AirToxScreen", color = "red", size = 3.2) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = NULL, y = "Block benzene (ppb)",
       title = "B) Census-block benzene (median of daily medians, unscaled) by case") +
  theme_bw(base_size = 11)
library(patchwork)
ggsave(file.path(BASE, "FinalFig", "FIG_mdl_sensitivity.png"),
       pA / pB, width = 9.5, height = 8, dpi = 350, bg = "white")
message("[Saved] FinalFig/FIG_mdl_sensitivity.png")
message("DONE.")
