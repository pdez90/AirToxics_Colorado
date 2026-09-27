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

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
})

BASE <- SUNCOR_BASE
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

# ---- MDL lookup -----------------------------------------------
# BUGFIX (2026-09-27): this section previously carried its OWN hand-typed
# ladder of van x pollutant x period MDLs, transcribed from the quarterly
# READ-ME files. Five entries did not match CDPHE_audit_MDLs.csv - the file
# 69_cdphe_audit_mdls.R writes straight from those same packets and that
# 70_table_s31.R (Table S3.1) reads:
#
#   EMU HCN 2025 Q1-Q2 ....... typed 0.18, audited 18 (Q1) and 2 (Q2).
#                              0.18 is the CAT *Toluene* MDL from the line
#                              20 rows above it; the whole S3.1 explanation
#                              of the HCN below-MDL fraction rested on it.
#   EMU HCN 2023 Q4-2024 Q4 .. audited 5, absent from the typed ladder.
#   CAT Benzene 2025 Q2 ...... typed 1.0, audited 0.3.
#   EMU Benzene 2025 Q2 ...... typed 1.5, audited 0.5.
#   CAT HCN 2025 Q2 .......... typed 5,   audited 10.
#
# Two independent transcriptions of one source is the defect, not the typos,
# so the ladder is deleted and the audited file is read here as well. The
# lookup is a per-quarter step function with carry-forward, identical to
# get_mdl() in 70_table_s31.R, so this script and Table S3.1 can no longer
# disagree about what the MDL was on a given day for a given van.
MDLF <- file.path(BASE, "CDPHE_audit_MDLs.csv")
if (!file.exists(MDLF))
  stop("CDPHE_audit_MDLs.csv not found. Run R_scripts/69_cdphe_audit_mdls.R first.")
.mdlraw <- fread(MDLF)
CMAP <- c(Benzene = "Benzene", Toluene = "Toluene",
          Trimethylbenzene = "Trimethylbenzene", Xylene = "Xylene",
          H2S = "Hydrogen sulfide (H2S)", HCN = "Hydrogen cyanide (HCN)")
stopifnot(all(CMAP %in% unique(.mdlraw$compound)))
mdl_tab <- rbindlist(lapply(names(CMAP), function(nm) {
  d <- .mdlraw[compound == CMAP[[nm]]][order(from_ym)]
  rbindlist(list(
    data.table(van = "CAT", pollutant = nm, from_ym = d$from_ym, mdl = as.numeric(d$cat_mdl)),
    data.table(van = "EMU", pollutant = nm, from_ym = d$from_ym, mdl = as.numeric(d$emu_mdl))))
}))[!is.na(mdl)]
message("MDL lookup read from CDPHE_audit_MDLs.csv: ", nrow(mdl_tab),
        " van x pollutant x quarter rows, ", min(mdl_tab$from_ym), " to ",
        max(mdl_tab$from_ym))

df[, day := as.Date(date)]
df[, ym := as.integer(format(day, "%Y%m"))]

# per-row MDL. Carry the last audited quarter forward; a row earlier than the
# first audited quarter for that van gets NA (it is not silently given the
# other van's value). If the van is unknown, take the larger of the two vans'
# values for that quarter, which is the conservative choice.
.mdl_step <- function(poll, .van, ym) {
  s <- mdl_tab[pollutant == poll & van == .van][order(from_ym)]
  if (!nrow(s)) return(rep(NA_real_, length(ym)))
  i <- findInterval(ym, s$from_ym)
  out <- rep(NA_real_, length(ym))
  out[i >= 1L] <- s$mdl[i[i >= 1L]]
  out
}
mdl_for <- function(poll) {
  cat_v <- .mdl_step(poll, "CAT", df$ym)
  emu_v <- .mdl_step(poll, "EMU", df$ym)
  out <- pmax(cat_v, emu_v, na.rm = TRUE)          # van unknown -> conservative
  out[!is.finite(out)] <- NA_real_
  k <- !is.na(df$van) & df$van == "CAT"; out[k] <- cat_v[k]
  k <- !is.na(df$van) & df$van == "EMU"; out[k] <- emu_v[k]
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
wide <- data.table::dcast(blk, block ~ case, value.var = "bval")
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
  ggplot2::annotate("text", x = 0.6, y = median(wide$ats), vjust = -0.6, hjust = 0,
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
