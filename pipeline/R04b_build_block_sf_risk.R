# ==============================================================
# R04b_build_block_sf_risk.R
# FIX for the R04 halt: the original Rmd never creates
# `block_sf_risk` — script 20 expects it with columns
# benzene_ppb_airtox, Population_airtox, GEOID20 (it was evidently
# built interactively in the original session). This script
# reconstructs it from the freshly regenerated block data:
#   - loads censusblocks_suncor_terminal_BINWEIGHTED_AB.RData (block_sf)
#   - renames airtox columns *_ppb -> *_ppb_airtox
#   - attaches block population (from block_sf if present, else from
#     the AirToxScreen file's population column)
# then sources script 20 and runs the risk diagnostics.
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R04b: build block_sf_risk + census-block benzene risk")

suppressPackageStartupMessages({ library(sf); library(dplyr); library(data.table) })

# ----------------------------------------------------------------
# 1) Load the freshly regenerated block results (from script 18)
# ----------------------------------------------------------------
load(file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB.RData"))  # block_sf, block_dt
stopifnot(exists("block_sf"))
diag_msg("  block_sf: ", nrow(block_sf), " blocks; airtox cols present: ",
         paste(intersect(c("benzene_ppb", "toluene_ppb", "xylene_ppb"), names(block_sf)), collapse = ", "))

block_sf_risk <- block_sf %>%
  rename(benzene_ppb_airtox = benzene_ppb,
         toluene_ppb_airtox = toluene_ppb,
         xylene_ppb_airtox  = xylene_ppb)

# ----------------------------------------------------------------
# 2) Population: prefer a population column already on block_sf
#    (census TIGER blocks carry POP20); else pull it from the
#    AirToxScreen csv's population column.
# ----------------------------------------------------------------
pop_col <- grep("^POP20$|^POP$|population", names(block_sf_risk), ignore.case = TRUE, value = TRUE)
pop_col <- setdiff(pop_col, grep("airtox", pop_col, ignore.case = TRUE, value = TRUE))[1]

if (!is.na(pop_col)) {
  diag_msg("  population source: block_sf column '", pop_col, "' (2020 census)")
  block_sf_risk$Population_airtox <- suppressWarnings(as.numeric(block_sf_risk[[pop_col]]))
} else {
  diag_msg("  no population column on block_sf — scanning AirToxScreen csv header...")
  ats_csv <- file.path(BASE, "airtoxscreen.csv")
  hdr <- names(fread(ats_csv, nrows = 0))
  pcand <- grep("population|pop", hdr, ignore.case = TRUE, value = TRUE)
  diag_msg("  candidate population columns in airtoxscreen.csv: ",
           ifelse(length(pcand), paste(pcand, collapse = ", "), "NONE"))
  stopifnot(length(pcand) >= 1)
  idc <- grep("^Block$|GEOID", hdr, ignore.case = TRUE, value = TRUE)[1]
  ats <- fread(ats_csv, select = c(idc, pcand[1]), colClasses = "character")
  setnames(ats, c("GEOID20", "Population_airtox"))
  ats[, GEOID20 := gsub("\\D", "", GEOID20)]
  ats[, GEOID20 := gsub(" ", "0", sprintf("%015s", GEOID20))]
  ats[, Population_airtox := suppressWarnings(as.numeric(Population_airtox))]
  ats <- unique(ats, by = "GEOID20")
  block_sf_risk$GEOID20 <- as.character(block_sf_risk$GEOID20)
  block_sf_risk <- left_join(block_sf_risk, as.data.frame(ats), by = "GEOID20")
  diag_msg("  joined population for ", sum(is.finite(block_sf_risk$Population_airtox)),
           " of ", nrow(block_sf_risk), " blocks")
}

diag_msg(sprintf("  population summary: total %s | blocks with pop>0: %s",
                 format(sum(block_sf_risk$Population_airtox, na.rm = TRUE), big.mark = ","),
                 format(sum(block_sf_risk$Population_airtox > 0, na.rm = TRUE), big.mark = ",")))

# ----------------------------------------------------------------
# 3) Run the risk script (script 20) with block_sf_risk in scope
# ----------------------------------------------------------------
source(file.path(BASE, "R_scripts", "20_census_block_level_health_risks.R"))

# ----------------------------------------------------------------
# 4) Diagnostics vs manuscript numbers
# ----------------------------------------------------------------
diag_section("R04b-DIAG: risk numbers vs manuscript")
diag_msg("  Manuscript (old delays): 1,120 common blocks; 83,828 residents;")
diag_msg("  AirToxScreen risk 0.077-0.275 cases; mobile risk 0.183-0.650; ratio ~2.4x")
diag_msg("  --> compare with the COMMON-blocks count, population, and risk values")
diag_msg("      printed by script 20 just above; update Abstract + Section 3.3 if changed.")
for (nm in ls()) {
  if (grepl("risk|common", nm, ignore.case = TRUE) && !identical(nm, "block_sf_risk")) {
    val <- try(get(nm), silent = TRUE)
    if (!inherits(val, "try-error") && (is.numeric(val) || is.data.frame(val)) &&
        (length(val) <= 20 || is.data.frame(val))) {
      diag_msg("  object ", nm, ":")
      out <- capture.output(print(val))
      for (l in head(out, 15)) diag_msg("    ", l)
    }
  }
}
diag_msg("\nR04b complete.")
