# ==============================================================
# 75_airtoxscreen_from_epa.R  (2026-09-22)
# Rebuild airtoxscreen.xlsx / airtoxscreen.csv (the inputs of script 18 and
# R04b) from EPA's public 2020 AirToxScreen block-level ambient
# concentrations for EPA Region 8:
#   https://gaftp.epa.gov/rtrmodeling_public/AirToxScreen/2020/Ambient%20Concentrations/
#   Region8_2020ATS_Ambient_Concentrations.xlsx      (~290 MB)
#
# That workbook is 183 columns wide and 1.9 GB unzipped, so the five needed
# columns (State, Block, Population, BENZENE, TOLUENE, XYLENES (MIXED
# ISOMERS)) are streamed out of it by R_scripts/75_extract_airtoxscreen.py
# into airtoxscreen_epa_region8_CO.csv (Colorado blocks only), which this
# script then converts.
#
# CHECK: the manuscript's 1,668 risk blocks carry their AirToxScreen benzene
# in baseline_manuscript/blocks.rds. Each of those polygons is matched to its
# 2020 census block and the benzene values compared, so the rebuilt file is
# only accepted if it reproduces the published run.
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
CO   <- file.path(BASE, "airtoxscreen_epa_region8_CO.csv")

if (!file.exists(CO)) {
  src <- c(file.path(BASE, "Region8_2020ATS_Ambient_Concentrations.xlsx"),
           path.expand("~/Downloads/Region8_2020ATS_Ambient_Concentrations.xlsx"),
           path.expand("~/Downloads/Additional_suncor/Region8_2020ATS_Ambient_Concentrations.xlsx"))
  src <- src[file.exists(src)][1]
  if (is.na(src))
    stop("Neither airtoxscreen_epa_region8_CO.csv nor Region8_2020ATS_Ambient_Concentrations.xlsx found.\n",
         "Download the Region 8 workbook from\n",
         "https://gaftp.epa.gov/rtrmodeling_public/AirToxScreen/2020/Ambient%20Concentrations/\n",
         "into ~/Downloads and re-run.")
  message("[ATS] extracting Colorado blocks from ", src, " (a few minutes)")
  st <- system2("python3", c(shQuote(file.path(BASE, "R_scripts", "75_extract_airtoxscreen.py")),
                             shQuote(src), shQuote(CO)))
  if (st != 0 || !file.exists(CO)) stop("extraction failed (exit ", st, ")")
}

ats <- fread(CO, colClasses = "character")
setnames(ats, make.names(names(ats)), skip_absent = TRUE)  # no-op guard
stopifnot(all(c("Block", "BENZENE", "TOLUENE") %in% names(ats)))
xyl <- grep("XYLENE", names(ats), value = TRUE)[1]
ats[, Block := gsub(" ", "0", sprintf("%015s", gsub("\\D", "", Block)))]
ats <- unique(ats[nchar(Block) == 15 & startsWith(Block, "08")], by = "Block")
num <- function(v) suppressWarnings(as.numeric(gsub(",", "", v)))
message(sprintf("[ATS] Colorado blocks: %s | population total %s",
                format(nrow(ats), big.mark = ","),
                format(sum(num(ats$Population), na.rm = TRUE), big.mark = ",")))
for (p in c("BENZENE", "TOLUENE", xyl)) {
  v <- num(ats[[p]])
  message(sprintf("[ATS]   %-24s finite %s | min %.4g  median %.4g  max %.4g ug/m3",
                  p, format(sum(is.finite(v)), big.mark = ","),
                  min(v, na.rm = TRUE), median(v, na.rm = TRUE), max(v, na.rm = TRUE)))
}

# ---- write the two files script 18 / R04b expect ---------------------------
out <- ats[, .(Block, Population, BENZENE, TOLUENE, `XYLENES (MIXED ISOMERS)` = get(xyl))]
if (!requireNamespace("writexl", quietly = TRUE))
  install.packages("writexl", repos = "https://cloud.r-project.org")
writexl::write_xlsx(as.data.frame(out), file.path(BASE, "airtoxscreen.xlsx"))
fwrite(out, file.path(BASE, "airtoxscreen.csv"))
message("[ATS] wrote airtoxscreen.xlsx and airtoxscreen.csv in ", BASE)

# ---- CHECK against the manuscript run --------------------------------------
bl_f <- file.path(BASE, "baseline_manuscript", "blocks.rds")
if (!file.exists(bl_f)) { message("[ATS] CHECK skipped: ", bl_f, " missing"); quit(save = "no") }
bl <- readRDS(bl_f)
message("[ATS] CHECK: matching the manuscript's ", nrow(bl), " risk blocks to 2020 census blocks")
options(tigris_use_cache = TRUE)
blocks <- tigris::blocks(state = "08", year = 2020, class = "sf", progress_bar = FALSE)
blocks <- st_transform(blocks[, "GEOID20"], st_crs(bl))
pts <- suppressWarnings(st_point_on_surface(st_make_valid(bl)))
j <- st_join(pts, blocks, join = st_within)
d <- data.table(GEOID20 = j$GEOID20, bz_ms = j$benzene_ppb_airtox)
ugm3_to_ppb <- function(ugm3, T_C = 25, P_hPa = 830, MW = 78.11)
  (ugm3 * 8.314 * (T_C + 273.15) * 1e3) / (MW * P_hPa * 100)
out[, bz_new := ugm3_to_ppb(num(BENZENE))]
d <- merge(d, out[, .(GEOID20 = Block, bz_new)], by = "GEOID20", all.x = TRUE)
d[, rel := abs(bz_new - bz_ms) / pmax(abs(bz_ms), 1e-12)]
n_found <- sum(is.finite(d$bz_new)); n_match <- sum(d$rel < 1e-4, na.rm = TRUE)
message(sprintf("[ATS] CHECK: %d of %d manuscript blocks found in the rebuilt file; benzene identical (rel. diff < 1e-4) in %d; median |rel. diff| %.2g",
                n_found, nrow(d), n_match, median(d$rel, na.rm = TRUE)))
if (n_match >= 0.99 * nrow(d)) {
  message("[ATS] CHECK PASSED: the rebuilt airtoxscreen file reproduces the manuscript's AirToxScreen benzene.")
} else {
  message("[ATS] CHECK FAILED: values differ from the published run - do NOT use for Phase 2 until resolved.")
  fwrite(d[order(-rel)][1:min(.N, 50)], file.path(BASE, "airtoxscreen_check_mismatches.csv"))
  quit(status = 2)
}
