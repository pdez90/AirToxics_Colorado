# ==============================================================
# 69  CDPHE audit MDLs, straight from the quarterly data packets
#
# The HB21-1189 read-me PDFs do not print MDL values. They say:
#   "The reported Method Detection Limits (MDLs) for each compound and
#    asset are reported on the Quarterly Summary tab of the Data Summary."
# So this script reads that tab out of every packet in Updated/ and writes
# CDPHE_audit_MDLs.csv, which 70_table_s31.R then uses. Nothing about the
# detection limits is typed into the pipeline by hand.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/69_cdphe_audit_mdls.R
#
# The Goodrich route is excluded, as everywhere else in this work. Both
# remaining routes carry the same MDL table within a quarter; that is
# asserted rather than assumed.
# ==============================================================
suppressPackageStartupMessages({library(data.table); library(readxl)})

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
DIR  <- file.path(BASE, "Updated")
OUT  <- file.path(BASE, "CDPHE_audit_MDLs.csv")

files <- list.files(DIR, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[!grepl("Goodrich", basename(files))]
files <- files[!startsWith(basename(files), "~$")]     # Excel lock files
message("packets to read: ", length(files))

COMPOUNDS <- c("Benzene", "Toluene", "Xylene", "Trimethylbenzene",
               "Hydrogen sulfide (H2S)", "Hydrogen cyanide (HCN)")

grab <- function(f) {
  b <- basename(f)
  m <- regmatches(b, regexec("^(\\d{4})_Q(\\d)_", b))[[1]]
  if (!length(m)) { message("  skip (unrecognised name): ", b); return(NULL) }
  x <- suppressMessages(read_excel(f, sheet = "Quarterly Summary",
                                   range = "A1:F14", col_names = FALSE,
                                   .name_repair = "minimal"))
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  hdr <- which(apply(x, 1, function(r) any(grepl("CAT Audit MDL", r, fixed = TRUE))))
  if (!length(hdr)) stop("no 'CAT Audit MDL' header in ", b)
  hdr <- hdr[1]
  cat_col <- which(grepl("CAT Audit MDL", x[hdr, ], fixed = TRUE))[1]
  emu_col <- which(grepl("EMU Audit MDL", x[hdr, ], fixed = TRUE))[1]
  cmp_col <- which(grepl("Compound of Interest", x[hdr, ], fixed = TRUE))[1]
  body <- x[(hdr + 1):nrow(x), , drop = FALSE]
  keep <- !is.na(body[[cmp_col]]) & nzchar(trimws(body[[cmp_col]]))
  body <- body[keep, , drop = FALSE]
  num <- function(v) { v <- trimws(as.character(v)); v[v %in% c("n/a","N/A","","NA")] <- NA
                       suppressWarnings(as.numeric(v)) }
  data.table(year = as.integer(m[2]), quarter = as.integer(m[3]),
             route = if (grepl("Suncor", b)) "Suncor" else "HEP",
             compound = trimws(body[[cmp_col]]),
             cat_mdl = num(body[[cat_col]]), emu_mdl = num(body[[emu_col]]))
}

all <- rbindlist(lapply(files, function(f) { message("  ", basename(f)); grab(f) }))
stopifnot(nrow(all) > 0)

# the routes present must report the same MDL table for a given quarter
mism <- all[, .(n = uniqueN(paste(cat_mdl, emu_mdl)), routes = uniqueN(route)),
            by = .(year, quarter, compound)][n > 1]
if (nrow(mism)) { print(mism); stop("routes disagree on the audit MDL table") }
message("routes agree on every quarter/compound: TRUE (",
        uniqueN(all$route), " route(s) read)")

mdl <- unique(all[, .(year, quarter, compound, cat_mdl, emu_mdl)])
mdl[, from_ym := year * 100L + (quarter - 1L) * 3L + 1L]
setorder(mdl, compound, from_ym)
fwrite(mdl, OUT)
message("wrote ", OUT, " (", nrow(mdl), " rows)")

cat("\n== CDPHE audit MDLs (ppbV), by quarter ==\n")
for (cp in COMPOUNDS) {
  d <- mdl[compound == cp]
  if (!nrow(d)) { cat(sprintf("  %-24s (absent from the packets)\n", cp)); next }
  cat(sprintf("  %-24s CAT %s\n", cp,
              paste(sprintf("%dQ%d:%s", d$year, d$quarter,
                            ifelse(is.na(d$cat_mdl), "-", format(d$cat_mdl))), collapse = "  ")))
  cat(sprintf("  %-24s EMU %s\n", "",
              paste(sprintf("%dQ%d:%s", d$year, d$quarter,
                            ifelse(is.na(d$emu_mdl), "-", format(d$emu_mdl))), collapse = "  ")))
}
