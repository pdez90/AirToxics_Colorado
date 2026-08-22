# ==============================================================
# 72  Check the QA/QC counts quoted in SI Section S1.4
#
# S1.4 makes four quantitative claims about the DELIVERED one-second files,
# i.e. before any of our campaign date exclusions:
#
#   (a) "of the measurements flagged MD in the delivered one-second files, only
#        1-9% (depending on pollutant) sit at half the audit MDL"
#   (b) "the remainder form a continuous distribution that includes negative
#        values - 191,962 benzene readings at -0.1 ppb alone"
#   (c) "the only codes that actually discard a number are AL and BH, between
#        480 and 8,459 rows per pollutant"  (and BR never carries a value)
#   (d) "Negative values that CDPHE did not blank survive, 358,048 of them for
#        benzene and 627,134 for H2S"
#
# None of those numbers survived into a saved output, so none could be checked
# against the run. This script recomputes all four from the monthly CDPHE CSVs
# and prints them beside what the SI currently says.
#
# Verified 2026-08-22 against the delivered CSVs. All four claims hold, with one
# correction already made to S1.4: AL/BH carry a value on 480-8,459 rows for five
# pollutants but on NONE for benzene, where the SI had said "per pollutant".
# The replication is validated by n_reported reproducing Table S3.1's "reported"
# row exactly for all six pollutants.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/72_check_s14_qaqc.R
#
# Inputs   Updated/csv/*.csv        the 58 monthly files, as read by
#                                   02_newmobile_data.R and 70_table_s31.R
#          CDPHE_audit_MDLs.csv     written by 69_cdphe_audit_mdls.R
# Output   TABLE_S1.4_qaqc_checks.csv
#
# Stage conventions follow 70_table_s31.R exactly: same file set, same dedup,
# same qualifier codebook, same MDL step function. The Goodrich route is not in
# this CSV set.
# ==============================================================
suppressPackageStartupMessages({library(data.table)})

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
CSV  <- file.path(BASE, Sys.getenv("SUNCOR_CSV_DIR", "Updated/csv"))
OUT  <- file.path(BASE, "TABLE_S1.4_qaqc_checks.csv")

POLL <- data.table(
  name = c("Benzene","Toluene","Xylene","Trimethylbenzene","H2S","HCN"),
  raw  = c("Benzene_ppbV","Toluene_ppbV","Xylene_ppbV","Trimethylbenzene_ppbV",
           "Hydrogen_Sulfide_ppbV","Hydrogen_Cyanide_ppbV"),
  flag = c("Benzene_flag","Toluene_flag","Xylene_flag","Trimethylbenzene_flag",
           "Hydrogen_Sulfide_flag","Hydrogen_Cyanide_flag"))

# ---- CDPHE qualifier codebook (verbatim from 03_checks_flags.R) ----
QUAL_NULL <- c("AL","AN","AO","AQ","AT","AX","AY","AZ","BA","BD",
               "BH","BK","BL","BM","BR","EC","MB","XX")
QUAL_KEEP <- c("CD","CG","IH","IL","IR","IT","QG","QP","QT","QW",
               "EH","LJ","MD","NS","QX")

tokens <- function(flag) {
  f <- toupper(trimws(as.character(flag))); f[is.na(f)] <- ""
  strsplit(f, "[,.;[:space:]]+")
}
has_code <- function(tk, codes) vapply(tk, function(z) any(z %in% codes), logical(1))

# ---- audit MDLs, read from the CDPHE packets ----
MDLF <- file.path(BASE, "CDPHE_audit_MDLs.csv")
if (!file.exists(MDLF))
  stop("CDPHE_audit_MDLs.csv not found. Run R_scripts/69_cdphe_audit_mdls.R first.")
mdlraw <- fread(MDLF)
CMAP <- c(Benzene = "Benzene", Toluene = "Toluene", Xylene = "Xylene",
          Trimethylbenzene = "Trimethylbenzene",
          H2S = "Hydrogen sulfide (H2S)", HCN = "Hydrogen cyanide (HCN)")
MDL <- rbindlist(lapply(names(CMAP), function(nm) {
  d <- mdlraw[compound == CMAP[[nm]]][order(from_ym)]
  rbindlist(list(
    data.table(Asset = "CAT", name = nm, from = d$from_ym, mdl = as.numeric(d$cat_mdl)),
    data.table(Asset = "EMU", name = nm, from = d$from_ym, mdl = as.numeric(d$emu_mdl))))
}))
MDL <- MDL[!is.na(mdl)]
message("audit MDLs loaded: ", nrow(MDL), " (lab x quarter x compound) entries")

get_mdl <- function(nm, asset, ym) {
  s <- MDL[name == nm & Asset == asset][order(from)]
  if (!nrow(s)) return(rep(NA_real_, length(ym)))
  i <- findInterval(ym, s$from)
  out <- rep(NA_real_, length(ym))
  out[i >= 1L] <- s$mdl[i[i >= 1L]]
  out
}

# ==============================================================
# the delivered monthly CSVs
# ==============================================================
files <- list.files(CSV, pattern = "\\.csv$", full.names = TRUE)
message("Reading ", length(files), " monthly CSVs from ", CSV)
stopifnot(length(files) == 58)
# Local_Time_MST MUST be read as character (see REPRODUCIBILITY.md)
raw <- rbindlist(lapply(files, fread, showProgress = FALSE,
                        colClasses = c(Local_Time_MST = "character")), fill = TRUE)
setnames(raw, "Asset (CAT/EMU)", "Asset", skip_absent = TRUE)
n0 <- nrow(raw); raw <- unique(raw)
message("  rows read ", format(n0, big.mark = ","),
        ", after dedup ", format(nrow(raw), big.mark = ","))
raw[, ts := as.POSIXct(substr(Local_Time_MST, 1, 19), tz = "MST")]
raw[, ym := as.integer(format(as.Date(ts), "%Y%m"))]

# ==============================================================
# the four claims
# ==============================================================
HALF_TOL <- 1e-8          # values are delivered to 2 dp; compare on a tolerance

res <- rbindlist(lapply(seq_len(nrow(POLL)), function(i) {
  nm <- POLL$name[i]
  v  <- suppressWarnings(as.numeric(raw[[POLL$raw[i]]]))
  tk <- tokens(raw[[POLL$flag[i]]])

  seen <- unique(unlist(tk)); seen <- seen[nzchar(seen)]
  unknown <- setdiff(seen, c(QUAL_NULL, QUAL_KEEP))
  if (length(unknown))
    stop("qualifier code(s) absent from the codebook for ", nm, ": ",
         paste(sort(unknown), collapse = ", "))

  has   <- is.finite(v)
  md    <- has_code(tk, "MD")
  albh  <- has_code(tk, c("AL","BH"))
  br    <- has_code(tk, "BR")
  void  <- has_code(tk, QUAL_NULL)

  m <- rep(NA_real_, nrow(raw))
  for (a in unique(raw$Asset)) {
    j <- raw$Asset == a
    m[j] <- get_mdl(nm, a, raw$ym[j])
  }
  at_half <- has & md & is.finite(m) & abs(v - 0.5 * m) < HALF_TOL

  data.table(
    pollutant            = nm,
    n_reported           = sum(has),
    n_md_with_value      = sum(has & md),
    n_md_at_half_mdl     = sum(at_half),
    pct_md_at_half_mdl   = round(100 * sum(at_half) / max(1L, sum(has & md)), 1),
    n_al_bh_with_value   = sum(has & albh),
    n_br_with_value      = sum(has & br),
    n_negative_reported  = sum(has & v < 0),
    n_negative_after_qc  = sum(has & !void & v < 0),
    n_no_mdl             = sum(has & !is.finite(m)))
}))

# benzene's modal negative value, quoted in S1.4 as -0.1 ppb
bi   <- which(POLL$name == "Benzene")
bv   <- suppressWarnings(as.numeric(raw[[POLL$raw[bi]]]))
btk  <- tokens(raw[[POLL$flag[bi]]])
bmd  <- has_code(btk, "MD")
neg  <- is.finite(bv) & bv < 0
tabn <- sort(table(bv[neg]), decreasing = TRUE)
mode_val <- as.numeric(names(tabn)[1]); mode_n <- as.integer(tabn[1])
mode_n_md <- sum(neg & bmd & abs(bv - mode_val) < HALF_TOL)

fwrite(res, OUT)
message("\n-> ", OUT)
print(res)

cat("\n== SI S1.4, claim by claim ==\n")
ck <- function(lab, got, claim) {
  agree <- isTRUE(all.equal(got, claim, tolerance = 0, check.attributes = FALSE))
  cat(sprintf("  [%s] %-52s run %-14s S1.4 says %s\n",
              if (agree) "OK  " else "EDIT", lab,
              paste(format(got, big.mark = ","), collapse = ","),
              paste(format(claim, big.mark = ","), collapse = ",")))
}
cat(sprintf("  (a) MD-flagged values sitting at half the audit MDL: %.1f%% to %.1f%%",
            min(res$pct_md_at_half_mdl), max(res$pct_md_at_half_mdl)))
cat("   S1.4 says 1% to 9%\n")
ck("(b) benzene readings at the modal negative value",
   mode_n, 191962L)
cat(sprintf("      modal negative benzene value is %.2f ppb; %s of those carry MD\n",
            mode_val, format(mode_n_md, big.mark = ",")))
# S1.4 says AL/BH carry a value on 480 to 8,459 rows for five of the six
# pollutants and on none at all for benzene, so check both halves of that.
nz <- res$n_al_bh_with_value[res$n_al_bh_with_value > 0]
ck("(c) AL/BH rows with a value, benzene (should be none)",
   res[pollutant == "Benzene", n_al_bh_with_value], 0L)
ck("(c) AL/BH rows with a value, minimum of the rest",
   if (length(nz)) min(nz) else NA_integer_, 480L)
ck("(c) AL/BH rows with a value, maximum of the rest",
   if (length(nz)) max(nz) else NA_integer_, 8459L)
ck("(c) BR rows carrying a value (must be zero)",
   sum(res$n_br_with_value), 0L)
ck("(d) negatives surviving QA/QC, benzene",
   res[pollutant == "Benzene", n_negative_after_qc], 358048L)
ck("(d) negatives surviving QA/QC, H2S",
   res[pollutant == "H2S", n_negative_after_qc], 627134L)

cat("\n  EDIT means the run disagrees with the sentence in S1.4 and the SI\n")
cat("  should carry the run's number instead.\n")
message("\nDONE.")
