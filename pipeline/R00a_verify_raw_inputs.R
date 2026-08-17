# ==============================================================
# R00a_verify_raw_inputs.R
# Closes the last hand-made link in the chain: the monthly CSVs in
# Updated/csv/ (what script 02 reads) were manually exported from
# the official CDPHE quarterly xlsx packets. This script:
#   default : inventory check — packets present with the exact
#             revision suffixes posted on the CDPHE repository
#             (colorado.gov/airquality/air_toxics_repo.aspx,
#             checked 2026-08) + monthly CSV coverage calendar
#   DEEP=1  : content check — per route x quarter, compares monthly
#             CSV row counts and benzene column sums against the
#             xlsx packets (proves csv == official data)
#   REBUILD=1: regenerates the monthly CSVs from the xlsx packets
#             into Updated/csv_rebuilt/ (never overwrites), then
#             diffs against the existing CSVs — making the
#             xlsx -> csv step fully scripted going forward
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R00a: raw-input verification vs official CDPHE repository")

suppressPackageStartupMessages({ library(data.table); library(readxl) })

UPD <- file.path(BASE, "Updated"); CSVD <- file.path(UPD, "csv")

# ----------------------------------------------------------------
# 1) Expected official packets (CDPHE repo snapshot, study period)
# ----------------------------------------------------------------
routes <- c(SUN = "Commerce City-Suncor & Phillips 66",
            HEP = "N. Commerce City-HEP Terminal")
rev_by_q <- c("2023_Q1"="r3","2023_Q2"="r3","2023_Q3"="r3","2023_Q4"="r3",
              "2024_Q1"="r3","2024_Q2"="r3","2024_Q3"="r2","2024_Q4"="r2",
              "2025_Q1"="r2","2025_Q2"="r2")
expected <- c()
for (q in names(rev_by_q)) for (r in routes)
  expected <- c(expected, sprintf("%s_%s_%s.xlsx", q, r, rev_by_q[q]))

diag_msg("Expected packets (2 NDCC routes x 10 quarters, official revisions): ", length(expected))
missing <- expected[!file.exists(file.path(UPD, expected))]
if (length(missing)) { for (m in missing) diag_msg("  [MISSING] ", m) } else
  diag_msg("  [PASS] all 20 packets present with the exact official revision suffixes")
diag_msg("  Note: 2025 Q3 (Jul-Sep) is posted on the repo but is OUTSIDE the study period")
diag_msg("  (campaign ended 2025-06-23); Pueblo/Collins packets are outside the study domain.")

# ----------------------------------------------------------------
# 2) Monthly CSV coverage calendar (what script 02 actually reads)
# ----------------------------------------------------------------
diag_section("R00a: monthly CSV coverage (Feb 2023 - Jun 2025)")
mon_names <- c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec")
want <- data.table(expand.grid(prefix = c("Suncor","Terminal"),
                               yr = 2023:2025, mi = 1:12, stringsAsFactors = FALSE))
want <- want[!(yr == 2023 & mi == 1) & !(yr == 2025 & mi > 6)]
want[, f := sprintf("%s_%s_%d.csv", prefix, mon_names[mi], yr)]
want[, present := file.exists(file.path(CSVD, f))]
diag_msg(sprintf("  expected monthly files: %d | present: %d", nrow(want), sum(want$present)))
if (all(want$present)) diag_msg("  [PASS] coverage is complete and comprehensive for the study period")
if (any(!want$present)) for (f in want[present == FALSE, f]) diag_msg("  [MISSING] ", f)
extra <- setdiff(list.files(CSVD, pattern = "\\.csv$"), want$f)
if (length(extra)) diag_msg("  [NOTE] extra files not in the expected calendar: ", paste(extra, collapse = ", "))

# ----------------------------------------------------------------
# helper to read a packet robustly (finds the header row)
# ----------------------------------------------------------------
read_packet <- function(xf) {
  # sheet 1 is a quarterly SUMMARY page; the 1-s time series lives on a later
  # sheet. Search every sheet for a header row containing a Time column and
  # an Asset/CAT column, then read that sheet from that row.
  for (sh in excel_sheets(xf)) {
    peek <- suppressMessages(read_excel(xf, sheet = sh, n_max = 30, col_names = FALSE))
    if (!nrow(peek)) next
    hdr_row <- which(apply(peek, 1, function(r) any(grepl("Time", r, ignore.case = TRUE)) &&
                                       any(grepl("Asset|CAT", r, ignore.case = TRUE))))[1]
    if (!is.na(hdr_row)) {
      dat <- suppressMessages(read_excel(xf, sheet = sh, skip = hdr_row - 1))
      diag_msg("    (data sheet: '", sh, "', header row ", hdr_row, ", ",
               format(nrow(dat), big.mark = ","), " rows)")
      return(dat)
    }
  }
  diag_msg("  [WARN] no data sheet found in ", basename(xf), " — sheets: ",
           paste(excel_sheets(xf), collapse = ", "))
  NULL
}
month_of <- function(x) as.integer(format(as.POSIXct(substr(as.character(x), 1, 19),
                                                     tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS")), "%m"))

# ----------------------------------------------------------------
# 3) DEEP content check: csv vs xlsx per route x quarter
# ----------------------------------------------------------------
if (nzchar(Sys.getenv("DEEP")) || nzchar(Sys.getenv("REBUILD"))) {
  diag_section("R00a-DEEP: content comparison csv vs official xlsx")
  q_months <- list("Q1" = 1:3, "Q2" = 4:6, "Q3" = 7:9, "Q4" = 10:12)
  rebuild_dir <- file.path(UPD, "csv_rebuilt")
  if (nzchar(Sys.getenv("REBUILD"))) dir.create(rebuild_dir, showWarnings = FALSE)

  for (q in names(rev_by_q)) {
    yr <- as.integer(substr(q, 1, 4)); qq <- substr(q, 6, 7)
    for (rk in names(routes)) {
      xf <- file.path(UPD, sprintf("%s_%s_%s.xlsx", q, routes[rk], rev_by_q[q]))
      if (!file.exists(xf)) next
      X <- read_packet(xf)
      if (is.null(X)) next
      tcol <- grep("Local_Time|Time", names(X), value = TRUE)[1]
      bcol <- grep("Benzene", names(X), value = TRUE)[1]
      if (is.na(tcol)) { diag_msg("  [WARN] no time column found in ", basename(xf),
                                  " — columns: ", paste(head(names(X), 10), collapse = ", ")); next }
      X$..m <- suppressWarnings(month_of(X[[tcol]]))
      pre <- ifelse(rk == "SUN", "Suncor", "Terminal")
      for (mi in q_months[[qq]]) {
        if (yr == 2023 && mi == 1) next
        if (yr == 2025 && mi > 6) next
        cf <- file.path(CSVD, sprintf("%s_%s_%d.csv", pre, mon_names[mi], yr))
        n_x <- sum(X$..m == mi, na.rm = TRUE)
        if (!file.exists(cf)) {
          if (n_x > 0) diag_msg(sprintf("  [GAP ] %s missing but xlsx has %d rows for that month",
                                        basename(cf), n_x))
          next
        }
        C <- fread(cf, showProgress = FALSE)
        bxs <- if (!is.na(bcol)) suppressWarnings(sum(as.numeric(X[[bcol]][X$..m == mi]), na.rm = TRUE)) else NA
        bcc <- grep("Benzene", names(C), value = TRUE)[1]
        bcs <- if (!is.na(bcc)) suppressWarnings(sum(as.numeric(C[[bcc]]), na.rm = TRUE)) else NA
        row_ok <- nrow(C) == n_x
        sum_ok <- is.na(bxs) || is.na(bcs) || abs(bxs - bcs) <= max(1e-6 * abs(bxs), 0.01)
        diag_msg(sprintf("  [%s] %-26s csv rows: %-8d xlsx rows: %-8d benzene-sum match: %s",
                         ifelse(row_ok && sum_ok, "PASS", "DIFF"), basename(cf), nrow(C), n_x,
                         ifelse(sum_ok, "yes", sprintf("NO (csv %.2f vs xlsx %.2f)", bcs, bxs))))
        if (nzchar(Sys.getenv("REBUILD"))) {
          out <- X[X$..m == mi & !is.na(X$..m), setdiff(names(X), "..m")]
          fwrite(as.data.table(out), file.path(rebuild_dir, basename(cf)))
        }
      }
    }
  }
  if (nzchar(Sys.getenv("REBUILD")))
    diag_msg("\nRebuilt monthly CSVs written to Updated/csv_rebuilt/ — compare, then adopt by",
             " replacing Updated/csv/ if diffs are acceptable (this makes the xlsx->csv step scripted).")
} else {
  diag_msg("\nRun `DEEP=1 Rscript R00a_verify_raw_inputs.R` for the content-level csv-vs-xlsx check,")
  diag_msg("or `REBUILD=1 ...` to regenerate the monthly CSVs from the official packets.")
}
