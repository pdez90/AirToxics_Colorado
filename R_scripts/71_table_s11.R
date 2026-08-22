# ==============================================================
# 71  Table S1.1 measurement periods, from the measurement data
#
# Table S1.1 asserts a measurement period for each instrument. This script
# derives those periods from the files the instruments produced, so the table
# reports what was measured rather than what was remembered.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/71_table_s11.R
#
# Writes TABLE_S1.1_periods.csv.
#
# Sources
#   mobile_wswd.RData   mobile PTR-ToF-MS aromatics (CDPHE vans CAT and EMU)
#   ascent_2023.csv     La Casa, Vocus Elf, summer 2023
#   lacasa3.csv         La Casa, Vocus Elf, winter 2023/24
#   ascent_2024.csv     La Casa, Vocus 2R, summer 2024
#
# The two La Casa families use different date orders in the same kind of
# column, so the order is DETERMINED from the data rather than assumed: a
# column is day-first only if some row has a first field above 12. If both
# orders parse every row and neither is decidable, the script stops rather
# than guessing.
# ==============================================================
suppressPackageStartupMessages({library(data.table)})

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
OUT  <- file.path(BASE, "TABLE_S1.1_periods.csv")

parse_dates <- function(x, label) {
  x <- trimws(as.character(x))
  x <- x[nzchar(x) & !is.na(x)]
  first <- suppressWarnings(as.integer(sub("^(\\d+)/.*$", "\\1", x)))
  second <- suppressWarnings(as.integer(sub("^\\d+/(\\d+)/.*$", "\\1", x)))
  day_first <- any(first > 12, na.rm = TRUE)
  mon_first <- any(second > 12, na.rm = TRUE)
  if (day_first && mon_first)
    stop(label, ": both fields exceed 12 somewhere - the date order is inconsistent")
  if (!day_first && !mon_first)
    stop(label, ": no field ever exceeds 12 - the date order cannot be determined")
  # two-digit years first: R's %Y happily reads "23" as the year 23 AD, so a
  # four-digit format can "succeed" on a two-digit file and silently produce
  # dates two millennia off. The year range is checked as well.
  fmts <- if (day_first) c("%d/%m/%y %H:%M", "%d/%m/%Y %H:%M")
          else           c("%m/%d/%y %H:%M", "%m/%d/%Y %H:%M")
  for (f in fmts) {
    d <- as.POSIXct(x, format = f, tz = "MST")
    yr <- as.integer(format(d, "%Y"))
    if (!any(is.na(d)) && all(yr >= 2015 & yr <= 2030, na.rm = TRUE)) {
      message(sprintf("  %-28s %s  (%s, %s rows)", label,
                      if (day_first) "day-first" else "month-first", f,
                      format(length(d), big.mark = ",")))
      return(d)
    }
  }
  stop(label, ": no tried format parsed every row")
}

span <- function(d) c(format(min(d), "%Y-%m-%d"), format(max(d), "%Y-%m-%d"),
                      as.character(uniqueN(as.Date(d))))

rows <- list()
message("La Casa stationary records:")

f <- file.path(BASE, "ascent_2023.csv")
if (file.exists(f)) {
  d <- fread(f, showProgress = FALSE)
  ts <- parse_dates(d[[1]], "ascent_2023.csv (Vocus Elf)")
  ok <- !is.na(d[[grep("Benzene", names(d), ignore.case = TRUE)[1]]])
  s <- span(ts[ok]); sa <- span(ts)
  rows[[length(rows)+1]] <- data.table(
    instrument = "Vocus Elf", deployment = "La Casa, summer 2023",
    file = basename(f), first_record = sa[1], last_record = sa[2],
    days_with_record = sa[3], first_benzene = s[1], last_benzene = s[2],
    days_with_benzene = s[3])
}

f <- file.path(BASE, "lacasa3.csv")
if (file.exists(f)) {
  d <- fread(f, showProgress = FALSE)
  ts <- parse_dates(d[[1]], "lacasa3.csv (Vocus Elf, winter)")
  ok <- !is.na(d[[grep("Toluene", names(d), ignore.case = TRUE)[1]]])
  s <- span(ts[ok]); sa <- span(ts)
  rows[[length(rows)+1]] <- data.table(
    instrument = "Vocus Elf", deployment = "La Casa, winter 2023/24",
    file = basename(f), first_record = sa[1], last_record = sa[2],
    days_with_record = sa[3], first_benzene = NA_character_,
    last_benzene = NA_character_, days_with_benzene = NA_character_)
}

f <- file.path(BASE, "ascent_2024.csv")
if (file.exists(f)) {
  d <- fread(f, showProgress = FALSE)
  ts <- parse_dates(d[[1]], "ascent_2024.csv (Vocus 2R)")
  ok <- !is.na(d[[grep("benzene", names(d), ignore.case = TRUE)[1]]])
  s <- span(ts[ok]); sa <- span(ts)
  rows[[length(rows)+1]] <- data.table(
    instrument = "Vocus 2R", deployment = "La Casa, summer 2024",
    file = basename(f), first_record = sa[1], last_record = sa[2],
    days_with_record = sa[3], first_benzene = s[1], last_benzene = s[2],
    days_with_benzene = s[3])
}

message("\nMobile record:")
load(file.path(BASE, "mobile_wswd.RData"))   # -> out
m <- as.data.table(out); rm(out)
m <- m[Site != "Goodrich Corporation (Collins Aerospace)"]
m[, day := as.Date(date)]
for (a in c("ALL", sort(unique(m$Asset)))) {
  d <- if (a == "ALL") m else m[Asset == a]
  b <- d[is.finite(Benzene_ppb)]
  message(sprintf("  %-28s %s to %s  (%s days with benzene)", 
                  paste0("mobile, ", a), min(b$day), max(b$day), uniqueN(b$day)))
  rows[[length(rows)+1]] <- data.table(
    instrument = "PTR-ToF-MS (mobile)",
    deployment = if (a == "ALL") "CDPHE vans, both" else paste("CDPHE van", a),
    file = "mobile_wswd.RData",
    first_record = format(min(d$day)), last_record = format(max(d$day)),
    days_with_record = as.character(uniqueN(d$day)),
    first_benzene = format(min(b$day)), last_benzene = format(max(b$day)),
    days_with_benzene = as.character(uniqueN(b$day)))
}

res <- rbindlist(rows, fill = TRUE)
fwrite(res, OUT)
message("\nwrote ", OUT)
cat("\n=========== Table S1.1 measurement periods (from data) ===========\n")
print(res, nrows = 50)
cat("=================================================================\n")
