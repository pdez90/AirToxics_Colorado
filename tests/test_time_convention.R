# ==============================================================
# test_time_convention.R
#
# Locks down the ONE unusual thing about time in this pipeline:
#
#   `date` is a FIXED-MST WALL CLOCK STORED WITH A UTC ATTRIBUTE.
#   It is NOT an absolute UTC instant.
#
# The tzone attribute is a carrier for the clock reading, not a claim about
# the instant. Everything downstream depends on that, and the failure mode of
# getting it wrong is SILENT: script 06 still returns a full set of wind
# matches, they are just all 6-7 hours away from the observation.
#
# Run:  Rscript tests/test_time_convention.R
# Exits non-zero on any failure, so it can go straight into CI.
# ==============================================================

suppressPackageStartupMessages({ library(lubridate) })

.fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) {
    cat("  PASS  ", what, "\n", sep = "")
  } else {
    cat("  FAIL  ", what, "\n", sep = "")
    .fail <<- .fail + 1L
  }
  invisible(NULL)
}
section <- function(x) cat("\n== ", x, " ==\n", sep = "")

# --------------------------------------------------------------
# 1) THE UNIT TEST: one summer timestamp, one winter timestamp.
#
# Local_Time_MST is fixed UTC-7 all year, so the SAME clock reading must map
# to the SAME UTC hour in both seasons. America/Denver does not: it lands an
# hour early in summer. This is the whole convention in four assertions.
# --------------------------------------------------------------
section("unit test: 09:00 Local_Time_MST -> 16:00 UTC, in both seasons")

to_utc <- function(clock, zone) {
  # `clock` is the wall-clock reading as stored (attribute UTC, no shift).
  d <- ymd_hms(clock, tz = "UTC")
  format(with_tz(force_tz(d, zone), "UTC"), "%H:%M")
}

summer <- "2024-07-17 09:00:00"
winter <- "2024-01-15 09:00:00"

ok(to_utc(summer, "MST") == "16:00", 'summer 09:00 --force_tz("MST")--> 16:00 UTC')
ok(to_utc(winter, "MST") == "16:00", 'winter 09:00 --force_tz("MST")--> 16:00 UTC')
cat("    (fixed UTC-7: the season must not matter)\n")

ok(to_utc(summer, "America/Denver") == "15:00",
   'summer 09:00 --force_tz("America/Denver")--> 15:00 UTC  [WRONG BY ONE HOUR]')
ok(to_utc(winter, "America/Denver") == "16:00",
   'winter 09:00 --force_tz("America/Denver")--> 16:00 UTC  [agrees only in winter]')
cat("    (this is why America/Denver is not interchangeable here: it is right\n",
    "     in winter, which is exactly what makes the summer error easy to miss)\n", sep = "")

# The rounding step P04/H04 apply must not move the instant either.
r_summer <- with_tz(force_tz(round(ymd_hms("2024-07-17 09:22:26", tz = "UTC"), "hour"), "MST"), "UTC")
ok(format(r_summer, "%Y-%m-%d %H:%M") == "2024-07-17 16:00",
   "P04/H04 chain: round -> force_tz(MST) -> with_tz(UTC) gives the 16:00 UTC HRRR hour")

# --------------------------------------------------------------
# 2) THE RAW-DATA ASSERTION: every Local_Time_MST string carries -0700.
#
# This is the assertion that lives in 02_newmobile_data.R. It is repeated here
# so the test suite fails if a future data delivery changes the offset, even
# if nobody re-runs 02.
# --------------------------------------------------------------
section("raw data: Local_Time_MST offset is -0700 in every month")

csv_dir <- Sys.getenv("SUNCOR_CSV_DIR", "Updated/csv")
files <- list.files(csv_dir, pattern = "^(Suncor|Terminal)_.*\\.csv$", full.names = TRUE)

if (!length(files)) {
  cat("  SKIP  no monthly CSVs found under '", csv_dir, "'\n", sep = "")
  cat("        set SUNCOR_CSV_DIR to run this section\n")
} else {
  offs <- character(0)
  months <- character(0)
  for (f in files) {
    x <- utils::read.csv(f, nrows = 5000, colClasses = "character")
    if (!"Local_Time_MST" %in% names(x)) next
    v <- x$Local_Time_MST[nzchar(x$Local_Time_MST)]
    if (!length(v)) next
    offs   <- unique(c(offs, substr(v, 20, 24)))
    months <- unique(c(months, substr(v, 6, 7)))
  }
  ok(identical(offs, "-0700"),
     sprintf("all offsets are -0700 across %d files (observed: %s)",
             length(files), paste(offs, collapse = ", ")))
  ok(length(months) == 12,
     sprintf("offset verified in all 12 calendar months (found %d)", length(months)))
}

# --------------------------------------------------------------
# 3) THE DST DIAGNOSTIC.
#
# The offsets could in principle be mislabelled - a file that says -0700 while
# actually holding civil time would pass section 2. This tests the labels
# against human behaviour instead: crews start at a fixed CIVIL hour, so if the
# clock really is fixed MST, the day's first record must fall about an hour
# EARLIER during daylight-saving months. Measured over the campaign it does,
# by 0.95 h (95% CI 0.60-1.30) - consistent with 1.00 h, and 0.00 h rejected.
# --------------------------------------------------------------
section("DST diagnostic: day-start times shift by ~1 h across the DST boundary")

if (!length(files)) {
  cat("  SKIP  needs the monthly CSVs\n")
} else {
  first <- list()
  for (f in files) {
    x <- utils::read.csv(f, colClasses = "character")
    if (!"Local_Time_MST" %in% names(x)) next
    v <- x$Local_Time_MST[nzchar(x$Local_Time_MST)]
    if (!length(v)) next
    d <- substr(v, 1, 10); t <- substr(v, 12, 19)
    agg <- tapply(t, d, min)
    for (k in names(agg)) first[[k]] <- min(c(first[[k]], agg[[k]]))
  }
  day <- names(first)
  hr  <- vapply(first, function(s) {
    p <- as.numeric(strsplit(s, ":")[[1]]); p[1] + p[2] / 60
  }, numeric(1))
  dst <- as.POSIXlt(as.POSIXct(paste(day, "12:00:00"), tz = "America/Denver"))$isdst == 1

  cat(sprintf("  sampling days: %d (DST %d, standard time %d)\n",
              length(hr), sum(dst), sum(!dst)))
  if (sum(dst) >= 5 && sum(!dst) >= 5) {
    tt <- t.test(hr[!dst], hr[dst])
    d_h <- unname(diff(rev(tt$estimate)))
    cat(sprintf("  mean day-start: DST %.2f h, standard %.2f h, difference %.2f h [95%% CI %.2f, %.2f]\n",
                mean(hr[dst]), mean(hr[!dst]), d_h, tt$conf.int[1], tt$conf.int[2]))
    p_vs_1 <- t.test(hr[!dst] - 1, hr[dst])$p.value
    p_vs_0 <- tt$p.value
    ok(p_vs_1 > 0.05,
       sprintf("consistent with a 1.00 h shift (fixed-MST labels truthful); p = %.3f", p_vs_1))
    ok(p_vs_0 < 0.05,
       sprintf("inconsistent with no shift (labels are NOT civil time); p = %.2g", p_vs_0))
  } else {
    cat("  SKIP  not enough days on each side of the DST boundary\n")
  }

  transitions <- as.Date(c("2023-03-12", "2023-11-05", "2024-03-10",
                           "2024-11-03", "2025-03-09", "2025-11-02"))
  ok(sum(as.Date(day) %in% transitions) == 0,
     "no sampling day falls on a DST transition date (the ambiguous-hour cases never arise)")
}

# --------------------------------------------------------------
# 4) THE INTERMEDIATES ON DISK still carry the convention.
#
# If an intermediate was rebuilt under a different parse, this catches it
# without needing to re-run the pipeline.
# --------------------------------------------------------------
section("intermediates on disk carry the convention")

base <- Sys.getenv("SUNCOR_BASE", ".")
checked <- 0L
for (nm in c("mobile.RData", "mobile_wswd.RData", "bgcorrected_out_merge.RData")) {
  f <- file.path(base, nm)
  if (!file.exists(f)) next
  ev <- new.env(); load(f, envir = ev)
  for (obj in ls(ev)) {
    d <- ev[[obj]]
    if (!is.data.frame(d) || !"date" %in% names(d)) next
    checked <- checked + 1L
    ok(identical(attr(d$date, "tzone"), "UTC"),
       sprintf("%s$%s$date has tzone 'UTC' (got '%s')",
               nm, obj, paste(attr(d$date, "tzone"), collapse = "/")))
    h <- as.integer(format(d$date[is.finite(d$date)][seq_len(min(50000, sum(is.finite(d$date))))],
                           "%H", tz = "UTC"))
    ok(all(h >= 4 & h <= 22),
       sprintf("%s$%s$date read as a clock gives daytime hours (%d-%d), as MST field work must",
               nm, obj, min(h), max(h)))
  }
  rm(ev)
}
if (!checked) cat("  SKIP  no intermediates found; set SUNCOR_BASE to the working folder\n")

# --------------------------------------------------------------
# 5) NO SCRIPT CONVERTS PIPELINE TIMESTAMPS VIA America/Denver.
#
# This is the mechanical enforcement. The convention has now been re-broken
# twice in two different places by code that looked locally reasonable
# (36_hysplit, and H04's helper default plus its example call), so a static
# scan is worth more than another comment. Mentions inside comments are fine -
# most of them exist to explain why the zone is wrong - so only live code
# counts.
# --------------------------------------------------------------
section("no live code interprets pipeline timestamps as America/Denver")

roots <- Sys.getenv("SUNCOR_CODE_DIRS",
                    paste("R_scripts", "pipeline", "plume_scripts", "hrrr_scripts",
                          "methane", "rerun_pipeline", sep = ","))
dirs  <- trimws(strsplit(roots, ",")[[1]])
dirs  <- dirs[dir.exists(dirs)]
if (!length(dirs)) {
  cat("  SKIP  none of the code directories found from here\n")
} else {
  hits <- character(0)
  for (d in dirs) {
    for (f in list.files(d, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) {
      ln <- readLines(f, warn = FALSE)
      code <- ln
      code[grepl("^\\s*#", code)] <- ""          # drop whole-line comments
      code <- sub("#.*$", "", code)               # drop trailing comments
      # Only a CONVERSION counts. Naming the zone in a stop() message - which
      # several of these files now do, to explain why it is refused - is not a
      # use of it, and neither is a membership test against an allowed set.
      bad <- grep("(force_tz|with_tz|as\\.POSIXct|ymd_hms?|tz_local\\s*=|tzone\\s*=|tz\\s*=)[^\"]*\"America/Denver\"",
                  code)
      bad <- bad[!grepl("stop\\(|warning\\(|%in%|!=", code[bad])]
      if (length(bad))
        hits <- c(hits, sprintf("%s:%d: %s", f, bad, trimws(code[bad])))
    }
  }
  ok(length(hits) == 0,
     sprintf("scanned %s: no live America/Denver conversion", paste(dirs, collapse = ", ")))
  if (length(hits)) for (h in hits) cat("        ", h, "\n", sep = "")
}

# 5b) The HRRR helper must not expose the zone as a caller's choice at all.
# It was an argument, it defaulted to America/Denver, and the example call in
# the same file passed it. Removing the argument is what stops that recurring.
h04 <- unlist(lapply(dirs, function(d)
  list.files(d, pattern = "^H04_hrrr\\.R$", recursive = TRUE, full.names = TRUE)))
if (!length(h04)) {
  cat("  SKIP  H04_hrrr.R not found from here\n")
} else {
  for (f in h04) {
    ln <- readLines(f, warn = FALSE)
    i <- grep("download_hrrr_and_join_mobile\\s*<-\\s*function\\(", ln)[1]
    j <- grep("^\\)\\s*\\{", ln); j <- min(j[j > i])
    sig <- paste(sub("#.*$", "", ln[i:j]), collapse = " ")
    ok(!grepl("tz_local", sig),
       sprintf("%s: download_hrrr_and_join_mobile() takes no tz_local argument", basename(f)))
    ok(any(grepl('^H04_TZ_LOCAL\\s*<-\\s*"(MST|Etc/GMT\\+7)"', ln)),
       sprintf("%s: the zone is a hard-coded fixed-UTC-7 constant", basename(f)))
  }
}

# 5c) END-TO-END: the same wall-clock reading must give the same HRRR hour
# through BOTH paths. P04 and H04 are independent implementations of one step,
# and they have already diverged twice - on the zone, and on round vs floor.
#
# This lifts the conversion out of each FILE and evaluates it, rather than
# re-typing the expression here. Re-typing it would make the test tautological:
# it would compare two copies of whatever this file says, not what the pipeline
# does.
section("end-to-end: P04 and H04 map the same clock to the same HRRR hour")

find_one <- function(pat) {
  hits <- unlist(lapply(dirs, function(d)
    list.files(d, pattern = pat, recursive = TRUE, full.names = TRUE)))
  if (length(hits)) hits[1] else NA_character_
}
f_p04 <- find_one("^P04_join_with_mobile_toxics_data\\.R$")
f_h04 <- find_one("^H04_hrrr\\.R$")

if (is.na(f_p04) || is.na(f_h04)) {
  cat("  SKIP  need both P04 and H04 on disk\n")
} else {
  clocks <- ymd_hms(c("2024-07-17 09:22:26",   # summer, rounds down
                      "2024-07-17 09:45:10",   # summer, rounds UP (floor/round differ)
                      "2024-01-15 09:22:26",   # winter, rounds down
                      "2024-01-15 09:45:10"),  # winter, rounds up
                    tz = "UTC")

  # --- P04: its three df$hour lines, executed verbatim ---
  p04_lines <- grep("^df\\$hour\\s*<-", readLines(f_p04, warn = FALSE), value = TRUE)
  ev <- new.env(parent = environment())
  ev$df <- data.frame(date = clocks)
  for (l in p04_lines) eval(parse(text = l), envir = ev)
  hour_p04 <- ev$df$hour

  # --- H04: its constant and its hour_utc expression, executed verbatim ---
  h04_src  <- readLines(f_h04, warn = FALSE)
  const    <- grep("^H04_TZ_LOCAL\\s*<-", h04_src, value = TRUE)[1]
  hour_exp <- grep("hour_utc\\s*=\\s*with_tz\\(", h04_src, value = TRUE)[1]
  hour_exp <- sub(",\\s*$", "", sub("^\\s*hour_utc\\s*=\\s*", "", hour_exp))
  ev2 <- new.env(parent = environment())
  eval(parse(text = const), envir = ev2)
  ev2$.dt_local <- clocks
  hour_h04 <- eval(parse(text = hour_exp), envir = ev2)

  cat(sprintf("  P04 lines executed: %d;  H04 constant: %s\n",
              length(p04_lines), trimws(const)))
  print(data.frame(
    mst_clock = format(clocks,   "%Y-%m-%d %H:%M", tz = "UTC"),
    P04_hour  = format(hour_p04, "%Y-%m-%d %H:%M", tz = "UTC"),
    H04_hour  = format(hour_h04, "%Y-%m-%d %H:%M", tz = "UTC")
  ), row.names = FALSE)

  ok(length(p04_lines) == 3 && nzchar(const) && nzchar(hour_exp),
     "both conversions were located in their source files (not re-typed here)")
  ok(identical(as.numeric(hour_p04), as.numeric(hour_h04)),
     "P04 and H04 give the SAME HRRR hour for every summer and winter case")
  ok(format(hour_p04[1], "%H:%M", tz = "UTC") == "16:00" &&
     format(hour_p04[3], "%H:%M", tz = "UTC") == "16:00",
     "09:22 MST -> 16:00 UTC in July AND January (no daylight-saving shift)")
}

# --------------------------------------------------------------
cat(sprintf("\n%s  (%d failure%s)\n",
            if (.fail == 0L) "ALL TIME-CONVENTION TESTS PASSED" else "TIME-CONVENTION TESTS FAILED",
            .fail, if (.fail == 1L) "" else "s"))
if (.fail > 0L) quit(status = 1L)
