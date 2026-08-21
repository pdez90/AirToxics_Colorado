# ==============================================================
# H04  HRRR
# Auto-split from Suncor_plume2.Rmd  (section 4 of 4)
# ==============================================================

#HRRR

# ============================================================
# HRRR (NOAA/AWS via Herbie) -> point-sample -> JOIN to mobile df
#   - Designed for BIG df (e.g., 2,708,051 rows)
#   - Avoids downloading “everything”; downloads ONLY the hours you need
#   - Deduplicates point requests within each hour (rounding) to speed up
#   - Disk cache per hour (parquet) so reruns resume instantly
#   - Uses your existing run_hrrr_uv_pbl_clouds_on_df_fast() sampler
#   - Outputs: df with u10, v10, hpbl, lcc, tcdc + windspd, winddir
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(arrow)
  library(future)
})

load("/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData")
df<-df[!is.na(df$Longitude),]
df<-df[!is.na(df$Latitude),]
# TIME CONVENTION (2026-08-21): `df$date` is a FIXED-MST WALL CLOCK STORED WITH
# A UTC ATTRIBUTE - not yet an absolute UTC instant
# (see the note in 02_newmobile_data.R). force_tz("MST") asserts that reading
# rather than converting it; with_tz then gives the true UTC hour HRRR is
# indexed by. "America/Denver" here would fetch HRRR one hour early for every
# daylight-saving record. Identical to P04_join_with_mobile_toxics_data.R -
# if you change one, change both.
if (!identical(attr(df$date, "tzone"), "UTC")) {
  stop("H04: `date` is labelled `", paste(attr(df$date, "tzone"), collapse = "/"),
       "`, not `UTC`. This pipeline stores a fixed-MST wall clock with a UTC ",
       "attribute, not an absolute UTC instant ",
       "(see 02_newmobile_data.R). The HRRR hour would be wrong.")
}
df$hour<-round(df$date, "hour")
df$hour<-force_tz(df$hour, "MST")   # the reading IS MST; assert it, do not convert
df$hour<-with_tz(df$hour, "UTC")

# ---- Optional: better parallel on macOS (fork) avoids huge future globals export
# If you're on Windows, keep multisession.
setup_future_for_hrrr <- function(workers = max(1, parallel::detectCores() - 1)) {
  if (.Platform$OS.type == "windows") {
    plan(multisession, workers = workers)
  } else {
    # macOS/Linux
    plan(multicore, workers = workers)
  }
  invisible(TRUE)
}

# ---- Helper: safe parquet read/write
.safe_read_parquet <- function(path) {
  tryCatch(arrow::read_parquet(path), error = function(e) NULL)
}
.safe_write_parquet <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(x, path)
  invisible(TRUE)
}

# ---- MAIN FUNCTION
# Requires that you have defined run_hrrr_uv_pbl_clouds_on_df_fast() in your session.
download_hrrr_and_join_mobile <- function(
  df,
  datetime_col = "date",     # your timestamp column
  lat_col      = "Latitude",
  lon_col      = "Longitude",
  tz_local     = "MST",            # NOT configurable in practice - see the check below.
                                   # Local_Time_MST is fixed UTC-7 year round.
  fxx          = 0L,               # 0 = analysis hour
  cache_dir    = "/Users/priyanka/Downloads/Suncor/hrrr_hour_cache",
  round_deg    = 3,    # 3 ~ 100m; try 2 (~1km) if still heavy
  chunk_hours  = 24,   # process N hours per batch for memory control
  parallel_hours = TRUE,  # parallelize across hour-batches (recommended)
  workers      = max(1, parallel::detectCores() - 1),
  overwrite_cache = FALSE,
  verbose      = TRUE
) {
  stopifnot(is.data.frame(df))
  if (!all(c(datetime_col, lat_col, lon_col) %in% names(df))) {
    stop("df must contain columns: ", paste(c(datetime_col, lat_col, lon_col), collapse = ", "))
  }
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Install {arrow} for parquet caching.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Install {lubridate}.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install {dplyr}.")

  # ---------------------------------------------------------------
  # TIME CONVENTION (2026-08-21). This function used to default to
  # tz_local = "America/Denver", and the example call at the bottom of this
  # file passed it explicitly. That silently undid the conversion the top of
  # this script gets right: `df$date` is a FIXED-MST WALL CLOCK STORED WITH A
  # UTC ATTRIBUTE, so re-interpreting it as Denver civil time fetches HRRR one
  # hour early for every daylight-saving record - about 70% of the sampling
  # days. The dataset has exactly one time convention, so this is no longer a
  # caller's choice: anything other than a fixed UTC-7 zone is refused rather
  # than honoured.
  if (!tz_local %in% c("MST", "Etc/GMT+7")) {
    stop("H04: tz_local = '", tz_local, "' is not accepted. Local_Time_MST is fixed ",
         "UTC-7 year round (no daylight saving), so the only valid values are \"MST\" ",
         "or \"Etc/GMT+7\". Passing \"America/Denver\" would fetch HRRR one hour early ",
         "for every daylight-saving record. See the time-convention note in ",
         "02_newmobile_data.R and tests/test_time_convention.R.")
  }
  .tz_in <- attr(df[[datetime_col]], "tzone")
  if (!identical(.tz_in, "UTC")) {
    stop("H04: `", datetime_col, "` is labelled '", paste(.tz_in, collapse = "/"),
         "', not 'UTC'. This pipeline stores a fixed-MST wall clock with a UTC ",
         "attribute, not an absolute UTC instant (see 02_newmobile_data.R). ",
         "The HRRR hour would be wrong.")
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # --- prep: keep original row order id
  df0 <- df %>%
    mutate(.row_id__ = dplyr::row_number()) %>%
    filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]]))

  # --- build UTC hour
  # force_tz ASSERTS that the wall-clock reading is MST (it moves the label,
  # not the reading); with_tz then gives the real instant HRRR is indexed by.
  #
  # BUGFIX (2026-08-21): this used floor_date() while
  # P04_join_with_mobile_toxics_data.R - the path that produced the plume
  # results - uses round(). Two implementations of the same step disagreed by
  # up to an hour on roughly half the records: 09:45 floors to 09:00 but rounds
  # to 10:00. HRRR f00 is an instantaneous analysis field, so the NEAREST hour
  # is the right match for an instantaneous observation, and matching P04 is
  # what makes the two paths interchangeable. Now round(), as P04 does.
  df0 <- df0 %>%
    mutate(
      .dt_local = .data[[datetime_col]],
      hour_utc  = with_tz(force_tz(round(.dt_local, "hour"), tz_local), "UTC"),
      lat_q     = round(as.numeric(.data[[lat_col]]), round_deg),
      lon_q     = round(as.numeric(.data[[lon_col]]), round_deg)
    )

  # --- request table (unique hour + quantized lat/lon)
  req <- df0 %>%
    distinct(hour_utc, lat_q, lon_q) %>%
    rename(Latitude = lat_q, Longitude = lon_q)

  hours <- sort(unique(req$hour_utc))
  n_hours <- length(hours)
  if (verbose) {
    message("Rows in df (non-missing lat/lon): ", nrow(df0))
    message("Unique (hour, lat_q, lon_q) requests: ", nrow(req))
    message("Unique hours (UTC): ", n_hours)
    message("Cache dir: ", cache_dir)
  }

  # --- function to process a vector of hours (sequential inside)
  process_hours_block <- function(hours_block) {
    out_list <- vector("list", length(hours_block))

    for (i in seq_along(hours_block)) {
      h <- hours_block[i]
      stamp <- format(h, "%Y%m%d_%H")
      cache_file <- file.path(cache_dir, paste0("hrrr_", stamp, "_f", sprintf("%02d", as.integer(fxx)), ".parquet"))

      if (!overwrite_cache && file.exists(cache_file)) {
        cached <- .safe_read_parquet(cache_file)
        if (!is.null(cached)) {
          out_list[[i]] <- cached
          if (verbose) message("Cache hit: ", basename(cache_file))
          next
        }
      }

      sub <- req %>% filter(hour_utc == h)

      if (verbose) message("Fetching HRRR for hour ", as.character(h), " (n=", nrow(sub), " points)")

      # NOTE: parallel=FALSE here intentionally.
      # We parallelize across hour-blocks, which is usually better & avoids heavy globals.
      got <- run_hrrr_uv_pbl_clouds_on_df_fast(
        sub,
        time_col = "hour_utc",
        lat_col  = "Latitude",
        lon_col  = "Longitude",
        fxx      = fxx,
        parallel = FALSE
      ) %>%
        mutate(hour_utc = h) %>%   # ensure hour preserved
        select(hour_utc, Latitude, Longitude, u10, v10, hpbl, lcc, tcdc)

      .safe_write_parquet(got, cache_file)
      out_list[[i]] <- got
    }

    bind_rows(out_list)
  }

  # --- split hours into blocks
  blocks <- split(hours, ceiling(seq_along(hours) / chunk_hours))

  # --- run blocks (optionally parallel)
  if (parallel_hours) {
    setup_future_for_hrrr(workers = workers)

    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("{future.apply} not installed; running hour-blocks sequentially.")
      hrrr_pts <- bind_rows(lapply(blocks, process_hours_block))
    } else {
      # Export only what’s needed; avoid globals blowups
      hrrr_pts <- bind_rows(
        future.apply::future_lapply(
          blocks,
          FUN = process_hours_block,
          future.globals = list(
            process_hours_block = process_hours_block,
            req = req, cache_dir = cache_dir,
            overwrite_cache = overwrite_cache,
            fxx = fxx,
            verbose = verbose,
            .safe_read_parquet = .safe_read_parquet,
            .safe_write_parquet = .safe_write_parquet
          ),
          future.packages = c("dplyr", "lubridate", "arrow")
        )
      )
    }
  } else {
    hrrr_pts <- bind_rows(lapply(blocks, process_hours_block))
  }

  # --- join back to full df0 via quantized coords + hour
  out <- df0 %>%
    left_join(
      hrrr_pts %>%
        mutate(
          lat_q = round(as.numeric(Latitude), round_deg),
          lon_q = round(as.numeric(Longitude), round_deg)
        ) %>%
        select(hour_utc, lat_q, lon_q, u10, v10, hpbl, lcc, tcdc),
      by = c("hour_utc", "lat_q", "lon_q")
    ) %>%
    mutate(
      windspd = sqrt(u10^2 + v10^2),
      winddir = (270 - atan2(v10, u10) * 180/pi) %% 360
    ) %>%
    arrange(.row_id__) %>%
    select(-.row_id__, -.dt_local, -lat_q, -lon_q)

  out
}

# ============================================================
# EXAMPLE: run for your big mobile df
# ============================================================

# 1) Make sure you have your sampler function loaded/defined first:
#    run_hrrr_uv_pbl_clouds_on_df_fast <- function(...) { ... }  # <-- your function

# 2) Load your data
# load("/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData")  # expects object `df`

# 3) Run HRRR join (writes per-hour parquet cache to disk)
out_hrrr <- download_hrrr_and_join_mobile(
  df,
  datetime_col = "date",
  lat_col      = "Latitude",
  lon_col      = "Longitude",
  tz_local     = "MST",   # fixed UTC-7; "America/Denver" is refused by the function
  fxx          = 0L,
  cache_dir    = "/Users/priyanka/Downloads/Suncor/hrrr_hour_cache",
  round_deg    = 3,         # bump to 2 if too many unique points
  chunk_hours  = 24,
  parallel_hours = TRUE,
  workers      = max(1, parallel::detectCores() - 1),
  overwrite_cache = FALSE,
  verbose      = TRUE
)

# 4) Quick checks vs your measured wind
cor(out_hrrr$windspd, out_hrrr$ws, use = "pairwise.complete.obs")
cor(out_hrrr$winddir, out_hrrr$wd, use = "pairwise.complete.obs")

# 5) Save
save(out_hrrr, file = "/Users/priyanka/Downloads/Suncor/mobile_hrrr.RData")
