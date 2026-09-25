# ==============================================================
# P04  Join with Mobile Toxics Data
# Auto-split from Suncor_plume.Rmd  (section 4 of 10)
# ==============================================================

#Join with Mobile Toxics Data

library(future)
options(future.globals.maxSize = 20 * 1024^3)

# ==============================================================
# DEADLOCK FIX (2026-09-23)
#
# This was plan(multicore, workers = detectCores() - 1), and the HRRR join
# below was called with parallel = TRUE. multicore means FORK. worker_fetch()
# in P03 calls reticulate into Python (Herbie -> boto3 -> S3, xarray, cfgrib),
# and forking a process that already holds a live, multi-threaded Python
# interpreter is a textbook deadlock: the child inherits the import lock and
# GIL state from threads that do not exist in it, so the first Python call in
# the worker blocks forever.
#
# Observed 2026-09-23: P03's single-threaded test fetch succeeded, P04 printed
# its [TIME] line, and then the run sat for 6h15m with no output and no files
# written before it was killed.
#
# multisession (separate R processes, no fork) would be the usual answer, but
# each fresh worker would also need use_virtualenv("r-reticulate") re-applied
# before reticulate could find Herbie - P02 only sets that in the parent - so
# it trades a hang for a likely "module not found". Sequential needs none of
# that: the parent already has the virtualenv attached and the Python helper
# built, which is exactly the state the successful test fetch ran in.
#
# Cost: roughly 1-2 h for ~1,000 hour-groups instead of a parallel run that
# does not finish. Set HRRR_PARALLEL=1 to opt back into the parallel path.
# ==============================================================
.HRRR_PAR <- nzchar(Sys.getenv("HRRR_PARALLEL"))
if (.HRRR_PAR) {
  plan(multisession, workers = max(1, parallel::detectCores() - 1))
  message("[HRRR] HRRR_PARALLEL set: using multisession. If workers fail to ",
          "find the herbie module, unset it and re-run sequentially.")
} else {
  plan(sequential)
  message("[HRRR] sequential fetch (fork + reticulate deadlocks; see the note ",
          "in P04). Expect roughly 1-2 h. Progress prints every 25 hour-groups.")
}

load("/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData")
df<-df[!is.na(df$Longitude),]
df<-df[!is.na(df$Latitude),]
# ===============================================================
# TIME CONVENTION (2026-08-21)
#
# `df$date` is a FIXED-MST WALL CLOCK STORED WITH A UTC ATTRIBUTE - it is NOT
# yet an absolute UTC instant. See the long note at 02_newmobile_data.R where
# `date` is built, and the offset assertion there. HRRR is indexed by real UTC hours, so
# the clock has to be converted, and that is exactly what these three lines do:
# round the clock to the hour, ASSERT that the clock is MST (force_tz replaces
# the label without moving the reading), then convert that instant to UTC.
#
# Why force_tz("MST") and not force_tz("America/Denver"): Local_Time_MST is
# fixed UTC-7 year round, with no daylight-saving shift (verified on the raw
# strings and again from the seasonal pattern of crew start times). Using
# "America/Denver" would interpret a summer 09:00 MST reading as 09:00 MDT and
# fetch HRRR one hour early for every daylight-saving record - roughly 70% of
# the sampling days - which would change the wind direction, the plume
# admission test, the stability class and the inversion.
#
# The assertion below is the point of failure if someone later "fixes" script
# 02 to parse Local_Time_MST as America/Denver: the label would no longer be
# UTC, and this would stop rather than silently double-shift.
if (!identical(attr(df$date, "tzone"), "UTC")) {
  stop("P04: `date` is labelled `", paste(attr(df$date, "tzone"), collapse = "/"),
       "`, not `UTC`. This pipeline stores a fixed-MST wall clock with a UTC ",
       "attribute, not an absolute UTC instant (see ",
       "02_newmobile_data.R). Re-run 02 with that convention before joining HRRR, ",
       "or the HRRR hour will be wrong.")
}
df$hour<-round(df$date, "hour")
df$hour<-force_tz(df$hour, "MST")   # the reading IS MST; assert it, do not convert
df$hour<-with_tz(df$hour, "UTC")
message(sprintf("[TIME] first record: clock %s MST -> HRRR hour %s UTC",
                format(df$date[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                format(df$hour[1], "%Y-%m-%d %H:%M", tz = "UTC")))


message(sprintf("[HRRR] %s rows across %s distinct hours to fetch",
                format(nrow(df), big.mark = ","),
                format(length(unique(df$hour)), big.mark = ",")))
.t_hrrr <- Sys.time()
out_hrrr <- run_hrrr_uv_pbl_clouds_on_df_fast(
  df,
  time_col = "hour",
  lat_col  = "Latitude",
  lon_col  = "Longitude",
  fxx      = 0,
  parallel = .HRRR_PAR
)
message(sprintf("[HRRR] fetch completed in %.1f min",
                as.numeric(difftime(Sys.time(), .t_hrrr, units = "mins"))))
print(out_hrrr)

res <- out_hrrr %>%
  dplyr::mutate(
    windspd = sqrt(u10^2 + v10^2),
    winddir = (270 - atan2(v10, u10) * 180/pi) %% 360
  )

cor(res$windspd, res$ws, use="pairwise.complete.obs")
cor(res$winddir, res$wd, use="pairwise.complete.obs")

save(res, file="/Users/priyanka/Downloads/Suncor/mobile_hrrr.RData")
