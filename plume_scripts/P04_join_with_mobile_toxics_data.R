# ==============================================================
# P04  Join with Mobile Toxics Data
# Auto-split from Suncor_plume.Rmd  (section 4 of 10)
# ==============================================================

#Join with Mobile Toxics Data

library(future)
options(future.globals.maxSize = 20 * 1024^3)  # not strictly needed with multicore, but ok
plan(multicore, workers = max(1, parallel::detectCores() - 1))

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


out_hrrr <- run_hrrr_uv_pbl_clouds_on_df_fast(
  df,
  time_col = "hour",      # or your real column names
  lat_col  = "Latitude",
  lon_col  = "Longitude",
  fxx      = 0,
  parallel = TRUE         # or FALSE
)
print(out_hrrr)

res <- out_hrrr %>%
  dplyr::mutate(
    windspd = sqrt(u10^2 + v10^2),
    winddir = (270 - atan2(v10, u10) * 180/pi) %% 360
  )

cor(res$windspd, res$ws, use="pairwise.complete.obs")
cor(res$winddir, res$wd, use="pairwise.complete.obs")

save(res, file="/Users/priyanka/Downloads/Suncor/mobile_hrrr.RData")
