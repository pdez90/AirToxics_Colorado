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
df$hour<-round(df$date, "hour")
df$hour<-force_tz(df$hour, "MST")
df$hour<-with_tz(df$hour, "UTC")


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
