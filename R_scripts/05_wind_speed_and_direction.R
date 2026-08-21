# ==============================================================
# 05  Wind speed and direction
# Auto-split from Suncor.Rmd  (section 5 of 40)
# ==============================================================

#Wind speed and direction

load("/Users/priyanka/Downloads/Suncor/mobile.RData")
#2023
wind_2023<-read.csv("/Users/priyanka/Downloads/Suncor/hourly_WIND_2023.csv")
wind_2023<-subset(wind_2023, wind_2023$State.Code==8)
#wind_2023<-subset(wind_2023, wind_2023$Site.Num ==28)
#Lat: 39.7861, Lon:-104.9886
#units: Knots, Degrees Compass

wind_2024<-read.csv("/Users/priyanka/Downloads/Suncor/hourly_WIND_2024.csv")
wind_2024<-subset(wind_2024, wind_2024$State.Code==8)
#wind_2024<-subset(wind_2024, wind_2024$Site.Num ==28)
#Lat: 39.7861, Lon:-104.9886
#units: Knots, Degrees Compass

wind_2025<-read.csv("/Users/priyanka/Downloads/Suncor/hourly_WIND_2025.csv")
wind_2025<-subset(wind_2025, wind_2025$State.Code==8)
#wind_2025<-subset(wind_2025, wind_2025$Site.Num ==28)
#Lat: 39.7861, Lon:-104.9886
#units: Knots, Degrees Compass

wind_2023<-rbind(wind_2023, wind_2024, wind_2025)
rm(wind_2024, wind_2025)

# KEY FIX (2026-08-20): read.csv strips leading zeros from the AQS code
# columns, so unpadded pasting made (8, 1, 234) and (8, 12, 34) both
# collapse to "81234" and two distinct monitoring stations merged into one.
# AQS field widths are State 2, County 3, Site 4.
.n_before <- length(unique(paste(wind_2023$State.Code, wind_2023$County.Code,
                                wind_2023$Site.Num)))
.sn <- vapply(list(wind_2023$State.Code, wind_2023$County.Code, wind_2023$Site.Num),
              function(v) sum(is.na(suppressWarnings(as.integer(v)))), integer(1))
stopifnot("05: State.Code / County.Code / Site.Num contain non-integer values; AQS ID cannot be padded" =
            all(.sn == 0L))
wind_2023$Site.Num <- sprintf("%02d%03d%04d",
                              as.integer(wind_2023$State.Code),
                              as.integer(wind_2023$County.Code),
                              as.integer(wind_2023$Site.Num))
message(sprintf("[KEY] AQS station IDs: %d distinct (State,County,Site) triples -> %d padded IDs%s",
                .n_before, length(unique(wind_2023$Site.Num)),
                if (.n_before == length(unique(wind_2023$Site.Num))) "" else "  <-- COLLISION, investigate"))
wind_2023<-subset(wind_2023, select=c("Latitude", "Longitude", "Site.Num", "Date.Local", "Time.Local", "Parameter.Name","Sample.Measurement"))
wind_2023$date<-wind_2023$date<-paste(wind_2023$Date.Local, wind_2023$Time.Local)
wind_2023<-wind_2023[,c(3, 1, 2, 8, 6, 7)]
ws<-wind_2023[wind_2023$Parameter.Name=="Wind Speed - Resultant",]
wd<-wind_2023[wind_2023$Parameter.Name=="Wind Direction - Resultant",]

ws<-ws[,-5]
wd<-wd[,-5]

colnames(ws)<-c("SiteNum", "Latitude", "Longitude", "date", "ws")
colnames(wd)<-c("SiteNum", "Latitude", "Longitude", "date", "wd")

# ---------------------------------------------------------------
# UNIT FIX (2026-08-19): AQS reports "Wind Speed - Resultant" in KNOTS
# (see the '#units: Knots' notes above, taken from the AQS metadata). The
# value was previously carried through unconverted while every downstream
# consumer treated it as m/s: the source-probability filter (Section
# 2.5.3.1) thresholds at "> 1 m/s", and 53_windrose.R labels its legend
# "m/s". Converting here fixes all consumers at once.
# NOT affected: the plume inversion, which uses HRRR `windspd` (true m/s).
# ---------------------------------------------------------------
KNOTS_TO_MS <- 0.514444
.ws_kn_median <- stats::median(ws$ws, na.rm = TRUE)
ws$ws <- ws$ws * KNOTS_TO_MS
message(sprintf("[UNITS] AQS wind knots -> m/s (x%.6f); median %.2f kn -> %.2f m/s",
                KNOTS_TO_MS, .ws_kn_median, stats::median(ws$ws, na.rm = TRUE)))
# BUGFIX (2026-08-20): POC (parameter occurrence code) is dropped by the
# subset() above, so a station reporting wind on two POCs yields two `ws` rows
# and two `wd` rows with an identical (SiteNum, Latitude, Longitude, date) key.
# merge() then produces FOUR rows, pairing every speed with every direction -
# including two combinations that were never measured together. 06's
# `.SD[.N]` per (SiteNum, hour) would then keep an arbitrary one, so the wind
# joined to a mobile record could carry one monitor's speed with another's
# direction. Collapse each series to one record per station-hour first.
.wkey <- c("SiteNum", "Latitude", "Longitude", "date")
.dw <- sum(duplicated(ws[, .wkey])); .dd <- sum(duplicated(wd[, .wkey]))
if (.dw > 0 || .dd > 0) {
  message(sprintf("[POC] %d duplicate speed and %d duplicate direction records on the station-hour key; keeping the first of each (check POC in the AQS export).",
                  .dw, .dd))
}
ws <- ws[!duplicated(ws[, .wkey]), ]
wd <- wd[!duplicated(wd[, .wkey]), ]

wind_2023<-merge(ws, wd, all=TRUE)
rm(ws, wd)
wind_2023$date<-lubridate::ymd_hm(wind_2023$date)

save(wind_2023, file="/Users/priyanka/Downloads/Suncor/wind_suncor_pueblo1.RData")
