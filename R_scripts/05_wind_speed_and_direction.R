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

wind_2023$Site.Num<-paste0(wind_2023$State.Code, wind_2023$County.Code, wind_2023$Site.Num)
wind_2023<-subset(wind_2023, select=c("Latitude", "Longitude", "Site.Num", "Date.Local", "Time.Local", "Parameter.Name","Sample.Measurement"))
wind_2023$date<-wind_2023$date<-paste(wind_2023$Date.Local, wind_2023$Time.Local)
wind_2023<-wind_2023[,c(3, 1, 2, 8, 6, 7)]
ws<-wind_2023[wind_2023$Parameter.Name=="Wind Speed - Resultant",]
wd<-wind_2023[wind_2023$Parameter.Name=="Wind Direction - Resultant",]

ws<-ws[,-5]
wd<-wd[,-5]

colnames(ws)<-c("SiteNum", "Latitude", "Longitude", "date", "ws")
colnames(wd)<-c("SiteNum", "Latitude", "Longitude", "date", "wd")
wind_2023<-merge(ws, wd, all=TRUE)
rm(ws, wd)
wind_2023$date<-lubridate::ymd_hm(wind_2023$date)

save(wind_2023, file="/Users/priyanka/Downloads/Suncor/wind_suncor_pueblo1.RData")
