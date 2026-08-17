# ==============================================================
# 02  NewMobile data
# Auto-split from Suncor.Rmd  (section 2 of 40)
# ==============================================================

#NewMobile data
# Qualifier Flags	Description	Qualifier Type
# AL	Voided by Operator.	Null Data Qualifier
# AN	Machine Malfunction.	Null Data Qualifier
# AO	Bad Weather	Null Data Qualifier
# AQ	 Collection Error	Null Data Qualifier
# AT	Calibration	Null Data Qualifier
# AX	Precision Check	Null Data Qualifier
# AY	Q C Control Points (zero/span)	Null Data Qualifier
# AZ	Q C Audit	Null Data Qualifier
# BA	Maintenance/Routine Repairs	Null Data Qualifier
# BD	Auto Calibration 	Null Data Qualifier
# BH	Interference/co-elution/misidentification	Null Data Qualifier
# BK	Site computer/data logger down	Null Data Qualifier
# BL	QA Audit	Null Data Qualifier
# BM	Accuracy check	Null Data Qualifier
# BR	Sample value below acceptable range	Null Data Qualifier
# CD	Corrected or modified data.	Informational Only
# CG	Corrected or modified GPS data	Informational Only
# EC	Exceeds Critical Criteria	Null Data Qualifier
# EH	Estimated;  Exceeds Upper Range	Quality Assurance Qualifier
# IH	Fireworks	Informational Only
# IL	Other	Informational Only
# IR	Unique Traffic Disruption	Informational Only
# IT	Wildfire	Informational Only
# LJ	Identification Of Analyte Is Acceptable; Reported Value Is An Estimate	Quality Assurance Qualifier
# MB	Method Blank (Analytical)	Null Data Qualifier
# MD	Value less than MDL	Quality Assurance Qualifier
# NS	Influenced by nearby source	Quality Assurance Qualifier
# QG	GPS questionable	Informational Only
# QP	Pressure sensor questionable	Informational Only
# QT	Temperature sensor questionable	Informational Only
# QW	Wind speed and direction questionable	Informational Only
# QX 	Does not meet QC criteria 	Quality Assurance Qualifier
# XX	Experimental Data	Null Data Qualifier

mobile_feb_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Feb_2023.csv")
mobile_march_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_March_2023.csv")
mobile_april_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_April_2023.csv")
mobile_may_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_May_2023.csv")
mobile_june_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_June_2023.csv")
mobile_july_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_July_2023.csv")
mobile_aug_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Aug_2023.csv")
mobile_sep_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Sep_2023.csv")
mobile_oct_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Oct_2023.csv")
mobile_nov_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Nov_2023.csv")
mobile_dec_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Dec_2023.csv")

mobile_jan_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Jan_2024.csv")
mobile_feb_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Feb_2024.csv")
mobile_march_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_March_2024.csv")
mobile_april_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_April_2024.csv")
mobile_may_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_May_2024.csv")
mobile_june_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_June_2024.csv")
mobile_july_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_July_2024.csv")
mobile_aug_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Aug_2024.csv")
mobile_sep_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Sep_2024.csv")
mobile_oct_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Oct_2024.csv")
mobile_nov_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Nov_2024.csv")
mobile_dec_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Dec_2024.csv")

mobile_jan_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Jan_2025.csv")
mobile_feb_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_Feb_2025.csv")
mobile_march_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_March_2025.csv")
mobile_april_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_April_2025.csv")
mobile_may_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_May_2025.csv")
mobile_june_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Suncor_June_2025.csv")

mobile<-rbind(mobile_feb_2023, mobile_march_2023, mobile_april_2023, mobile_may_2023, mobile_june_2023, mobile_july_2023, mobile_aug_2023, mobile_sep_2023, mobile_oct_2023, mobile_nov_2023, mobile_dec_2023, 
              
mobile_jan_2024, mobile_feb_2024, mobile_march_2024, mobile_april_2024, mobile_may_2024, mobile_june_2024, mobile_july_2024, mobile_aug_2024, mobile_sep_2024, mobile_oct_2024, mobile_nov_2024, mobile_dec_2024,

mobile_jan_2025, mobile_feb_2025, mobile_march_2025, mobile_april_2025, mobile_may_2025, mobile_june_2025)

rm(mobile_feb_2023, mobile_march_2023, mobile_april_2023, mobile_may_2023, mobile_june_2023, mobile_july_2023, mobile_aug_2023, mobile_sep_2023, mobile_oct_2023, mobile_nov_2023, mobile_dec_2023, 
mobile_jan_2024, mobile_feb_2024, mobile_march_2024, mobile_april_2024, mobile_may_2024, mobile_june_2024, mobile_july_2024, mobile_aug_2024, mobile_sep_2024, mobile_oct_2024, mobile_nov_2024, mobile_dec_2024,
mobile_jan_2025, mobile_feb_2025, mobile_march_2025, mobile_april_2025, mobile_may_2025, mobile_june_2025)

mobile$Site<-"Suncor and Phillips 66 Terminal"

#Terminal
mobile_feb_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Feb_2023.csv")
mobile_march_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_March_2023.csv")
mobile_april_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_April_2023.csv")
mobile_may_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_May_2023.csv")
mobile_june_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_June_2023.csv")
mobile_july_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_July_2023.csv")
mobile_aug_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Aug_2023.csv")
mobile_sep_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Sep_2023.csv")
mobile_oct_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Oct_2023.csv")
mobile_nov_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Nov_2023.csv")
mobile_dec_2023<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Dec_2023.csv")

mobile_jan_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Jan_2024.csv")
mobile_feb_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Feb_2024.csv")
mobile_march_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_March_2024.csv")
mobile_april_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_April_2024.csv")
mobile_may_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_May_2024.csv")
mobile_june_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_June_2024.csv")
mobile_july_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_July_2024.csv")
mobile_aug_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Aug_2024.csv")
mobile_sep_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Sep_2024.csv")
mobile_oct_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Oct_2024.csv")
mobile_nov_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Nov_2024.csv")
mobile_dec_2024<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Dec_2024.csv")

mobile_jan_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Jan_2025.csv")
mobile_feb_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_Feb_2025.csv")
mobile_march_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_March_2025.csv")
mobile_april_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_April_2025.csv")
mobile_may_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_May_2025.csv")
mobile_june_2025<-read.csv("/Users/priyanka/Downloads/Suncor/Updated/csv/Terminal_June_2025.csv")

mobile1<-rbind(mobile_feb_2023, mobile_march_2023, mobile_april_2023, mobile_may_2023, mobile_june_2023, mobile_july_2023, mobile_aug_2023, mobile_sep_2023, mobile_oct_2023, mobile_nov_2023, mobile_dec_2023, 
              
mobile_jan_2024, mobile_feb_2024, mobile_march_2024, mobile_april_2024, mobile_may_2024, mobile_june_2024, mobile_july_2024, mobile_aug_2024, mobile_sep_2024, mobile_oct_2024, mobile_nov_2024, mobile_dec_2024,

mobile_jan_2025, mobile_feb_2025, mobile_march_2025, mobile_april_2025, mobile_may_2025, mobile_june_2025     )

rm(mobile_feb_2023, mobile_march_2023, mobile_april_2023, mobile_may_2023, mobile_june_2023, mobile_july_2023, mobile_aug_2023, mobile_sep_2023, mobile_oct_2023, mobile_nov_2023, mobile_dec_2023, 
mobile_jan_2024, mobile_feb_2024, mobile_march_2024, mobile_april_2024, mobile_may_2024, mobile_june_2024, mobile_july_2024, mobile_aug_2024, mobile_sep_2024, mobile_oct_2024, mobile_nov_2024, mobile_dec_2024,
mobile_jan_2025, mobile_feb_2025, mobile_march_2025, mobile_april_2025, mobile_may_2025, mobile_june_2025)

mobile1$Site<-"Holly Energy Partners (Sinclair) Terminal"

mobile<-rbind(mobile, mobile1)
rm(mobile1)
mobile$date<-substr(mobile$Local_Time_MST, 1, 19)
mobile$date<- lubridate::ymd_hms(mobile$date, tz="America/Denver")

mobile<-mobile %>% dplyr::arrange(date)

print(nrow(mobile))
#2392325
mobile$Latitude <-ifelse(mobile$GPS_flag=="", mobile$Latitude, NA)
mobile$Longitude <-ifelse(mobile$GPS_flag=="", mobile$Longitude, NA)

mobile<-mobile[!is.na(mobile$Latitude),]
mobile<-mobile[!is.na(mobile$Longitude),]
print(nrow(mobile))
#2044090
# rows that are duplicated forward OR backward
a <- mobile[duplicated(mobile) | duplicated(mobile, fromLast = TRUE), ]
nrow(a)
rm(a)
#35,143
mobile<-mobile[!duplicated(mobile),]
#2024964
a_time <- mobile[
  duplicated(mobile$date) |
  duplicated(mobile$date, fromLast = TRUE),
]
nrow(a_time)

sum(!is.na(mobile$Benzene_ppbV))
sum(!is.na(mobile$Toluene_ppbV))
sum(!is.na(mobile$Xylene_ppbV))
sum(!is.na(mobile$Trimethylbenzene_ppbV))
sum(!is.na(mobile$Hydrogen_Sulfide_ppbV))
sum(!is.na(mobile$Hydrogen_Cyanide_ppbV))
