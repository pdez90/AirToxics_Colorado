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
# ---------------------------------------------------------------
# GPS QA/QC (2026-08-20)
# Every analysis in this paper is spatial - 500 m cells, census blocks, 100 m
# hotspot buffers, plume geometry - so a position CDPHE has flagged as
# questionable makes the observation unusable even when the concentration
# itself is sound. Any observation whose GPS_flag is not empty is therefore
# removed. That is the behaviour this line already had; it is written out
# explicitly here, counted and reported, rather than left implicit in an
# untrimmed string comparison that would have failed open on stray whitespace.
#
# What this removes (counted from the 58 monthly CDPHE CSVs):
#   QG      GPS questionable          121,207 rows, ALL with parseable coords
#   QG,CG   + corrected GPS data      116,106 rows, 99.8% with parseable coords
#   QG,AQ / AQ,QG / QG,AQ,AL / AN / AL / AQ / QG,CG,AL   ~110,000 rows, whose
#           coordinates are already blank because they carry a null qualifier
#   QC        7 rows - a code absent from the CDPHE codebook
#   Total: 348,235 of 2,392,325 rows (14.6%).
#
# Note this is STRICTER than the null-qualifier rule applied to the pollutant
# channels in 03_checks_flags.R: QG and CG are classified "Informational Only"
# in the CDPHE codebook, and ~237,000 of the removed rows carry valid
# coordinates. That asymmetry is deliberate and is stated in the manuscript -
# an informational caveat on a concentration is tolerable, an informational
# caveat on the position is not, because position is what every analysis here
# is built on.
# ---------------------------------------------------------------
mobile$GPS_flag <- trimws(as.character(mobile$GPS_flag))
mobile$GPS_flag[is.na(mobile$GPS_flag)] <- ""
.gps_bad <- nzchar(mobile$GPS_flag)
.gps_tab <- sort(table(mobile$GPS_flag[.gps_bad]), decreasing = TRUE)
message(sprintf("[GPS] removing %s of %s rows (%.1f%%) carrying a GPS flag",
                format(sum(.gps_bad), big.mark = ","),
                format(nrow(mobile), big.mark = ","), 100 * mean(.gps_bad)))
for (.k in names(.gps_tab)) {
  message(sprintf("[GPS]    %-12s %s", .k,
                  format(as.integer(.gps_tab[[.k]]), big.mark = ",")))
}
.gps_known <- c("QG","CG","AL","AN","AO","AQ","AT","AX","AY","AZ","BA","BD",
                "BH","BK","BL","BM","BR","EC","MB","XX","IH","IL","IR","IT",
                "QP","QT","QW","EH","LJ","MD","NS","QX")
.gps_seen <- unique(unlist(strsplit(mobile$GPS_flag[.gps_bad], "[,.;[:space:]]+")))
.gps_seen <- toupper(.gps_seen[nzchar(.gps_seen)])
.gps_unknown <- setdiff(.gps_seen, .gps_known)
if (length(.gps_unknown)) {
  message(sprintf("[GPS]    NOTE: code(s) absent from the CDPHE codebook, also removed: %s",
                  paste(sort(.gps_unknown), collapse = ", ")))
}

mobile$Latitude  <- ifelse(.gps_bad, NA, mobile$Latitude)
mobile$Longitude <- ifelse(.gps_bad, NA, mobile$Longitude)

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
