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
# ===============================================================
# TIME CONVENTION (2026-08-21) - READ THIS BEFORE CHANGING THE LINE BELOW
#
# `Local_Time_MST` is FIXED Mountain Standard Time, UTC-7 all year, with no
# daylight-saving shift. Verified two ways:
#   (a) every raw string carries the literal offset -0700, in all 12 months
#       (asserted below, on every row, every run); and
#   (b) crews start at a fixed CIVIL hour, so on a true-MST clock the day's
#       first record must fall an hour EARLIER during daylight-saving months.
#       Over the 101 sampling days it does, by 0.95 h (95% CI 0.60-1.30), which
#       is consistent with exactly 1.00 h (p = 0.77) and rules out 0.00 h
#       (p < 1e-4). The offsets are truthful, not mislabelled civil time.
#
# NAMING, so no future reader assumes the attribute means what it usually means:
#   `date` is a FIXED-MST WALL CLOCK STORED WITH A UTC ATTRIBUTE.
#   It is NOT yet an absolute UTC instant.
# The "UTC" label is a carrier, chosen so the clock reading survives untouched
# through every downstream operation. To obtain the real instant, assert the
# zone the reading is in - force_tz(x, "MST") - and then convert. P04 and H04
# are the only places that need to.
#
# Two consumers depend on exactly this:
#   * 06_merge_with_wind.R joins to EPA AQS `Date.Local`/`Time.Local`, which
#     AQS publishes in LOCAL STANDARD time - the same clock. Both sides carry
#     their clock labelled UTC, so the hourly join pairs like with like.
#     Parsing this column as "America/Denver" instead makes `date` an instant
#     6-7 h away from the AQS clock reading; the join still SUCCEEDS, silently
#     pairing every mobile record with wind measured 6 h later in summer and
#     7 h later in winter. Verified by running 06's join logic on synthetic
#     data under all three parses.
#   * P04/H04 need a true instant to fetch HRRR, and get it explicitly with
#     force_tz(hour, "MST") -> with_tz("UTC"). That works because the wall
#     clock is MST, which is precisely what this parse preserves.
#
# So: tz = "UTC" here is not a shortcut, it is the convention, and it is the
# convention every existing intermediate on disk was built with.
# ===============================================================
.off <- substr(mobile$Local_Time_MST, 20, 24)
.off_tab <- table(.off[nzchar(.off)])
if (!identical(names(.off_tab), "-0700")) {
  stop("Local_Time_MST does not carry a uniform -0700 offset. Observed: ",
       paste(sprintf("%s (n=%d)", names(.off_tab), as.integer(.off_tab)), collapse = ", "),
       ". The whole pipeline assumes fixed MST; re-derive the time convention ",
       "before going further.")
}
message(sprintf("[TIME] Local_Time_MST offset uniform -0700 on all %d rows; `date` is a fixed-MST wall clock stored with a UTC attribute (not an absolute UTC instant).",
                sum(nzchar(.off))))

mobile$date<-substr(mobile$Local_Time_MST, 1, 19)
mobile$date<- lubridate::ymd_hms(mobile$date, tz="UTC")   # fixed-MST wall clock stored with a UTC attribute - see note above

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
