# ==============================================================
# 03  Checks (flags)
# Auto-split from Suncor.Rmd  (section 3 of 40)
# ==============================================================

#Checks (flags) 

mobile$Benzene_flag<-str_trim(mobile$Benzene_flag)
mobile$Toluene_flag<-str_trim(mobile$Toluene_flag)
mobile$Xylene_flag<-str_trim(mobile$Xylene_flag)
mobile$Trimethylbenzene_flag<-str_trim(mobile$Trimethylbenzene_flag)
mobile$Hydrogen_Sulfide_flag<-str_trim(mobile$Hydrogen_Sulfide_flag)
mobile$Hydrogen_Cyanide_flag<-str_trim(mobile$Hydrogen_Cyanide_flag)

mobile$Benzene_ppbV<-ifelse(mobile$Benzene_flag=="" | mobile$Benzene_flag=="BR" | mobile$Benzene_flag=="MD" | mobile$Benzene_flag== "LJ", mobile$Benzene_ppbV, NA)

mobile$Toluene_ppbV <-ifelse(mobile$Toluene_flag=="" | mobile$Toluene_flag=="BR" | mobile$Toluene_flag=="BR,LJ"| mobile$Toluene_flag=="BR,QT" | mobile$Toluene_flag=="MD" |mobile$Toluene_flag=="LJ"| mobile$Toluene_flag=="LJ,MD"| mobile$Toluene_flag=="QT"| mobile$Toluene_flag=="QT,MD" , mobile$Toluene_ppbV, NA)

mobile$Trimethylbenzene_ppbV <-ifelse(mobile$Trimethylbenzene_flag==""| mobile$Trimethylbenzene_flag=="BR" |
mobile$Trimethylbenzene_flag=="BR,LJ"| mobile$Trimethylbenzene_flag=="BR,QT" |                  mobile$Trimethylbenzene_flag=="MD"| mobile$Trimethylbenzene_flag=="LJ"| mobile$Trimethylbenzene_flag=="LJ,MD"| mobile$Trimethylbenzene_flag=="QT"| mobile$Trimethylbenzene_flag=="QT,MD", mobile$Trimethylbenzene_ppbV, NA)

mobile$Xylene_ppbV <-ifelse(mobile$Xylene_flag==""| mobile$Xylene_flag=="BR" | mobile$Xylene_flag=="BR,LJ"| 
mobile$Xylene_flag=="BR,QT" | mobile$Xylene_flag=="MD" |mobile$Xylene_flag=="LJ"| mobile$Xylene_flag=="LJ,MD"| mobile$Xylene_flag=="QT"| mobile$Xylene_flag=="QT,MD", mobile$Xylene_ppbV, NA)

mobile$Hydrogen_Cyanide_ppbV <-ifelse(mobile$Hydrogen_Cyanide_flag==""| mobile$Hydrogen_Cyanide_flag=="BR" |
mobile$Hydrogen_Cyanide_flag=="BR,LJ" |
mobile$Hydrogen_Cyanide_flag=="BR,QT" |
mobile$Hydrogen_Cyanide_flag=="CD" | mobile$Hydrogen_Cyanide_flag=="CD,BR" |
mobile$Hydrogen_Cyanide_flag=="CD,LJ" |
mobile$Hydrogen_Cyanide_flag=="CD,LJ,MD"|
mobile$Hydrogen_Cyanide_flag=="CD,MD"|
mobile$Hydrogen_Cyanide_flag=="MD" |
mobile$Hydrogen_Cyanide_flag=="MD,LJ" |
mobile$Hydrogen_Cyanide_flag=="LJ,MD" |
mobile$Hydrogen_Cyanide_flag=="LJ"|
mobile$Hydrogen_Cyanide_flag=="LJ,EH"|
mobile$Hydrogen_Cyanide_flag=="CD,LJ,MD"|
mobile$Hydrogen_Cyanide_flag=="CD,MD"|
mobile$Hydrogen_Cyanide_flag=="QT"| mobile$Hydrogen_Cyanide_flag=="QT,MD"|
mobile$Hydrogen_Cyanide_flag=="LJ,EH"|
mobile$Hydrogen_Cyanide_flag=="CD", mobile$Hydrogen_Cyanide_ppbV, NA)

mobile$Hydrogen_Sulfide_ppbV <-ifelse(mobile$Hydrogen_Sulfide_flag==""| mobile$Hydrogen_Sulfide_flag=="BR" |                     mobile$Hydrogen_Sulfide_flag=="BR,LJ" | mobile$Hydrogen_Sulfide_flag=="BR,QT" |                  mobile$Hydrogen_Sulfide_flag=="MD" | mobile$Hydrogen_Sulfide_flag=="LJ"| mobile$Hydrogen_Sulfide_flag=="LJ,MD"| mobile$Hydrogen_Sulfide_flag=="QT"| mobile$Hydrogen_Sulfide_flag=="QT,MD", mobile$Hydrogen_Sulfide_ppbV, NA)

sum(!is.na(mobile$Benzene_ppbV))
sum(!is.na(mobile$Toluene_ppbV))
sum(!is.na(mobile$Xylene_ppbV))
sum(!is.na(mobile$Trimethylbenzene_ppbV))
sum(!is.na(mobile$Hydrogen_Sulfide_ppbV))
sum(!is.na(mobile$Hydrogen_Cyanide_ppbV))

mobile$Wind_Speed_mph <-ifelse(mobile$MetData_flag=="", mobile$Wind_Speed_mph, NA)
mobile$Wind_Direction_deg <-ifelse(mobile$MetData_flag=="", mobile$Wind_Direction_deg, NA)

mobile$day<-as.Date(mobile$date)
mobile$Benzene_ppbV<-ifelse(mobile$day=="2024-08-13"| mobile$day=="2024-08-14", NA, mobile$Benzene_ppbV )
mobile$Toluene_ppbV<-ifelse(mobile$day=="2024-08-13"| mobile$day=="2024-08-14", NA, mobile$Toluene_ppbV )
mobile$Trimethylbenzene_ppbV<-ifelse(mobile$day=="2024-08-13"|mobile$day=="2024-08-14", NA, mobile$Trimethylbenzene_ppbV )
mobile$Xylene_ppbV<-ifelse(mobile$day=="2024-08-13"|mobile$day=="2024-08-14", NA, mobile$Xylene_ppbV)
mobile$Hydrogen_Cyanide_ppbV<-ifelse(mobile$day=="2025-01-02"|mobile$day=="2025-01-03", NA, mobile$Hydrogen_Cyanide_ppbV)
mobile$Hydrogen_Cyanide_ppbV<-ifelse(mobile$date> "2025-01-22 00:00:00", mobile$Hydrogen_Cyanide_ppbV, NA)

mobile <- mobile %>%
  dplyr::mutate(date = as.Date(date)) %>%  # ensure Date format
  dplyr::mutate(
    across(
      c(Benzene_ppbV,
        Toluene_ppbV,
        Trimethylbenzene_ppbV,
        Xylene_ppbV,
        Hydrogen_Sulfide_ppbV,
        Hydrogen_Cyanide_ppbV),
      ~ ifelse(
          date >= as.Date("2023-04-16") &
          date <= as.Date("2023-09-20"),
          NA,
          .
        )
    )
  )

sum(!is.na(mobile$Benzene_ppbV))
sum(!is.na(mobile$Toluene_ppbV))
sum(!is.na(mobile$Xylene_ppbV))
sum(!is.na(mobile$Trimethylbenzene_ppbV))
sum(!is.na(mobile$Hydrogen_Sulfide_ppbV))
sum(!is.na(mobile$Hydrogen_Cyanide_ppbV))

summary_stats <- mobile %>%
  summarise(
    total_nonNA = sum(!is.na(Trimethylbenzene_ppbV)),
    flagged_MD_BR = sum(
      !is.na(Trimethylbenzene_ppbV) &
      !is.na(Trimethylbenzene_flag) &
      str_detect(Trimethylbenzene_flag, "MD|BR")
    ),
    percent_flagged = 100 * flagged_MD_BR / total_nonNA
  )
summary_stats

#H2S: likely waste water
mobile<-subset(mobile, select=c("Asset..CAT.EMU.", "Site", "Local_Time_MST", "Latitude", "Longitude", "Wind_Speed_mph", "Wind_Direction_deg", "Relative_Humidity_percent", "Pressure_mb", "Temperature_F", "Hydrogen_Cyanide_ppbV", "Hydrogen_Sulfide_ppbV", "Benzene_ppbV", "Toluene_ppbV", "Trimethylbenzene_ppbV", "Xylene_ppbV"))
colnames(mobile)<-c("Asset", "Site", "date", "Latitude", "Longitude", "ws", "wd", "Relative_Humidity_percent", "Pressure_mb", "Temperature_F", "Hydrogen_Cyanide_ppb", "Hydrogen_Sulfide_ppb", "Benzene_ppb", "Toluene_ppb", "Trimethylbenzene_ppb", "Xylene_ppb")
mobile$date<-substr(mobile$date, 1, 19)
mobile$date<- lubridate::ymd_hms(mobile$date)
df<-mobile
rm(mobile)

rm(a_time, summary_stats)


##Time Delay Sampling
# Delay depends on Asset -> CAT: BTEX 4 s, HCN 6 s, H2S 21 s | EMU: BTEX 5 s, HCN 3 s, H2S 17 s

library(dplyr)
library(lubridate)
library(zoo)

# ============================================================
# Build 1-second met panel (per Asset-Site-day) + short-gap interpolation,
# shift pollutant timestamps by instrument delays,
# aggregate pollutants to 1-second means, then join.
#
# KEY CHANGE:
#   When multiple rows fall in the same second for the same AssetSiteDay,
#   keep the LATEST original row within that second (based on original timestamp).
# ============================================================

# ----------------------------
# 0) BASIC DIAGNOSTICS (input)
# ----------------------------
message("Initial df rows: ", nrow(df))
message("Initial df unique Asset: ", dplyr::n_distinct(df$Asset))
message("Initial df unique Site:  ", dplyr::n_distinct(df$Site))
message("Initial df unique Asset+Site: ", df %>% dplyr::distinct(Asset, Site) %>% nrow())
message("Initial df time span: ", min(df$date, na.rm = TRUE), " to ", max(df$date, na.rm = TRUE))

# ----------------------------
# 1) MET: keep latest row per (AssetSiteDay, second), then interpolate
# ----------------------------
# df columns:
# 1 Asset, 2 Site, 3 date, 4:10 met vars, 11:16 pollutants

met <- df[, 1:10] %>%
  dplyr::mutate(
    day          = as.Date(date),
    AssetSiteDay = paste0(Asset, "_", Site, "_", day),
    date_raw     = date,                       # keep original timestamp
    date         = floor_date(date, "second")  # second-binned timestamp
  )

# DIAGNOSTIC: duplicates after flooring to seconds
dup_met_keys <- met %>% dplyr::count(AssetSiteDay, date) %>% dplyr::filter(n > 1)
message("Met duplicate (AssetSiteDay,second) groups BEFORE dedupe: ", nrow(dup_met_keys))
if (nrow(dup_met_keys) > 0) {
  message("Top met duplicate groups (up to 10):")
  print(head(dup_met_keys %>% arrange(desc(n)), 10))
}

# KEEP LATEST ROW within each second (by original date_raw)
met_u <- met %>%
  dplyr::arrange(AssetSiteDay, date, date_raw) %>%                  # ascending raw time
  dplyr::group_by(AssetSiteDay, date) %>%
  dplyr::slice_tail(n = 1) %>%                                      # last = latest
  dplyr::ungroup() %>%
  dplyr::select(-date_raw) %>%                                      # drop helper
  dplyr::arrange(AssetSiteDay, date)

# DIAGNOSTIC: duplicates AFTER dedupe (should be zero)
dup_met_keys2 <- met_u %>% dplyr::count(AssetSiteDay, date) %>% dplyr::filter(n > 1)
message("Met duplicate (AssetSiteDay,second) groups AFTER dedupe: ", nrow(dup_met_keys2))
rm(dup_met_keys, dup_met_keys2)

# Build 1-second grid + interpolate WITHIN each AssetSiteDay
asd <- unique(met_u$AssetSiteDay)
met1_list <- vector("list", length(asd))

for (i in seq_along(asd)) {
  key <- asd[i]
  temp <- met_u %>%
    dplyr::filter(AssetSiteDay == key) %>%
    dplyr::arrange(date)

  if (nrow(temp) == 0) next

  full_time <- tibble(date = seq(min(temp$date), max(temp$date), by = "1 sec"))

  temp1 <- full_time %>%
    left_join(temp, by = "date") %>%
    dplyr::mutate(
      Latitude  = if (all(is.na(Latitude)))  Latitude  else na.approx(Latitude,  x = date, maxgap = 3, na.rm = FALSE),
      Longitude = if (all(is.na(Longitude))) Longitude else na.approx(Longitude, x = date, maxgap = 3, na.rm = FALSE),

      ws = if (all(is.na(ws))) ws else na.approx(ws, x = date, maxgap = 3, na.rm = FALSE),
      wd = if (all(is.na(wd))) wd else na.approx(wd, x = date, maxgap = 3, na.rm = FALSE),

      Relative_Humidity_percent = if (all(is.na(Relative_Humidity_percent))) Relative_Humidity_percent
        else na.approx(Relative_Humidity_percent, x = date, maxgap = 3, na.rm = FALSE),

      Pressure_mb = if (all(is.na(Pressure_mb))) Pressure_mb
        else na.approx(Pressure_mb, x = date, maxgap = 3, na.rm = FALSE),

      Temperature_F = if (all(is.na(Temperature_F))) Temperature_F
        else na.approx(Temperature_F, x = date, maxgap = 3, na.rm = FALSE),

      Asset        = first(temp$Asset),
      Site         = first(temp$Site),
      day          = first(temp$day),
      AssetSiteDay = key
    )

  met1_list[[i]] <- temp1
  if (i %% 25 == 0) message("Processed ", i, " / ", length(asd))
}

met1 <- bind_rows(met1_list)

# Flag which 1-second rows were present in met_u (0) vs created by time-grid (1)
met1 <- met1 %>%
  dplyr::mutate(interpolated = if_else(
    paste0(AssetSiteDay, "_", date) %in% paste0(met_u$AssetSiteDay, "_", met_u$date),
    0L, 1L
  ))

message("met1 rows (1-second grid, all AssetSiteDays): ", nrow(met1))
message("met1 % interpolated: ", round(100 * mean(met1$interpolated == 1, na.rm = TRUE), 2), "%")

# -----------------------------------
# 2) POLLUTANTS: shift & aggregate 1s
# -----------------------------------
btex <- df[, c(1, 2, 3, 13:16)]
h2s  <- df[, c(1, 2, 3, 12)]
hcn  <- df[, c(1, 2, 3, 11)]

# Apply delays (pollutant concentrations occurred BEFORE recorded "date")
# Instrument sampling delay depends on the mobile lab (Asset):
#   CAT:  BTEX 4 s,  HCN 6 s, H2S 21 s
#   EMU:  BTEX 5 s,  HCN 3 s, H2S 17 s
# Build a per-row delay (in seconds) keyed on Asset, then shift timestamps back.
.asset_delay <- function(asset, pollutant) {
  a <- toupper(trimws(as.character(asset)))
  is_cat <- a == "CAT"                      # anything not "CAT" is treated as EMU
  switch(pollutant,
    btex = ifelse(is_cat, 4, 5),
    hcn  = ifelse(is_cat, 6, 3),
    h2s  = ifelse(is_cat, 21, 17)
  )
}

btex$date <- floor_date(btex$date - dseconds(.asset_delay(btex$Asset, "btex")), "second")
h2s$date  <- floor_date(h2s$date  - dseconds(.asset_delay(h2s$Asset,  "h2s")),  "second")
hcn$date  <- floor_date(hcn$date  - dseconds(.asset_delay(hcn$Asset,  "hcn")),  "second")

# Aggregate pollutants to unique (Asset, Site, date) at 1-second resolution (mean within second)
btex_1s <- btex %>%
  dplyr::group_by(Asset, Site, date) %>%
  dplyr::summarise(
    Benzene_ppb          = mean(Benzene_ppb, na.rm = TRUE),
    Toluene_ppb          = mean(Toluene_ppb, na.rm = TRUE),
    Trimethylbenzene_ppb = mean(Trimethylbenzene_ppb, na.rm = TRUE),
    Xylene_ppb           = mean(Xylene_ppb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Benzene_ppb          = ifelse(is.nan(Benzene_ppb), NA_real_, Benzene_ppb),
    Toluene_ppb          = ifelse(is.nan(Toluene_ppb), NA_real_, Toluene_ppb),
    Trimethylbenzene_ppb = ifelse(is.nan(Trimethylbenzene_ppb), NA_real_, Trimethylbenzene_ppb),
    Xylene_ppb           = ifelse(is.nan(Xylene_ppb), NA_real_, Xylene_ppb)
  )

h2s_1s <- h2s %>%
  dplyr::group_by(Asset, Site, date) %>%
  dplyr::summarise(
    Hydrogen_Sulfide_ppb = mean(Hydrogen_Sulfide_ppb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(Hydrogen_Sulfide_ppb = ifelse(is.nan(Hydrogen_Sulfide_ppb), NA_real_, Hydrogen_Sulfide_ppb))

hcn_1s <- hcn %>%
  dplyr::group_by(Asset, Site, date) %>%
  dplyr::summarise(
    Hydrogen_Cyanide_ppb = mean(Hydrogen_Cyanide_ppb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(Hydrogen_Cyanide_ppb = ifelse(is.nan(Hydrogen_Cyanide_ppb), NA_real_, Hydrogen_Cyanide_ppb))

# Ensure met1 is unique at 1-second using (Asset,Site,date) (keep latest row already handled upstream)
met1_1s <- met1 %>%
  dplyr::arrange(Asset, Site, date) %>%
  dplyr::distinct(Asset, Site, date, .keep_all = TRUE)

# Restrict met1 to only (Asset, Site, date) seconds that appear in any pollutant table
keys <- bind_rows(
  btex_1s %>% dplyr::select(Asset, Site, date),
  h2s_1s  %>% dplyr::select(Asset, Site, date),
  hcn_1s  %>% dplyr::select(Asset, Site, date)
) %>% dplyr::distinct()

met1_1s <- met1_1s %>% semi_join(keys, by = c("Asset", "Site", "date"))

# ----------------------------
# 3) Join & final cleaning
# ----------------------------
df_out <- met1_1s %>%
  left_join(btex_1s, by = c("Asset", "Site", "date")) %>%
  left_join(h2s_1s,  by = c("Asset", "Site", "date")) %>%
  left_join(hcn_1s,  by = c("Asset", "Site", "date"))

message("df_out rows after joins: ", nrow(df_out), " (should equal met1_1s rows: ", nrow(met1_1s), ")")

# Keep rows with location
df_out <- df_out %>% dplyr::filter(!is.na(Latitude), !is.na(Longitude))
message("df_out rows after dropping missing lat/lon: ", nrow(df_out))

# ----------------------------
# 3b) NATIVE-CADENCE AVERAGING for the slow instruments
# ----------------------------
# The Vocus B (HCN, ~2 s) and Picarro G2204 (H2S/CH4, ~5 s) acquire
# more slowly than 1 s; in the CDPHE-delivered record their most
# recent reading is forward-filled to every second. AFTER the delay
# shift above, we average each slow species to its native acquisition
# interval within each Asset-Site-day, so all downstream maps,
# hotspots, and plume analyses use genuine (non-forward-filled)
# values. Aromatics (Vocus Eiger, 1 s) keep native 1-s resolution.
# For MULTI-POLLUTANT point-level comparisons, additionally average
# every involved species to the common 5 s (longest interval).
# Set NATIVE_CADENCE <- FALSE to reproduce the original 1-s record.
NATIVE_CADENCE <- TRUE
H2S_INTERVAL_S <- 5    # Picarro G2204 acquisition interval
HCN_INTERVAL_S <- 2    # Vocus B acquisition interval
if (NATIVE_CADENCE) {
  # RESTRICT-TO-MEASURED-SECONDS: assign the block mean back only to
  # seconds that already held a value; genuine gaps stay NA (this only
  # de-noises the delivered values to native cadence - it does not
  # gap-fill, so coverage and campaign medians are preserved).
  # We also keep the RAW delivered H2S (Hydrogen_Sulfide_ppb_raw) so
  # the plume-inversion branch can detect on the un-averaged signal;
  # pre-averaging flattens the plume rise/fall shape and collapses the
  # retained-plume set (maps/hotspots use the averaged column).
  .avg_native <- function(x) { m <- mean(x, na.rm = TRUE); if (is.nan(m)) NA_real_ else m }
  .n_h2s0 <- sum(!is.na(df_out$Hydrogen_Sulfide_ppb))
  .n_hcn0 <- sum(!is.na(df_out$Hydrogen_Cyanide_ppb))
  df_out <- df_out %>%
    dplyr::mutate(
      Hydrogen_Sulfide_ppb_raw = Hydrogen_Sulfide_ppb,   # keep delivered H2S for plumes
      Hydrogen_Cyanide_ppb_raw = Hydrogen_Cyanide_ppb,
      .epoch = as.numeric(date), .day = as.Date(date),
      .blk_h2s = floor(.epoch / H2S_INTERVAL_S),
      .blk_hcn = floor(.epoch / HCN_INTERVAL_S)) %>%
    dplyr::group_by(Asset, Site, .day, .blk_h2s) %>%
    dplyr::mutate(.mean_h2s = .avg_native(Hydrogen_Sulfide_ppb)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Asset, Site, .day, .blk_hcn) %>%
    dplyr::mutate(.mean_hcn = .avg_native(Hydrogen_Cyanide_ppb)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Hydrogen_Sulfide_ppb = dplyr::if_else(is.na(Hydrogen_Sulfide_ppb_raw), NA_real_, .mean_h2s),
      Hydrogen_Cyanide_ppb = dplyr::if_else(is.na(Hydrogen_Cyanide_ppb_raw), NA_real_, .mean_hcn)) %>%
    dplyr::select(-.epoch, -.day, -.blk_h2s, -.blk_hcn, -.mean_h2s, -.mean_hcn)
  message("Native-cadence averaging applied (within Asset-Site-day, measured seconds only): ",
          "H2S -> ", H2S_INTERVAL_S, " s, HCN -> ", HCN_INTERVAL_S, " s.")
  message("  H2S non-NA ", .n_h2s0, " -> ", sum(!is.na(df_out$Hydrogen_Sulfide_ppb)),
          " | HCN non-NA ", .n_hcn0, " -> ", sum(!is.na(df_out$Hydrogen_Cyanide_ppb)),
          "  (coverage preserved; raw H2S kept in Hydrogen_Sulfide_ppb_raw for plumes)")
}

# ----------------------------
# 4) Save outputs + counts
# ----------------------------
write.csv(df_out, "/Users/priyanka/Downloads/Suncor/mobile.csv", row.names = FALSE)
save(df_out, file = "/Users/priyanka/Downloads/Suncor/mobile.RData")
