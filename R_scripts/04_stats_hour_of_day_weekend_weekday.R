# ==============================================================
# 04  Stats (hour of day, weekend/weekday)
# Auto-split from Suncor.Rmd  (section 4 of 40)
# ==============================================================

#Stats (hour of day, weekend/weekday)

library(dplyr)
library(lubridate)
require(tidyverse)
load("/Users/priyanka/Downloads/Suncor/mobile.RData")

c(
  Benzene_ppb             = sum(!is.na(df_out$Benzene_ppb)),
  Toluene_ppb             = sum(!is.na(df_out$Toluene_ppb)),
  Xylene_ppb              = sum(!is.na(df_out$Xylene_ppb)),
  Trimethylbenzene_ppb    = sum(!is.na(df_out$Trimethylbenzene_ppb)),
  Hydrogen_Sulfide_ppb    = sum(!is.na(df_out$Hydrogen_Sulfide_ppb)),
  Hydrogen_Cyanide_ppb    = sum(!is.na(df_out$Hydrogen_Cyanide_ppb))
)

library(dplyr)

# Pollutant columns
pollutants <- c(
  "Benzene_ppb",
  "Toluene_ppb",
  "Xylene_ppb",
  "Trimethylbenzene_ppb",
  "Hydrogen_Sulfide_ppb",
  "Hydrogen_Cyanide_ppb"
)

summary_table_clean <- df_out %>%
  dplyr::select(all_of(pollutants)) %>%
  pivot_longer(cols = everything(),
               names_to = "Pollutant",
               values_to = "Value") %>%
  dplyr::group_by(Pollutant) %>%
  dplyr::summarise(
    N      = sum(!is.na(Value)),
    Min    = min(Value, na.rm = TRUE),
    P5     = quantile(Value, 0.05, na.rm = TRUE),
    P25    = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    P75    = quantile(Value, 0.75, na.rm = TRUE),
    P95    = quantile(Value, 0.95, na.rm = TRUE),
    P99    = quantile(Value, 0.99, na.rm = TRUE),
    Max    = max(Value, na.rm = TRUE),
    Mean   = mean(Value, na.rm = TRUE),
    SD     = sd(Value, na.rm = TRUE),
    .groups = "drop"
  )
# --- make sure df_out$date is POSIXct
# df_out$date <- ymd_hms(df_out$date, tz = "America/Denver")  # only if needed

# Use only rows with a timestamp
d0 <- df_out %>% dplyr::filter(!is.na(date))

# ----------------------------
# 1) How many unique run-days by weekday
#    ("Mobile sampling runs took place across X Mondays, ...")
# ----------------------------
run_days <- d0 %>%
  dplyr::mutate(day = as.Date(date),
         wday = wday(date, label = TRUE, abbr = FALSE, week_start = 1)) %>% # Monday=1
  dplyr::distinct(day, wday)

weekday_counts <- run_days %>%
  dplyr::count(wday, name = "n_days") %>%
  # ensure Mon..Sun order
  dplyr::mutate(wday = factor(wday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  dplyr::arrange(wday)

weekday_counts

# handy named vector if you want to paste into text
weekday_counts_vec <- setNames(weekday_counts$n_days, as.character(weekday_counts$wday))
weekday_counts_vec


# ----------------------------
# 2) Typical sampling window (A am – B pm) covering 88% of measurements
#    -> use the 6th and 94th percentiles of time-of-day
# ----------------------------
mins_midnight <- d0 %>%
  dplyr::mutate(mins = hour(date) * 60 + minute(date) + second(date) / 60) %>%
  dplyr::pull(mins)

A_mins <- as.numeric(quantile(mins_midnight, 0.06, na.rm = TRUE))
B_mins <- as.numeric(quantile(mins_midnight, 0.94, na.rm = TRUE))

# convert minutes since midnight -> "h:mm am/pm"
fmt_time <- function(m) {
  h24 <- floor(m / 60)
  mi  <- round(m %% 60)
  # handle rounding to 60
  if (mi == 60) { mi <- 0; h24 <- (h24 + 1) %% 24 }
  ampm <- ifelse(h24 < 12, "am", "pm")
  h12  <- h24 %% 12
  if (h12 == 0) h12 <- 12
  sprintf("%d:%02d %s", h12, mi, ampm)
}

A_txt <- fmt_time(A_mins)
B_txt <- fmt_time(B_mins)

message("Typical window covering 88% of measurements: ", A_txt, " – ", B_txt)


# ----------------------------
# 3) Fraction of overall measurements in each hour bin:
#    6–7, 7–8, …, 16–17 (4–5 pm)
# ----------------------------
hour_bins <- tibble(
  bin = c("6–7 am","7–8 am","8–9 am","9–10 am","10–11 am","11 am–noon",
          "noon–1 pm","1–2 pm","2–3 pm","3–4 pm","4–5 pm"),
  start_h = 6:16,
  end_h   = 7:17
)

hour_frac <- d0 %>%
  dplyr::mutate(h = hour(date)) %>%
  dplyr::count(h, name = "n") %>%
  right_join(tibble(h = 6:16), by = "h") %>%
  # BUGFIX (2026-08-20): `bin = hour_bins$bin` assumed the rows were still in
  # order 6..16, but right_join appends unmatched y rows at the END. An hour
  # with zero observations therefore landed last and every label after it was
  # shifted by one. Sort, then join the label on the hour rather than by
  # position, so the pairing cannot silently drift.
  dplyr::arrange(h) %>%
  dplyr::mutate(n = ifelse(is.na(n), 0L, n),
         frac = n / nrow(d0)) %>%
  dplyr::left_join(dplyr::transmute(hour_bins, h = start_h, bin), by = "h") %>%
  dplyr::select(bin, n, frac)

hour_frac

# If you want the fractions as a single vector in the order you listed:
fractions_vec <- hour_frac$frac
fractions_vec

# Optional: print as percentages nicely
hour_frac %>%
  dplyr::mutate(percent = 100 * frac) %>%
  dplyr::select(bin, percent)
