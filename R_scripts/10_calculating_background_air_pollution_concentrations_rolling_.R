# ==============================================================
# 10  Calculating background air pollution concentrations (rolling lowest 20th percentile for 20 min intervals), identify plumes (Peter de Carlo's methods)
# Auto-split from Suncor.Rmd  (section 10 of 40)
# ==============================================================

#Calculating background air pollution concentrations (rolling lowest 20th percentile for 20 min intervals), identify plumes (Peter de Carlo's methods)

library(slider)
library(dplyr)

load("/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")
df <- out
rm(out)

# --- background setup
df$datetime <- df$date

df <- df %>%
  dplyr::rename(
    Benzene          = Benzene_ppb,
    Toluene          = Toluene_ppb,
    Trimethylbenzene = Trimethylbenzene_ppb,
    Xylene           = Xylene_ppb,
    H2S              = Hydrogen_Sulfide_ppb,
    HCN              = Hydrogen_Cyanide_ppb
  )

# Raw (un-averaged) H2S for the PLUME branch: the plume inversion detects on
# the delivered signal, since native-cadence averaging (script 03) flattens the
# plume rise/fall shape. We compute a parallel baseline_H2S_raw / plume_H2S_raw
# from the delivered H2S so P04-P06 carry them through to the plume detector.
if ("Hydrogen_Sulfide_ppb_raw" %in% names(df)) df$H2S_raw <- df$Hydrogen_Sulfide_ppb_raw

# List the pollutants you want to process (only existing columns will be used)
pollutants <- c("Benzene", "Toluene", "Trimethylbenzene", "Xylene", "H2S", "HCN", "H2S_raw")
cols <- intersect(pollutants, names(df))
stopifnot(length(cols) > 0, inherits(df$datetime, "POSIXt"))

# Sort + define day-group (no timezone manipulation)
df <- df[order(df$datetime), ]
df$day <- paste0(as.Date(df$datetime), df$Site)

# Window size = ±10 min around each time point
before_sec <- 600
after_sec  <- 600

# Require at least 30 non-missing points in the window
min_n <- 30

# Helpers (min_n enforced)
q20 <- function(v, min_n = 30) {
  vv <- v[is.finite(v)]
  if (length(vv) < min_n) return(NA_real_)
  unname(stats::quantile(vv, 0.20, names = FALSE))
}

sd_safe <- function(v, min_n = 30) {
  vv <- v[is.finite(v)]
  if (length(vv) < min_n) return(NA_real_)
  stats::sd(vv)
}

# Preallocate output columns
for (nm in cols) {
  df[[paste0("baseline_", nm)]] <- NA_real_
  df[[paste0("sd_", nm)]]       <- NA_real_
  df[[paste0("plume_", nm)]]    <- FALSE  # plume is TRUE/FALSE only
}

# Compute per day
split_idx <- split(seq_len(nrow(df)), df$day)

for (idx in split_idx) {
  i_num <- as.numeric(df$datetime[idx])  # numeric seconds index

  for (nm in cols) {
    x <- as.numeric(df[[nm]][idx])

    b <- slide_index_dbl(
      .x = x, .i = i_num,
      .before = before_sec, .after = after_sec,
      .complete = FALSE, # partial windows allowed
      .f = function(v) q20(v, min_n = min_n)
    )

    s <- slide_index_dbl(
      .x = x, .i = i_num,
      .before = before_sec, .after = after_sec,
      .complete = FALSE, # partial windows allowed
      .f = function(v) sd_safe(v, min_n = min_n)
    )

    df[[paste0("baseline_", nm)]][idx] <- b
    df[[paste0("sd_", nm)]][idx]       <- s

    # plume: TRUE/FALSE only; FALSE whenever baseline/sd are NA or x is NA
    df[[paste0("plume_", nm)]][idx] <- !is.na(x) & !is.na(b) & !is.na(s) & (x >= (b + 3 * s))
  }
}

save(df, file = "/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge_rolling.RData")
