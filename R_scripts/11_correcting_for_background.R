# ==============================================================
# 11  Correcting for background
# Auto-split from Suncor.Rmd  (section 11 of 40)
# ==============================================================

#Correcting for background

load("/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge_rolling.RData")

library(dplyr)

df <- df %>%
  dplyr::mutate(day = as.Date(date))

# KEY FIX (2026-08-20): matched to script 10 - the daily median background
# must be per-vehicle for the same reason the rolling window is. Asset is
# carried through script 06's select(), so it is available here.
stopifnot("Asset" %in% names(df))

# --- compute daily/site/asset median of baseline_* (NA-safe, finite-safe)
medianpollutants <- df %>%
  dplyr::group_by(day, Site, Asset) %>%
  dplyr::summarise(
    median_bgBenzene          = median(baseline_Benzene[is.finite(baseline_Benzene)], na.rm = TRUE),
    median_bgToluene          = median(baseline_Toluene[is.finite(baseline_Toluene)], na.rm = TRUE),
    median_bgTrimethylbenzene = median(baseline_Trimethylbenzene[is.finite(baseline_Trimethylbenzene)], na.rm = TRUE),
    median_bgXylene           = median(baseline_Xylene[is.finite(baseline_Xylene)], na.rm = TRUE),
    median_bgH2S              = median(baseline_H2S[is.finite(baseline_H2S)], na.rm = TRUE),
    median_bgHCN              = median(baseline_HCN[is.finite(baseline_HCN)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # median(numeric(0)) becomes Inf/-Inf with na.rm sometimes; coerce non-finite to NA
  dplyr::mutate(across(starts_with("median_bg"), ~ ifelse(is.finite(.x), .x, NA_real_)))

df <- df %>%
  left_join(medianpollutants, by = c("day", "Site", "Asset"))

rm(medianpollutants); gc()

# --- helper: your correction rule, with NA + zero guards
# If baseline <= obs: obs - baseline + median_bg
# Else: obs * median_bg / baseline
# Returns NA if any needed piece missing; avoids division by 0
bg_correct <- function(obs, base, med) {
  # BUGFIX (2026-08-20): the multiplicative branch guarded only `base != 0`,
  # not `base <= 0`. This pipeline deliberately RETAINS negative readings, and
  # they are common in exactly the two species whose MDLs sit far above ambient:
  # 34.2% of retained H2S values and 17.3% of benzene are negative (counted
  # from the 58 monthly CDPHE CSVs; toluene, xylene, TMB and HCN are delivered
  # non-negative). The lowest-20th-percentile baseline of a 20-minute window is
  # therefore frequently <= 0 for H2S, and then:
  #   base <  0 and base > obs  ->  obs * med / base FLIPS THE SIGN, so a
  #                                 -3 ppb reading is published as +1.5 ppb
  #   base == 0                 ->  the row falls through BOTH branches and is
  #                                 silently deleted as NA
  # Neither is intended. Equation 3 exists to avoid negative corrected values
  # when a POSITIVE background exceeds the observation; it is only meaningful
  # for a positive baseline. Fall back to the additive form (Equation 2)
  # whenever the baseline is not strictly positive - that is well defined,
  # order-preserving, and leaves a genuinely negative observation negative,
  # which is what SI Section S4.1.1 says happens.
  out <- rep(NA_real_, length(obs))
  ok  <- is.finite(obs) & is.finite(base) & is.finite(med)

  # Equation 2 (additive): baseline at or below the observation, OR any
  # baseline that is not strictly positive.
  ok_add <- ok & ((base <= obs) | (base <= 0))
  out[ok_add] <- obs[ok_add] - base[ok_add] + med[ok_add]

  # Equation 3 (multiplicative): only when a STRICTLY POSITIVE baseline
  # exceeds the observation.
  ok_ratio <- ok & (base > obs) & (base > 0)
  out[ok_ratio] <- obs[ok_ratio] * med[ok_ratio] / base[ok_ratio]

  .n_np <- sum(ok & (base <= 0) & (base > obs))
  if (.n_np > 0) {
    message(sprintf("[BG] %s rows had a non-positive baseline above the observation; Eq.2 used instead of Eq.3 (previously a sign flip or a silent NA)",
                    format(.n_np, big.mark = ",")))
  }
  out
}

df$sBenzene          <- bg_correct(df$Benzene,          df$baseline_Benzene,          df$median_bgBenzene)
df$sToluene          <- bg_correct(df$Toluene,          df$baseline_Toluene,          df$median_bgToluene)
df$sTrimethylbenzene <- bg_correct(df$Trimethylbenzene, df$baseline_Trimethylbenzene, df$median_bgTrimethylbenzene)
df$sXylene           <- bg_correct(df$Xylene,           df$baseline_Xylene,           df$median_bgXylene)
df$sH2S              <- bg_correct(df$H2S,              df$baseline_H2S,              df$median_bgH2S)
df$sHCN              <- bg_correct(df$HCN,              df$baseline_HCN,              df$median_bgHCN)

# --- Checking difference between measured and corrected pollutants (finite-safe)
rel_diff <- function(obs, corr) {
  out <- rep(NA_real_, length(obs))
  ok <- is.finite(obs) & obs > 0 & is.finite(corr)
  out[ok] <- (obs[ok] - corr[ok]) / obs[ok]
  out
}

b1 <- rel_diff(df$Benzene,          df$sBenzene);          print(summary(b1))
b2 <- rel_diff(df$Toluene,          df$sToluene);          print(summary(b2))
b3 <- rel_diff(df$Trimethylbenzene, df$sTrimethylbenzene); print(summary(b3))
b4 <- rel_diff(df$Xylene,           df$sXylene);           print(summary(b4))
b5 <- rel_diff(df$H2S,              df$sH2S);              print(summary(b5))
b6 <- rel_diff(df$HCN,              df$sHCN);              print(summary(b6))

rm(b1, b2, b3, b4, b5, b6); gc()

save(df, file = "/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData")
