# ==============================================================
# 48  LA CASA SCALING-FACTOR SENSITIVITY (SI)
# Recomputes the temporal scaling factor under five constructions
# and propagates each to the benzene risk comparison. Because the
# scaling is a single multiplicative factor applied to block
# concentrations, and risk is linear in concentration, the risk
# range and the mobile:AirToxScreen ratio scale exactly with s.
# Constructions:
#   A binweighted  - baseline: La Casa 24/7 mean / mobile-bin-weighted
#                    La Casa mean (weekday x hour bins; script 17)
#   B window       - La Casa 24/7 mean / La Casa weekday 08-15h mean
#   C hour_only    - weights by hour of day only (ignore weekday)
#   D median       - medians in place of means (24/7 median /
#                    weighted median-of-bin-medians)
#   E none         - no scaling (s = 1)
# Outputs:
#   TABLE_scaling_sensitivity.csv
#   FinalFig/FIG_scaling_sensitivity.png
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(lubridate); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"

# canonical anchors (delay-corrected pipeline)

# SCALING FACTORS (2026-09-23): read them from the file R04 writes instead of
# hard-coding. The 300 m headquarters exclusion moved the factors from
# 1.149/1.228/1.377 to 1.165/1.274/1.443, and a hard-coded constant would have
# left this table on the old scaling while the block surface used the new one.
.sf_file <- file.path("/Users/priyanka/Downloads/Suncor", "lacasa_scaling_factors_option1_binweighted.RData")
.sf_get <- function(pol, fallback) {
  if (!file.exists(.sf_file)) { message("[SCALING] file absent - using documented value for ", pol); return(fallback) }
  e <- new.env(); load(.sf_file, envir = e); o <- get(ls(e)[1], envir = e)
  if (!all(c("pollutant", "ratio_all_over_mobilelike") %in% names(o))) return(fallback)
  r <- as.numeric(o[["ratio_all_over_mobilelike"]])[match(pol, o[["pollutant"]])]
  if (length(r) != 1L || !is.finite(r)) fallback else r
}
S_BASE_BENZ <- .sf_get("benzene", 1.149)   # baseline benzene factor, from R04
RISK_LO <- 0.113; RISK_HI <- 0.402   # mobile risk range at S_BASE_BENZ
ATS_LO <- 0.117; ATS_HI <- 0.416
RATIO_BASE <- 0.97

# ---- mobile bin weights ---------------------------------------
message("Loading mobile data for bin weights...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
DT <- as.data.table(out); rm(out); gc()
DT <- DT[Site != "Goodrich Corporation (Collins Aerospace)"]
DT[, `:=`(wday_num = as.integer(strftime(date, "%u")),
          hour_of_day = hour(date))]
mob_w <- DT[, .N, by = .(wday_num, hour_of_day)][, w := N / sum(N)][]
mob_h <- DT[, .N, by = hour_of_day][, w := N / sum(N)][]
message("  mobile bins: ", nrow(mob_w), " (weekday x hour), ",
        nrow(mob_h), " (hour only)")
rm(DT); gc()

# ---- La Casa (same three files as script 17) ------------------
# TIME CONVENTION (2026-09-22): the ascent files carry FOUR time columns —
# 1: MST clock, 2: MST as YYYYMMDDhhmmss, 3: MDT clock, 4: MDT as YYYYMMDDhhmmss.
# Column 3 was being used as `date`. In ascent_2024.csv column 3 is one hour
# ahead of column 1 (it really is MDT), while the mobile record carries the MST
# wall clock, so the 2024 La Casa deployment was being compared one hour out.
# (ascent_2023.csv was delivered with columns 1 and 3 identical, both MST, so it
# was never affected.) Checked against EPA AQS resultant wind speed at the three
# Denver-area stations within 15 km: hourly correlation peaks at lag 0 for
# column 1 in both years (r = 0.94 in 2023, 0.96 in 2024) and at -1 h for
# column 3 in 2024. La Casa is therefore read from column 1 (MST) below.
rd <- function(f, cn, parser) {
  x <- read.csv(file.path(BASE, f), stringsAsFactors = FALSE)
  colnames(x) <- cn
  x$date <- parser(x$date)
  if ("date_mst" %in% cn) {                 # ascent files: use the MST column
    .mst <- parser(x$date_mst)
    .off <- as.numeric(difftime(x$date, .mst, units = "hours"))
    stopifnot(all(is.na(.off) | .off %in% c(0, 1)))
    x$date <- .mst
  }
  x
}
cn12 <- c("date_mst","date_mst1","date","date_mdt","benzene","toluene",
          "xylene","wd","ws","temp_far","temp_c","rh")
lc1 <- rd("ascent_2023.csv", cn12, dmy_hm)
lc2 <- rd("ascent_2024.csv", cn12, dmy_hm)
lc3 <- rd("lacasa3.csv", c("date","toluene","xylene"), mdy_hm)
lc3$benzene <- NA_real_
lc <- rbindlist(list(lc1[, c("date","benzene","toluene","xylene")],
                     lc2[, c("date","benzene","toluene","xylene")],
                     lc3[, c("date","benzene","toluene","xylene")]),
                use.names = TRUE)
lc <- lc[!is.na(date)]
lc[, `:=`(wday_num = as.integer(strftime(date, "%u")),
          hour_of_day = hour(date))]
message("La Casa rows: ", nrow(lc), " | span ", min(lc$date), " - ", max(lc$date))

wmean <- function(x, w) { ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_); sum(x[ok] * w[ok]) / sum(w[ok]) }

factors <- list()
for (poll in c("benzene", "toluene", "xylene")) {
  v <- lc[[poll]]
  overall_mean <- mean(v, na.rm = TRUE)
  overall_med  <- median(v, na.rm = TRUE)
  # A binweighted (baseline construction)
  binm <- lc[, .(m = mean(get(poll), na.rm = TRUE),
                 md = median(get(poll), na.rm = TRUE)),
             by = .(wday_num, hour_of_day)]
  binm <- merge(binm, mob_w, by = c("wday_num", "hour_of_day"))
  sA <- overall_mean / wmean(binm$m, binm$w)
  # B simple weekday-daytime window
  win <- lc[wday_num <= 5 & hour_of_day >= 8 & hour_of_day <= 15]
  sB <- overall_mean / mean(win[[poll]], na.rm = TRUE)
  # C hour-only weights
  hm <- lc[, .(m = mean(get(poll), na.rm = TRUE)), by = hour_of_day]
  hm <- merge(hm, mob_h, by = "hour_of_day")
  sC <- overall_mean / wmean(hm$m, hm$w)
  # D median-based
  sD <- overall_med / wmean(binm$md, binm$w)
  factors[[poll]] <- data.table(
    pollutant = poll,
    A_binweighted = round(sA, 3), B_window = round(sB, 3),
    C_hour_only = round(sC, 3), D_median = round(sD, 3), E_none = 1)
  message(sprintf("%-8s A=%.3f  B=%.3f  C=%.3f  D=%.3f", poll, sA, sB, sC, sD))
}
factors <- rbindlist(factors)

# validate baseline reproduction
sA_benz <- factors[pollutant == "benzene", A_binweighted]
message("Baseline benzene factor reproduced: ", sA_benz,
        " (canonical ", S_BASE_BENZ, "; should agree within ~2%)")

# ---- propagate to benzene risk --------------------------------
risk <- factors[pollutant == "benzene",
                .(construction = c("A_binweighted", "B_window",
                                   "C_hour_only", "D_median", "E_none"),
                  s = c(A_binweighted, B_window, C_hour_only, D_median, 1))]
risk[, `:=`(
  risk_lo = round(RISK_LO * s / S_BASE_BENZ, 3),
  risk_hi = round(RISK_HI * s / S_BASE_BENZ, 3),
  ratio_vs_ATS = round(RATIO_BASE * s / S_BASE_BENZ, 2))]
out <- merge(factors, risk[construction == "A_binweighted",
                           .(pollutant = "benzene")], by = "pollutant",
             all.x = TRUE)  # cosmetic no-op keeps column order stable
fwrite(factors, file.path(BASE, "TABLE_scaling_sensitivity_factors.csv"))
fwrite(risk, file.path(BASE, "TABLE_scaling_sensitivity_risk.csv"))
print(factors); print(risk)

# ---- figure ---------------------------------------------------
lab <- c(A_binweighted = "A: bin-weighted\n(baseline)",
         B_window = "B: weekday\n08-15h window", C_hour_only = "C: hour-only\nweights",
         D_median = "D: median-\nbased", E_none = "E: no\nscaling")
risk[, clab := factor(lab[construction], levels = lab)]
p <- ggplot(risk, aes(clab, ratio_vs_ATS)) +
  geom_col(fill = "#4292c6", width = 0.6, color = "grey20", linewidth = 0.2) +
  geom_hline(yintercept = 1, linetype = 2, color = "red") +
  geom_text(aes(label = sprintf("s = %.2f\nrisk %.3f-%.3f", s, risk_lo, risk_hi)),
            vjust = -0.25, size = 3.1, lineheight = 0.95) +
  scale_y_continuous(limits = c(0, max(risk$ratio_vs_ATS) * 1.25)) +
  labs(x = NULL,
       y = "Aggregate mobile : AirToxScreen risk ratio",
       caption = "Red dashed line: parity with AirToxScreen (0.117-0.416 excess cases across 1,667 common blocks). Labels give the benzene scaling factor s and the resulting mobile risk range; risk scales exactly linearly with s.") +
  theme_bw(base_size = 12) +
  theme(plot.caption = element_text(size = 8.5, hjust = 0))
ggsave(file.path(BASE, "FinalFig", "FIG_scaling_sensitivity.png"),
       p, width = 8.5, height = 5.2, dpi = 400, bg = "white")
message("[Saved] FinalFig/FIG_scaling_sensitivity.png")
message("DONE.")
