# ==============================================================
# 17  Suncor + Terminal: Calculating scaling factors to calculate daily averages
# Auto-split from Suncor.Rmd  (section 17 of 40)
# ==============================================================

#Suncor + Terminal: Calculating scaling factors to calculate daily averages

# ============================================================
# COMPLETE CHUNK — Option 1 BIN-WEIGHTED La Casa scaling
# Includes: load mobile_corrected.RData -> DT, then bin-weighting,
# then (optionally) applies ratios to blk_sf if it exists.
# No timezone changes.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(lubridate)
})

# ----------------------------
# 0) Load MOBILE (corrected) points -> DT
# ----------------------------
load("/Users/priyanka/Downloads/Suncor/mobile_corrected.RData")
# your object name is out_sf in this file
out_merge <- out_sf
rm(out_sf)

stopifnot(exists("out_merge"))
DT <- as.data.table(out_merge)
stopifnot("date" %in% names(DT), inherits(DT$date, "POSIXt"))

# (Optional) your earlier HCN censoring rule
cut_dt <- as.POSIXct("2025-01-22 00:00:00", tz = attr(DT$date, "tzone"))
if ("HCN" %in% names(DT)) DT[, HCN := fifelse(date > cut_dt, HCN, NA_real_)]

# For weights (no tz changes)
DT[, `:=`(
  day        = as.Date(date),
  wday_num   = as.integer(strftime(date, "%u")),  # Mon=1 ... Sun=7
  hour_of_day = lubridate::hour(date)
)]

# ----------------------------
# 1) MOBILE weights over bins (weekday x hour)
# ----------------------------
mob_w <- DT[, .N, by = .(wday_num, hour_of_day)]
mob_w[, w := N / sum(N)]
setorder(mob_w, wday_num, hour_of_day)

wday_lab <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

message("Mobile total weight by weekday:")
mob_w_day <- mob_w[, .(w = sum(w), N = sum(N)), by = wday_num]
mob_w_day[, weekday := wday_lab[wday_num]]
setorder(mob_w_day, wday_num)
print(mob_w_day)

message("Mobile total weight by hour:")
mob_w_hour <- mob_w[, .(w = sum(w), N = sum(N)), by = hour_of_day]
setorder(mob_w_hour, hour_of_day)
print(mob_w_hour)

# ----------------------------
# 2) READ La Casa (inside this chunk)
# ----------------------------
lacasa1 <- read.csv("/Users/priyanka/Downloads/Suncor/ascent_2023.csv", stringsAsFactors = FALSE)
colnames(lacasa1) <- c("date_mst","date_mst1","date","date_mdt","benzene","toluene","xylene","wd","ws","temp_far","temp_c","rh")
lacasa1$date <- lubridate::dmy_hm(lacasa1$date)

lacasa2 <- read.csv("/Users/priyanka/Downloads/Suncor/ascent_2024.csv", stringsAsFactors = FALSE)
colnames(lacasa2) <- c("date_mst","date_mst1","date","date_mdt","benzene","toluene","xylene","wd","ws","temp_far","temp_c","rh")
lacasa2$date <- lubridate::dmy_hm(lacasa2$date)

lacasa3 <- read.csv("/Users/priyanka/Downloads/Suncor/lacasa3.csv", stringsAsFactors = FALSE)
colnames(lacasa3) <- c("date","toluene","xylene")
lacasa3$date <- lubridate::mdy_hm(lacasa3$date)

# ensure consistent columns for bind_rows
if (!("benzene" %in% names(lacasa3))) lacasa3$benzene <- NA_real_
for (nm in c("wd","ws","temp_far","temp_c","rh","date_mst","date_mst1","date_mdt")) {
  if (!nm %in% names(lacasa3)) lacasa3[[nm]] <- NA
}

lacasa <- dplyr::bind_rows(lacasa1, lacasa2, lacasa3) %>%
  dplyr::mutate(
    wday_num    = as.integer(strftime(date, "%u")),  # Mon=1 ... Sun=7
    hour_of_day = lubridate::hour(date)
  )

message("La Casa rows total: ", nrow(lacasa))
message("La Casa date span: ", min(lacasa$date, na.rm = TRUE), " to ", max(lacasa$date, na.rm = TRUE))

# ----------------------------
# 3) Restrict La Casa to MOBILE bins, compute BIN MEANS, then weight
# ----------------------------
lc_dt <- as.data.table(lacasa)

setkey(mob_w, wday_num, hour_of_day)
setkey(lc_dt, wday_num, hour_of_day)

# keep only bins present in MOBILE
lc_dt2 <- lc_dt[mob_w, on = .(wday_num, hour_of_day), nomatch = 0L]
message("La Casa rows after restricting to mobile bins: ", nrow(lc_dt2))

# compute La Casa mean within each bin (each bin contributes once)
lc_bin <- lc_dt2[, .(
  benzene_bin = mean(benzene, na.rm = TRUE),
  toluene_bin = mean(toluene, na.rm = TRUE),
  xylene_bin  = mean(xylene,  na.rm = TRUE),
  n_lacasa    = .N
), by = .(wday_num, hour_of_day)]

# join to mobile weights (ensures bins align)
setkey(lc_bin, wday_num, hour_of_day)
lc_bin <- lc_bin[mob_w, on = .(wday_num, hour_of_day)]

# diagnostics: weight coverage by non-missing bin means
cov_pct <- function(v, w) {
  ok <- is.finite(v)
  if (!any(ok)) return(0)
  100 * sum(w[ok]) / sum(w)
}
message("Mobile weight coverage by non-missing La Casa BIN means: ",
        "benzene=", round(cov_pct(lc_bin$benzene_bin, lc_bin$w), 2), "%, ",
        "toluene=", round(cov_pct(lc_bin$toluene_bin, lc_bin$w), 2), "%, ",
        "xylene=",  round(cov_pct(lc_bin$xylene_bin,  lc_bin$w), 2), "%")

message("Top mobile bins (by weight) with La Casa bin means:")
tmp_top <- copy(lc_bin)
tmp_top[, weekday := wday_lab[wday_num]]
setorder(tmp_top, -w)
print(tmp_top[1:20, .(weekday, hour_of_day, w, N, n_lacasa, benzene_bin, toluene_bin, xylene_bin)])

# weighted mean helper
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# overall 24/7 means (La Casa)
lc_overall <- c(
  benzene = mean(lacasa$benzene, na.rm = TRUE),
  toluene = mean(lacasa$toluene, na.rm = TRUE),
  xylene  = mean(lacasa$xylene,  na.rm = TRUE)
)

# mobile-like means (La Casa bin means weighted by MOBILE bin weights)
lc_mobilelike <- c(
  benzene = wmean(lc_bin$benzene_bin, lc_bin$w),
  toluene = wmean(lc_bin$toluene_bin, lc_bin$w),
  xylene  = wmean(lc_bin$xylene_bin,  lc_bin$w)
)

scale_factors <- data.table(
  pollutant = names(lc_overall),
  lc_mean_all = unname(lc_overall),
  lc_mean_mobilelike = unname(lc_mobilelike)
)
scale_factors[, ratio_all_over_mobilelike := lc_mean_all / lc_mean_mobilelike]

message("=== Option 1 BIN-WEIGHTED scaling factors (La Casa overall / mobile-like) ===")
print(scale_factors)

save(scale_factors,
     file = "/Users/priyanka/Downloads/Suncor/lacasa_scaling_factors_option1_binweighted.RData")

# ============================================================
# 4) PLOTS — how bins and scaling factors were developed
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(tidyr)
  library(forcats)
  library(gridExtra)
})

# ----------------------------
# A) Prepare plotting data
# ----------------------------
plot_bins <- copy(lc_bin)
plot_bins[, weekday := factor(wday_lab[wday_num], levels = wday_lab)]

# full weekday x hour grid so empty bins appear explicitly
full_grid <- CJ(
  wday_num = 1:7,
  hour_of_day = 0:23
)
full_grid[, weekday := factor(wday_lab[wday_num], levels = wday_lab)]

plot_bins_full <- merge(
  full_grid,
  plot_bins,
  by = c("wday_num", "hour_of_day", "weekday"),
  all.x = TRUE,
  sort = FALSE
)

# long format for La Casa bin means
plot_bins_long <- melt(
  plot_bins_full,
  id.vars = c("wday_num", "hour_of_day", "weekday", "w", "N", "n_lacasa"),
  measure.vars = c("benzene_bin", "toluene_bin", "xylene_bin"),
  variable.name = "pollutant",
  value.name = "bin_mean"
)

plot_bins_long[, pollutant := factor(
  pollutant,
  levels = c("benzene_bin", "toluene_bin", "xylene_bin"),
  labels = c("Benzene", "Toluene", "Xylene")
)]

# scaling summary for plotting
sf_plot <- copy(scale_factors)
sf_plot[, pollutant := factor(
  pollutant,
  levels = c("benzene", "toluene", "xylene"),
  labels = c("Benzene", "Toluene", "Xylene")
)]

sf_long <- melt(
  sf_plot,
  id.vars = "pollutant",
  measure.vars = c("lc_mean_all", "lc_mean_mobilelike"),
  variable.name = "mean_type",
  value.name = "value"
)

sf_long[, mean_type := factor(
  mean_type,
  levels = c("lc_mean_all", "lc_mean_mobilelike"),
  labels = c("La Casa 24/7 mean", "La Casa mobile-like weighted mean")
)]

# ----------------------------
# B) Plot 1: Mobile sampling weight heatmap
# ----------------------------
p_weight <- ggplot(plot_bins_full, aes(x = hour_of_day, y = weekday, fill = w)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey95",
    labels = percent_format(accuracy = 0.01)
  ) +
  labs(
    title = "Mobile monitoring sampling weights by weekday and hour",
    x = "Hour of day",
    y = NULL,
    fill = "Weight"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ----------------------------
# C) Plot 2: Number of mobile observations per bin
# ----------------------------
p_count <- ggplot(plot_bins_full, aes(x = hour_of_day, y = weekday, fill = N)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_fill_viridis_c(option = "B", na.value = "grey95") +
  labs(
    title = "Mobile observations per weekday-hour bin",
    x = "Hour of day",
    y = NULL,
    fill = "N"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ----------------------------
# D) Plot 3: La Casa coverage in mobile bins
# ----------------------------
plot_bins_full[, lacasa_present := !is.na(n_lacasa) & n_lacasa > 0]

p_cov <- ggplot(plot_bins_full, aes(x = hour_of_day, y = weekday, fill = lacasa_present)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_fill_manual(values = c("FALSE" = "grey90", "TRUE" = "#1b9e77")) +
  labs(
    title = "La Casa coverage for bins used by mobile monitoring",
    x = "Hour of day",
    y = NULL,
    fill = "La Casa data"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ----------------------------
# E) Plot 4: Distribution of mobile bin weights
# ----------------------------
p_wdist <- ggplot(mob_w, aes(x = w)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white") +
  scale_x_continuous(labels = percent_format(accuracy = 0.01)) +
  labs(
    title = "Distribution of mobile bin weights",
    x = "Bin weight",
    y = "Number of bins"
  ) +
  theme_minimal(base_size = 12)

# ----------------------------
# F) Plot 5: La Casa bin means across weekday-hour bins
# ----------------------------
p_means <- ggplot(plot_bins_long, aes(x = hour_of_day, y = weekday, fill = bin_mean)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_fill_viridis_c(na.value = "grey90") +
  facet_wrap(~ pollutant, ncol = 1, scales = "free") +
  labs(
    title = "La Casa mean concentration within mobile-used bins",
    x = "Hour of day",
    y = NULL,
    fill = "Bin mean"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ----------------------------
# G) Plot 6: Compare overall vs mobile-like weighted means
# ----------------------------
p_scale <- ggplot(sf_long, aes(x = pollutant, y = value, fill = mean_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    data = sf_plot,
    aes(
      x = pollutant,
      y = pmax(lc_mean_all, lc_mean_mobilelike) * 1.05,
      label = paste0("Ratio = ", round(ratio_all_over_mobilelike, 2))
    ),
    inherit.aes = FALSE,
    size = 3.8
  ) +
  labs(
    title = "Scaling factors: overall vs mobile-like weighted means",
    x = NULL,
    y = "Mean concentration",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ----------------------------
# H) Print plots
# ----------------------------
print(p_weight)
print(p_count)
print(p_cov)
print(p_wdist)
print(p_means)
print(p_scale)

# ----------------------------
# I) Optional: save plots
# ----------------------------
ggsave(
  "/Users/priyanka/Downloads/Suncor/mobile_weights_heatmap.png",
  p_weight, width = 10, height = 4.5, dpi = 300
)

ggsave(
  "/Users/priyanka/Downloads/Suncor/mobile_bin_counts_heatmap.png",
  p_count, width = 10, height = 4.5, dpi = 300
)

ggsave(
  "/Users/priyanka/Downloads/Suncor/lacasa_bin_coverage.png",
  p_cov, width = 10, height = 4.5, dpi = 300
)

ggsave(
  "/Users/priyanka/Downloads/Suncor/mobile_weight_distribution.png",
  p_wdist, width = 7, height = 4.5, dpi = 300
)

ggsave(
  "/Users/priyanka/Downloads/Suncor/lacasa_bin_means_by_pollutant.png",
  p_means, width = 10, height = 11, dpi = 300
)

ggsave(
  "/Users/priyanka/Downloads/Suncor/scaling_factor_comparison.png",
  p_scale, width = 8, height = 5, dpi = 300
)

# ============================================================
# COMBINED PUBLICATION FIGURE
# ============================================================

library(patchwork)

# Option: use only ONE pollutant for cleaner main figure
p_means_single <- plot_bins_long[pollutant == "Toluene"] %>%
  ggplot(aes(x = hour_of_day, y = weekday, fill = bin_mean)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(na.value = "grey90") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "B. Diurnal pattern (La Casa, Toluene)",
    x = "Hour of day",
    y = NULL,
    fill = "Conc."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

p_weight_labeled <- p_weight +
  labs(title = "A. Mobile sampling distribution")

p_wdist_labeled <- p_wdist +
  labs(title = "C. Distribution of bin weights")

p_scale_labeled <- p_scale +
  labs(title = "D. Scaling factors")

combined_plot <- (p_weight_labeled | p_means_single) /
                 (p_wdist_labeled | p_scale_labeled)

ggsave(
  "/Users/priyanka/Downloads/Suncor/FINAL_scaling_figure.png",
  combined_plot,
  width = 12,
  height = 8,
  dpi = 300
)

combined_plot
