# ==============================================================
# 49  LA CASA DAY/NIGHT AND WEEKDAY/WEEKEND RATIOS (SI)
# Quantifies how concentrations during the mobile sampling window
# (weekdays ~08:00-15:59) compare with the periods the campaign
# could not sample: nights and weekends. Uses the same La Casa
# files as the scaling analysis (ascent_2023/2024 + lacasa3).
# Definitions:
#   mobile window : weekday (Mon-Fri) 08:00-15:59
#   night         : 20:00-05:59 (all days)
#   weekday day   : Mon-Fri 06:00-19:59
#   weekend day   : Sat-Sun 06:00-19:59
# Outputs:
#   TABLE_lacasa_daynight_ratios.csv
#   FinalFig/FIG_lacasa_diurnal.png
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(lubridate); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
rd <- function(f, cn, parser) {
  x <- read.csv(file.path(BASE, f), stringsAsFactors = FALSE)
  colnames(x) <- cn
  x$date <- parser(x$date)
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
lc[, `:=`(hour = hour(date), wd_num = as.integer(strftime(date, "%u")))]
message("La Casa rows: ", format(nrow(lc), big.mark = ","),
        " | span ", min(lc$date), " - ", max(lc$date))
message("  non-missing: benzene ", sum(is.finite(lc$benzene)),
        " | toluene ", sum(is.finite(lc$toluene)),
        " | xylene ", sum(is.finite(lc$xylene)))

lc[, period := fcase(
  wd_num <= 5 & hour >= 8 & hour <= 15, "mobile_window",
  hour >= 20 | hour <= 5,               "night",
  wd_num <= 5,                          "weekday_other_day",
  default =                             "weekend_day")]
print(lc[, .N, by = period])

stats <- rbindlist(lapply(c("benzene", "toluene", "xylene"), function(poll) {
  v <- lc[[poll]]
  s <- lc[is.finite(v), .(mean = mean(get(poll)), median = median(get(poll)),
                          n = .N), by = period]
  s[, pollutant := poll]
  s
}))
wide_m <- dcast(stats, pollutant ~ period, value.var = "mean")
wide_md <- dcast(stats, pollutant ~ period, value.var = "median")
ratios <- wide_m[, .(
  pollutant,
  mean_mobile_window = round(mobile_window, 3),
  mean_night = round(night, 3),
  mean_weekend_day = round(weekend_day, 3),
  ratio_night_over_window_mean = round(night / mobile_window, 2),
  ratio_weekend_over_window_mean = round(weekend_day / mobile_window, 2))]
ratios_md <- wide_md[, .(
  pollutant,
  ratio_night_over_window_median = round(night / mobile_window, 2),
  ratio_weekend_over_window_median = round(weekend_day / mobile_window, 2))]
ratios <- merge(ratios, ratios_md, by = "pollutant")
fwrite(ratios, file.path(BASE, "TABLE_lacasa_daynight_ratios.csv"))
print(ratios)
message("Interpretation check: the bin-weighted scaling factors already fold ",
        "these differences into the 24-h scaling (S4.1/S4.3).")

# ---- diurnal figure -------------------------------------------
diur <- rbindlist(lapply(c("benzene", "toluene", "xylene"), function(poll) {
  v <- lc[[poll]]
  s <- lc[is.finite(v), .(mean = mean(get(poll)),
                          lo = quantile(get(poll), 0.25),
                          hi = quantile(get(poll), 0.75), n = .N),
          by = .(hour, weekend = wd_num >= 6)]
  s[, pollutant := poll]
  s
}))
diur[, daytype := ifelse(weekend, "Weekend", "Weekday")]
p <- ggplot(diur, aes(hour, mean, color = daytype, fill = daytype)) +
  annotate("rect", xmin = 8, xmax = 16, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey40") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~pollutant, scales = "free_y") +
  scale_color_manual(values = c(Weekday = "#2166ac", Weekend = "#b2182b"),
                     name = NULL) +
  scale_fill_manual(values = c(Weekday = "#2166ac", Weekend = "#b2182b"),
                    guide = "none") +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  labs(x = "Hour of day (local)", y = "Concentration (ppb)",
       caption = "Lines: hourly means at the La Casa stationary site; ribbons: interquartile range. Shaded band: the mobile campaign's weekday driving window (08:00-15:59).") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 8.5, hjust = 0))
ggsave(file.path(BASE, "FinalFig", "FIG_lacasa_diurnal.png"),
       p, width = 10, height = 4.4, dpi = 400, bg = "white")
message("[Saved] FinalFig/FIG_lacasa_diurnal.png")
message("DONE.")
