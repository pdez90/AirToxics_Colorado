# ==============================================================
# 43  FIGURE S3.1 (reproduced from the delay-corrected raw data)
# Two-panel map of the mobile monitoring routes with, per route:
#   - number of runs (sampling days)
#   - median run duration (hours)
#   - average driving speed (km/h), computed from consecutive GPS
#     fixes (gaps <= 5 s, implausible speeds > 130 km/h dropped)
# Outputs:
#   FinalFig/FigureS3.1_routes.png
#   figureS31_runs_summary.csv  (per-run duration/distance/speed)
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggspatial); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out)
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
stopifnot("date" %in% names(df))
message("  rows: ", format(nrow(df), big.mark = ","),
        " | sites: ", paste(unique(df$Site), collapse = " / "))

setorder(df, Site, date)
df[, day := as.Date(date)]

# ---- per-run duration / distance / speed ----------------------
Rk <- 6371.0088
hav_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 +
       cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * Rk * asin(pmin(1, sqrt(a)))
}
df[, `:=`(lat1 = shift(Latitude), lon1 = shift(Longitude),
          t1 = shift(as.numeric(date))), by = .(Site, day)]
df[, dt_s := as.numeric(date) - t1]
df[, seg_km := hav_km(lat1, lon1, Latitude, Longitude)]
ok <- df[is.finite(seg_km) & is.finite(dt_s) & dt_s > 0 & dt_s <= 5]
ok[, seg_kmh := seg_km / (dt_s / 3600)]
n0 <- nrow(ok); ok <- ok[seg_kmh <= 130]
message("  GPS segments used: ", format(nrow(ok), big.mark = ","),
        " (dropped ", n0 - nrow(ok), " > 130 km/h glitches)")

runs <- ok[, .(dur_h = as.numeric(difftime(max(date), min(date), units = "hours")),
               km = sum(seg_km), move_h = sum(dt_s) / 3600), by = .(Site, day)]
runs[, speed_kmh := km / move_h]
message("  runs (site x day): ", nrow(runs))
stopifnot(nrow(runs) > 0, all(is.finite(runs$speed_kmh)))
fwrite(runs, file.path(BASE, "figureS31_runs_summary.csv"))

stats <- runs[, .(n_runs = .N,
                  med_dur_h = median(dur_h),
                  mean_speed_kmh = mean(speed_kmh)), by = Site]
print(stats)

# ---- map ------------------------------------------------------
df[, Route := ifelse(grepl("Suncor", Site),
                     "A) Suncor & Phillips 66 route",
                     "B) Sinclair Terminal route")]
stats[, Route := ifelse(grepl("Suncor", Site),
                        "A) Suncor & Phillips 66 route",
                        "B) Sinclair Terminal route")]
# thin points for plotting only (~150k per route)
pts <- df[, .SD[seq(1, .N, by = max(1L, .N %/% 150000)),
                .(Longitude, Latitude)], by = Route]
message("  plot points: ", nrow(pts))
stopifnot(all(pts$Longitude > -106 & pts$Longitude < -104),
          all(pts$Latitude > 39 & pts$Latitude < 41))

padx <- 0.012; pady <- 0.012
xlim <- unname(range(pts$Longitude)) + c(-padx, padx)
ylim <- unname(range(pts$Latitude)) + c(-pady, pady)
lab_df <- stats[, .(Route,
  x = xlim[1] + 0.004, y = ylim[2] - 0.004,
  lab = sprintf("Runs: %d\nMedian duration: %.1f h\nMean speed: %.0f km/h",
                n_runs, med_dur_h, mean_speed_kmh))]

p <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_point(data = pts, aes(Longitude, Latitude),
             size = 0.12, alpha = 0.25, color = "#2166ac") +
  geom_label(data = lab_df, aes(x = x, y = y, label = lab),
             hjust = 0, vjust = 1, size = 3.5, fontface = "bold",
             fill = "white", alpha = 0.92, label.size = 0.25) +
  facet_wrap(~Route, ncol = 2) +
  coord_sf(crs = 4326, default_crs = 4326, xlim = xlim, ylim = ylim,
           expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  labs(caption = "Basemap: CARTO Positron. Blue points are 1-s mobile measurement locations (thinned for display). Run duration and average driving speed computed from consecutive GPS fixes.",
       x = NULL, y = NULL) +
  theme_bw(base_size = 13) +
  theme(panel.border = element_rect(color = "black", linewidth = 0.8, fill = NA),
        strip.background = element_rect(fill = "grey93", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        plot.caption = element_text(size = 9, hjust = 0),
        panel.grid = element_blank())

out_png <- file.path(BASE, "FinalFig", "FigureS3.1_routes.png")
ggsave(out_png, p, width = 14, height = 5.4, dpi = 450, bg = "white")
message("[Saved] ", out_png)
