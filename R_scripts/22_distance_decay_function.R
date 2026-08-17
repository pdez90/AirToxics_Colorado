# ==============================================================
# 22  Distance decay function
# Auto-split from Suncor.Rmd  (section 22 of 40)
# ==============================================================

#Distance decay function

# Distance-to-road decay curves (Primary Roads + Ramps) — cleaned + reproducible
# - Takes your point data (df) with lon/lat + pollutants
# - Computes distance (meters) to nearest Primary Road / Ramp
# - Summarizes concentration vs distance using distance bins
# - Plots median with 10–90% ribbon by pollutant
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

# ----------------------------
# 0) Inputs
# ----------------------------
# expects:
#   df: data.frame with Longitude, Latitude, date, and pollutant columns
#   all_colorado_roads: sf lines with MTFCC codes
stopifnot(exists("df"), exists("all_colorado_roads"))

out_rds <- "/Users/priyanka/Downloads/Suncor/dist_road.RData"
out_fig <- "/Users/priyanka/Downloads/Suncor/FinalFig/road_distance_decay.png"

# road class labels
mtfcc_labs <- c(
  S1100 = "Primary Roads",
  S1200 = "Secondary Roads",
  S1400 = "Local Roads",
  S1500 = "Vehicular Trail",
  S1630 = "Ramps",
  S1640 = "Service Drives",
  S1710 = "Pedestrian Trail",
  S1720 = "Stairway",
  S1730 = "Alley",
  S1740 = "Private Road",
  S1820 = "Bike Path",
  S1830 = "Horse Trail"
)

# pollutants to include (only those present will be used)
pollutants <- c("Benzene", "Toluene", "Trimethylbenzene", "Xylene", "H2S", "HCN",
                "sBenzene", "sToluene", "sTrimethylbenzene", "sXylene", "sH2S", "sHCN")
pollutants <- intersect(pollutants, names(df))
stopifnot(length(pollutants) > 0)

# ----------------------------
# 1) Prep roads: keep only Primary Roads + Ramps
# ----------------------------
roads <- all_colorado_roads

stopifnot(inherits(roads, "sf"), "MTFCC" %in% names(roads))
roads <- roads %>%
  mutate(road_class = dplyr::recode(as.character(MTFCC), !!!mtfcc_labs, .default = as.character(MTFCC))) %>%
  filter(road_class %in% c("Primary Roads", "Ramps"))

# ----------------------------
# 2) Points -> sf and compute distance to nearest road (meters)
# ----------------------------
stopifnot(all(c("Longitude", "Latitude") %in% names(df)))

pts_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Project to meters for distance (UTM 13N is fine for CO Front Range)
pts_sf_m <- st_transform(pts_sf, 32613)
roads_m  <- st_transform(roads, 32613)

message("Computing distance to nearest Primary Road/Ramp (st_nearest_feature)...")
t0 <- Sys.time()

# st_nearest_feature uses a spatial tree index (same approach as script 21,
# 65 sec on this data) — replaces nngeo::st_nn, which brute-forces point-to-line
# distances and was on pace for 100+ hours over 2.6M points.
idx <- st_nearest_feature(pts_sf_m, roads_m)
pts_sf_m$dist_to_road_m <- as.numeric(
  st_distance(pts_sf_m, roads_m[idx, ], by_element = TRUE))

message("Done. Elapsed: ", round(difftime(Sys.time(), t0, units = "secs"), 1), " sec")
message(sprintf("  dist_to_road_m: median %.0f m | p95 %.0f m | max %.0f m | NA: %d",
                median(pts_sf_m$dist_to_road_m, na.rm = TRUE),
                quantile(pts_sf_m$dist_to_road_m, 0.95, na.rm = TRUE),
                max(pts_sf_m$dist_to_road_m, na.rm = TRUE),
                sum(is.na(pts_sf_m$dist_to_road_m))))

save(pts_sf_m, file = out_rds)

# ----------------------------
# 3) Long format + binned summaries
# ----------------------------
DT <- as.data.table(st_drop_geometry(pts_sf_m))

keep_cols <- c("Latitude", "Longitude", "date", "dist_to_road_m", pollutants)
keep_cols <- intersect(keep_cols, names(DT))
DT <- DT[, ..keep_cols]

L <- data.table::melt(   # qualified: reshape2::melt masks data.table's method
  DT,
  id.vars = c("Latitude", "Longitude", "date", "dist_to_road_m"),
  measure.vars = pollutants,
  variable.name = "Pollutant",
  value.name = "value"
)

# distance bins (choose 150–300; higher = smoother but slower)
nbins <- 200

summ <- L %>%
  filter(is.finite(dist_to_road_m), is.finite(value)) %>%
  mutate(bin = cut(dist_to_road_m, breaks = nbins)) %>%
  group_by(Pollutant, bin) %>%
  summarise(
    x   = mean(dist_to_road_m),
    n   = n(),
    med = median(value),
    q10 = quantile(value, 0.10, names = FALSE, type = 7),
    q90 = quantile(value, 0.90, names = FALSE, type = 7),
    .groups = "drop"
  )

# ----------------------------
# 4) Plot
# ----------------------------
p <- ggplot(summ, aes(x = x)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "grey85") +
  geom_line(aes(y = med), linewidth = 0.8) +
  facet_wrap(~ Pollutant, scales = "free_y") +
  labs(
    x = "Distance to nearest Primary Road/Ramp (m)",
    y = "Pollutant concentration"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = out_fig,
  plot = p,
  width = 8.5, height = 8.5, dpi = 600, bg = "white"
)
