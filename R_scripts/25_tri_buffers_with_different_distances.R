# ==============================================================
# 25  TRI buffers with different distances
# Auto-split from Suncor.Rmd  (section 25 of 40)
# ==============================================================

#TRI buffers with different distances

# ============================================================
# TRI buffer analysis (fast) + clean plots (no cropping)
# - Reads TRI facilities and bg-corrected mobile data
# - Long format (Pollutant, value)
# - Computes nearest-facility distance once (RANN kNN) in meters
# - Flags Inside/Outside for multiple radii (vectorized)
# - Summarises mean + 95% CI by radius, pollutant, inside/outside
# - Adds Mann–Whitney p-values (Inside vs Outside) per radius & pollutant
# - Saves a clean multi-panel figure with headroom (no top cropping)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(RANN)
})

# ----------------------------
# 0) Inputs
# ----------------------------
tri_file   <- "/Users/priyanka/Downloads/Suncor/TRI.csv"
df_rdata   <- "/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData"
out_dir    <- "/Users/priyanka/Downloads/Suncor/FinalFig"
out_plot   <- file.path(out_dir, "tri_distance_mean_ci_clean.jpeg")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Radii (meters)
buffer_distances_m <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)

# Use Denver-local CRS for meters
denver_crs <- sf::st_crs(26913)  # NAD83 / UTM zone 13N (meters)

# ----------------------------
# 1) Load TRI facilities
# ----------------------------
tri <- read.csv(tri_file, stringsAsFactors = FALSE)
# dedupe on lat/lon columns (your original: cols 4,5)
tri <- tri[!duplicated(tri[, c(4, 5)]), ]

stopifnot(all(c("Latitude", "Longitude") %in% names(tri)) ||
            all(c("LATITUDE", "LONGITUDE") %in% names(tri)) ||
            all(c("lat", "lon") %in% names(tri)) ||
            all(c("Latitude", "Longitude") %in% names(tri)))

# If TRI column names aren’t exactly Longitude/Latitude, map them here safely
if (!all(c("Longitude", "Latitude") %in% names(tri))) {
  # Try common variants
  nm_lon <- intersect(names(tri), c("Longitude","LONGITUDE","lon","LON","X"))
  nm_lat <- intersect(names(tri), c("Latitude","LATITUDE","lat","LAT","Y"))
  stopifnot(length(nm_lon) >= 1, length(nm_lat) >= 1)
  names(tri)[match(nm_lon[1], names(tri))] <- "Longitude"
  names(tri)[match(nm_lat[1], names(tri))] <- "Latitude"
}

tri_sf <- st_as_sf(tri, coords = c("Longitude", "Latitude"), crs = 4326)

# ----------------------------
# 2) Load bg-corrected mobile data and reshape long
# ----------------------------
load(df_rdata)  # expects `df`
stopifnot(exists("df"))

# HCN censoring + drop Goodrich site
df$HCN <- ifelse(df$date > "2025-01-22 00:00:00", df$HCN, NA)
df <- df[df$Site != "Goodrich Corporation (Collins Aerospace)", ]

# Keep only needed columns (date, lat/lon, bg-corrected pollutants)
# Your original assumed: date, Latitude, Longitude in positions 3:5 and bg-corr in 50:55
# Safer: grab by names if available; otherwise fall back to your indices
need_core <- c("date", "Latitude", "Longitude")
polls_bgc <- c("sBenzene","sToluene","sXylene","sTrimethylbenzene","sH2S","sHCN")

have_core <- intersect(need_core, names(df))
have_poll <- intersect(polls_bgc, names(df))

if (length(have_core) < 3 || length(have_poll) == 0) {
  # fallback to your original indices if names aren’t present
  df_sub <- as.data.frame(df)[, c(3, 4, 5, 50:55)]
  setDT(df_sub)
  L <- data.table::melt(df_sub,
            id.vars = c("date", "Latitude", "Longitude"),
            variable.name = "Pollutant",
            value.name = "value")
} else {
  df_sub <- as.data.frame(df)[, c(have_core, have_poll)]
  setDT(df_sub)
  L <- data.table::melt(df_sub,
            id.vars = have_core,
            variable.name = "Pollutant",
            value.name = "value")
}

L[, Pollutant := as.character(Pollutant)]
L <- L[is.finite(Latitude) & is.finite(Longitude)]

df_sf <- st_as_sf(as.data.frame(L), coords = c("Longitude", "Latitude"), crs = 4326)

# ----------------------------
# 3) Transform to local CRS (meters)
# ----------------------------
tri_local <- st_transform(tri_sf, denver_crs)
df_local  <- st_transform(df_sf,  denver_crs)

# ----------------------------
# 4) Nearest-facility distance ONCE (fast)
# ----------------------------
df_xy  <- st_coordinates(df_local)[, 1:2, drop = FALSE]
tri_xy <- st_coordinates(tri_local)[, 1:2, drop = FALSE]

nn   <- RANN::nn2(data = tri_xy, query = df_xy, k = 1)
dmin <- as.numeric(nn$nn.dists[, 1])  # meters

# Build a row_id to join flags back cleanly
df_local$row_id <- seq_len(nrow(df_local))

# ----------------------------
# 5) Expand to radii + inside flags (vectorized, no spatial loops)
# ----------------------------
R <- length(buffer_distances_m)
n <- nrow(df_local)

flags <- data.frame(
  row_id   = rep(df_local$row_id, each = R),
  distance = rep(buffer_distances_m, times = n)
)
flags$inside <- dmin[flags$row_id] <= flags$distance

df_flagged <- df_local %>%
  left_join(flags, by = "row_id") %>%
  mutate(
    inside = factor(inside, levels = c(FALSE, TRUE), labels = c("Outside", "Inside")),
    distance_label = paste0("\u2264", distance, " m")
  )

# ----------------------------
# 6) Summaries + Mann–Whitney p-values
# ----------------------------
dpf <- st_drop_geometry(df_flagged)

# pseudo-log epsilon to handle zeros smoothly
min_pos <- suppressWarnings(min(dpf$value[dpf$value > 0], na.rm = TRUE))
epsilon <- if (is.finite(min_pos)) min_pos / 2 else 1e-6

summ <- dpf %>%
  group_by(Pollutant, distance, inside) %>%
  summarise(
    # BUGFIX (2026-08-20): n was n(), which counts rows including NA values,
    # while mean and sd used na.rm = TRUE. se = sd/sqrt(n) was therefore
    # understated by sqrt(n_all / n_finite) and ci95 correspondingly too
    # narrow - worst for sHCN, which is NA for the whole record before
    # 2025-01-22. The printed "n = ..." labels were the inflated counts too.
    # Sibling script 24 already drops non-finite values before summarising.
    n    = sum(is.finite(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    se   = sd / sqrt(pmax(n, 1)),
    .groups = "drop"
  ) %>%
  mutate(ci95 = 1.96 * se)

tests <- dpf %>%
  group_by(Pollutant, distance) %>%
  summarise(
    p_value = {
      v_in  <- value[inside == "Inside"]
      v_out <- value[inside == "Outside"]
      if (length(v_in) >= 3 && length(v_out) >= 3) {
        suppressWarnings(stats::wilcox.test(v_in, v_out)$p.value)
      } else NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(sig = !is.na(p_value) & p_value < 0.05)

summ2 <- summ %>% left_join(tests, by = c("Pollutant", "distance"))

# n labels: slight x offset to reduce overlap between groups
x_range <- range(summ2$distance, na.rm = TRUE)
x_delta <- max((x_range[2] - x_range[1]) * 0.01, 0.5)

summ_labels <- summ2 %>%
  mutate(
    x_lab = distance + ifelse(inside == "Inside", +x_delta, -x_delta),
    n_lab = paste0("n = ", format(n, big.mark = ","))
  )

# ----------------------------
# 7) Clean plot (no cropping)
# ----------------------------
p2 <- ggplot(summ2, aes(x = distance, y = mean, group = inside)) +
  geom_line(aes(color = inside), linewidth = 0.7) +
  geom_errorbar(
    aes(ymin = pmax(mean - ci95, 0), ymax = mean + ci95, color = inside),
    width = 0, linewidth = 0.5
  ) +
  geom_point(
    aes(color = inside, fill = sig),
    shape = 21, size = 2.7, stroke = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = summ_labels,
    aes(x = x_lab, y = mean + ci95, label = n_lab),
    size = 3.0,
    show.legend = FALSE,
    min.segment.length = 0,
    segment.size = 0.25,
    direction = "y",
    box.padding = 0.25,
    point.padding = 0.20,
    max.overlaps = Inf,
    seed = 123
  ) +
  facet_wrap(~ Pollutant, scales = "free_y") +
  scale_x_continuous(breaks = buffer_distances_m) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = epsilon, base = 10),
    # more headroom so labels don't get chopped
    expand = expansion(mult = c(0.05, 0.45))
  ) +
  scale_color_manual(values = c("Outside" = "blue", "Inside" = "red"), name = NULL) +
  scale_fill_manual(
    values = c(`TRUE` = "#d55e00", `FALSE` = "white"),
    labels = c(`TRUE` = "p < 0.05", `FALSE` = "ns"),
    name   = "M–W test"
  ) +
  coord_cartesian(clip = "off") +
  labs(x = "Buffer radius (m)", y = "Mean concentration") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(t = 18, r = 12, b = 12, l = 12)
  )

ggsave(
  filename = out_plot,
  plot = p2,
  width = 12.5, height = 12.5, units = "in",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)

message("Saved: ", out_plot)
