# ==============================================================
# 24  Join with TRI
# Auto-split from Suncor.Rmd  (section 24 of 40)
# ==============================================================

#Join with TRI

# ============================================================
# TRI proximity analysis (bg-corrected pollutants) + CLEAN FIGURE
# - Reads TRI facility points
# - Loads bgcorrected_out_merge.RData (expects df)
# - Filters (HCN censoring + remove Goodrich)
# - Reshapes to long (robust to df being data.frame/data.table/sf)
# - Computes nearest TRI distance (fast kNN via RANN in UTM 13N)
# - Creates inside/outside flags for multiple buffer radii
# - Summarizes mean ± 95% CI + n by radius, pollutant, inside
# - Runs Wilcoxon tests per (radius, pollutant)
# - Plots mean vs radius with CI, with n labels and significance stars
#   (extra top margin so nothing gets cropped)
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(sf)
  library(RANN)
})

# ----------------------------
# 0) Paths
# ----------------------------
tri_csv   <- "/Users/priyanka/Downloads/Suncor/TRI.csv"
df_rdata  <- "/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData"
out_dir   <- "/Users/priyanka/Downloads/Suncor/FinalFig"
out_file  <- file.path(out_dir, "tri_buffer_mean_ci_clean.png")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# 1) Read TRI + de-dup lon/lat
# ----------------------------
tri <- read.csv(tri_csv, stringsAsFactors = FALSE)

# Try to identify lon/lat columns robustly
lon_col <- intersect(c("Longitude", "longitude", "LON", "lon"), names(tri))[1]
lat_col <- intersect(c("Latitude", "latitude", "LAT", "lat"), names(tri))[1]
stopifnot(!is.na(lon_col), !is.na(lat_col))

tri <- tri[!duplicated(tri[, c(lon_col, lat_col)]), ]

tri_sf <- st_as_sf(tri, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)

# ----------------------------
# 2) Load mobile bg-corrected data (expects df)
# ----------------------------
load(df_rdata)
stopifnot(exists("df"))

# If df is sf, drop geometry now
if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)

# Basic filters matching your workflow
# (HCN censoring after 2025-01-22)
if ("HCN" %in% names(df) && "date" %in% names(df)) {
  df$HCN <- ifelse(df$date > "2025-01-22 00:00:00", df$HCN, NA)
}
if ("Site" %in% names(df)) {
  df <- df[df$Site != "Goodrich Corporation (Collins Aerospace)", ]
}

# ----------------------------
# 3) Reshape to long (bg-corrected pollutants)
# ----------------------------
# bg-corrected pollutant columns you want
polls_bgc <- c("sBenzene","sToluene","sXylene","sTrimethylbenzene","sH2S","sHCN")

# core columns needed
need_core <- c("date", "Latitude", "Longitude")

have_core <- intersect(need_core, names(df))
have_poll <- intersect(polls_bgc, names(df))

# Make a plain data.frame so data.table melt is always happy
df_plain <- as.data.frame(df)

if (length(have_core) == 3 && length(have_poll) > 0) {
  df_sub <- df_plain[, c(have_core, have_poll), drop = FALSE]
  setDT(df_sub)
  L <- data.table::melt(
    df_sub,
    id.vars = have_core,
    variable.name = "Pollutant",
    value.name = "value"
  )
} else {
  # fallback: your original indices (only if df has enough cols)
  if (ncol(df_plain) < 55) {
    stop(
      "Could not find required columns (date/Latitude/Longitude and s* pollutants), ",
      "and df has <55 columns so index fallback (3,4,5,50:55) is unsafe."
    )
  }
  df_sub <- df_plain[, c(3, 4, 5, 50:55), drop = FALSE]
  setDT(df_sub)
  L <- data.table::melt(
    df_sub,
    id.vars = names(df_sub)[1:3],
    variable.name = "Pollutant",
    value.name = "value"
  )
  # rename core columns to expected names for downstream
  setnames(L, old = names(L)[1:3], new = c("date","Latitude","Longitude"))
}

# Keep finite coords + finite values
L <- L[is.finite(Latitude) & is.finite(Longitude)]
L <- L[is.finite(value)]
L[, Pollutant := as.character(Pollutant)]

# Convert to sf (points)
df_sf <- st_as_sf(
  as.data.frame(L),
  coords = c("Longitude","Latitude"),
  crs = 4326,
  remove = FALSE
)

# ----------------------------
# 4) Project to Denver-local CRS + nearest TRI distance (FAST)
# ----------------------------
denver_crs <- sf::st_crs(26913)  # NAD83 / UTM zone 13N (meters)
tri_local  <- st_transform(tri_sf, denver_crs)
df_local   <- st_transform(df_sf, denver_crs)

# kNN distance to nearest TRI facility (meters)
df_xy  <- sf::st_coordinates(df_local)[, 1:2, drop = FALSE]
tri_xy <- sf::st_coordinates(tri_local)[, 1:2, drop = FALSE]
nn     <- RANN::nn2(data = tri_xy, query = df_xy, k = 1)
dmin_m <- as.numeric(nn$nn.dists[, 1])  # meters

# ----------------------------
# 5) Inside/outside flags for multiple radii
# ----------------------------
buffer_distances_m <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)

dpf <- df_local |>
  st_drop_geometry() |>
  mutate(
    dmin_m = dmin_m,
    row_id = row_number()
  )

# Expand each observation across radii and flag inside/outside
flags <- tidyr::crossing(
  row_id   = dpf$row_id,
  distance = buffer_distances_m
) |>
  mutate(
    inside = dpf$dmin_m[row_id] <= distance,
    inside = factor(inside, levels = c(FALSE, TRUE), labels = c("Outside","Inside")),
    distance_label = paste0("\u2264", distance, " m")
  )

dpf2 <- dpf |>
  select(row_id, date, Latitude, Longitude, Pollutant, value) |>
  left_join(flags, by = "row_id")

# ----------------------------
# 6) Summaries + Wilcoxon tests per (distance, Pollutant)
# ----------------------------
summ <- dpf2 |>
  group_by(Pollutant, distance, distance_label, inside) |>
  summarise(
    n    = n(),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    se   = sd / sqrt(pmax(n, 1)),
    .groups = "drop"
  ) |>
  mutate(ci95 = 1.96 * se)

tests <- dpf2 |>
  group_by(Pollutant, distance) |>
  summarise(
    p_value = {
      v_in  <- value[inside == "Inside"]
      v_out <- value[inside == "Outside"]
      if (length(v_in) >= 3 && length(v_out) >= 3) suppressWarnings(wilcox.test(v_in, v_out)$p.value) else NA_real_
    },
    .groups = "drop"
  ) |>
  mutate(
    sig_lab = case_when(
      is.na(p_value)        ~ "",
      p_value < 0.001       ~ "***",
      p_value < 0.01        ~ "**",
      p_value < 0.05        ~ "*",
      TRUE                  ~ ""
    )
  )

summ2 <- summ |>
  left_join(tests, by = c("Pollutant","distance"))

# Labels for n (place near top of errorbar, offset inside/outside slightly)
x_delta <- max(diff(range(buffer_distances_m)) * 0.01, 10)

summ_labels <- summ2 |>
  mutate(
    x_lab = distance + ifelse(inside == "Inside", +x_delta, -x_delta),
    n_lab = paste0("n=", format(n, big.mark = ",")),
    y_lab = mean + ci95
  )

# ----------------------------
# 7) Clean plot (no cropping) + ROBUST to Inf/NA (fixes viewport error)
# ----------------------------

# pseudo-log so zeros/near-zeros behave better than log10
min_pos <- suppressWarnings(min(dpf2$value[dpf2$value > 0], na.rm = TRUE))
epsilon <- if (is.finite(min_pos)) min_pos / 2 else 1e-6

# ---- SAFETY: drop any non-finite summary rows (prevents viewport=0)
summ2 <- summ2 %>%
  dplyr::mutate(
    mean = as.numeric(mean),
    ci95 = as.numeric(ci95)
  ) %>%
  dplyr::filter(is.finite(distance), is.finite(mean), is.finite(ci95))

if (nrow(summ2) == 0) {
  stop("summ2 has 0 finite rows (mean/ci95). Check that 'value' has finite numbers after filtering.")
}

# Labels for n (place near top of errorbar, offset inside/outside slightly)
x_delta <- max(diff(range(buffer_distances_m)) * 0.01, 10)

summ_labels <- summ2 %>%
  dplyr::mutate(
    x_lab = distance + ifelse(inside == "Inside", +x_delta, -x_delta),
    n_lab = paste0("n=", format(n, big.mark = ",")),
    y_lab = mean + ci95
  ) %>%
  dplyr::filter(is.finite(x_lab), is.finite(y_lab))

# Significance text position (ONLY where finite)
sig_pos <- summ2 %>%
  dplyr::group_by(Pollutant, distance, distance_label) %>%
  dplyr::summarise(
    y_sig   = max(mean + ci95, na.rm = TRUE),
    sig_lab = dplyr::first(sig_lab),
    .groups = "drop"
  ) %>%
  dplyr::mutate(y_sig = y_sig * 1.25) %>%
  dplyr::filter(is.finite(y_sig), !is.na(sig_lab), sig_lab != "")

p <- ggplot(summ2, aes(x = distance, y = mean, group = inside, color = inside)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  geom_errorbar(aes(ymin = pmax(mean - ci95, 0), ymax = mean + ci95),
                width = 0, na.rm = TRUE) +
  geom_point(size = 2.2, na.rm = TRUE) +

  # n labels (only finite rows)
  ggrepel::geom_text_repel(
    data = summ_labels,
    aes(x = x_lab, y = y_lab, label = n_lab),
    inherit.aes = FALSE,
    size = 3.0,
    show.legend = FALSE,
    direction = "y",
    min.segment.length = 0,
    segment.size = 0.2,
    box.padding = 0.15,
    point.padding = 0.15,
    seed = 123
  ) +

  # stars (only where present + finite)
  geom_text(
    data = sig_pos,
    aes(x = distance, y = y_sig, label = sig_lab),
    inherit.aes = FALSE,
    size = 4.2,
    fontface = "bold",
    vjust = 0
  ) +

  scale_x_continuous(breaks = buffer_distances_m) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = epsilon, base = 10),
    expand = expansion(mult = c(0.05, 0.35))
  ) +
  scale_color_manual(values = c("Outside" = "blue", "Inside" = "red"), name = NULL) +
  labs(
    x = "Buffer radius (m)",
    y = "Mean concentration (ppb)",
    caption = "Error bars: mean ± 95% CI. Stars: Wilcoxon test (Inside vs Outside) at each radius."
  ) +
  facet_wrap(~ Pollutant, scales = "free_y", ncol = 2) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 15, b = 15, l = 15)
  ) +
  coord_cartesian(clip = "off")

# Save + print safely
ggsave(out_file, p, width = 10.5, height = 10.5, dpi = 600, bg = "white", limitsize = FALSE)
print(p)
