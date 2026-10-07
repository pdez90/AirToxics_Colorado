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
# - Plots mean vs radius with n labels. No confidence intervals or significance
#   marks (2026-09-27): both would be computed on individual one-second
#   observations, which are strongly autocorrelated, so the intervals would be
#   far too narrow and the p-values not valid (SI section S4.1). The panels are
#   descriptive.
#   (extra top margin so nothing gets cropped)
# ============================================================

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
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
tri_csv   <- file.path(SUNCOR_BASE, "TRI.csv")
df_rdata  <- file.path(SUNCOR_BASE, "bgcorrected_out_merge.RData")
out_dir   <- file.path(SUNCOR_BASE, "FinalFig")
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

# ----------------------------
# 4) Nearest TRI distance (metres, UTM 13N)
# ----------------------------
# (2026-09-27) Memory: the earlier version built an sf object of every
# long-format row and then cross-joined every row with all eight radii
# (~120 million rows), which exceeds 7 GB. Distances are now computed once
# per unique coordinate and the inside/outside summaries radius by radius.
# The numbers are identical; only the memory footprint changed.
denver_crs <- sf::st_crs(26913)  # NAD83 / UTM zone 13N (meters)
tri_local  <- st_transform(tri_sf, denver_crs)
tri_xy     <- sf::st_coordinates(tri_local)[, 1:2, drop = FALSE]
U <- unique(L[, .(Longitude, Latitude)])
u_xy <- sf::sf_project(from = "EPSG:4326", to = "EPSG:26913",
                       pts = as.matrix(U[, .(Longitude, Latitude)]))
U[, dmin_m := as.numeric(RANN::nn2(data = tri_xy, query = u_xy, k = 1)$nn.dists[, 1])]
L <- merge(L, U, by = c("Longitude", "Latitude"), sort = FALSE)

# ----------------------------
# 5-6) Inside/outside summaries for multiple radii
# ----------------------------
buffer_distances_m <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)

summ <- rbindlist(lapply(buffer_distances_m, function(r) {
  L[, .(n = .N, mean = mean(value), sd = sd(value)),
    by = .(Pollutant, inside = factor(dmin_m <= r, levels = c(FALSE, TRUE),
                                      labels = c("Outside", "Inside")))][
    , `:=`(distance = r, distance_label = paste0("\u2264", r, " m"))]
}))
summ[, se := sd / sqrt(pmax(n, 1))]
summ[, ci95 := 1.96 * se]
summ <- as_tibble(summ)
dpf2 <- L   # used below only for the pseudo-log epsilon

summ2 <- summ

# Labels for n (place near top of errorbar, offset inside/outside slightly)
x_delta <- max(diff(range(buffer_distances_m)) * 0.01, 10)

summ_labels <- summ2 |>
  mutate(
    x_lab = distance + ifelse(inside == "Inside", +x_delta, -x_delta),
    n_lab = paste0("n=", format(n, big.mark = ",")),
    y_lab = mean
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
  dplyr::filter(is.finite(distance), is.finite(mean))

if (nrow(summ2) == 0) {
  stop("summ2 has 0 finite rows (mean/ci95). Check that 'value' has finite numbers after filtering.")
}

# Labels for n (place near top of errorbar, offset inside/outside slightly)
x_delta <- max(diff(range(buffer_distances_m)) * 0.01, 10)

summ_labels <- summ2 %>%
  dplyr::mutate(
    x_lab = distance + ifelse(inside == "Inside", +x_delta, -x_delta),
    n_lab = paste0("n=", format(n, big.mark = ",")),
    y_lab = mean
  ) %>%
  dplyr::filter(is.finite(x_lab), is.finite(y_lab))

p <- ggplot(summ2, aes(x = distance, y = mean, group = inside, color = inside)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
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

  scale_x_continuous(breaks = buffer_distances_m) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = epsilon, base = 10),
    expand = expansion(mult = c(0.05, 0.35))
  ) +
  scale_color_manual(values = c("Outside" = "blue", "Inside" = "red"), name = NULL) +
  labs(
    x = "Buffer radius (m)",
    y = "Mean mixing ratio (ppb)",
    caption = "Points: mean of individual observations. No intervals or tests are shown: consecutive observations are autocorrelated (SI section S4.1)."
  ) +
  # strip labels only: drop the "s" prefix of the background-corrected columns
  # (as script 21 does) and subscript the 2 in H2S
  facet_wrap(~ Pollutant, scales = "free_y", ncol = 2,
             labeller = as_labeller(function(x) sub("^H2S$", "H\u2082S", sub("^s(?=[A-Z])", "", x, perl = TRUE)))) +
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
