# ==============================================================
# M05_methane_sourceprob_facilities.R
# Methane source-probability map in the EXACT style of Figure 3
# (script 26): cartolight basemap, 250 m grid, sigma = 900 m
# Gaussian smooth, p99 events with ws > 1 m/s, 15 km upwind rays
# (150 m step), weights pmin(val/p99, 5) * exp(-d/12 km),
# viridis option "C" with 2-98% squished limits, TRI facilities
# as small white dots, and the same key-facility overlay:
#   Suncor / Sinclair / Phillips 66 (red), WWTF1 + WWTF2 (green),
#   Woodshop (purple), Refuel sites (blue).
# Output: sourceprob_maps/sourceprob_methane_p99_..._refuel_adjusted.png
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("M05: Methane source-probability map, Figure-3 style")

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(readr); library(ggplot2)
  library(ggspatial); library(scales); library(terra); library(tibble)
})

# ---- settings copied from script 26 (Figure 3) ----
tile_type <- "cartolight"; tile_zoom <- 11; dpi_out <- 450
surface_alpha <- 0.75; tri_alpha <- 0.35
crs_ll <- 4326; crs_m <- 32613
ray_len_m <- 15000; ray_step_m <- 150
grid_res_m <- 250; sigma_m <- 900; kernel_radius <- 3
min_ws <- 1.0; pad <- 1500
out_dir <- file.path(BASE, "sourceprob_maps")
dir.create(out_dir, showWarnings = FALSE)

# ---- methane data ----
load(file.path(BASE, "mobile_methane_wind_bg.RData"))
ch4 <- df_ch4_bg
diag_msg("  methane rows: ", format(nrow(ch4), big.mark = ","),
         " | columns: ", paste(head(names(ch4), 15), collapse = ", "))
stopifnot(all(c("ch4_ppm", "wd", "ws", "Latitude", "Longitude") %in% names(ch4)))

p99 <- as.numeric(quantile(ch4$ch4_ppm, 0.99, na.rm = TRUE))
use <- ch4 %>%
  filter(is.finite(ch4_ppm), ch4_ppm >= p99,
         is.finite(wd), is.finite(ws), ws > min_ws,
         is.finite(Latitude), is.finite(Longitude))
diag_msg(sprintf("  p99 = %.3f ppm | events with valid wind (> %.0f m/s): %s",
                 p99, min_ws, format(nrow(use), big.mark = ",")))
if (nrow(use) < 10) stop("Too few methane exceedance events with valid wind.")

pts_ll <- st_as_sf(use, coords = c("Longitude", "Latitude"), crs = crs_ll)
pts_m  <- st_transform(pts_ll, crs_m)

# bbox from ALL methane points (same construction as script 26)
all_m <- st_transform(st_as_sf(ch4[is.finite(ch4$Latitude), ],
                               coords = c("Longitude", "Latitude"), crs = crs_ll), crs_m)
bb_m <- st_bbox(all_m)
xlim_m <- unname(c(bb_m["xmin"], bb_m["xmax"])) + c(-pad, pad)
ylim_m <- unname(c(bb_m["ymin"], bb_m["ymax"])) + c(-pad, pad)

# ---- rays (verbatim logic from script 26) ----
xy <- st_coordinates(pts_m)
w0 <- pmin(use$ch4_ppm / p99, 5)
steps <- seq(0, ray_len_m, by = ray_step_m)
b <- use$wd * pi / 180
ux <- sin(b); uy <- cos(b)
n_steps <- length(steps); n_rays <- nrow(use)
X <- rep(xy[, 1], each = n_steps) + rep(ux, each = n_steps) * rep(steps, times = n_rays)
Y <- rep(xy[, 2], each = n_steps) + rep(uy, each = n_steps) * rep(steps, times = n_rays)
W <- rep(w0, each = n_steps) * exp(-rep(steps, times = n_rays) / 12000)
pts <- data.frame(x = X, y = Y, w = W)
pts <- pts[is.finite(pts$x) & is.finite(pts$y) & is.finite(pts$w), ]
diag_msg(sprintf("  rays: %s events x %d steps = %s weighted points",
                 format(n_rays, big.mark = ","), n_steps,
                 format(nrow(pts), big.mark = ",")))

r0 <- terra::rast(xmin = xlim_m[1], xmax = xlim_m[2],
                  ymin = ylim_m[1], ymax = ylim_m[2],
                  resolution = grid_res_m, crs = st_crs(crs_m)$wkt)
v <- terra::vect(pts, geom = c("x", "y"), crs = st_crs(crs_m)$wkt)
r_sum <- terra::rasterize(v, r0, field = "w", fun = "sum", background = 0)
sigma_cells <- sigma_m / grid_res_m
rad <- max(1, ceiling(kernel_radius * sigma_cells))
K <- outer(seq(-rad, rad), seq(-rad, rad),
           function(i, j) exp(-0.5 * (i^2 + j^2) / sigma_cells^2))
K <- K / sum(K)
r_sm <- terra::focal(r_sum, w = K, fun = "sum", na.policy = "omit", fillvalue = 0)
mx <- terra::global(r_sm, fun = "max", na.rm = TRUE)[1, 1]
r_prob <- r_sm / mx
df_r <- as.data.frame(r_prob, xy = TRUE, na.rm = FALSE)
names(df_r) <- c("x", "y", "prob")

imax <- df_r[which.max(df_r$prob), ]
mx_ll <- st_transform(st_sfc(st_point(c(imax$x, imax$y)), crs = crs_m), crs_ll)
diag_msg(sprintf("  max-probability cell at (%.5f, %.5f) — compare with M04's (39.872, -104.876)",
                 st_coordinates(mx_ll)[2], st_coordinates(mx_ll)[1]))

# ---- TRI overlay ----
tri_sf <- NULL
tri_csv <- file.path(BASE, "TRI.csv")
if (file.exists(tri_csv)) {
  tri_raw <- suppressMessages(read_csv(tri_csv, show_col_types = FALSE))
  nm <- names(tri_raw)
  loncol <- nm[grepl("^lon", nm, ignore.case = TRUE)][1]
  latcol <- nm[grepl("^lat", nm, ignore.case = TRUE)][1]
  if (!is.na(loncol) && !is.na(latcol)) {
    tri_ok <- tri_raw[is.finite(tri_raw[[loncol]]) & is.finite(tri_raw[[latcol]]), ]
    tri_sf <- st_transform(st_as_sf(tri_ok, coords = c(loncol, latcol), crs = crs_ll), crs_m)
    bb_poly <- st_as_sfc(st_bbox(c(xmin = xlim_m[1], xmax = xlim_m[2],
                                   ymin = ylim_m[1], ymax = ylim_m[2]), crs = st_crs(crs_m)))
    tri_sf <- tri_sf[st_intersects(tri_sf, bb_poly, sparse = FALSE), ]
    diag_msg("  TRI facilities in frame: ", nrow(tri_sf))
  }
} else diag_msg("  [WARN] TRI.csv not found — map will omit the TRI dot layer")

# ---- key facilities: VERBATIM from script 26 (colors, sizes, nudges) ----
key_facilities <- tibble(
  label = c("Suncor", "Sinclair", "Phillips 66", "WWTF1", "WWTF2",
            "Woodshop", "Refuel", "Refuel", "Refuel", "Refuel"),
  lat = c(39.803333, 39.8724, 39.79668,
          39.80822838231637, 39.87304794998779, 39.791382444842746,
          39.79935581470166, 39.783338577716854, 39.886063120868805, 39.88659578717162),
  lon = c(-104.945556, -104.8861, -104.94236,
          -104.9553246877205, -104.91204700295945, -104.94754520948433,
          -104.88376424570843, -105.10918240337975, -104.84531380337543, -104.88371420337545),
  point_fill = c("red", "red", "red", "green3", "green3", "purple3",
                 "dodgerblue3", "dodgerblue3", "dodgerblue3", "dodgerblue3"),
  point_color = rep("white", 10),
  text_color = c("red", "red", "red", "green4", "green4", "purple4",
                 "dodgerblue4", "dodgerblue4", "dodgerblue4", "dodgerblue4"),
  point_size = c(3.8, 3.8, 3.8, 4.2, 4.2, 3.8, 3.3, 3.3, 3.3, 3.3),
  nudge_x = c(350, 250, -350, -850, 700, -900, -350, -1100, 900, 250),
  nudge_y = c(350, 350, -350, 350, 350, -350, 350, 350, 350, -400)
)
key_sf <- st_transform(st_as_sf(key_facilities, coords = c("lon", "lat"), crs = crs_ll), crs_m)
key_sf$key_label <- key_facilities$label

# ---- plot (identical styling to Figure 3 panels) ----
q <- quantile(df_r$prob[df_r$prob > 0], probs = c(0.02, 0.98), na.rm = TRUE)
lims <- as.numeric(q)

p <- ggplot() +
  annotation_map_tile(type = tile_type, zoom = tile_zoom) +
  geom_raster(data = df_r, aes(x = x, y = y, fill = prob), alpha = surface_alpha) +
  { if (!is.null(tri_sf)) geom_sf(data = tri_sf, shape = 21, size = 1.9, stroke = 0.2,
                                  fill = "white", color = "black", alpha = tri_alpha) else NULL } +
  geom_sf(data = key_sf, aes(size = point_size), shape = 21, stroke = 0.7,
          fill = key_sf$point_fill, color = key_sf$point_color,
          alpha = 0.98, show.legend = FALSE) +
  lapply(seq_len(nrow(key_sf)), function(i)
    geom_sf_text(data = key_sf[i, ], aes(label = key_label),
                 color = key_sf$text_color[i], size = 3.2, fontface = "bold",
                 nudge_x = key_facilities$nudge_x[i], nudge_y = key_facilities$nudge_y[i],
                 check_overlap = FALSE)) +
  scale_size_identity() +
  scale_fill_viridis_c(option = "C", limits = lims, oob = scales::squish,
                       name = "Relative\nprobability") +
  coord_sf(crs = st_crs(crs_m), xlim = xlim_m, ylim = ylim_m, expand = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold")) +
  labs(title = "Methane source probability",
       subtitle = sprintf("Upwind projection of p99 events | p99=%.3f ppm | n exceed=%s | rays=%g km | smooth σ=%d m",
                          p99, format(n_rays, big.mark = ","), ray_len_m / 1000, sigma_m),
       x = NULL, y = NULL)

out_png <- file.path(out_dir, "sourceprob_methane_p99_weighted_smoothed_TRI_KEY_CONTEXT_refuel_adjusted.png")
ggsave(out_png, p, width = 8.7, height = 7.5, units = "in", dpi = dpi_out, bg = "white")
diag_msg("\n[Saved] ", out_png)
diag_msg("Drop-in replacement for SI Figure S7.1 (same style as main-text Figure 3).")
