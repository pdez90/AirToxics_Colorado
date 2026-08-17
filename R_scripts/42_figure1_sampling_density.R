# ==============================================================
# 42  FIGURE 1 (redesigned per Reviewer 1)
# Side-by-side 500 m sampling-density panels, one per route, with:
#   - cells colored by total 1-s measurements (log10 scale)
#   - overlays with DISTINCT SHAPES (grayscale-safe):
#       triangle = Stationary Measurement Site - La Casa
#       square   = Covered facilities (HB21-1189)
#       diamond  = EPA AQS wind sites
#   - enlarged, repositioned labels; figure border; panel tags A/B
#   - basemap credited in caption (CARTO Positron / cartolight)
# Output: FinalFig/Figure1_sampling_density.png (+ per-panel counts CSV)
# ==============================================================

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(data.table); library(ggplot2)
  library(ggspatial); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data + 500 m grid...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out)
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
message("  rows: ", format(nrow(df), big.mark = ","),
        " | routes: ", paste(unique(df$Site), collapse = " / "))

grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"), quiet = TRUE)
# work in the grid's native metric CRS; transform only centroid POINTS
st_crs(grid) <- 26913   # NAD83 / UTM 13N (per .prj; set explicitly to be safe)
cent_m  <- st_centroid(st_geometry(grid))
cent_ll <- st_coordinates(st_transform(cent_m, 4326))
cells <- data.table(id = grid$id, clon = cent_ll[, 1], clat = cent_ll[, 2])
message("  grid cells: ", nrow(cells), " | lon range: ",
        paste(round(range(cells$clon), 3), collapse = " to "),
        " | lat range: ", paste(round(range(cells$clat), 3), collapse = " to "))
stopifnot(all(cells$clon > -106 & cells$clon < -104),
          all(cells$clat > 39 & cells$clat < 41))   # sanity: real degrees

# assign each 1-s point to a 500 m cell (nearest centroid, in meters)
pts <- st_transform(st_as_sf(df[, .(Longitude, Latitude)],
                             coords = c("Longitude", "Latitude"), crs = 4326), 26913)
message("Snapping ", format(nrow(pts), big.mark = ","), " points to 500 m cells...")
t0 <- Sys.time()
idx <- st_nearest_feature(pts, cent_m)
message("  done in ", round(difftime(Sys.time(), t0, units = "secs")), " s")
df[, cell := grid$id[idx]]

cnt <- df[, .(n = .N), by = .(cell, Site)]
message("  cells with data: ", uniqueN(cnt$cell),
        " | per-route cells: ",
        paste(cnt[, uniqueN(cell), by = Site]$V1, collapse = ", "))
fwrite(cnt, file.path(BASE, "figure1_cell_counts_by_route.csv"))

grid_cnt <- merge(cnt, cells, by.x = "cell", by.y = "id")
grid_cnt[, Route := ifelse(grepl("Suncor", Site),
                           "A) Suncor & Phillips 66 route",
                           "B) Sinclair Terminal route")]
message("  plot rows: ", nrow(grid_cnt), " (must be > 0)")
stopifnot(nrow(grid_cnt) > 0)
# 500 m expressed in degrees at the domain's mean latitude
lat0 <- mean(grid_cnt$clat)
tile_w <- 500 / (111320 * cos(lat0 * pi / 180))
tile_h <- 500 / 110540

# ---- overlays -------------------------------------------------
covered <- data.frame(
  name = c("Suncor Energy refinery", "Sinclair Terminal", "Phillips 66 Terminal"),
  lat = c(39.803333, 39.8724, 39.79668),
  lon = c(-104.945556, -104.8861, -104.94236),
  type = "Covered facilities (HB21-1189)")
wind <- read.csv(file.path(BASE, "wind_sites.csv"))
wind <- data.frame(name = "", lat = wind$Lat_wind, lon = wind$Lon_wind,
                   type = "EPA AQS wind sites")
wind <- wind[is.finite(wind$lat), ]
# La Casa: the wind site nearest the known La Casa location
lacasa_ll <- c(39.7794, -105.0052)
dd <- sqrt((wind$lat - lacasa_ll[1])^2 + (wind$lon - lacasa_ll[2])^2)
message("  wind sites: ", nrow(wind), " | nearest to La Casa: ",
        round(min(dd) * 111, 2), " km")
if (min(dd) < 0.02) {   # ~2 km: that station IS La Casa
  lac <- wind[which.min(dd), ]; wind <- wind[-which.min(dd), ]
  lac$name <- "La Casa"; lac$type <- "Stationary Measurement Site - La Casa"
} else {
  lac <- data.frame(name = "La Casa", lat = lacasa_ll[1], lon = lacasa_ll[2],
                    type = "Stationary Measurement Site - La Casa")
}
ovl <- rbind(covered, wind, lac)
ovl$type <- factor(ovl$type, levels = c("Stationary Measurement Site - La Casa",
                                        "Covered facilities (HB21-1189)",
                                        "EPA AQS wind sites"))

padx <- 0.012; pady <- 0.012
xlim <- unname(range(grid_cnt$clon)) + c(-padx, padx)
ylim <- unname(range(grid_cnt$clat)) + c(-pady, pady)
message("  frame: lon ", paste(round(xlim, 3), collapse = " to "),
        " | lat ", paste(round(ylim, 3), collapse = " to "))

lab_df <- rbind(
  data.frame(name = covered$name, lat = covered$lat, lon = covered$lon,
             nx = c(-0.030, 0.012, 0.014), ny = c(0.008, 0.006, -0.007)),
  data.frame(name = "La Casa", lat = lac$lat, lon = lac$lon,
             nx = -0.020, ny = -0.007))

p <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  geom_tile(data = grid_cnt, aes(x = clon, y = clat, fill = n),
            width = tile_w, height = tile_h, alpha = 0.85) +
  scale_fill_viridis_c(trans = "log10",
                       name = "1-s measurements\nper 500 m cell",
                       labels = label_number(big.mark = ","),
                       option = "D") +
  geom_point(data = ovl, aes(x = lon, y = lat, shape = type),
             size = 4.2, stroke = 1.4, fill = "white", color = "black") +
  scale_shape_manual(values = c(
    "Stationary Measurement Site - La Casa" = 24,   # triangle
    "Covered facilities (HB21-1189)" = 22,          # square
    "EPA AQS wind sites" = 23),                     # diamond
    name = NULL) +
  geom_segment(data = lab_df,
               aes(x = lon + nx * 0.65, y = lat + ny * 0.65, xend = lon, yend = lat),
               linewidth = 0.35, color = "grey20") +
  geom_label(data = lab_df,
             aes(x = lon + nx, y = lat + ny, label = name),
             size = 3.4, fontface = "bold", linewidth = 0.25,
             fill = "white", alpha = 0.9) +
  facet_wrap(~Route, ncol = 2) +
  coord_sf(crs = 4326, default_crs = 4326, xlim = xlim, ylim = ylim, expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  labs(caption = "Basemap: CARTO Positron. Cells are the 500 m analysis grid; color shows the total number of 1-s measurements collected in each cell over the campaign (log scale).",
       x = NULL, y = NULL) +
  theme_bw(base_size = 13) +
  theme(panel.border = element_rect(color = "black", linewidth = 0.8, fill = NA),
        strip.background = element_rect(fill = "grey93", color = "black"),
        strip.text = element_text(face = "bold", size = 13),
        legend.position = "right",
        plot.caption = element_text(size = 9, hjust = 0),
        panel.grid = element_blank())

out_png <- file.path(BASE, "FinalFig", "Figure1_sampling_density.png")
ggsave(out_png, p, width = 14, height = 7.2, dpi = 450, bg = "white")
message("[Saved] ", out_png)
print(cnt[, .(cells = uniqueN(cell), total_obs = sum(n)), by = Site])
