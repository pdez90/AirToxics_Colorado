# ==============================================================
# 12  Finding 500 meter road segment
# Auto-split from Suncor.Rmd  (section 12 of 40)
# ==============================================================

#Finding 500 meter road segment
# (https://stackoverflow.com/questions/59766153/left-join-based-on-closest-lat-lon-in-r
# )

library(sf)
library(dplyr)

load("/Users/priyanka/Downloads/Suncor/bgcorrected_out_merge.RData")
out_merge <- df

grid <- st_read("/Users/priyanka/Downloads/Suncor/Grid_500m_generated/grid_500m.shp", quiet = TRUE) %>%
  st_transform(4326)

# One lon/lat per grid feature: centroid coordinates
grid_cent <- st_centroid(grid)
cc <- st_coordinates(grid_cent)
grid$Lon_grid <- cc[, 1]   # X = lon
grid$Lat_grid <- cc[, 2]   # Y = lat

# Keep only rows with coords
out_merge <- out_merge %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(label = seq_len(n()))

# Make sf points (keep original lon/lat columns)
out_sf <- st_as_sf(
  out_merge,
  coords = c("Longitude", "Latitude"),
  remove = FALSE,
  crs = 4326
)

# Nearest grid feature index for each point
idx <- st_nearest_feature(out_sf, grid)

# Bind nearest grid attributes (includes polygon geometry from grid)
out_sf <- bind_cols(out_sf, st_drop_geometry(grid[idx, ]))

# Distance from point to the NEAREST GRID CELL
# If you want distance to the polygon boundary, this is fine.
# If you want distance to the grid centroid instead, use grid_cent[idx, ].
out_sf$dist_m <- as.numeric(st_distance(out_sf, grid[idx, ], by_element = TRUE))

# GUARD (2026-08-20): st_nearest_feature() ALWAYS returns a cell. A point that
# falls outside the grid entirely is therefore snapped to the nearest edge
# cell, and its concentration is pooled into that cell's statistics as though
# it had been measured there - which would corrupt exactly the boundary cells
# of Figure 2. dist_m was already being computed here and then never used, so
# nothing detected it. A point inside a cell has dist_m == 0 by construction,
# so any positive distance means the observation lay outside the grid.
#
# R00b_make_grids.R builds the grid from a documented domain box
# (lon -105.25..-104.70, lat 39.60..40.00) that is deliberately generous
# relative to the measured extent (lon ~-105.18..-104.75, lat ~39.73..39.91),
# so in a healthy run this drops nothing. It is a guard against a future grid
# or route change silently piling transit air onto the domain edge.
MAX_SNAP_M <- 0.5    # metres; a point inside a cell is at distance 0
.n_out <- sum(out_sf$dist_m > MAX_SNAP_M, na.rm = TRUE)
message(sprintf("[GRID] %s of %s observations lie outside the 500 m grid (max snap distance %.1f m)",
                format(.n_out, big.mark = ","), format(nrow(out_sf), big.mark = ","),
                if (.n_out > 0) max(out_sf$dist_m, na.rm = TRUE) else 0))
if (.n_out > 0) {
  message("[GRID] distance-to-grid quantiles for those points (m): ",
          paste(round(stats::quantile(out_sf$dist_m[out_sf$dist_m > MAX_SNAP_M],
                                      c(0.5, 0.9, 1)), 1), collapse = " | "))
  message("[GRID] dropping them: they were not measured in any grid cell, and ",
          "snapping them to the nearest edge cell would attribute their ",
          "concentrations to a location they were not sampled at.")
  out_sf <- out_sf[out_sf$dist_m <= MAX_SNAP_M, ]
}

save(out_sf, file = "/Users/priyanka/Downloads/Suncor/mobile_corrected.RData")
