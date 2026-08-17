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

save(out_sf, file = "/Users/priyanka/Downloads/Suncor/mobile_corrected.RData")
