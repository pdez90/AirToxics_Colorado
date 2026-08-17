# ==============================================================
# 08  Polarplot maps
# Auto-split from Suncor.Rmd  (section 8 of 40)
# ==============================================================

#Polarplot maps

# ============================================================
# Stable Polar / Windrose Maps (renamed pollutants)
# ============================================================
library(sf)
library(data.table)
library(openair)
library(openairmaps)

# ----------------------------
# Load data
# ----------------------------
load("/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")
df <- out
rm(out)
setDT(df)

# ----------------------------
# Rename pollutant columns
# ----------------------------
setnames(df,
         old = c("Benzene_ppb",
                 "Toluene_ppb",
                 "Trimethylbenzene_ppb",
                 "Xylene_ppb",
                 "Hydrogen_Sulfide_ppb",
                 "Hydrogen_Cyanide_ppb"),
         new = c("Benzene",
                 "Toluene",
                 "Trimethylbenzene",
                 "Xylene",
                 "H2S",
                 "HCN"),
         skip_absent = TRUE)

pollutants <- c(
  "Benzene",
  "Toluene",
  "Trimethylbenzene",
  "Xylene",
  "H2S",
  "HCN"
)

# ----------------------------
# Read grid
# ----------------------------
grid <- st_read("/Users/priyanka/Downloads/Suncor/grid5km_generated/grid5km.shp", quiet = TRUE)
grid <- st_transform(grid, 4326)

cc_grid <- st_coordinates(st_centroid(st_geometry(grid)))  # robust: polygon or point grid
grid$latitude  <- cc_grid[,2]
grid$longitude <- cc_grid[,1]

# ----------------------------
# Assign nearest grid id
# ----------------------------
pts_sf <- st_as_sf(df,
                   coords = c("Longitude","Latitude"),
                   crs = 4326,
                   remove = FALSE)

nearest_idx <- st_nearest_feature(pts_sf, grid)
df$grid_id  <- grid$id[nearest_idx]

# ----------------------------
# Keep ONLY necessary columns (prevents crashing)
# ----------------------------
keep_cols <- c("grid_id","ws","wd", pollutants)
df <- df[, ..keep_cols]

# Attach grid center coordinates
grid_coords <- as.data.table(grid)[, .(grid_id = id, latitude, longitude)]
df <- merge(df, grid_coords, by = "grid_id", all.x = TRUE)

# ----------------------------
# Keep grid ids with >= 50k obs
# ----------------------------
cnt <- df[, .N, by = grid_id]
keep_ids <- cnt[N >= 50000, grid_id]

b <- df[grid_id %in% keep_ids]

# ----------------------------
# DIAG + runtime control for NWR
# statistic = "nwr" is kernel regression over RAW observations, so cost is
# ~ n_obs x 10,000 grid points PER CELL PER POLLUTANT. With >=50k obs/cell
# it ran 6+ h without finishing one pollutant. Downsample each cell to
# POLAR_MAXN rows (default 20,000; reproducible seed). The NWR surface is a
# smooth of the wd/ws field, so 20k obs/cell changes it negligibly.
#   POLAR_MAXN=0 Rscript ...   -> no cap (full data, very slow)
#   POLAR_MAXN=50000 ...       -> custom cap
# ----------------------------
message("Grid cells with >= 50k obs: ", length(keep_ids))
print(cnt[N >= 50000][order(-N)])
MAXN <- suppressWarnings(as.integer(Sys.getenv("POLAR_MAXN", "20000")))
if (is.na(MAXN)) MAXN <- 20000L
if (MAXN > 0) {
  set.seed(42)
  n_before <- nrow(b)
  b <- b[, if (.N > MAXN) .SD[sample(.N, MAXN)] else .SD, by = grid_id]
  message(sprintf("Downsampled for NWR: %s -> %s rows (cap %s/cell; POLAR_MAXN=0 to disable)",
                  format(n_before, big.mark = ","), format(nrow(b), big.mark = ","),
                  format(MAXN, big.mark = ",")))
} else message("POLAR_MAXN=0: no downsampling (expect many hours per pollutant)")

# ----------------------------
# Clean wind
# ----------------------------
b[, ws := as.numeric(ws)]
b[, wd := as.numeric(wd)]
b <- b[is.finite(ws) & is.finite(wd) & ws >= 0 & wd >= 0 & wd <= 360]

# ----------------------------
# WINDROSE
# ----------------------------
w<- windroseMap(
  b,
  latitude  = "latitude",
  longitude = "longitude",
  ws.int    = 1,
  static    = TRUE,
  limits    = "fixed",
  cols      = "turbo",
  legend    = TRUE
)

  w <- w +
     ggplot2::coord_sf(
         xlim = c(-105.15, -104.80),
         ylim = c(39.70, 39.95),
         expand = FALSE
    )
  
ggplot2::ggsave("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_wind.jpeg",
     plot = w,
     width = 18,
     height = 10,
     dpi = 300,
     bg = "white")
# ----------------------------
# POLAR MAPS
# ----------------------------
for (p in pollutants) {

  t_p <- Sys.time()
  message("Processing: ", p)

  dpp <- b[is.finite(get(p)) & get(p) >= 0]

  if (nrow(dpp) == 0) {
    message("  Skipping ", p, " (no valid rows)")
    next
  }

  out_file <- paste0(
    "/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_",
    gsub("[^A-Za-z0-9]+","_",p),
    ".jpeg"
  )

   p<- polarMap(
      dpp,
      latitude  = "latitude",
      longitude = "longitude",
      pollutant = p,
      statistic = "nwr",
      static    = TRUE,
      limits    = "fixed",
      legend    = TRUE
    )
  
  p <- p +
     ggplot2::coord_sf(
         xlim = c(-105.15, -104.80),
         ylim = c(39.70, 39.95),
         expand = FALSE
    )
  
ggplot2::ggsave(out_file,
     plot = p,
     width = 18,
     height = 10,
     dpi = 300,
     bg = "white")


  message(sprintf("  Saved: %s  (%.1f min)", out_file,
                  as.numeric(difftime(Sys.time(), t_p, units = "mins"))))
}
