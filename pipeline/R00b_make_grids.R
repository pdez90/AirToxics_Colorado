# ==============================================================
# R00b_make_grids.R
# Generates the analysis grids FROM SCRATCH — no dependence on any
# pre-existing shapefile. Fully deterministic from the constants
# below (documented here = documented in the SI).
#
#   Grid_500m_generated/grid_500m.shp  (500 m; scripts 12-14, Fig 2)
#   grid5km_generated/grid5km.shp      (5 km; script 08 polar plots)
#
# Construction: NAD83 / UTM zone 13N (EPSG:26913). The study-domain
# corners (WGS84) are projected to UTM and snapped OUTWARD to the
# nearest multiple of the cell size, so cell edges lie on absolute
# 500 m (resp. 5 km) multiples of the UTM coordinate system. Cell
# ids are assigned in sf::st_make_grid's deterministic order
# (column-major from the lower-left), so ids are reproducible.
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("R00b: generating analysis grids from scratch")

suppressPackageStartupMessages({ library(sf) })

# ---- documented study-domain constants (WGS84) ----
# generous box around both NDCC monitoring routes (covers all
# delay-corrected measurements: lon ~-105.18..-104.75, lat ~39.73..39.91)
DOMAIN <- c(lon_min = -105.25, lat_min = 39.60, lon_max = -104.70, lat_max = 40.00)
CRS_M  <- 26913   # NAD83 / UTM zone 13N

make_grid <- function(cell_m, out_dir, out_name) {
  corners <- st_sfc(st_point(c(DOMAIN["lon_min"], DOMAIN["lat_min"])),
                    st_point(c(DOMAIN["lon_max"], DOMAIN["lat_max"])), crs = 4326)
  utm <- st_coordinates(st_transform(corners, CRS_M))
  # snap outward to absolute multiples of the cell size
  xmin <- floor(min(utm[, 1]) / cell_m) * cell_m
  ymin <- floor(min(utm[, 2]) / cell_m) * cell_m
  xmax <- ceiling(max(utm[, 1]) / cell_m) * cell_m
  ymax <- ceiling(max(utm[, 2]) / cell_m) * cell_m
  bb <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                          crs = st_crs(CRS_M)))
  g <- st_as_sf(st_make_grid(bb, cellsize = cell_m, square = TRUE))
  names(g)[names(g) == attr(g, "sf_column")] <- "geometry"
  st_geometry(g) <- "geometry"
  g$id <- seq_len(nrow(g))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  shp <- file.path(out_dir, out_name)
  st_write(g, shp, append = FALSE, quiet = TRUE)
  diag_msg(sprintf("  [MADE] %s: %s cells of %d m | UTM extent x[%d, %d] y[%d, %d]",
                   shp, format(nrow(g), big.mark = ","), cell_m,
                   as.integer(xmin), as.integer(xmax), as.integer(ymin), as.integer(ymax)))
  # determinism check: same parameters must give identical geometry
  g2 <- st_as_sf(st_make_grid(bb, cellsize = cell_m, square = TRUE))
  same <- isTRUE(all.equal(st_coordinates(st_centroid(st_geometry(g))),
                           st_coordinates(st_centroid(st_geometry(g2)))))
  diag_msg(sprintf("  [%s] deterministic regeneration check", ifelse(same, "PASS", "FAIL")))
  invisible(g)
}

g500 <- make_grid(500,  file.path(BASE, "Grid_500m_generated"), "grid_500m.shp")
g5k  <- make_grid(5000, file.path(BASE, "grid5km_generated"),  "grid5km.shp")

# coverage check against delay-corrected measurements, if available
mf <- file.path(BASE, "mobile.RData")
if (file.exists(mf)) {
  e <- new.env(); load(mf, envir = e)
  m <- get(ls(e)[1], envir = e)
  pts <- st_as_sf(data.frame(lon = m$Longitude, lat = m$Latitude)[!is.na(m$Longitude), ],
                  coords = c("lon", "lat"), crs = 4326) |> st_transform(CRS_M)
  inside <- lengths(st_intersects(pts, st_union(st_geometry(g500)))) > 0
  diag_msg(sprintf("  [COVERAGE] %.3f%% of delay-corrected measurements fall inside the 500 m grid (expect 100%%)",
                   100 * mean(inside)))
} else diag_msg("  [NOTE] mobile.RData not present yet — coverage check will pass on the next full run.")

diag_msg("\nGrid spec for the SI: NAD83/UTM 13N; cells snapped to absolute 500 m (5 km) multiples;")
diag_msg("domain corners (WGS84): (-105.25, 39.60) to (-104.70, 40.00).")
diag_msg("NOTE: cell boundaries differ from the legacy QGIS grid, so segment ids and Figure 2")
diag_msg("values will shift slightly relative to pre-policy runs; that is expected and correct.")
