# ==============================================================
# 14  Aggregate stats for each 500 m segment
# Auto-split from Suncor.Rmd  (section 14 of 40)
# ==============================================================

#Aggregate stats for each 500 m segment

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(lubridate)
  library(e1071)   # skewness()
})

# ----------------------------
# 0) Load + basic prep
# ----------------------------
load("/Users/priyanka/Downloads/Suncor/mobile_corrected.RData")
if (exists("out_sf")) out_merge <- out_sf
stopifnot(exists("out_merge"))

DT <- as.data.table(out_merge)

# Ensure day exists and is Date
DT[, day := as.Date(date)]

# raw pollutants: Benzene, Toluene, Trimethylbenzene, Xylene, H2S, HCN
# bg-corrected:   sBenzene, sToluene, sTrimethylbenzene, sXylene, sH2S, sHCN
raw_vars <- c("Benzene","Toluene","Trimethylbenzene","Xylene","H2S","HCN")
cor_vars <- paste0("s", raw_vars)

have_raw <- intersect(raw_vars, names(DT))
have_cor <- intersect(cor_vars, names(DT))

stopifnot("id" %in% names(DT), "day" %in% names(DT))
stopifnot(length(have_raw) > 0)

# ----------------------------
# 1) Helpers (fast + NA-safe)
# ----------------------------
v_median <- function(x) median(x, na.rm = TRUE)
v_mean   <- function(x) mean(x, na.rm = TRUE)
v_sd     <- function(x) sd(x, na.rm = TRUE)
v_var    <- function(x) var(x, na.rm = TRUE)
v_mad    <- function(x) mad(x, na.rm = TRUE)
v_skew   <- function(x) {
  xx <- x[is.finite(x)]
  if (length(xx) < 3) return(NA_real_)
  e1071::skewness(xx, na.rm = TRUE, type = 2)
}
n_nonmiss <- function(x) sum(is.finite(x))

# ----------------------------
# 2) Long-form: summaries pooled over ALL points (across sites)
# ----------------------------
# NOTE: Site is intentionally NOT used in id.vars or grouping.
raw_long <- melt(
  DT,
  id.vars = c("id","day"),
  measure.vars = have_raw,
  variable.name = "pollutant",
  value.name = "value"
)[, version := "raw"]

cor_long <- NULL
if (length(have_cor) > 0) {
  cor_long <- melt(
    DT,
    id.vars = c("id","day"),
    measure.vars = have_cor,
    variable.name = "pollutant",
    value.name = "value"
  )
  cor_long[, pollutant := sub("^s", "", pollutant)]  # sBenzene -> Benzene
  cor_long[, version := "bgcorr"]
}

L <- rbindlist(list(raw_long, cor_long), use.names = TRUE, fill = TRUE)

# pooled summaries per grid cell (id x pollutant x version)
seg_pooled <- L[, .(
  n_total     = .N,
  n_nonmiss   = n_nonmiss(value),
  median      = v_median(value),
  mean        = v_mean(value),
  sd          = v_sd(value),
  var         = v_var(value),
  mad         = v_mad(value),
  skewness    = v_skew(value),
  n_days      = uniqueN(day[is.finite(value)])  # days with any finite value
), by = .(id, pollutant, version)]

# ----------------------------
# 3) DeCarlo approach: daily summaries then across days (across sites)
# ----------------------------
daily <- L[is.finite(value),
           .(daily_mean = mean(value), daily_median = median(value)),
           by = .(id, day, pollutant, version)]

seg_decarlo <- daily[, .(
  mean_of_daily_means     = mean(daily_mean, na.rm = TRUE),
  median_of_daily_medians = median(daily_median, na.rm = TRUE),
  n_days_any              = uniqueN(day)
), by = .(id, pollutant, version)]

# ----------------------------
# 4) Merge pooled + DeCarlo into ONE tidy output
# ----------------------------
seg_long <- merge(
  seg_pooled,
  seg_decarlo,
  by = c("id","pollutant","version"),
  all.x = TRUE
)

# seg_long columns:
# id | pollutant | version | n_total | n_nonmiss | median | mean | sd | var | mad | skewness | n_days
# | mean_of_daily_means | median_of_daily_medians | n_days_any

# ----------------------------
# 5) Join geometry from grid (centroid + lon/lat)
# ----------------------------
grid <- sf::st_read("/Users/priyanka/Downloads/Suncor/Grid_500m_generated/grid_500m.shp", quiet = TRUE) |>
  sf::st_transform(4326)

grid_cent <- sf::st_centroid(grid)
cc <- sf::st_coordinates(grid_cent)
grid$Lon_grid <- cc[,1]
grid$Lat_grid <- cc[,2]

stopifnot("id" %in% names(grid))

seg_long_dt <- as.data.table(seg_long)
grid_dt <- as.data.table(sf::st_drop_geometry(grid))

seg_long_dt <- merge(
  seg_long_dt,
  grid_dt[, .(id, Lon_grid, Lat_grid)],
  by = "id",
  all.x = TRUE
)

# sf for mapping (geometry repeats by pollutant/version)
seg_long_sf <- merge(
  seg_long_dt,
  sf::st_as_sf(grid)[, c("id","geometry")],
  by = "id",
  all.x = TRUE
)
seg_long_sf <- sf::st_as_sf(seg_long_sf)

# ----------------------------
# 6) Wide table (one row per id)
# ----------------------------
seg_long_melt <- melt(
  seg_long_dt,
  id.vars = c("id","pollutant","version","Lon_grid","Lat_grid"),
  measure.vars = setdiff(names(seg_long_dt), c("id","pollutant","version","Lon_grid","Lat_grid")),
  variable.name = "stat",
  value.name = "value"
)

seg_long_melt[, colname := paste(version, pollutant, stat, sep = "_")]

seg_wide <- dcast(
  seg_long_melt,
  id + Lon_grid + Lat_grid ~ colname,
  value.var = "value"
)

seg_wide_sf <- merge(
  as.data.table(seg_wide),
  sf::st_as_sf(grid)[, c("id","geometry")],
  by = "id",
  all.x = TRUE
)
seg_wide_sf <- sf::st_as_sf(seg_wide_sf)

# ----------------------------
# 7) Save outputs
# ----------------------------
save(
  seg_long_dt, seg_long_sf, seg_wide, seg_wide_sf,
  file = "/Users/priyanka/Downloads/Suncor/segment500_summaries_acrossSites.RData"
)

sf::st_write(
  seg_long_sf,
  "/Users/priyanka/Downloads/Suncor/segment500_summaries_long_acrossSites.gpkg",
  append = FALSE, quiet = TRUE
)

sf::st_write(
  seg_wide_sf,
  "/Users/priyanka/Downloads/Suncor/segment500_summaries_wide_acrossSites.gpkg",
  append = FALSE, quiet = TRUE
)
