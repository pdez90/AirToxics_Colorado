# ==============================================================
# 32  Identifying close to TRI
# Auto-split from Suncor.Rmd  (section 32 of 40)
# ==============================================================

#Identifying close to TRI

# ============================================================
# Add nearest TRI facility to MASTER_hotspot_group_index.csv
# - Robust TRI column detection (incl. "TRI Facility ID/Name")
# - Computes nearest facility by straight-line distance (meters)
# - Updates: tri_dist_m, tri_dist_km, tri_id, tri_name
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
})

# ---- paths
f_tri    <- "/Users/priyanka/Downloads/Suncor/TRI.csv"
f_master <- "/Users/priyanka/Downloads/Suncor/hotspot_group_reports/MASTER_hotspot_group_index.csv"
out_master_updated <- "/Users/priyanka/Downloads/Suncor/hotspot_group_reports/MASTER_hotspot_group_index_with_TRI.csv"

# ============================================================
# 1) Load MASTER (hotspot groups)
# ============================================================
stopifnot(file.exists(f_master))
master <- readr::read_csv(f_master, show_col_types = FALSE)

need_master <- c("group_id", "Longitude", "Latitude")
miss_m <- setdiff(need_master, names(master))
if (length(miss_m) > 0) stop("[MASTER] Missing columns: ", paste(miss_m, collapse = ", "))

master_sf_ll <- sf::st_as_sf(master, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
master_sf_m  <- sf::st_transform(master_sf_ll, 32613)  # meters (UTM 13N)

message("[MASTER] Loaded groups: ", nrow(master_sf_m))

# ============================================================
# 2) Load TRI as sf (robust column detection)
# ============================================================
tri_sf_m <- NULL
tri_id_col <- NA_character_
tri_name_col <- NA_character_

stopifnot(file.exists(f_tri))
tri_raw <- readr::read_csv(f_tri, show_col_types = FALSE)
nm <- names(tri_raw)

# lon/lat candidates (your TRI has "Latitude"/"Longitude")
lon_candidates <- c("Longitude","longitude","LON","lon","LONGITUDE","Long","X","x")
lat_candidates <- c("Latitude","latitude","LAT","lat","LATITUDE","Lat","Y","y")

lon_col <- lon_candidates[lon_candidates %in% nm][1]
lat_col <- lat_candidates[lat_candidates %in% nm][1]

if (is.na(lon_col) || is.na(lat_col)) {
  stop("[TRI] Could not find lon/lat columns. Available columns:\n", paste(nm, collapse = ", "))
}

# ID/name candidates (includes your exact columns with spaces)
id_candidates <- c(
  "TRI Facility ID", "TRI_Facility_ID", "TRIFID", "TRI_ID", "TRIID",
  "FacilityID", "FACILITY_ID", "REGISTRY_ID", "FRS ID", "FRS_ID", "ID"
)
name_candidates <- c(
  "TRI Facility Name", "TRI_Facility_Name", "FACILITY_NAME", "FacilityName",
  "NAME", "Name", "FACILITY"
)

tri_id_col   <- id_candidates[id_candidates %in% nm][1]
tri_name_col <- name_candidates[name_candidates %in% nm][1]

# keep only rows with finite coordinates
tri_raw2 <- tri_raw %>%
  dplyr::mutate(
    .lon = suppressWarnings(as.numeric(.data[[lon_col]])),
    .lat = suppressWarnings(as.numeric(.data[[lat_col]]))
  ) %>%
  dplyr::filter(is.finite(.lon), is.finite(.lat))

tri_sf_ll <- sf::st_as_sf(tri_raw2, coords = c(".lon",".lat"), crs = 4326, remove = FALSE)
tri_sf_m  <- sf::st_transform(tri_sf_ll, 32613)

message(
  "[TRI] Loaded facilities: ", nrow(tri_sf_m),
  " | lon=", lon_col, " lat=", lat_col,
  " | id=", ifelse(is.na(tri_id_col), "NA", tri_id_col),
  " | name=", ifelse(is.na(tri_name_col), "NA", tri_name_col)
)

# ============================================================
# 3) Nearest TRI for each group
# ============================================================
nearest_tri_one <- function(pt_m) {
  d <- as.numeric(sf::st_distance(tri_sf_m, pt_m))
  k <- which.min(d)

  tri_id <- NA_character_
  tri_name <- NA_character_

  if (!is.na(tri_id_col) && tri_id_col %in% names(tri_sf_m)) {
    tri_id <- as.character(sf::st_drop_geometry(tri_sf_m[k, , drop = FALSE])[[tri_id_col]][1])
  }
  if (!is.na(tri_name_col) && tri_name_col %in% names(tri_sf_m)) {
    tri_name <- as.character(sf::st_drop_geometry(tri_sf_m[k, , drop = FALSE])[[tri_name_col]][1])
  }

  list(tri_dist_m = d[k], tri_id = tri_id, tri_name = tri_name)
}

tri_out <- lapply(seq_len(nrow(master_sf_m)), function(i) {
  pt <- master_sf_m[i, ]
  ans <- nearest_tri_one(pt)
  data.frame(
    group_id    = master_sf_m$group_id[i],
    tri_dist_m  = ans$tri_dist_m,
    tri_dist_km = ans$tri_dist_m / 1000,
    tri_id      = ans$tri_id,
    tri_name    = ans$tri_name,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# ============================================================
# 4) Join back + write
# ============================================================
master_updated <- master %>%
  dplyr::select(-dplyr::any_of(c("tri_dist_m","tri_dist_km","tri_id","tri_name"))) %>%
  dplyr::left_join(tri_out, by = "group_id") %>%
  dplyr::arrange(group_id)
# quick sanity prints
message("\n[CHECK] Any missing tri_dist_m? ", sum(!is.finite(master_updated$tri_dist_m)))
message("[CHECK] Example rows:")
print(head(dplyr::select(master_updated,
                         group_id, tri_dist_m, tri_id, tri_name), 10))

readr::write_csv(master_updated, out_master_updated)
message("\nWrote updated MASTER with TRI:\n  ", out_master_updated)
