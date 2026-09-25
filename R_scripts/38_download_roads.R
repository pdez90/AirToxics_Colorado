# ==============================================================
# 38  Download Roads
# Auto-split from Suncor.Rmd  (section 38 of 40)
# ==============================================================

#Download Roads

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
require(tigris)
suppressPackageStartupMessages(library(sf))   # st_write (2026-09-22: was missing)
colorado_counties <- counties(state = "CO", year = 2024)
colorado_roads_list <- list()

# Loop through each county in Colorado and download roads
for (county_name in colorado_counties$NAME) {
  # Download roads for the current county
  county_roads <- roads(state = "CO", county = county_name, year = 2024)

  # Store the downloaded roads in the list
  colorado_roads_list[[county_name]] <- county_roads
}

# Combine all the downloaded roads into a single sf object (optional)
all_colorado_roads <- do.call(rbind, colorado_roads_list)

st_write(all_colorado_roads, file.path(SUNCOR_BASE, "all_colorado_roads.gpkg"), append=FALSE)

save(all_colorado_roads, file=file.path(SUNCOR_BASE, "all_colorado_roads.RData"))
