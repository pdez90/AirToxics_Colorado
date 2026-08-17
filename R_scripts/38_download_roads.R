# ==============================================================
# 38  Download Roads
# Auto-split from Suncor.Rmd  (section 38 of 40)
# ==============================================================

#Download Roads

require(tigris)
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

st_write(all_colorado_roads, "/Users/priyanka/Downloads/Suncor/all_colorado_roads.gpkg", append=FALSE)

save(all_colorado_roads, file="/Users/priyanka/Downloads/Suncor/all_colorado_roads.RData")
