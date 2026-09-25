# ==============================================================
# 23  TRI
# Auto-split from Suncor.Rmd  (section 23 of 40)
#
# TRI.csv is EPA's download for the whole of Colorado and holds one row per
# facility per reporting YEAR (752 rows, 2021-2023). De-duplicating on the
# coordinate pair gives the 284 unique facility locations in the state.
#
# COUNT FIX (2026-09-23): section 2.1 of the manuscript described the study
# domain as containing "752 Toxics Release Inventory facilities", i.e. the raw
# facility-year row count for the whole state. The domain counts are computed
# here instead, so the number in the text is reproducible:
#   - inside the mobile route's bounding box (the "study domain", ~31 x 20 km)
#   - within 1 km and within 3 km of an actual mobile measurement
# Output: TABLE_tri_domain_counts.csv
# ==============================================================

tri <- read.csv("/Users/priyanka/Downloads/Suncor/TRI.csv")
tri <- tri[!duplicated(tri[, c(4, 5)]), ]
write.csv(tri, file = "/Users/priyanka/Downloads/Suncor/TRI_subset.csv")

# ---- domain and proximity counts ------------------------------------------
# These need the wind-merged analysis set, so they are skipped (with a clear
# message, not an error) if this script is run before R02.
suppressPackageStartupMessages({ library(data.table); library(sf) })
BASE <- "/Users/priyanka/Downloads/Suncor"
.mw <- file.path(BASE, "mobile_wswd.RData")
if (!file.exists(.mw)) {
  message("[23] mobile_wswd.RData not found - TRI_subset.csv written, but the ",
          "domain/proximity counts (TABLE_tri_domain_counts.csv, quoted in ",
          "section 2.1) need R02 to have run first. Re-run this script after it.")
  .skip <- TRUE
} else .skip <- FALSE
if (!.skip) local({
load(.mw)                                           # `out`
d <- as.data.table(out); rm(out); gc()
d <- d[is.finite(Latitude) & is.finite(Longitude) &
       Site != "Goodrich Corporation (Collins Aerospace)"]
xy <- st_coordinates(st_transform(st_as_sf(d[, .(Longitude, Latitude)],
        coords = c("Longitude", "Latitude"), crs = 4326), 32613))

t2 <- as.data.table(tri)[is.finite(Latitude) & is.finite(Longitude)]
tp <- st_coordinates(st_transform(st_as_sf(t2[, .(Longitude, Latitude)],
        coords = c("Longitude", "Latitude"), crs = 4326), 32613))

bb <- c(xmin = min(xy[, 1]), xmax = max(xy[, 1]),
        ymin = min(xy[, 2]), ymax = max(xy[, 2]))
inbox <- tp[, 1] >= bb["xmin"] & tp[, 1] <= bb["xmax"] &
         tp[, 2] >= bb["ymin"] & tp[, 2] <= bb["ymax"]

# exact nearest-measurement distance, windowed to +/- 3 km for speed
mind <- vapply(seq_len(nrow(tp)), function(i) {
  k <- which(abs(xy[, 1] - tp[i, 1]) <= 3000 & abs(xy[, 2] - tp[i, 2]) <= 3000)
  if (!length(k)) return(Inf)
  sqrt(min((xy[k, 1] - tp[i, 1])^2 + (xy[k, 2] - tp[i, 2])^2))
}, numeric(1))

res <- data.table(
  quantity = c("TRI facility-year rows in TRI.csv (all Colorado)",
               "unique TRI facility locations (all Colorado)",
               "route bounding box, E-W (km)",
               "route bounding box, N-S (km)",
               "TRI locations inside the route bounding box",
               "TRI locations within 1 km of a mobile measurement",
               "TRI locations within 2 km of a mobile measurement",
               "TRI locations within 3 km of a mobile measurement"),
  value = c(nrow(read.csv(file.path(BASE, "TRI.csv"))), nrow(tp),
            round((bb["xmax"] - bb["xmin"]) / 1000, 1),
            round((bb["ymax"] - bb["ymin"]) / 1000, 1),
            sum(inbox), sum(mind <= 1000), sum(mind <= 2000), sum(mind <= 3000)))
fwrite(res, file.path(BASE, "TABLE_tri_domain_counts.csv"))
print(res)
cat("\nSection 2.1 quotes: inside the domain, within 1 km, within 3 km.\n")
})
