# ==============================================================
# 85_block_assignment_sensitivity.R
# Manuscript section 3.8 and SI S4.5 state that block assignment is sensitive
# to position at the metre scale, because the laboratories drive on roads and
# roads are census-block boundaries. This script measures it on the same
# observations and join as 58_bootstrap_blocks.R / 18 (background-corrected
# benzene, exact coordinates, st_within in the block CRS): the share of
# observations whose block changes when the coordinates are rounded to five
# decimal places (about 1 m), including observations that drop out of, or
# enter, the common-block set.
#   -> TABLE_block_assignment_sensitivity.csv
#   Rscript R_scripts/85_block_assignment_sensitivity.R
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
BASE <- SUNCOR_BASE
load(file.path(BASE, "bgcorrected_out_merge.RData")); df <- as.data.table(df)
df <- df[is.finite(sBenzene) & is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)", .(Longitude, Latitude)]
gc()
g <- st_read(file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"), quiet = TRUE)
idcol <- grep("GEOID", names(g), value = TRUE)[1]
gblk <- g[, idcol]
assign_blocks <- function(lon, lat) {
  pts <- st_transform(st_as_sf(data.table(row_id = seq_along(lon), lon, lat),
                               coords = c("lon", "lat"), crs = 4326), st_crs(g))
  j <- as.data.table(st_drop_geometry(st_join(pts, gblk, join = st_within, left = FALSE)))
  # a point on a shared boundary is kept in every block it touches (as in 18 / 58):
  # summarise each point by its sorted set of blocks
  j[, .(blocks = paste(sort(get(idcol)), collapse = ";")), by = row_id]
}
a <- assign_blocks(df$Longitude, df$Latitude)
b <- assign_blocks(round(df$Longitude, 5), round(df$Latitude, 5))
m <- merge(a, b, by = "row_id", all = TRUE, suffixes = c("_exact", "_round"))
changed <- m[is.na(blocks_exact) | is.na(blocks_round) | blocks_exact != blocks_round]
res <- data.table(n_observations = nrow(df), n_in_common_blocks_exact = nrow(a),
                  n_changed = nrow(changed), pct_changed = 100 * nrow(changed) / nrow(a))
fwrite(res, file.path(BASE, "TABLE_block_assignment_sensitivity.csv"))
print(res)
message("-> ", file.path(BASE, "TABLE_block_assignment_sensitivity.csv"))
