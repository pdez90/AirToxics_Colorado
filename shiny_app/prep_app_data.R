# ==============================================================
# prep_app_data.R — builds compact data files for the Shiny explorer
# Run ONCE (rerun after any pipeline rerun):
#   cd /Users/priyanka/Downloads/Suncor/shiny_app
#   Rscript prep_app_data.R
# Reads only reproducible pipeline outputs; writes data/*.rds
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(dplyr)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
OUT  <- file.path(BASE, "shiny_app", "data")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
msg <- function(...) message("[prep] ", ...)

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")

# ---------- 1) raw mobile data -> 500 m cell summaries + event tables ----
msg("loading mobile_wswd (this is the big one)...")
load(file.path(BASE, "mobile_wswd.RData"))          # out
dt <- as.data.table(out); rm(out); gc()
dt <- dt[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]

grid <- st_read(file.path(BASE, "Grid_500m_generated", "grid_500m.shp"), quiet = TRUE)
st_crs(grid) <- 26913
cent_m  <- st_centroid(st_geometry(grid))
cent_ll <- st_coordinates(st_transform(cent_m, 4326))
pts <- st_transform(st_as_sf(dt[, .(Longitude, Latitude)],
                             coords = c("Longitude", "Latitude"), crs = 4326), 26913)
dt[, cell := grid$id[st_nearest_feature(pts, cent_m)]]
cells <- data.table(cell = grid$id, lon = cent_ll[, 1], lat = cent_ll[, 2])

cell_sum <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  s <- dt[is.finite(get(col)),
          .(pollutant = pn, n = .N,
            median = round(median(get(col)), 3),
            p95 = round(quantile(get(col), 0.95), 3),
            max = round(max(get(col)), 2)), by = cell]
  s
}))
cell_sum <- merge(cell_sum, cells, by = "cell")
saveRDS(cell_sum, file.path(OUT, "cells_summary.rds"))
msg("cells_summary.rds: ", nrow(cell_sum), " cell-pollutant rows")

# campaign-level summary stats (below-MDL fractions from Table S3.1)
summ <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]; v <- dt[[col]][is.finite(dt[[col]])]
  data.table(pollutant = pn, n = length(v),
             median = round(median(v), 3), p95 = round(quantile(v, 0.95), 3),
             p99 = round(quantile(v, 0.99), 3), max = round(max(v), 1))
}))
# %<MDL as reported in Table S3.1 (CDPHE flag-based; MDL values with co-authors)
summ[, pct_below_mdl := c(93, 40, 77, 56, 95, 96)[match(pollutant, names(POLLS))]]
saveRDS(summ, file.path(OUT, "summary_stats.rds"))
print(summ)

# exceedance events (>= p95, valid wind) for interactive source probability
events <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  t95 <- quantile(dt[[col]], 0.95, na.rm = TRUE)
  t99 <- quantile(dt[[col]], 0.99, na.rm = TRUE)
  dt[is.finite(get(col)) & get(col) >= t95 & is.finite(wd) & is.finite(ws) & ws > 1,
     .(pollutant = pn, lon = Longitude, lat = Latitude, wd, ws,
       value = get(col), thr95 = t95, thr99 = t99)]
}))
# methane events too
mfile <- file.path(BASE, "mobile_methane_wind_bg.RData")
if (file.exists(mfile)) {
  load(mfile)
  ch4 <- as.data.table(df_ch4_bg)
  t95 <- quantile(ch4$ch4_ppm, 0.95, na.rm = TRUE)
  t99 <- quantile(ch4$ch4_ppm, 0.99, na.rm = TRUE)
  ev4 <- ch4[is.finite(ch4_ppm) & ch4_ppm >= t95 & is.finite(wd) &
             is.finite(ws) & ws > 1 & is.finite(Latitude),
             .(pollutant = "Methane", lon = Longitude, lat = Latitude, wd, ws,
               value = ch4_ppm, thr95 = t95, thr99 = t99)]
  events <- rbind(events, ev4)
  # methane cell summary for raw page
  p4 <- st_transform(st_as_sf(ch4[is.finite(Latitude), .(Longitude, Latitude)],
                              coords = c("Longitude", "Latitude"), crs = 4326), 26913)
  ch4ok <- ch4[is.finite(Latitude)]
  ch4ok[, cell := grid$id[st_nearest_feature(p4, cent_m)]]
  m_sum <- ch4ok[is.finite(ch4_ppm),
                 .(pollutant = "Methane", n = .N, median = round(median(ch4_ppm), 3),
                   p95 = round(quantile(ch4_ppm, 0.95), 3),
                   max = round(max(ch4_ppm), 2)), by = cell]
  m_sum <- merge(m_sum, cells, by = "cell")
  saveRDS(rbind(cell_sum, m_sum), file.path(OUT, "cells_summary.rds"))
  msg("added methane: ", nrow(m_sum), " cells")
}
saveRDS(events, file.path(OUT, "events.rds"))
msg("events.rds: ", nrow(events), " exceedance events (>= p95, wind-valid)")

# ---- sampling coverage (raw-data page) -----------------------------------
stopifnot("date" %in% names(dt))
dd <- as.Date(dt$date)
hh <- as.integer(format(dt$date, "%H"))
ud <- unique(dd)
camp <- list(
  n_days = length(ud),
  first = format(min(ud), "%b %d, %Y"), last = format(max(ud), "%b %d, %Y"),
  wk = table(factor(format(ud, "%a"),
                    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))),
  h_lo = as.integer(quantile(hh, 0.01)), h_hi = as.integer(quantile(hh, 0.99)),
  pct_weekday = round(100 * mean(as.integer(format(dd, "%u")) <= 5), 1))
saveRDS(camp, file.path(OUT, "campaign.rds"))
msg("campaign.rds: ", camp$n_days, " sampling days, ", camp$first, " - ",
    camp$last, " | ", camp$pct_weekday, "% of obs on weekdays | hours ",
    camp$h_lo, "-", camp$h_hi)
print(camp$wk)

# ---- plume-event van positions (looked up while dt is still loaded) ------
pl0 <- fread(file.path(BASE, "FinalFig", "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv"))
pl0 <- pl0[sens_group == "baseline"]
pl0[, datetime := as.character(datetime)]
stopifnot("date" %in% names(dt))
keydt <- format(dt$date, "%Y-%m-%d %H:%M:%S")
h2s_rows <- which(is.finite(dt$Hydrogen_Sulfide_ppb))
tzn <- attr(dt$date, "tzone"); if (is.null(tzn)) tzn <- ""
plume_loc <- rbindlist(lapply(unique(pl0$datetime), function(ts) {
  i <- h2s_rows[keydt[h2s_rows] == ts]
  if (length(i) == 0) {   # fallback: nearest second on the H2S van
    tt <- as.POSIXct(ts, tz = tzn)
    i <- h2s_rows[which.min(abs(as.numeric(dt$date[h2s_rows]) - as.numeric(tt)))]
    msg("  plume ", ts, ": no exact timestamp match; nearest gap = ",
        round(abs(as.numeric(dt$date[i[1]]) - as.numeric(tt))), " s")
  }
  i <- i[1]
  data.table(datetime = ts, lat = dt$Latitude[i], lon = dt$Longitude[i])
}))
msg("plume locations matched: ", nrow(plume_loc))
print(plume_loc)
rm(dt, pts); gc()

# ---------- 2) census-block comparison ----------
g <- st_read(file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"),
             quiet = TRUE)
g <- st_transform(g, 4326)
g$ratio <- ifelse(g$benzene_ppb_airtox > 0,
                  g$sBenzene_med_of_daily_med_scaled / g$benzene_ppb_airtox, NA)
saveRDS(g[, c("benzene_ppb_airtox", "sBenzene_med_of_daily_med_scaled",
              "Population_airtox", "ratio")],
        file.path(OUT, "blocks.rds"))
msg("blocks.rds: ", nrow(g), " common blocks")

# ---------- 3) plumes ----------
pl <- fread(file.path(BASE, "FinalFig", "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv"))
pl <- pl[sens_group == "baseline"]
plumes <- pl[, .(plume_id, datetime = as.character(datetime),
                 dH2S_ppb = round(dH2S_ppb, 1),
                 wind_ms = round(u_ms, 2), dist_km = round(x_km, 2),
                 stability = CAT, rate_tpy = round(tpy_metric, 0))]
plumes <- merge(plumes, plume_loc, by = "datetime", all.x = TRUE, sort = FALSE)
stopifnot(all(is.finite(plumes$lat)))
saveRDS(plumes, file.path(OUT, "plumes.rds"))
msg("plumes.rds: ", nrow(plumes), " retained plumes")

# ---------- 4) hotspots ----------
groups <- fread(file.path(BASE, "MASTER_hotspot_group_index.csv"))
ch4res <- tryCatch(fread(file.path(BASE, "methane_at_toxics_hotspots.csv")),
                   error = function(e) NULL)
if (!is.null(ch4res))
  groups <- merge(groups, ch4res[, .(group_id, ch4_class, pct_ge_p95)],
                  by = "group_id", all.x = TRUE)
percl <- rbindlist(lapply(c("benzene", "toluene", "trimethylbenzene", "xylene",
                            "hydrogen_sulfide", "hydrogen_cyanide"), function(p) {
  f <- file.path(BASE, sprintf("cent_out_%s_persistent.csv", p))
  if (!file.exists(f)) return(NULL)
  d <- fread(f); d[, pollutant := p]; d
}), fill = TRUE)
ch4cl <- tryCatch(fread(file.path(BASE, "cent_out_methane_all.csv")),
                  error = function(e) NULL)
saveRDS(list(groups = groups, clusters = percl, methane = ch4cl),
        file.path(OUT, "hotspots.rds"))
msg("hotspots.rds: ", nrow(groups), " groups, ", nrow(percl), " pollutant clusters")

# ---------- 5) context layers ----------
key <- data.frame(
  name = c("Suncor Energy refinery", "Sinclair Denver Products Terminal",
           "Phillips 66 Denver Terminal", "WWTF1 (Robert W. Hite)",
           "WWTF2 (South Adams County)", "Woodshop",
           "Refuel (Central Park Blvd)", "Refuel (Kipling St)",
           "Refuel (12241 E 104th Ave)", "Refuel (8991 E 104th Ave)"),
  lat = c(39.803333, 39.8724, 39.79668, 39.80822838, 39.87304795, 39.79138244,
          39.79935581, 39.78333858, 39.88606312, 39.88659579),
  lon = c(-104.945556, -104.8861, -104.94236, -104.95532469, -104.91204700,
          -104.94754521, -104.88376425, -105.10918240, -104.84531380, -104.88371420),
  type = c(rep("Covered facility (HB21-1189)", 3), rep("Wastewater treatment", 2),
           "Woodshop", rep("Refueling station", 4)))
tri <- fread(file.path(BASE, "TRI.csv"))
loncol <- grep("^lon", names(tri), ignore.case = TRUE, value = TRUE)[1]
latcol <- grep("^lat", names(tri), ignore.case = TRUE, value = TRUE)[1]
namecol <- grep("name", names(tri), ignore.case = TRUE, value = TRUE)[1]
tri <- tri[is.finite(get(loncol)) & is.finite(get(latcol)),
           .(name = get(namecol), lon = get(loncol), lat = get(latcol))]
wind <- fread(file.path(BASE, "wind_sites.csv"))
wind <- data.frame(lat = wind$Lat_wind, lon = wind$Lon_wind)
wind <- wind[is.finite(wind$lat), ]
lacasa <- data.frame(name = "La Casa (stationary site)",
                     lat = 39.7794, lon = -105.0052)
saveRDS(list(key = key, tri = tri, wind = wind, lacasa = lacasa),
        file.path(OUT, "context.rds"))
msg("context.rds: ", nrow(key), " key facilities + ", nrow(tri), " TRI + ",
    nrow(wind), " wind sites")

msg("DONE. Files in ", OUT, ":")
print(file.info(list.files(OUT, full.names = TRUE))["size"])
