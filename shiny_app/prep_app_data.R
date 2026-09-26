# ==============================================================
# prep_app_data.R — builds compact data files for the Shiny explorer
# Run ONCE (rerun after any pipeline rerun):
#   cd "$SUNCOR_BASE"/shiny_app       (SUNCOR_BASE defaults to ~/Downloads/Suncor)
#   Rscript prep_app_data.R
# Reads only reproducible pipeline outputs; writes data/*.rds
# ==============================================================

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({
  library(data.table); library(sf); library(dplyr)
})

BASE <- SUNCOR_BASE
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

# TRI DOMAIN BOX (2026-09-25). The context map used to plot every row of
# TRI.csv - 752 facility-YEAR records for the whole of Colorado - while the
# panel text claims "all TRI facilities in the domain". Section 2.1 of the
# manuscript quotes 67: TRI.csv de-duplicated on coordinates (284 statewide
# locations, written by 23_tri.R as TRI_subset.csv) and restricted to the
# mobile route's bounding box. Capture that box here, while the full record is
# still in memory, and apply it where the context layer is built below.
TRI_BB <- st_bbox(pts)
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
# %<MDL: read from TABLE_S3.1.csv (written by 70_table_s31.R) rather than
# hard-coded, so the app cannot drift from the SI table. Falls back to the
# published values if the file is absent.
s31f <- file.path(BASE, "TABLE_S3.1.csv")
if (file.exists(s31f)) {
  s31 <- fread(s31f)
  key <- c(Benzene = "Benzene", Toluene = "Toluene",
           Trimethylbenzene = "Trimethylbenzene", Xylene = "Xylene",
           H2S = "H2S", HCN = "HCN")
  summ[, pct_below_mdl := round(s31$pct_belowMDL[match(key[pollutant], s31$pollutant)])]
  msg("below-MDL fractions read from TABLE_S3.1.csv")
} else {
  summ[, pct_below_mdl := c(93, 39, 76, 56, 98, 96)[match(pollutant, names(POLLS))]]
  warning("TABLE_S3.1.csv not found - using published below-MDL fractions")
}
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

# ---- daily tracks for the day-by-day animation slider --------------------
tr <- dt[, .(Longitude = round(Longitude, 4), Latitude = round(Latitude, 4),
             day = as.Date(date),
             Route = ifelse(grepl("Suncor", Site),
                            "Suncor & Phillips 66", "Sinclair Terminal"))]
tr <- tr[, .SD[seq(1, .N, by = max(1L, .N %/% 1500))], by = .(day, Route)]
saveRDS(tr, file.path(OUT, "daily_tracks.rds"))
msg("daily_tracks.rds: ", nrow(tr), " thinned points across ",
    uniqueN(tr$day), " days")

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

# ---------- 4b) methane ----------
# Methane is a SECONDARY analysis. The Picarro methane channel is not part of
# CDPHE's QA/QC'd public air-toxics repository and was not routinely calibrated
# over the campaign, so every methane number in the app is relative: where
# methane is elevated against its own local background, and which toxics
# hotspots it coincides with. Page 6 states that at the top of the page rather
# than in a footnote. This block only assembles what that page needs; each
# piece is optional, so a tree without the methane outputs still builds a
# working app.
ch4_sum  <- tryCatch(fread(file.path(BASE, "methane_hotspot_summary.csv")),
                     error = function(e) NULL)
ch4_pers <- tryCatch(fread(file.path(BASE, "cent_out_methane_persistent.csv")),
                     error = function(e) NULL)
if (!is.null(ch4_sum) || !is.null(ch4_pers) || !is.null(ch4res)) {
  saveRDS(list(summary = ch4_sum, persistent = ch4_pers, at_hotspots = ch4res),
          file.path(OUT, "methane.rds"))
  msg("methane.rds: ", if (is.null(ch4_sum)) 0L else nrow(ch4_sum), " summary row(s), ",
      if (is.null(ch4_pers)) 0L else nrow(ch4_pers), " persistent cluster(s), ",
      if (is.null(ch4res)) 0L else nrow(ch4res), " toxics groups carrying a CH4 class")
} else msg("methane.rds: skipped (no methane outputs found)")

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
# TRI_subset.csv is TRI.csv de-duplicated on coordinates by 23_tri.R; fall back
# to TRI.csv with the same de-duplication if 23 has not been run.
.trif <- file.path(BASE, "TRI_subset.csv")
if (file.exists(.trif)) {
  tri <- fread(.trif)
} else {
  message("[prep] TRI_subset.csv absent - de-duplicating TRI.csv here (run 23_tri.R)")
  tri <- fread(file.path(BASE, "TRI.csv"))
  tri <- tri[!duplicated(tri[, .(Latitude, Longitude)])]
}
loncol <- grep("^lon", names(tri), ignore.case = TRUE, value = TRUE)[1]
latcol <- grep("^lat", names(tri), ignore.case = TRUE, value = TRUE)[1]
namecol <- grep("name", names(tri), ignore.case = TRUE, value = TRUE)[1]
tri <- tri[is.finite(get(loncol)) & is.finite(get(latcol)),
           .(name = get(namecol), lon = get(loncol), lat = get(latcol))]
.n_state <- nrow(tri)
.tp <- st_coordinates(st_transform(st_as_sf(tri[, .(lon, lat)],
         coords = c("lon", "lat"), crs = 4326), 26913))
tri <- tri[.tp[, 1] >= TRI_BB[["xmin"]] & .tp[, 1] <= TRI_BB[["xmax"]] &
           .tp[, 2] >= TRI_BB[["ymin"]] & .tp[, 2] <= TRI_BB[["ymax"]]]
msg("TRI: ", .n_state, " unique statewide locations -> ", nrow(tri),
    " inside the route bounding box (section 2.1 quotes 67)")
wind <- fread(file.path(BASE, "wind_sites.csv"))
wind <- data.frame(lat = wind$Lat_wind, lon = wind$Lon_wind)
wind <- wind[is.finite(wind$lat), ]
lacasa <- data.frame(name = "La Casa (stationary site)",
                     lat = 39.7794, lon = -105.0052)
saveRDS(list(key = key, tri = tri, wind = wind, lacasa = lacasa),
        file.path(OUT, "context.rds"))
msg("context.rds: ", nrow(key), " key facilities + ", nrow(tri), " TRI + ",
    nrow(wind), " wind sites")

# ---------- 6) screening health-hazard tables (SI section S7) ----------
# Chronic hazard quotients / organ-system hazard indices on the census-block
# basis (74_health_hazard_screening.R) and the companion acute screen, plus the
# 500 m cell-resolved hazard indices used as the S7.3 sensitivity check
# (73_cumulative_risk.R) and the four temporal-scaling scenarios of S7.4
# (77_health_scaling_sensitivity.R). Every block-level value is read from the
# written tables - nothing is recomputed here - so the app cannot drift from
# the SI. The one place this block does arithmetic is the 500 m cell surface,
# which S7.4 never tabulates per cell: there it applies the factors that 77
# WROTE to the ppb means that 73 WROTE, which is exact because HQ is linear in
# concentration, and checks itself against 73's own scaled column below.
f71   <- file.path(BASE, "TABLE_S7.1_chronic_hazard.csv")
f72   <- file.path(BASE, "TABLE_S7.2_acute_screen.csv")
f73   <- file.path(BASE, "TABLE_S7.3_scaling_scenarios.csv")
f73b  <- file.path(BASE, "TABLE_S7.3b_scaling_by_pollutant.csv")
f74b  <- file.path(BASE, "TABLE_S7.4_breakeven_factors.csv")
fcell <- file.path(BASE, "TABLE_cumulative_HQ_by_cell.csv")
if (file.exists(f71) && file.exists(f72)) {
  chronic <- fread(f71)
  acute   <- fread(f72)

  # organ-system hazard indices = sum of the HQs of pollutants sharing an organ
  hi <- chronic[, .(pollutants = paste(pollutant, collapse = ", "),
                    HI_pwmean  = sum(HQ_pwmean),
                    HI_maxblock = sum(HQ_maxblock)), by = target_organ]
  setorder(hi, -HI_pwmean)

  # ---- S7.4 scaling scenarios (organ level, per-pollutant level, break-even)
  scen      <- if (file.exists(f73))  fread(f73)  else NULL
  scen_poll <- if (file.exists(f73b)) fread(f73b) else NULL
  brk       <- if (file.exists(f74b)) fread(f74b) else NULL
  if (is.null(scen_poll))
    warning("TABLE_S7.3b_scaling_by_pollutant.csv not found - run ",
            "77_health_scaling_sensitivity.R; the app's scaling toggle will ",
            "fall back to the unscaled baseline only")

  # ---- per-cell hazard indices by organ system ------------------------------
  # NOTE ON THE BASELINE. 73 writes EC/HQ columns that ALREADY carry the La
  # Casa factor for the three measured aromatics (scale_factor in that file),
  # so its HQ_mean is a scenario-B-like surface, not the unscaled baseline the
  # block tables use. Reading it straight was how the map and the sidebar came
  # to sit on two different bases with nothing in the app saying so. The
  # unscaled HQ is recovered from the written ppb mean and RfC, and every
  # scenario is then built from that one baseline.
  hcells <- NULL
  if (file.exists(fcell) && exists("cells")) {
    hq <- fread(fcell)
    if (all(c("cell", "tos", "HQ_mean", "mean_ppb", "rfc", "pollutant",
              "scale_factor", "n_days") %in% names(hq))) {
      hq[, HQ_unscaled := mean_ppb / rfc]

      # CHECK: factor x unscaled must reproduce 73's own scaled HQ column
      .chk <- hq[is.finite(scale_factor) & is.finite(HQ_mean) & is.finite(HQ_unscaled),
                 .(rel = max(abs(HQ_unscaled * scale_factor - HQ_mean) /
                             pmax(abs(HQ_mean), .Machine$double.eps))), by = pollutant]
      for (i in seq_len(nrow(.chk)))
        msg("  cell baseline check ", .chk$pollutant[i], ": rel.diff ",
            sprintf("%.2e", .chk$rel[i]), if (.chk$rel[i] < 1e-8) "  OK" else "  MISMATCH")
      if (nrow(.chk) && max(.chk$rel) >= 1e-8)
        warning("recovered unscaled cell HQ disagrees with 73's scaled column")

      if (!is.null(scen_poll)) {
        # 73 and 77 name two species differently; map explicitly so a renamed
        # species fails loudly here rather than silently dropping out of a
        # hazard index.
        NAMEMAP <- c(Benzene = "Benzene", Toluene = "Toluene",
                     Xylene = "Xylenes",
                     Trimethylbenzene = "1,2,4-Trimethylbenzene",
                     H2S = "H2S", HCN = "HCN")
        ORGANMAP <- c(`Hematological/Immunological` = "Hematological",
                      Neurological = "Neurological",
                      Respiratory = "Respiratory", Endocrine = "Endocrine")
        .miss <- setdiff(unique(hq$pollutant), names(NAMEMAP))
        if (length(.miss)) stop("unmapped pollutant in the cell table: ",
                                paste(.miss, collapse = ", "))
        .miss <- setdiff(unique(hq$tos), names(ORGANMAP))
        if (length(.miss)) stop("unmapped target organ in the cell table: ",
                                paste(.miss, collapse = ", "))
        hq[, `:=`(pname = NAMEMAP[pollutant], oname = ORGANMAP[tos])]
        .miss <- setdiff(unique(hq$pname), unique(scen_poll$pollutant))
        if (length(.miss)) stop("cell species absent from S7.3b: ",
                                paste(.miss, collapse = ", "))

        fac <- unique(scen_poll[, .(scenario, pollutant, factor)])
        hcells <- merge(
          hq[is.finite(HQ_unscaled), .(cell, pname, oname, n_days, HQ_unscaled)],
          fac, by.x = "pname", by.y = "pollutant", allow.cartesian = TRUE)
        hcells <- hcells[, .(HI = round(sum(HQ_unscaled * factor), 4),
                             pollutants = paste(sort(unique(pname)), collapse = ", "),
                             n_days = max(n_days, na.rm = TRUE)),
                         by = .(scenario, cell, organ = oname)]
        hcells <- merge(hcells, cells, by = "cell")
      } else {
        hcells <- hq[is.finite(HQ_unscaled),
                     .(HI = round(sum(HQ_unscaled), 4),
                       pollutants = paste(sort(unique(pollutant)), collapse = ", "),
                       n_days = max(n_days, na.rm = TRUE)),
                     by = .(cell, organ = tos)]
        hcells <- merge(hcells, cells, by = "cell")
      }
    }
  }
  saveRDS(list(chronic = chronic, acute = acute, hi = hi, cells = hcells,
               scen = scen, scen_poll = scen_poll, breakeven = brk),
          file.path(OUT, "hazard.rds"))
  msg("hazard.rds: ", nrow(chronic), " pollutants, ", nrow(hi),
      " organ systems, ",
      if (is.null(hcells)) 0 else nrow(hcells), " cell-organ rows, ",
      if (is.null(scen)) 0 else uniqueN(scen$scenario), " scaling scenarios")
  print(hi)
  if (!is.null(scen)) print(dcast(scen, organ ~ scenario, value.var = "HI_pwmean"))
} else {
  warning("S7 hazard tables not found - hazard.rds not written, ",
          "app page 7 will be hidden")
}

# ---------- 7) sync into the repo copy that actually deploys ----------
# OUT is the working folder; Posit Connect publishes from the git repo, so a
# refresh that stopped here would leave the deployed app on old data (this is
# exactly what happened between 2026-08-17 and 2026-08-24). Copy across
# whenever the repo working tree is present.
REPO_OUT <- file.path(BASE, "AirToxics_Colorado", "shiny_app", "data")
if (dir.exists(dirname(REPO_OUT))) {
  dir.create(REPO_OUT, showWarnings = FALSE, recursive = TRUE)
  src <- list.files(OUT, pattern = "\\.rds$", full.names = TRUE)
  ok <- file.copy(src, REPO_OUT, overwrite = TRUE)
  msg("synced ", sum(ok), "/", length(src), " .rds files into the repo copy: ",
      REPO_OUT)
  if (!all(ok)) warning("some files failed to copy into the repo copy")
} else {
  warning("repo working tree not found - remember to copy ", OUT,
          " into shiny_app/data in the repo before pushing")
}

msg("DONE. Files in ", OUT, ":")
print(file.info(list.files(OUT, full.names = TRUE))["size"])
