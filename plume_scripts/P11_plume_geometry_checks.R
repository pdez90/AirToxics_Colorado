# ==============================================================
# P11  Two geometry checks on the four retained WWTP H2S plumes
#
# (a) Does wind geometry discriminate the wastewater facility from the
#     Suncor refinery?  For each retained plume, the bearing from the van at
#     the peak to each source, the HRRR wind direction, and the off-axis
#     angle to each source.  Written 2026-09-27 in response to the review
#     comment that "selection for alignment with wastewater establishes
#     consistency with that source; it does not automatically discriminate
#     between nearby sources".  The refinery coordinates are the TRI facility
#     location for 80022CNCDN5801B in TRI_subset.csv; the refinery is a
#     ~1.5 km site, so the point is a proxy for its centre.
#
# (b) Receptor height.  P08 inverts at z = 1.5 m; the inlets are ~3 m above
#     ground.  Re-evaluate P08's vertical term at both heights for each
#     retained plume, at the stability class, distance and mixing depth the
#     inversion used, and report the change in the inferred rate.  This is
#     the receptor-height test that SI S6.4 previously inferred from the
#     source-height sensitivity instead of running.
#
# Inputs : WWTP_H2S_retained_plumes.csv (P07), TABLE_min_detectable_rate_plumes.csv
#          (46; carries the centreline stability class P08 used),
#          mobile_hrrr.RData, TRI_subset.csv
# Outputs: WWTP_H2S_plume_bearing_check.csv, WWTP_H2S_receptor_height_check.csv
# ==============================================================
suppressPackageStartupMessages(library(data.table))
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
BASE <- SUNCOR_BASE

e <- new.env(); load(file.path(BASE, "mobile_hrrr.RData"), envir = e)
d  <- as.data.table(get(if ("res" %in% ls(e)) "res" else ls(e)[1], envir = e)); rm(e)
rp <- fread(file.path(BASE, "WWTP_H2S_retained_plumes.csv"))
md <- fread(file.path(BASE, "TABLE_min_detectable_rate_plumes.csv"))
tri <- fread(file.path(BASE, "TRI_subset.csv"))

# ---- sources -------------------------------------------------------------
WWTP <- c(lat = 39.81000446758592, lon = -104.95562509611672)     # as in P05/P06/P09/P10
.lat <- grep("^lat", names(tri), ignore.case = TRUE, value = TRUE)[1]
.lon <- grep("^lon", names(tri), ignore.case = TRUE, value = TRUE)[1]
.id  <- names(tri)[vapply(tri, function(x) any(grepl("80022CNCDN5801B", x)), logical(1))][1]
ref_row <- tri[get(.id) == "80022CNCDN5801B"][1]
if (nrow(ref_row) != 1L) stop("Suncor refinery (80022CNCDN5801B) not found in TRI_subset.csv")
REF <- c(lat = as.numeric(ref_row[[.lat]]), lon = as.numeric(ref_row[[.lon]]))
message(sprintf("[SRC] WWTP %.5f, %.5f | refinery (TRI) %.5f, %.5f", WWTP["lat"], WWTP["lon"], REF["lat"], REF["lon"]))

bearing <- function(lat, lon, slat, slon) {            # initial bearing from (lat,lon) to source, deg clockwise from N
  r <- pi / 180; p1 <- lat * r; p2 <- slat * r; dl <- (slon - lon) * r
  x <- sin(dl) * cos(p2); y <- cos(p1) * sin(p2) - sin(p1) * cos(p2) * cos(dl)
  (atan2(x, y) / r + 360) %% 360
}
adiff <- function(a, b) abs(((a - b + 180) %% 360) - 180)
hav <- function(la, lo, LA, LO) { R <- 6371.0088; p_ <- pi / 180
  a <- sin((LA - la) * p_ / 2)^2 + cos(la * p_) * cos(LA * p_) * sin((LO - lo) * p_ / 2)^2; 2 * R * asin(pmin(1, sqrt(a))) }

peak_row <- function(i) {
  t0 <- as.POSIXct(rp$time_at_peak[i], tz = "UTC")
  w  <- d[Asset == rp$Asset[i] & datetime >= as.POSIXct(rp$start_time[i], tz = "UTC") & datetime <= as.POSIXct(rp$end_time[i], tz = "UTC")]
  list(win = w, pk = w[which.min(abs(as.numeric(datetime) - as.numeric(t0)))])
}

# ---- (a) bearings ----------------------------------------------------------
bear <- rbindlist(lapply(seq_len(nrow(rp)), function(i) {
  z <- peak_row(i); pk <- z$pk; w <- z$win
  bw <- bearing(pk$Latitude, pk$Longitude, WWTP["lat"], WWTP["lon"])
  br <- bearing(pk$Latitude, pk$Longitude, REF["lat"], REF["lon"])
  data.table(plume_id = rp$plume_id[i], time_at_peak = rp$time_at_peak[i],
             lat = round(pk$Latitude, 5), lon = round(pk$Longitude, 5),
             hrrr_wind_from_deg = round(pk$winddir, 1), station_wind_from_deg = round(pk$wd, 1),
             bearing_to_WWTP_deg = round(bw, 1), bearing_to_refinery_deg = round(br, 1),
             source_separation_deg = round(adiff(bw, br), 1),
             dist_WWTP_km = round(hav(pk$Latitude, pk$Longitude, WWTP["lat"], WWTP["lon"]), 2),
             dist_refinery_km = round(hav(pk$Latitude, pk$Longitude, REF["lat"], REF["lon"]), 2),
             offaxis_WWTP_deg = round(adiff(pk$winddir, bw), 1),
             offaxis_refinery_deg = round(adiff(pk$winddir, br), 1),
             window_mean_offaxis_WWTP_deg = round(mean(adiff(w$winddir, bearing(w$Latitude, w$Longitude, WWTP["lat"], WWTP["lon"])), na.rm = TRUE), 1),
             window_mean_offaxis_refinery_deg = round(mean(adiff(w$winddir, bearing(w$Latitude, w$Longitude, REF["lat"], REF["lon"])), na.rm = TRUE), 1))
}))
cat("\n== (a) Wind geometry: WWTP vs refinery at the four retained plumes ==\n"); print(bear, row.names = FALSE)
fwrite(bear, file.path(BASE, "WWTP_H2S_plume_bearing_check.csv"))
cat(sprintf("\n  source separation %.0f-%.0f deg; off-axis to WWTP %.0f-%.0f deg; off-axis to refinery %.0f-%.0f deg\n",
            min(bear$source_separation_deg), max(bear$source_separation_deg),
            min(bear$offaxis_WWTP_deg), max(bear$offaxis_WWTP_deg),
            min(bear$offaxis_refinery_deg), max(bear$offaxis_refinery_deg)))
.wa <- file.path(BASE, "TABLE_wind_source_agreement.csv")   # 86_wind_source_agreement.R
.wd_med <- if (file.exists(.wa)) sprintf("%.0f", fread(.wa)$median_wd_difference_deg) else "(run 86 first)"
cat("  Read: the +-10 deg acceptance window (P07) selects plumes aligned with the WWTP; at these\n",
    sprintf(" separations, and with HRRR and station wind directions differing by a median of %s deg\n", .wd_med),
    " (section 3.8), the same geometry is consistent with the refinery. It does not discriminate.\n")

# ---- (b) receptor height ---------------------------------------------------
# P08's vertical term, verbatim (image sources to VERT_N_IMAGES, well-mixed closed form).
VERT_N_IMAGES <- 50; WELL_MIXED_RATIO <- 1.6; H_SRC <- 12.2; MIN_HPBL <- 50
vertical_term <- function(z, H, s, L) {
  v <- exp(-0.5 * ((z - H) / s)^2) + exp(-0.5 * ((z + H) / s)^2)
  for (n in seq_len(VERT_N_IMAGES))
    v <- v + exp(-0.5 * ((z - H + 2 * n * L) / s)^2) + exp(-0.5 * ((z + H + 2 * n * L) / s)^2) +
             exp(-0.5 * ((z - H - 2 * n * L) / s)^2) + exp(-0.5 * ((z + H - 2 * n * L) / s)^2)
  if (s > WELL_MIXED_RATIO * L) v <- sqrt(2 * pi) * s / L
  v
}
sigma_z_pg <- function(CAT, x_km) { X <- x_km * 1000            # Briggs urban, as in P08
  switch(CAT, A = , B = 0.24 * X * (1 + 0.001 * X)^0.5, C = 0.20 * X,
         D = 0.14 * X * (1 + 0.0003 * X)^-0.5, E = , F = 0.08 * X * (1 + 0.0015 * X)^-0.5) }
rh <- rbindlist(lapply(seq_len(nrow(rp)), function(i) {
  m <- md[plume_id == rp$plume_id[i]]
  if (nrow(m) != 1L) stop("plume ", rp$plume_id[i], " not in TABLE_min_detectable_rate_plumes.csv")
  L <- max(m$hpbl_m, MIN_HPBL, H_SRC + 10); s <- sigma_z_pg(m$CAT, m$x_km)
  v1 <- vertical_term(1.5, H_SRC, s, L); v3 <- vertical_term(3.0, H_SRC, s, L)
  data.table(plume_id = rp$plume_id[i], stability_centreline = m$CAT, x_km = round(m$x_km, 2),
             hpbl_m = round(L), sigma_z_m = round(s), V_z1p5 = signif(v1, 6), V_z3p0 = signif(v3, 6),
             Q_ratio_z3_over_z1p5 = signif(v1 / v3, 6), pct_change_in_Q = round(100 * (v1 / v3 - 1), 3))
}))
cat("\n== (b) Receptor height 1.5 m -> 3.0 m: change in inferred rate ==\n"); print(rh, row.names = FALSE)
fwrite(rh, file.path(BASE, "WWTP_H2S_receptor_height_check.csv"))
cat(sprintf("  largest |change| %.3f%%  (Q is proportional to 1/V, so Q(3 m)/Q(1.5 m) = V(1.5)/V(3))\n", max(abs(rh$pct_change_in_Q))))
