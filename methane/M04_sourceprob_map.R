# ==============================================================
# M04_sourceprob_map.R
# Methane source-probability surface, mirroring manuscript
# Section 2.5.3.1 exactly:
#   events >= 99th pct, wind speed > 1 m/s, 15 km upwind rays,
#   150 m discretization, CWT-style weights capped at 5,
#   exponential kernel exp(-d/12000), 150 m grid,
#   Gaussian smoothing (sigma = 900 m), normalized 0-1.
# Outputs: methane_sourceprob.RData + methane_sourceprob_map.png
# ==============================================================

source("/Users/priyanka/Downloads/Suncor/rerun_pipeline/diagnostics_helpers.R")
diag_section("M04: Methane source-probability surface")

suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

load(file.path(BASE, "mobile_methane_wind_bg.RData"))
ch4 <- as.data.table(df_ch4_bg)

p99 <- quantile(ch4$ch4_ppm, 0.99, na.rm = TRUE)
ev <- ch4[ch4_ppm >= p99 & !is.na(wd) & !is.na(ws) & ws > 1 & !is.na(Latitude)]
diag_msg(sprintf("  events: %s at/above p99 (%.3f ppm) with wind > 1 m/s",
                 format(nrow(ev), big.mark = ","), p99))
diag_msg(sprintf("  [FILTER] events lost to missing/calm wind: %.1f%%",
                 100 * (1 - nrow(ev) / max(1, nrow(ch4[ch4_ppm >= p99])))))
if (nrow(ev) == 0) stop("No events with valid wind — check M02 wind-join coverage before rerunning M04.")

# --- upwind back-projection (meteorological wd = direction wind comes FROM)
RAY_KM <- 15; STEP_M <- 150; KERNEL_M <- 12000; W_CAP <- 5; SIGMA_M <- 900; GRID_M <- 150
ctr <- c(lon = median(ev$Longitude), lat = median(ev$Latitude))
to_xy <- function(lon, lat) cbind((lon - ctr["lon"]) * cos(ctr["lat"] * pi/180) * 111320,
                                  (lat - ctr["lat"]) * 110540)
ev_xy <- to_xy(ev$Longitude, ev$Latitude)
ev[, `:=`(x = ev_xy[, 1], y = ev_xy[, 2],
          w = pmin(ch4_ppm / p99, W_CAP),
          theta = (wd) * pi / 180)]        # upwind direction = toward where wind comes from

steps <- seq(STEP_M, RAY_KM * 1000, by = STEP_M)
diag_msg(sprintf("  rays: %d events x %d steps = %s weighted points",
                 nrow(ev), length(steps), format(nrow(ev) * length(steps), big.mark = ",")))

pts <- ev[, {
  dx <- sin(theta) * steps; dy <- cos(theta) * steps
  .(x = x + dx, y = y + dy, w = w * exp(-steps / KERNEL_M))
}, by = seq_len(nrow(ev))]

# --- aggregate to grid
pts[, `:=`(gx = round(x / GRID_M) * GRID_M, gy = round(y / GRID_M) * GRID_M)]
grid <- pts[, .(w = sum(w)), by = .(gx, gy)]

# --- Gaussian smoothing via FFT-free separable kernel on a matrix
xr <- range(grid$gx); yr <- range(grid$gy)
nx <- (xr[2] - xr[1]) / GRID_M + 1; ny <- (yr[2] - yr[1]) / GRID_M + 1
M <- matrix(0, nrow = ny, ncol = nx)
M[cbind((grid$gy - yr[1]) / GRID_M + 1, (grid$gx - xr[1]) / GRID_M + 1)] <- grid$w
k1 <- dnorm(seq(-3 * SIGMA_M, 3 * SIGMA_M, by = GRID_M), sd = SIGMA_M); k1 <- k1 / sum(k1)
smooth1 <- function(v) as.numeric(stats::filter(c(rep(0, length(k1) %/% 2), v, rep(0, length(k1) %/% 2)),
                     k1, sides = 2))[(length(k1) %/% 2 + 1):(length(k1) %/% 2 + length(v))]
M <- apply(M, 2, smooth1); M <- t(apply(M, 1, smooth1))
M[is.na(M)] <- 0; M <- M / max(M)

diag_msg(sprintf("  surface: %d x %d cells; max-probability cell at:", ny, nx))
imax <- which(M == max(M), arr.ind = TRUE)[1, ]
lon_max <- ctr["lon"] + (xr[1] + (imax[2] - 1) * GRID_M) / (cos(ctr["lat"] * pi/180) * 111320)
lat_max <- ctr["lat"] + (yr[1] + (imax[1] - 1) * GRID_M) / 110540
diag_msg(sprintf("    (%.5f, %.5f) — check against known CH4 sources (landfills, WWTFs,", lat_max, lon_max))
diag_msg("     gas distribution, Suncor) and against the H2S surface (Figure 3E)")

# --- save surface + quick map
sourceprob_ch4 <- list(M = M, xr = xr, yr = yr, grid_m = GRID_M, center = ctr, p99 = p99)
save(sourceprob_ch4, file = file.path(BASE, "methane_sourceprob.RData"))

gdf <- as.data.table(expand.grid(gx = seq(xr[1], xr[2], GRID_M), gy = seq(yr[1], yr[2], GRID_M)))
gdf[, prob := as.vector(t(M))]
gdf[, `:=`(lon = ctr["lon"] + gx / (cos(ctr["lat"] * pi/180) * 111320),
           lat = ctr["lat"] + gy / 110540)]
p <- ggplot(gdf[prob > 0.02], aes(lon, lat, fill = prob)) +
  geom_raster() + scale_fill_viridis_c(option = "inferno", name = "Relative\nprobability") +
  coord_quickmap() + theme_minimal() +
  labs(title = "Methane source-probability surface (99th pct events, 15 km upwind rays)",
       subtitle = sprintf("p99 = %.3f ppm | kernel exp(-d/12 km) | sigma = 900 m", p99),
       x = NULL, y = NULL)
ggsave(file.path(BASE, "methane_sourceprob_map.png"), p, width = 9, height = 7.5, dpi = 300, bg = "white")
diag_msg("\nSaved: methane_sourceprob.RData, methane_sourceprob_map.png")
