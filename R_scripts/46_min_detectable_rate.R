# ==============================================================
# 46  MINIMUM DETECTABLE EMISSION RATE (SI Section S6)
# Propagates the H2S detection limit through the SAME Gaussian plume
# model used for the WWTP inversion (P08: PG sigmas, 5-term vertical
# with reflections, H = 12.2 m, z = 1.5 m, centerline y = 0):
#   Q_min(x, stability, u) = rate producing a peak enhancement equal
#   to the H2S MDL at the mobile platform.
# MDL cases: 4, 5, 6 ppb (CDPHE audit MDLs for the Picarro G2204,
# CAT 5-6 ppb / EMU 2-5 ppb depending on period; central case 5 ppb).
# Outputs:
#   TABLE_min_detectable_rate_grid.csv    (full grid)
#   TABLE_min_detectable_rate_plumes.csv  (at the 4 retained plumes)
#   FinalFig/FIG_min_detectable_rate.png
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"

# ---- constants copied from P08 (do not change) ----------------
z_m <- 1.5
H_m <- 12.2
MW_H2S <- 34.08
mol_m3_air <- 2.7e25 / 6.022e23
seconds_per_year <- 365.25 * 24 * 3600

sigma_y_pg <- function(CAT, x_km) {
  X <- pmax(x_km, 1e-6) * 1000
  fcase(CAT %in% c("A","B"), 0.32*X*(1+(0.0004*X))^(-0.5),
        CAT == "C",          0.22*X*(1+(0.0004*X))^(-0.5),
        CAT == "D",          0.16*X*(1+(0.0004*X))^(-0.5),
        CAT %in% c("E","F"), 0.11*X*(1+(0.0004*X))^(-0.5),
        default = NA_real_)
}
sigma_z_pg <- function(CAT, x_km) {
  X <- pmax(x_km, 1e-6) * 1000
  fcase(CAT %in% c("A","B"), 0.24*X*(1+(0.001*X))^(0.5),
        CAT == "C",          0.20*X,
        CAT == "D",          0.14*X*(1+(0.0003*X))^(-0.5),
        CAT %in% c("E","F"), 0.08*X*(1+(0.00015*X))^(-0.5),
        default = NA_real_)
}
vert_term <- function(z, H, sigz, hpbl) {
  a <- exp(-0.5 * ((z - H)^2) / sigz^2)
  b <- exp(-0.5 * ((z + H)^2) / sigz^2)
  c2 <- exp(-0.5 * ((z + H - 2*hpbl)^2) / sigz^2)
  d <- exp(-0.5 * ((z - H - 2*hpbl)^2) / sigz^2)
  e <- exp(-0.5 * ((z - H + 2*hpbl)^2) / sigz^2)
  a + b + c2 + d + e
}

qmin_tpy <- function(mdl_ppb, x_km, CAT, u_ms, hpbl_m) {
  sigy <- sigma_y_pg(CAT, x_km); sigz <- sigma_z_pg(CAT, x_km)
  denom <- vert_term(z_m, H_m, sigz, hpbl_m)      # y = 0 -> crosswind = 1
  Q_ppm_m3_s <- ((mdl_ppb / 1000) * u_ms * (2 * pi * sigy * sigz)) / denom
  kg_s <- Q_ppm_m3_s * mol_m3_air * 1e-6 * (MW_H2S / 1000)
  kg_s * seconds_per_year / 1000
}

# ---- 1) grid over distance x stability x wind -----------------
HPBL0 <- 1500   # nominal mixed-layer depth (m); reflections negligible near-field
grid <- CJ(x_km = seq(0.25, 5, by = 0.05), CAT = c("B", "C", "D"),
           u_ms = c(2, 3.2, 5), mdl_ppb = c(4, 5, 6))
grid[, qmin_tpy := qmin_tpy(mdl_ppb, x_km, CAT, u_ms, HPBL0)]
stopifnot(all(is.finite(grid$qmin_tpy)), all(grid$qmin_tpy > 0))
fwrite(grid, file.path(BASE, "TABLE_min_detectable_rate_grid.csv"))
message("Grid: ", nrow(grid), " rows. Q_min at 2 km, stability D, 3.2 m/s, MDL 5: ",
        round(grid[x_km == 2 & CAT == "D" & u_ms == 3.2 & mdl_ppb == 5, qmin_tpy]),
        " t/yr")
print(dcast(grid[x_km %in% c(0.5, 1, 2, 3, 4) & mdl_ppb == 5],
            x_km ~ CAT + u_ms, value.var = "qmin_tpy", fun.aggregate = mean))

# ---- 2) at the 4 retained plumes' actual conditions -----------
pl <- fread(file.path(BASE, "FinalFig",
                      "WWTP_H2S_inversion_all_scenarios_METRIC_TPY.csv"))
pl <- pl[sens_group == "baseline"]
pt <- pl[, .(plume_id, datetime, dH2S_ppb, u_ms, x_km, CAT, hpbl_m,
             inferred_tpy = round(tpy_metric))]
pt[, qmin_mdl5 := round(qmin_tpy(5, x_km, CAT, u_ms, hpbl_m))]
pt[, qmin_mdl4 := round(qmin_tpy(4, x_km, CAT, u_ms, hpbl_m))]
pt[, qmin_mdl6 := round(qmin_tpy(6, x_km, CAT, u_ms, hpbl_m))]
pt[, snr_vs_mdl5 := round(dH2S_ppb / 5, 1)]
fwrite(pt, file.path(BASE, "TABLE_min_detectable_rate_plumes.csv"))
print(pt)

# ---- 3) figure ------------------------------------------------
rib <- grid[, .(lo = min(qmin_tpy), hi = max(qmin_tpy),
                mid = qmin_tpy[mdl_ppb == 5]), by = .(x_km, CAT, u_ms)]
rib[, u_lab := factor(sprintf("u = %.1f m/s", u_ms),
                      levels = sprintf("u = %.1f m/s", sort(unique(u_ms))))]
p <- ggplot(rib, aes(x_km, mid, color = CAT, fill = CAT)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = c(500, 2529), linetype = 3, color = "grey30") +
  annotate("text", x = 0.3, y = 500, vjust = -0.5, hjust = 0, size = 3,
           label = "smallest inferred WWTF rate (500 t/yr)", color = "grey30") +
  annotate("text", x = 0.3, y = 2529, vjust = -0.5, hjust = 0, size = 3,
           label = "largest inferred WWTF rate (2,529 t/yr)", color = "grey30") +
  geom_point(data = data.frame(x_km = pt$x_km, mid = pt$qmin_mdl5,
                               CAT = pt$CAT,
                               u_lab = sprintf("u = %.1f m/s",
                                 c(2, 3.2, 5)[pmin(3, pmax(1, findInterval(
                                   pt$u_ms, c(0, 2.6, 4.1, Inf))))])),
             aes(shape = "Retained plume conditions"), size = 2.6,
             stroke = 1, color = "black", fill = NA) +
  scale_shape_manual(values = c("Retained plume conditions" = 4), name = NULL) +
  scale_y_log10(labels = label_number(big.mark = ","),
                breaks = c(10, 30, 100, 300, 1000, 3000, 10000)) +
  scale_color_brewer(palette = "Set1", name = "Stability class") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  facet_wrap(~u_lab) +
  labs(x = "Downwind distance from source (km)",
       y = expression("Minimum detectable H"[2]*"S emission rate (t yr"^-1*", log scale)"),
       caption = "Solid lines: MDL = 5 ppb; shaded bands span MDL = 4-6 ppb. Same Gaussian formulation as the WWTF inversion (PG sigmas, 5-term vertical reflection, H = 12.2 m, z = 1.5 m, centerline receptor). X marks: conditions of the four retained plumes.") +
  theme_bw(base_size = 12) +
  theme(plot.caption = element_text(size = 8, hjust = 0),
        legend.position = "bottom")
ggsave(file.path(BASE, "FinalFig", "FIG_min_detectable_rate.png"),
       p, width = 11, height = 5.6, dpi = 400, bg = "white")
message("[Saved] FinalFig/FIG_min_detectable_rate.png")
message("DONE.")
