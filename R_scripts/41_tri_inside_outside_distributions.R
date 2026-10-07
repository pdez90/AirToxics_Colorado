# ==============================================================
# 41  TRI inside/outside 1 km distributions  (regenerates Figure S4.2)
# The original S4.2 was made interactively (no surviving code); this
# script reproduces it from bg-corrected data: distributions of each
# background-corrected pollutant within 1 km of a TRI facility vs
# beyond, with violins, boxplots, group medians (red), sample sizes,
# and Wilcoxon rank-sum statistics, on a log scale.
# Output: FinalFig/tri_inside_outside_1km_distributions.png
# ==============================================================

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({
  library(dplyr); library(sf); library(data.table); library(ggplot2); library(readr)
})

BASE <- SUNCOR_BASE
RADIUS_M <- 1000
set.seed(42)

message("Loading bg-corrected mobile data + TRI...")
load(file.path(BASE, "bgcorrected_out_merge.RData"))   # expects df
stopifnot(exists("df"))
polls <- c(Benzene = "sBenzene", Toluene = "sToluene", Xylene = "sXylene",
           Trimethylbenzene = "sTrimethylbenzene", H2S = "sH2S", HCN = "sHCN")
polls <- polls[polls %in% names(df)]
stopifnot(length(polls) >= 4)

tri_raw <- suppressMessages(read_csv(file.path(BASE, "TRI.csv"), show_col_types = FALSE))
nm <- names(tri_raw)
loncol <- nm[grepl("^lon", nm, ignore.case = TRUE)][1]
latcol <- nm[grepl("^lat", nm, ignore.case = TRUE)][1]
tri_ok <- tri_raw[is.finite(tri_raw[[loncol]]) & is.finite(tri_raw[[latcol]]), ]
message("  TRI facilities with coordinates: ", nrow(tri_ok))

# distance of every observation to nearest TRI facility (indexed, fast)
pts <- st_transform(st_as_sf(df, coords = c("Longitude", "Latitude"),
                             crs = 4326, remove = FALSE), 32613)
tri_sf <- st_transform(st_as_sf(tri_ok, coords = c(loncol, latcol), crs = 4326), 32613)
message("Computing distance to nearest TRI facility (st_nearest_feature)...")
t0 <- Sys.time()
idx <- st_nearest_feature(pts, tri_sf)
dist_m <- as.numeric(st_distance(pts, tri_sf[idx, ], by_element = TRUE))
message("  done in ", round(difftime(Sys.time(), t0, units = "secs")), " s")

DT <- as.data.table(st_drop_geometry(pts))
DT[, inside := factor(dist_m <= RADIUS_M, levels = c(FALSE, TRUE),
                      labels = c("Outside", "Inside"))]
message("  Inside 1 km: ", format(sum(DT$inside == "Inside"), big.mark = ","),
        " | Outside: ", format(sum(DT$inside == "Outside"), big.mark = ","))

# (2026-09-27) free the full-width tables before melting: the plot
# otherwise exceeds 7 GB of memory on a cloud runner.
DT <- DT[, c("inside", unname(polls)), with = FALSE]
rm(df, pts, idx, dist_m); invisible(gc())
L <- data.table::melt(DT,
                      id.vars = "inside", variable.name = "col", value.name = "value")
# BUGFIX (2026-08-20): the "> 0" truncation was meant for the log-scale
# display, but `stats` below (Wilcoxon W and p, the group medians, n_in and
# n_out written to tri_inside_outside_1km_stats.csv) was computed on the
# truncated set. Background-corrected concentrations are negative more often
# where levels are low - disproportionately Outside - so dropping them raised
# the Outside median and attenuated the inside/outside contrast that is the
# point of the figure. Keep every finite value for the statistics;
# scale_y_log10() below drops the non-positive ones from the DISPLAY only.
L <- L[is.finite(value)]
L[, Pollutant := factor(names(polls)[match(col, polls)],
                        levels = c("Benzene", "Toluene", "Xylene",
                                   "Trimethylbenzene", "H2S", "HCN"))]

# stats per panel
stats <- L[, {
  v_in <- value[inside == "Inside"]; v_out <- value[inside == "Outside"]
  wt <- suppressWarnings(wilcox.test(v_in, v_out))
  .(W = wt$statistic, p = wt$p.value,
    n_in = length(v_in), n_out = length(v_out),
    med_in = median(v_in), med_out = median(v_out))
}, by = Pollutant]
print(stats)
fwrite(stats, file.path(BASE, "tri_inside_outside_1km_stats.csv"))

meds <- L[, .(med = median(value)), by = .(Pollutant, inside)]
ns   <- stats[, .(Pollutant,
                  lab_in = sprintf("n = %s", format(n_in, big.mark = ",")),
                  lab_out = sprintf("n = %s", format(n_out, big.mark = ",")))]
# (2026-09-27) The Wilcoxon statistic is still computed and written to
# tri_inside_outside_1km_stats.csv, but it is no longer printed on the figure:
# it treats autocorrelated one-second observations as independent, so its
# p-value is not a valid significance level (SI section S4.1).
plab <- stats[, .(Pollutant,
                  lab = sprintf("Wilcoxon W = %.3g, p %s", W,
                                ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p))))]

# subsample points for display (full data used for all statistics)
# DISPLAY FLOOR (2026-09-27): background subtraction leaves floating-point
# residues of order 1e-15 ppb that are positive but not real values; on a log
# axis they stretched the benzene and H2S panels over 15 decades and printed
# the tick labels as "0.00". Values below 0.001 ppb are therefore not drawn.
# They remain in every statistic (medians, n, the CSV).
DISPLAY_FLOOR <- 1e-3
Ld <- L[value >= DISPLAY_FLOOR]
ns <- merge(ns, Ld[, .(ymin = min(value)), by = Pollutant], by = "Pollutant")
Lp <- Ld[, if (.N > 2000) .SD[sample(.N, 2000)] else .SD, by = .(Pollutant, inside)]

p <- ggplot(Ld, aes(x = inside, y = value)) +
  geom_jitter(data = Lp, width = 0.28, alpha = 0.05, size = 0.3, color = "grey40") +
  geom_violin(aes(fill = inside), alpha = 0.55, trim = TRUE, linewidth = 0.3) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.9, linewidth = 0.35) +
  geom_point(data = meds, aes(y = med), color = "red", size = 2.4) +
  # n labels: y = -Inf is dropped on a log axis, so they were never drawn;
  # place them at the lowest displayed value of each panel instead.
  geom_text(data = ns, aes(x = 2, y = ymin, label = lab_in),
            vjust = 1.6, size = 2.9, inherit.aes = FALSE) +
  geom_text(data = ns, aes(x = 1, y = ymin, label = lab_out),
            vjust = 1.6, size = 2.9, inherit.aes = FALSE) +
  facet_wrap(~Pollutant, ncol = 2, scales = "free_y",
             labeller = as_labeller(c(Benzene = "A) Benzene", Toluene = "B) Toluene",
                                      Xylene = "C) Xylene", Trimethylbenzene = "D) Trimethylbenzene",
                                      H2S = "E) H\u2082S", HCN = "F) HCN"))) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01, big.mark = ","),
                expand = expansion(mult = c(0.10, 0.05))) +
  scale_fill_manual(values = c(Outside = "steelblue", Inside = "indianred"), guide = "none") +
  labs(x = NULL, y = "Background-corrected mixing ratio (ppb, log scale)",
       caption = sprintf("Inside = within %g km of a TRI facility. Violins and boxes: values >= 0.001 ppb.\nRed dots: medians of all values, including those not drawn.", RADIUS_M / 1000)) +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "grey95", color = NA),
        panel.grid.minor = element_blank())

out_file <- file.path(BASE, "FinalFig", "tri_inside_outside_1km_distributions.png")
ggsave(out_file, p, width = 8.5, height = 8.5, dpi = 600, bg = "white")
message("[Saved] ", out_file)
