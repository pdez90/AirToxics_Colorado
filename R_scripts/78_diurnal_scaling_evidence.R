# ==============================================================
# 78  CAN AN AROMATIC SCALING FACTOR BE BORROWED FOR H2S AND HCN?
#     (SI Section S7.4; evidence behind the recommendation in 77)
#
# A La Casa bin-weighted factor is (24/7 mean) / (mobile-bin-weighted mean).
# It exceeds 1 precisely for species whose concentrations are LOW during the
# hours the van was on the road. Borrowing an aromatic factor for H2S or HCN
# therefore asserts that those two species share the aromatics' within-day
# behaviour. That assertion is testable on our own data, even without a
# stationary H2S/HCN record, by asking how each species varies across the
# sampling window.
#
# THE CONFOUND, AND HOW IT IS HANDLED. Hour of day is entangled with place:
# the routes are driven in a consistent order, so "1 pm" is also "the
# industrial corridor". A pooled hour-of-day mean therefore measures route
# geometry as much as time. Here the hour effect is estimated WITHIN 500 m
# cells and then averaged, so a cell visited at only one hour contributes
# nothing to the hour contrast:
#
#     median(cell, hour) ~ cell + factor(hour)
#
# fitted by least squares over cell-hour medians, cells restricted to those
# sampled in at least MIN_H distinct hours. Medians within cell-hour keep a
# single long dwell from dominating. The model is ADDITIVE, not multiplicative:
# negative and below-MDL values are retained everywhere in this pipeline (see
# REPRODUCIBILITY.md), so per-cell means can sit near zero and a ratio
# normalisation blows up. Hour effects are reported in ppb and, for
# readability, as (all-hour level + hour effect) / all-hour level.
#
# Uncertainty: cells are the resampling unit (nonparametric bootstrap over
# cells), because measurements within a cell are not independent.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/78_diurnal_scaling_evidence.R
#
# Inputs   mobile_wswd.RData
#          lacasa_scaling_factors_option1_binweighted.RData  (for the diagnostic)
#          TABLE_lacasa_daynight_ratios.csv   (written by 49; optional but wanted -
#          run this script AFTER 49 in figure group X)
# Outputs  TABLE_S7.5_diurnal_shape.csv        hour profile, controlled + pooled
#          TABLE_S7.6_afternoon_ratio.csv      afternoon:morning with bootstrap CI
#          TABLE_S7.7_nightday_vs_factor.csv   night:day ratio vs 24-h factor
#          FinalFig/FIG_S7_diurnal_shape.png
#          FinalFig/FIG_S7_afternoon_ratio.png
# ==============================================================
suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2)
})

BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))
FIGD <- file.path(BASE, "FinalFig"); dir.create(FIGD, showWarnings = FALSE)
SF   <- file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData")

HRS   <- 9:15     # hours with real coverage across the campaign
MIN_H <- 4        # a cell must be sampled in >= MIN_H distinct hours to contribute
CELL  <- 500      # m, matching the analysis grid
NBOOT <- 2000
set.seed(20260924)

P <- data.table(
  name = c("Benzene","Toluene","Xylene","Trimethylbenzene","H2S","HCN"),
  col  = c("Benzene_ppb","Toluene_ppb","Xylene_ppb","Trimethylbenzene_ppb",
           "Hydrogen_Sulfide_ppb","Hydrogen_Cyanide_ppb"),
  lacasa = c("benzene","toluene","xylene", NA, NA, NA))

message("Loading mobile record...")
load(file.path(BASE, "mobile_wswd.RData"))          # out
d <- as.data.table(out); rm(out); invisible(gc())
d <- d[is.finite(Latitude) & is.finite(Longitude) &
       Site != "Goodrich Corporation (Collins Aerospace)"]
# `date` is a fixed-MST wall clock carried with a UTC tzone attribute (see
# time_convention notes); format() therefore gives the MST hour directly.
d[, hr := as.integer(format(date, "%H"))]

pts <- st_transform(st_as_sf(d[, .(Longitude, Latitude)],
        coords = c("Longitude","Latitude"), crs = 4326), 32613)
xy <- st_coordinates(pts); rm(pts); invisible(gc())
d[, cell := paste0(floor(xy[,1] / CELL), "_", floor(xy[,2] / CELL))]
message(sprintf("  %s rows, %s distinct %d m cells, hours %d-%d retained",
                format(nrow(d), big.mark=","), format(uniqueN(d$cell), big.mark=","),
                CELL, min(HRS), max(HRS)))

# ---- within-cell hour effect ---------------------------------------------
# Cell fixed effects are absorbed exactly by within-cell demeaning of both the
# response and the hour dummies (Frisch-Waugh-Lovell), so the hour coefficients
# below are ordinary least squares on `m ~ cell + factor(hr)` without ever
# forming the several-hundred-column cell design matrix. This matters only for
# speed - it is what makes a 2,000-replicate bootstrap over cells feasible -
# and is verified against lm() on the full sample before any resampling.
fit_fast <- function(ch) {
  hrs <- sort(unique(ch$hr)); H <- length(hrs)
  if (H < 2L || uniqueN(ch$cell) < 2L) return(NULL)
  y <- as.numeric(ch$m)
  X <- matrix(0, nrow = length(y), ncol = H - 1L)
  for (k in seq_len(H - 1L)) X[, k] <- as.numeric(ch$hr == hrs[k + 1L])
  # within-cell demeaning
  g  <- as.integer(factor(ch$cell)); ng <- max(g)
  cn <- tabulate(g, ng)
  # g runs 1..ng with every level present, so rowsum() returns groups in order
  cmean <- function(v) (rowsum(v, g)[, 1L] / cn)[g]
  yd <- y - cmean(y)
  Xd <- X; for (k in seq_len(ncol(Xd))) Xd[, k] <- Xd[, k] - cmean(Xd[, k])
  qrX <- qr(Xd)
  if (qrX$rank < ncol(Xd)) return(NULL)
  b  <- qr.coef(qrX, yd); b[!is.finite(b)] <- 0
  r  <- yd - Xd %*% b
  rss1 <- sum(r^2); rss0 <- sum(yd^2)
  df1 <- ncol(Xd); df2 <- length(y) - ng - df1
  pF <- if (df2 > 0 && rss1 > 0)
          pf(((rss0 - rss1) / df1) / (rss1 / df2), df1, df2, lower.tail = FALSE)
        else NA_real_
  he <- c(0, as.numeric(b)); names(he) <- hrs
  he <- he - mean(he)
  list(eff = he, gm = mean(y), p = pF)
}

# --- verify the fast solver against lm() once per pollutant ---------------
verify_fast <- function(ch, nm) {
  f <- fit_fast(ch); if (is.null(f)) return(invisible(NULL))
  ch2 <- copy(ch)[, `:=`(cf = factor(cell), hf = factor(hr, levels = sort(unique(hr))))]
  m  <- lm(m ~ cf + hf, data = ch2); cf <- coef(m)
  he <- c(0, cf[grep("^hf", names(cf))]); he <- he - mean(he)
  d1 <- max(abs(as.numeric(he) - as.numeric(f$eff)))
  d2 <- abs(anova(m)["hf", "Pr(>F)"] - f$p)
  cat(sprintf("  [SOLVER] %-17s max coef diff vs lm() %.3e, p diff %.3e  [%s]\n",
              nm, d1, d2, if (d1 < 1e-8) "OK" else "MISMATCH"))
  if (d1 >= 1e-8) stop("fast within-cell solver disagrees with lm() for ", nm)
}

prof <- list(); meta <- list(); ratio <- list()
cat("\n== solver verification ==\n")
for (i in seq_len(nrow(P))) {
  nm <- P$name[i]; col <- P$col[i]
  z  <- d[hr %in% HRS & is.finite(get(col)), .(cell, hr, v = get(col))]
  ch <- z[, .(m = median(v)), by = .(cell, hr)]
  keep <- ch[, .(nh = uniqueN(hr)), by = cell][nh >= MIN_H, cell]
  ch <- ch[cell %in% keep]
  verify_fast(ch, nm)
  f  <- fit_fast(ch)
  if (is.null(f)) { message("[SKIP] ", nm, ": too few cells or hours"); next }
  hrs <- as.integer(names(f$eff))
  prof[[nm]] <- data.table(pollutant = nm, hr = hrs, eff_ppb = as.numeric(f$eff),
                           rel = as.numeric((f$gm + f$eff) / f$gm))
  meta[[nm]] <- data.table(pollutant = nm, cells = length(keep),
                           cell_hours = nrow(ch), level_ppb = f$gm, p_hour = f$p)

  amr <- function(eff, gm, h) {
    rel <- (gm + eff) / gm
    mean(rel[h %in% 13:14]) / mean(rel[h %in% 9:10])
  }
  obs <- amr(f$eff, f$gm, hrs)

  # bootstrap over CELLS (measurements within a cell are not independent)
  setkey(ch, cell)
  bs <- numeric(NBOOT)
  for (b in seq_len(NBOOT)) {
    cs <- sample(keep, length(keep), replace = TRUE)
    cb <- ch[.(cs), allow.cartesian = TRUE]
    cb[, cell := rep(seq_along(cs), ch[.(cs), .N, by = .EACHI]$N)]
    g2 <- fit_fast(cb)
    bs[b] <- if (is.null(g2)) NA_real_ else amr(g2$eff, g2$gm, as.integer(names(g2$eff)))
  }
  ratio[[nm]] <- data.table(pollutant = nm, aft_morn = obs,
                            lo = unname(quantile(bs, 0.025, na.rm = TRUE)),
                            hi = unname(quantile(bs, 0.975, na.rm = TRUE)),
                            n_boot_ok = sum(is.finite(bs)))
  message(sprintf("  %-17s %4d cells  p(hour) = %.2e  afternoon:morning = %.3f [%.3f, %.3f]",
                  nm, length(keep), f$p, obs, ratio[[nm]]$lo, ratio[[nm]]$hi))
}
prof <- rbindlist(prof); meta <- rbindlist(meta); ratio <- rbindlist(ratio)

# ---- pooled (uncontrolled) comparison ------------------------------------
pool <- rbindlist(lapply(seq_len(nrow(P)), function(i) {
  z <- d[hr %in% HRS & is.finite(get(P$col[i])), .(m = mean(get(P$col[i]))), by = hr][order(hr)]
  z[, .(pollutant = P$name[i], hr, rel_pooled = m / mean(m))] }))
pr <- pool[, .(aft_morn_pooled = mean(rel_pooled[hr %in% 13:14]) /
                                 mean(rel_pooled[hr %in% 9:10])), by = pollutant]

out_prof <- merge(prof, pool, by = c("pollutant","hr"), all.x = TRUE)
fwrite(out_prof, file.path(BASE, "TABLE_S7.5_diurnal_shape.csv"))
out_rat <- merge(merge(ratio, pr, by = "pollutant"), meta, by = "pollutant")
setorder(out_rat, -aft_morn)
fwrite(out_rat, file.path(BASE, "TABLE_S7.6_afternoon_ratio.csv"))

cat("\n== within-cell hour effect, relative to the all-hour level ==\n")
print(dcast(prof, hr ~ pollutant, value.var = "rel")[, lapply(.SD, function(x) round(x, 3))],
      row.names = FALSE)
cat("\n== afternoon (13-14) : morning (09-10), location-controlled vs pooled ==\n")
print(out_rat[, .(pollutant, cells, aft_morn = round(aft_morn, 3),
                  CI = sprintf("[%.3f, %.3f]", lo, hi),
                  pooled = round(aft_morn_pooled, 3),
                  p_hour = signif(p_hour, 2))], row.names = FALSE)

# ---- the diagnostic that matters -----------------------------------------
.sf_get <- function(pol) {
  if (!file.exists(SF)) return(NA_real_)
  e <- new.env(); load(SF, envir = e); o <- get(ls(e)[1], envir = e)
  as.numeric(o[["ratio_all_over_mobilelike"]])[match(pol, o[["pollutant"]])]
}
kn <- P[!is.na(lacasa), .(pollutant = name, factor_24h = vapply(lacasa, .sf_get, 0.0))]
cmp <- merge(out_rat[, .(pollutant, aft_morn)], kn, by = "pollutant", all.x = TRUE)
setorder(cmp, aft_morn)
cat("\n== the three species with a measured factor, against their daytime shape ==\n")
print(cmp[, .(pollutant, aft_morn = round(aft_morn, 3),
              measured_24h_factor = round(factor_24h, 3))], row.names = FALSE)
bz  <- cmp[pollutant == "Benzene"]
cat(sprintf(paste0(
  "\n  READ. A La Casa factor above 1 means the species is DEPLETED during the\n",
  "  sampling window relative to its 24-h mean. Two things follow from the\n",
  "  table above, and only two.\n\n",
  "  (1) H2S and HCN do NOT share the aromatics' within-window behaviour.\n",
  "      Toluene, xylene and 1,2,4-TMB all fall across the window (%.2f-%.2f),\n",
  "      while H2S and HCN rise (%.2f and %.2f), with bootstrap intervals that\n",
  "      exclude 1. The premise behind borrowing a factor - that these species\n",
  "      behave alike within the day - is contradicted by our own data.\n\n",
  "  (2) Within-window shape does NOT determine the 24-h factor. Benzene rises\n",
  "      across the window (%.3f, CI excludes 1) and still carries a measured\n",
  "      factor of %.3f, and among the two species that do decline the ordering\n",
  "      of the declines (toluene %.3f, xylene %.3f) is the reverse of the\n",
  "      ordering of their factors (%.3f, %.3f). Three calibration points cannot\n",
  "      support a regression, and benzene shows why: the factor is set largely\n",
  "      by the overnight hours we never sampled.\n\n",
  "  TAKEN TOGETHER these rule OUT borrowing an aromatic factor for H2S and HCN,\n",
  "  but they do NOT license the opposite claim that those factors are below 1.\n",
  "  The defensible answer is the one in 77_health_scaling_sensitivity.R: leave\n",
  "  S7 unscaled, report scenarios C and D as bounds, and quote the break-even\n",
  "  factors so a reader can see how far a borrowed factor would have to be\n",
  "  wrong before any S7 conclusion moved.\n"),
  cmp[pollutant == "Trimethylbenzene", aft_morn], cmp[pollutant == "Xylene", aft_morn],
  cmp[pollutant == "HCN", aft_morn], cmp[pollutant == "H2S", aft_morn],
  bz$aft_morn, bz$factor_24h,
  cmp[pollutant == "Toluene", aft_morn], cmp[pollutant == "Xylene", aft_morn],
  cmp[pollutant == "Toluene", factor_24h], cmp[pollutant == "Xylene", factor_24h]))

# ---- where the factor actually comes from ---------------------------------
# Script 49 (SI S4.4) compares La Casa concentrations inside the mobile driving
# window with nights and weekend daytimes. It reads only the La Casa record and
# a fixed clock window, so it is untouched by the 300 m exclusion. That table
# turns out to order the three measured species exactly as their 24-h factors
# are ordered - which the within-window shape above does NOT do - and pins down
# where the information in a factor lives.
DN <- file.path(BASE, "TABLE_lacasa_daynight_ratios.csv")
if (file.exists(DN)) {
  dn <- fread(DN)[, .(lacasa = pollutant, night_day = ratio_night_over_window_mean)]
  k2 <- P[!is.na(lacasa), .(pollutant = name, lacasa,
                            factor_24h = vapply(lacasa, .sf_get, 0.0))]
  k2 <- merge(k2, dn, by = "lacasa")
  k2 <- merge(k2, out_rat[, .(pollutant, aft_morn)], by = "pollutant")
  k2[, pass_through := (factor_24h - 1) / (night_day - 1)]
  setorder(k2, night_day)
  cat("\n== night:day ratio (S4.4) against the 24-h factor ==\n")
  print(k2[, .(pollutant, night_day, factor_24h = round(factor_24h, 3),
               pass_through = round(pass_through, 3),
               within_window_aft_morn = round(aft_morn, 3))], row.names = FALSE)
  cat(sprintf(paste0(
    "\n  The night:day ratio ranks the three species in the same order as their\n",
    "  factors (%.2f < %.2f < %.2f against %.3f < %.3f < %.3f), and the implied\n",
    "  pass-through (factor - 1) / (ratio - 1) is %.2f-%.2f for all three. The\n",
    "  within-window afternoon:morning ratio does not order them at all. The\n",
    "  24-h factor is therefore an OVERNIGHT quantity, and the reason no factor\n",
    "  can be given for H2S or HCN is not an analysis choice but a measurement\n",
    "  gap: La Casa records neither species, so there is no overnight\n",
    "  observation of either to form a ratio from. Closing it needs a continuous\n",
    "  overnight H2S/HCN record co-located with the driving domain - the CCND\n",
    "  and Suncor fenceline monitors are the obvious candidates.\n",
    "  (Three calibration points: a ranking and an order of magnitude, not a\n",
    "  regression to transfer.)\n"),
    k2$night_day[1], k2$night_day[2], k2$night_day[3],
    k2$factor_24h[1], k2$factor_24h[2], k2$factor_24h[3],
    min(k2$pass_through), max(k2$pass_through)))
  fwrite(k2, file.path(BASE, "TABLE_S7.7_nightday_vs_factor.csv"))
} else {
  message("[S4.4] TABLE_lacasa_daynight_ratios.csv absent - run 49_lacasa_daynight_ratios.R first")
}

cat(paste0(
  "\n  LIMITATION: this compares shape WITHIN 09-15, the only hours sampled. It\n",
  "  constrains the direction of a 24-h correction, not its size: nothing here\n",
  "  observes the overnight hours, when a stable nocturnal boundary layer can\n",
  "  raise all species. Only a co-located continuous H2S/HCN record - e.g. the\n",
  "  CCND and Suncor fenceline monitors - can supply a measured factor.\n"))

# ---- figures --------------------------------------------------------------
# Linetype separates the species that HAVE a La Casa factor from those that do
# not - which is the distinction the section is about. It deliberately does not
# separate "aromatic" from "not", because benzene is an aromatic that behaves
# like H2S and HCN here, and that is the point of item (2) above.
HASF <- P[!is.na(lacasa), name]
pl <- copy(prof)
pl[, grp := fifelse(pollutant %in% HASF,
                    "has a measured La Casa factor",
                    "no La Casa factor (1,2,4-TMB, H2S, HCN)")]
g1 <- ggplot(pl, aes(hr, rel, colour = pollutant, linetype = grp)) +
  geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  scale_x_continuous(breaks = HRS) +
  scale_linetype_manual(values = setNames(c("solid", "22"),
      c("has a measured La Casa factor", "no La Casa factor (1,2,4-TMB, H2S, HCN)"))) +
  labs(x = "hour of day (MST)",
       y = "concentration relative to the all-hour level",
       colour = NULL, linetype = NULL,
       title = "Within-cell hour-of-day shape, 500 m cells sampled in at least 4 hours",
       subtitle = paste("Toluene, xylene and 1,2,4-TMB fall across the window;",
                        "benzene, H2S and HCN rise")) +
  guides(linetype = guide_legend(order = 1), colour = guide_legend(order = 2, nrow = 2)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "vertical",
        plot.title = element_text(size = 11.5), plot.subtitle = element_text(size = 10))
ggsave(file.path(FIGD, "FIG_S7_diurnal_shape.png"), g1,
       width = 8.2, height = 5.4, dpi = 300)
message("-> ", file.path(FIGD, "FIG_S7_diurnal_shape.png"))

rr <- merge(out_rat[, .(pollutant, aft_morn, lo, hi)], kn, by = "pollutant", all.x = TRUE)
rr[, lab := fifelse(is.na(factor_24h), "no La Casa factor",
                    sprintf("measured factor %.3f", factor_24h))]
setorder(rr, aft_morn)
rr[, pollutant := factor(pollutant, levels = pollutant)]
g2 <- ggplot(rr, aes(aft_morn, pollutant, colour = is.na(factor_24h))) +
  geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.6) +
  geom_point(size = 2.4) +
  geom_text(aes(label = lab), hjust = 0, nudge_y = 0.28, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "grey25", `TRUE` = "#b2182b"),
                      labels = c(`FALSE` = "has a measured La Casa factor",
                                 `TRUE` = "no La Casa factor"), name = NULL) +
  expand_limits(x = max(rr$hi) * 1.25) +
  labs(x = "afternoon (13-14) : morning (09-10), within 500 m cells",
       y = NULL,
       title = "Within-window shape does not predict the 24-h scaling factor",
       subtitle = sprintf(paste("points are the within-cell estimate, bars %d-replicate bootstrap 95%% CIs over cells;",
                                "\nbenzene rises across the window yet still carries a factor above 1"), NBOOT)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(size = 11.5))
ggsave(file.path(FIGD, "FIG_S7_afternoon_ratio.png"), g2,
       width = 8.2, height = 4.8, dpi = 300)
message("-> ", file.path(FIGD, "FIG_S7_afternoon_ratio.png"))
message("DONE.")
