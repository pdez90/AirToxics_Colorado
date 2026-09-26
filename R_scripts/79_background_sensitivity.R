# ==============================================================
# 79  Sensitivity of every downstream result to the BACKGROUND DEFINITION
#     (SI section S4.6; Table S4.1; Figure S4.12)
#
# Section S4.1.1 defines the local background as the lowest 20th percentile of
# each pollutant over a rolling 20-minute window, evaluated per
# date x Site x Asset group (scripts 10 and 11). Both constants - the
# percentile and the window length - are conventions taken from the mobile
# monitoring literature, not quantities estimated from these data. This script
# asks what the paper's conclusions would have been under the other plausible
# choices, by re-running the whole background chain under a 3 x 3 grid
#
#     percentile   10th / 20th / 30th
#     window       10 / 20 / 30 minutes   (i.e. +/-300, +/-600, +/-900 s)
#
# and propagating each arm through EVERY stage that consumes the corrected
# s* columns:
#
#   500 m cell surface       as script 73 / SI S4.1.2 (median of daily medians
#                            per cell, >= 10 visit-days, La Casa scaling)
#   census-block surface     as script 18 (median-of-daily-medians and
#                            mean-of-daily-means per block)
#   benzene risk comparison  as script 20 (population-weighted, COMMON blocks,
#                            IUR 5.75 and 20.40 excess cases per ppb)
#   S7 hazard indices        as script 74 (block-resolved HQ and organ HI, both
#                            the population-weighted mean and the most-exposed
#                            block)
#
# NOT re-run, by design: the hotspot chain and the Gaussian plume inversion.
# The hotspot detector thresholds the RAW *_ppb columns against a campaign
# 99th percentile and never reads a background-corrected column, so it is
# algebraically independent of this grid; the plume work is frozen for this
# revision (SI S6).
#
# CORRECTNESS GATE. The background chain is reimplemented here rather than
# re-sourced, so the script first re-runs the published arm (20th percentile,
# 20-minute window) and requires it to reproduce every baseline_*, median_bg*
# and s* column of the pipeline's own mobile_corrected.RData EXACTLY, over all
# 2.55 million rows and all six pollutants. If any column differs the script
# stops: no sensitivity arm is reported from an engine that cannot reproduce
# the published one.
#
#   SUNCOR_BASE=~/Downloads/Suncor Rscript R_scripts/79_background_sensitivity.R
#
# Inputs
#   mobile_wswd.RData                                          script 06 output
#   mobile_corrected.RData                                     script 12 output (gate + 500 m cell ids)
#   censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData   script 18 output (block geometry, POP20, AirToxScreen)
#   lacasa_scaling_factors_option1_binweighted.RData            script 17 output
#   FinalFig/benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS.csv  script 20 output (published anchor)
# Outputs
#   TABLE_S4.1_background_sensitivity.csv
#   TABLE_S4.1b_background_sensitivity_cells.csv
#   FinalFig/FIG_S4.12_background_sensitivity.png
# ==============================================================

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2)
})
BASE <- SUNCOR_BASE
hr <- function(s) cat("\n========== ", s, " ==========\n", sep = "")

POLL       <- c("Benzene", "Toluene", "Trimethylbenzene", "Xylene", "H2S", "HCN")
PCTS       <- c(0.10, 0.20, 0.30)
HALVES     <- c(300, 600, 900)          # seconds either side; window = 2 x half
BASE_PCT   <- 0.20
BASE_HALF  <- 600
MIN_N      <- 30                        # script 10
MIN_VISITS <- 10                        # script 73
arm_label  <- function(p, h) sprintf("%dth / %d min", round(p * 100), 2 * h / 60)
BASE_LAB   <- arm_label(BASE_PCT, BASE_HALF)

# ---------------------------------------------------------------------------
# rolling quantile engine
# ---------------------------------------------------------------------------
# Exactly slider::slide_index_dbl(.before = half, .after = half,
# .complete = FALSE, .f = stats::quantile(v[is.finite(v)], p)) with NA when the
# window holds fewer than MIN_N finite values - i.e. q20() of script 10,
# generalised to an arbitrary percentile and window. The C++ path keeps a
# Fenwick tree over the window so the cost is O(n log n) instead of
# O(n x window log window); it is ~450x faster than the slider path and the
# gate below proves the two agree to the last bit. The pure-R path is kept so
# the script runs without a compiler.
.ROLLQ_CPP <- '
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector roll_q(NumericVector x, NumericVector i, double p, double half, int min_n) {
  const int n = x.size();
  NumericVector out(n, NA_REAL);
  if (n == 0) return out;
  std::vector<int> ord; ord.reserve(n);
  for (int k = 0; k < n; ++k) if (R_finite(x[k])) ord.push_back(k);
  const int m = (int)ord.size();
  if (m < min_n) return out;
  std::sort(ord.begin(), ord.end(), [&](int a, int b){ return x[a] < x[b]; });
  std::vector<int> rank(n, 0);
  std::vector<double> sv(m + 1, 0.0);
  for (int r = 0; r < m; ++r) { rank[ord[r]] = r + 1; sv[r + 1] = x[ord[r]]; }
  std::vector<int> bit(m + 1, 0);
  int LOG = 1; while ((1 << LOG) <= m) ++LOG;
  auto add = [&](int r, int v){ for (; r <= m; r += r & (-r)) bit[r] += v; };
  auto kth = [&](int k)->double {
    int pos = 0, rem = k;
    for (int pw = 1 << LOG; pw; pw >>= 1)
      if (pos + pw <= m && bit[pos + pw] < rem) { pos += pw; rem -= bit[pos]; }
    return sv[pos + 1];
  };
  int lo = 0, hi = 0, cnt = 0;
  for (int k = 0; k < n; ++k) {
    const double a = i[k] - half, b = i[k] + half;
    while (hi < n && i[hi] <= b) { if (rank[hi]) { add(rank[hi], 1); ++cnt; } ++hi; }
    while (lo < n && i[lo] <  a) { if (rank[lo]) { add(rank[lo], -1); --cnt; } ++lo; }
    if (cnt < min_n) { out[k] = NA_REAL; continue; }
    const double idx = 1.0 + (cnt - 1) * p;
    const int li = (int)std::floor(idx), hiI = (int)std::ceil(idx);
    const double xlo = kth(li);
    double qs = xlo;
    if (idx > (double)li) {
      const double xhi = (hiI == li) ? xlo : kth(hiI);
      if (xhi != xlo) { const double h = idx - (double)li; qs = (1.0 - h) * xlo + h * xhi; }
    }
    out[k] = qs;
  }
  return out;
}
'
USE_CPP <- requireNamespace("Rcpp", quietly = TRUE) &&
  !inherits(try(Rcpp::sourceCpp(code = .ROLLQ_CPP), silent = TRUE), "try-error")
if (!USE_CPP) {
  message("[ENGINE] Rcpp unavailable - falling back to slider (expect a few hours)")
  stopifnot(requireNamespace("slider", quietly = TRUE))
  roll_q <- function(x, i, p, half, min_n) {
    slider::slide_index_dbl(.x = x, .i = i, .before = half, .after = half,
                            .complete = FALSE, .f = function(v) {
                              vv <- v[is.finite(v)]
                              if (length(vv) < min_n) return(NA_real_)
                              unname(stats::quantile(vv, p, names = FALSE))
                            })
  }
} else message("[ENGINE] Rcpp rolling-quantile engine compiled")

# script 11, verbatim
bg_correct <- function(obs, base, med) {
  out <- rep(NA_real_, length(obs))
  ok  <- is.finite(obs) & is.finite(base) & is.finite(med)
  ok_add   <- ok & ((base <= obs) | (base <= 0)); out[ok_add]   <- obs[ok_add] - base[ok_add] + med[ok_add]
  ok_ratio <- ok & (base > obs) & (base > 0);     out[ok_ratio] <- obs[ok_ratio] * med[ok_ratio] / base[ok_ratio]
  out
}

# ---------------------------------------------------------------------------
# inputs
# ---------------------------------------------------------------------------
hr("Inputs")
load(file.path(BASE, "mobile_wswd.RData")); d <- as.data.table(out); rm(out); gc()
setnames(d, c("Benzene_ppb", "Toluene_ppb", "Trimethylbenzene_ppb", "Xylene_ppb",
              "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb"), POLL)
# keep only what this script uses - the wind, met and QA columns are dead weight
# here and the arms allocate 18 new 2.5-million-row vectors each
d <- d[, c("date", "Asset", "Site", "Latitude", "Longitude", POLL), with = FALSE]
d[, datetime := date]
setorder(d, datetime)                                     # script 10 sorts globally
d[, day := as.Date(datetime)]
d[, grp := paste0(day, "_", Site, "_", Asset)]            # script 10 key
d[, tnum := as.numeric(datetime)]
cat(sprintf("mobile record: %s rows, %d background groups\n",
            format(nrow(d), big.mark = ","), uniqueN(d$grp)))

e <- new.env(); load(file.path(BASE, "mobile_corrected.RData"), envir = e)
pipe <- get(ls(e)[1], envir = e); rm(e)
KEY  <- c("date", "Asset", "Site", "Latitude", "Longitude")
GATECOLS <- c(paste0("baseline_", POLL), paste0("median_bg", POLL), paste0("s", POLL))
pipe <- as.data.table(sf::st_drop_geometry(pipe))[, c(KEY, "id", GATECOLS), with = FALSE]
stopifnot(uniqueN(pipe, by = KEY) == nrow(pipe), uniqueN(d, by = KEY) == nrow(d))
setnames(pipe, GATECOLS, paste0("pipe_", GATECOLS))

e <- new.env(); load(file.path(BASE, "censusblocks_suncor_terminal_BINWEIGHTED_AB_overlap.RData"), envir = e)
bo <- get(ls(e)[1], envir = e); rm(e)
blocks <- as.data.table(sf::st_drop_geometry(bo))
cat(sprintf("block surface: %s blocks with mobile + AirToxScreen coverage\n",
            format(nrow(blocks), big.mark = ",")))

e <- new.env(); load(file.path(BASE, "lacasa_scaling_factors_option1_binweighted.RData"), envir = e)
sfac <- as.data.table(get(ls(e)[1], envir = e)); rm(e)
RATIO <- stats::setNames(sfac$ratio_all_over_mobilelike, sfac$pollutant)
cat(sprintf("La Casa factors: benzene %.4f | toluene %.4f | xylene %.4f\n",
            RATIO[["benzene"]], RATIO[["toluene"]], RATIO[["xylene"]]))

# ---- 500 m cell id + the gate columns, joined once (script 12 assignment) ----
.npipe <- nrow(pipe)
d <- merge(d, pipe, by = KEY, all.x = TRUE); rm(pipe); gc()
setorder(d, datetime)
stopifnot(nrow(d) == .npipe, !anyNA(d$id))

# ---- census block: point-in-polygon, as script 18 ---------------------------
hr("Point-in-block assignment")
bo69  <- sf::st_transform(bo, 4269)
bgeom <- sf::st_geometry(bo69)
bg20  <- as.character(bo69$GEOID20)
# chunked so the point layer and the index list never both exist at full size
.CH <- 250000L
d[, GEOID20 := NA_character_]
for (.s in seq(1L, nrow(d), by = .CH)) {
  .e   <- min(.s + .CH - 1L, nrow(d))
  .pts <- sf::st_transform(sf::st_as_sf(d[.s:.e, .(Longitude, Latitude)],
                                        coords = c("Longitude", "Latitude"), crs = 4326), 4269)
  .ix  <- sf::st_within(.pts, bgeom)
  set(d, .s:.e, "GEOID20",
      bg20[vapply(.ix, function(z) if (length(z)) z[1] else NA_integer_, integer(1))])
  rm(.pts, .ix)
}
gc()
cat(sprintf("%s of %s observations fall inside a covered block (%d distinct blocks)\n",
            format(sum(!is.na(d$GEOID20)), big.mark = ","),
            format(nrow(d), big.mark = ","), uniqueN(d$GEOID20, na.rm = TRUE)))
# This join is repeated identically for every arm, so any residual disagreement
# with the published surface (which joined against the full Colorado block
# layer) cancels in every arm-to-arm comparison reported below.

# ---------------------------------------------------------------------------
# health constants - mirrored from scripts 73 and 74
# ---------------------------------------------------------------------------
MOLAR_VOL <- 8.314 * 298.15 / 83000 * 1000        # 29.8653 L/mol, 25 C, 830 hPa
HP <- data.table(
  name  = c("Benzene", "Toluene", "Xylenes", "1,2,4-Trimethylbenzene", "H2S", "HCN"),
  poll  = c("Benzene", "Toluene", "Xylene", "Trimethylbenzene", "H2S", "HCN"),
  MW    = c(78.11, 92.14, 106.16, 120.19, 34.08, 27.03),
  RfC   = c(30, 5000, 100, 60, 2, 0.8),           # IRIS chronic RfC, ug/m3
  organ = c("Hematological", "Neurological", "Neurological", "Neurological",
            "Respiratory", "Endocrine"))
HP[, cf := MW / MOLAR_VOL]
rfc_ppb <- stats::setNames(
  c(0.03, 5, 0.06, 0.1, 0.002, 0.0008) * 1000 /
    c(78.11, 92.14, 120.19, 106.17, 34.08, 27.03) * MOLAR_VOL,
  c("Benzene", "Toluene", "Trimethylbenzene", "Xylene", "H2S", "HCN"))
TOS <- c(Benzene = "Hematological/Immunological", Toluene = "Neurological",
         Trimethylbenzene = "Neurological", Xylene = "Neurological",
         H2S = "Respiratory", HCN = "Endocrine")
SCALE_CELL <- c(Benzene = RATIO[["benzene"]], Toluene = RATIO[["toluene"]],
                Trimethylbenzene = NA_real_, Xylene = RATIO[["xylene"]],
                H2S = NA_real_, HCN = NA_real_)
IUR <- c(low = 5.75, high = 20.40)                # excess cases per ppb per million

# ---------------------------------------------------------------------------
# one arm
# ---------------------------------------------------------------------------
run_arm <- function(p, half, keep_rows = FALSE) {
  for (nm in POLL)
    d[, (paste0("b_", nm)) := roll_q(as.numeric(get(nm)), tnum, p, half, MIN_N), by = grp]
  for (nm in POLL) {
    d[, (paste0("m_", nm)) := stats::median(get(paste0("b_", nm))[is.finite(get(paste0("b_", nm)))],
                                            na.rm = TRUE), by = .(day, Site, Asset)]
    d[, (paste0("s", nm)) := bg_correct(as.numeric(get(nm)), get(paste0("b_", nm)),
                                        get(paste0("m_", nm)))]
  }
  bg <- paste0("s", POLL)

  # ---- census blocks (script 18) ----
  DTp  <- d[!is.na(GEOID20) & !is.na(day)]
  bmed <- DTp[, lapply(.SD, stats::median, na.rm = TRUE), by = .(GEOID20, day), .SDcols = bg
            ][, lapply(.SD, stats::median, na.rm = TRUE), by = .(GEOID20), .SDcols = bg]
  setnames(bmed, bg, paste0(bg, "_med_of_daily_med"))
  bmea <- DTp[, lapply(.SD, mean, na.rm = TRUE), by = .(GEOID20, day), .SDcols = bg
            ][, lapply(.SD, mean, na.rm = TRUE), by = .(GEOID20), .SDcols = bg]
  setnames(bmea, bg, paste0(bg, "_mean_of_daily_mean"))
  bcnt <- DTp[, .(n_points = .N, n_days = uniqueN(day)), by = .(GEOID20)]
  bdt  <- Reduce(function(x, y) merge(x, y, by = "GEOID20", all = TRUE),
                 list(bmed, bmea, bcnt))[!is.na(n_points)]
  for (q in c("med_of_daily_med", "mean_of_daily_mean"))
    for (nm in c("Benzene", "Toluene", "Xylene"))
      bdt[, (paste0("s", nm, "_", q, "_scaled")) := RATIO[[tolower(nm)]] * get(paste0("s", nm, "_", q))]
  bdt <- merge(blocks[, .(GEOID20, POP20, benzene_ppb)], bdt, by = "GEOID20", all.x = TRUE)

  # ---- benzene risk on COMMON blocks (script 20) ----
  pop <- suppressWarnings(as.numeric(bdt$POP20))
  mb  <- bdt$sBenzene_med_of_daily_med_scaled
  ab  <- bdt$benzene_ppb
  cm  <- is.finite(pop) & pop > 0 & is.finite(ab) & is.finite(mb)
  rk  <- function(x, f) f * sum(x[cm] * pop[cm]) / 1e6
  risk <- data.table(
    n_common = sum(cm), population = sum(pop[cm]),
    airtox_pw_ppb = sum(ab[cm] * pop[cm]) / sum(pop[cm]),
    mobile_pw_ppb = sum(mb[cm] * pop[cm]) / sum(pop[cm]),
    airtox_cases_low = rk(ab, IUR[["low"]]),  mobile_cases_low  = rk(mb, IUR[["low"]]),
    airtox_cases_high = rk(ab, IUR[["high"]]), mobile_cases_high = rk(mb, IUR[["high"]]),
    cor_block = stats::cor(ab[cm], mb[cm]))
  risk[, ratio_mobile_over_airtox := mobile_pw_ppb / airtox_pw_ppb]

  # ---- S7 block-resolved hazard (script 74) ----
  chronic <- rbindlist(lapply(seq_len(nrow(HP)), function(i) {
    x  <- suppressWarnings(as.numeric(bdt[[paste0("s", HP$poll[i], "_mean_of_daily_mean")]]))
    ok <- is.finite(x) & is.finite(pop)
    data.table(pollutant = HP$name[i], target_organ = HP$organ[i],
               HQ_pwmean   = sum(x[ok] * pop[ok]) / sum(pop[ok]) * HP$cf[i] / HP$RfC[i],
               HQ_maxblock = max(x[ok]) * HP$cf[i] / HP$RfC[i])
  }))
  HI <- chronic[, .(HI_pwmean = sum(HQ_pwmean), HI_maxblock = sum(HQ_maxblock)), by = target_organ]

  # ---- 500 m cell surface (script 73 / SI S4.1.2) ----
  cells <- rbindlist(lapply(POLL, function(pn) {
    col <- paste0("s", pn); v <- d[[col]]
    if (!any(is.finite(v))) return(NULL)
    dd <- d[is.finite(get(col)), .(dmed = stats::median(get(col)), dmean = mean(get(col))),
            by = .(id, day)]
    per <- dd[, .(n_days = .N, median_ppb = stats::median(dmed), mean_ppb = mean(dmean)), by = id]
    per[, pollutant := pn][]
  }), use.names = TRUE)[n_days >= MIN_VISITS]
  cells[, scale_factor := SCALE_CELL[pollutant]]
  cells[, EC_median := pmax(fifelse(is.na(scale_factor), median_ppb, median_ppb * scale_factor), 0)]
  cells[, HQ_median := EC_median / rfc_ppb[pollutant]]
  cells[, tos := TOS[pollutant]]
  ntos    <- table(TOS)
  hi_cell <- cells[, .(HI = sum(HQ_median, na.rm = TRUE), nch = .N), by = .(id, tos)
                 ][nch == as.integer(ntos[tos])]
  cellsum <- cells[, .(n_cells = .N, EC_median_mean = mean(EC_median),
                       EC_median_med = stats::median(EC_median),
                       EC_median_max = max(EC_median)), by = pollutant]
  cellhi  <- hi_cell[, .(n_cells = .N, HI_median = stats::median(HI), HI_max = max(HI),
                         pct_HI_gt1 = 100 * sum(HI > 1) / .N), by = tos]

  rows <- if (keep_rows) d[, c(paste0("b_", POLL), paste0("m_", POLL), bg), with = FALSE] else NULL
  set(d, NULL, c(paste0("b_", POLL), paste0("m_", POLL), bg), NULL)
  list(risk = risk, chronic = chronic, HI = HI, cellsum = cellsum, cellhi = cellhi,
       cellwide = dcast(cells, id ~ pollutant, value.var = "EC_median"), rows = rows)
}

# ---------------------------------------------------------------------------
# GATE - the published arm must be reproduced exactly, row by row
# ---------------------------------------------------------------------------
hr(sprintf("GATE: re-running the published arm (%s)", BASE_LAB))
base_arm <- run_arm(BASE_PCT, BASE_HALF, keep_rows = TRUE)
.pairs <- rbindlist(lapply(POLL, function(nm) data.table(
  column = c(paste0("baseline_", nm), paste0("median_bg", nm), paste0("s", nm)),
  mine   = c(paste0("b_", nm), paste0("m_", nm), paste0("s", nm)),
  theirs = paste0("pipe_", c(paste0("baseline_", nm), paste0("median_bg", nm), paste0("s", nm))))))
gate <- rbindlist(lapply(seq_len(nrow(.pairs)), function(i) {
  a <- as.numeric(base_arm$rows[[.pairs$mine[i]]]); b <- as.numeric(d[[.pairs$theirs[i]]])
  k <- is.finite(a) & is.finite(b)
  data.table(column = .pairs$column[i], n = sum(k),
             na_mismatch = sum(is.na(a) != is.na(b)),
             max_abs_diff = if (!sum(k)) NA_real_ else max(abs(a[k] - b[k])))
}))
print(gate, row.names = FALSE)
if (!all(gate$na_mismatch == 0L) || !all(gate$max_abs_diff == 0, na.rm = TRUE))
  stop("GATE FAILED: the reimplemented background chain does not reproduce mobile_corrected.RData")
cat("\nGATE PASSED: every baseline, run median and corrected column reproduced exactly\n",
    sprintf("           (%s rows x %d pollutants, max absolute difference 0)\n",
            format(nrow(d), big.mark = ","), length(POLL)), sep = "")
base_arm$rows <- NULL
set(d, NULL, paste0("pipe_", GATECOLS), NULL); gc()

# the published anchor, for the record
anchor <- file.path(BASE, "FinalFig", "benzene_risk_summary_BINWEIGHTED_COMMONBLOCKS.csv")
if (file.exists(anchor)) {
  pr <- fread(anchor)
  cat(sprintf("\npublished benzene risk summary: %s common blocks, %s residents, mobile %.4f ppb, %.4f-%.4f cases\n",
              format(pr$n_blocks[1], big.mark = ","), format(pr$total_population_used[1], big.mark = ","),
              pr$pop_weighted_mean_ppb[2], pr$risk_5_75[2], pr$risk_20_40[2]))
  cat(sprintf("re-run of the same arm here : %s common blocks, %s residents, mobile %.4f ppb, %.4f-%.4f cases (%.3f%% apart)\n",
              format(base_arm$risk$n_common, big.mark = ","), format(base_arm$risk$population, big.mark = ","),
              base_arm$risk$mobile_pw_ppb, base_arm$risk$mobile_cases_low, base_arm$risk$mobile_cases_high,
              100 * abs(base_arm$risk$mobile_pw_ppb - pr$pop_weighted_mean_ppb[2]) / pr$pop_weighted_mean_ppb[2]))
}

# ---------------------------------------------------------------------------
# the nine arms
# ---------------------------------------------------------------------------
hr("Running the 3 x 3 grid")
G <- CJ(p = PCTS, half = HALVES)
arms <- vector("list", nrow(G))
for (r in seq_len(nrow(G))) {
  p <- G$p[r]; h <- G$half[r]; t0 <- Sys.time()
  a <- if (p == BASE_PCT && h == BASE_HALF) base_arm else run_arm(p, h)
  arms[[r]] <- a
  cat(sprintf("  %-14s %5.1f s | mobile %.4f ppb | %.4f-%.4f cases | ratio %.3f | endocrine HI %.3f\n",
              arm_label(p, h), as.numeric(Sys.time() - t0, units = "secs"),
              a$risk$mobile_pw_ppb, a$risk$mobile_cases_low, a$risk$mobile_cases_high,
              a$risk$ratio_mobile_over_airtox, a$HI[target_organ == "Endocrine", HI_pwmean]))
  gc()
}
names(arms) <- arm_label(G$p, G$half)

# ---------------------------------------------------------------------------
# Table S4.1
# ---------------------------------------------------------------------------
hr("Table S4.1")
risk <- rbindlist(lapply(seq_len(nrow(G)), function(r)
  cbind(data.table(arm = names(arms)[r], percentile = G$p[r] * 100, window_min = 2 * G$half[r] / 60),
        arms[[r]]$risk)))
hi <- rbindlist(lapply(seq_len(nrow(G)), function(r)
  cbind(data.table(arm = names(arms)[r]), arms[[r]]$HI)))
hiw <- dcast(hi, arm ~ target_organ, value.var = "HI_pwmean")
setnames(hiw, setdiff(names(hiw), "arm"), paste0("HI_pwmean_", setdiff(names(hiw), "arm")))
hix <- dcast(hi, arm ~ target_organ, value.var = "HI_maxblock")
setnames(hix, setdiff(names(hix), "arm"), paste0("HI_maxblock_", setdiff(names(hix), "arm")))

# 500 m benzene cell surface, agreement with the published arm
bcw <- arms[[BASE_LAB]]$cellwide[is.finite(Benzene)]
agr <- rbindlist(lapply(names(arms), function(nm) {
  m <- merge(bcw[, .(id, base = Benzene)], arms[[nm]]$cellwide[, .(id, x = Benzene)], by = "id")
  m <- m[is.finite(base) & is.finite(x)]
  data.table(arm = nm, n_cells = nrow(m),
             cells_identical_pct = 100 * mean(m$x == m$base),
             cells_within_one_step_pct = 100 * mean(abs(m$x - m$base) <= 0.05 * RATIO[["benzene"]] + 1e-9),
             median_abs_diff_ppb = stats::median(abs(m$x - m$base)),
             max_abs_diff_ppb = max(abs(m$x - m$base)),
             pearson_r = stats::cor(m$base, m$x))
}))

TS41 <- Reduce(function(x, y) merge(x, y, by = "arm", sort = FALSE),
               list(risk, hiw, hix, agr))
setorder(TS41, percentile, window_min)
fwrite(TS41, file.path(BASE, "TABLE_S4.1_background_sensitivity.csv"))
message("-> ", file.path(BASE, "TABLE_S4.1_background_sensitivity.csv"))
print(TS41[, .(arm, mobile_pw_ppb = round(mobile_pw_ppb, 4),
               cases = sprintf("%.3f-%.3f", mobile_cases_low, mobile_cases_high),
               ratio = round(ratio_mobile_over_airtox, 3),
               HI_endo = round(HI_pwmean_Endocrine, 3),
               HI_resp = round(HI_pwmean_Respiratory, 3),
               HI_neuro = round(HI_pwmean_Neurological, 3),
               HI_haem = round(HI_pwmean_Hematological, 3),
               cells_same_pct = round(cells_identical_pct, 1))], row.names = FALSE)

cellsum <- rbindlist(lapply(seq_len(nrow(G)), function(r)
  cbind(data.table(arm = names(arms)[r]), arms[[r]]$cellsum)))
cellhi <- rbindlist(lapply(seq_len(nrow(G)), function(r)
  cbind(data.table(arm = names(arms)[r]), arms[[r]]$cellhi)))
fwrite(merge(cellsum, cellhi, by.x = c("arm", "pollutant"), by.y = c("arm", "tos"),
             all = TRUE, suffixes = c("", "_HI")),
       file.path(BASE, "TABLE_S4.1b_background_sensitivity_cells.csv"))
message("-> ", file.path(BASE, "TABLE_S4.1b_background_sensitivity_cells.csv"))

# ---------------------------------------------------------------------------
# Figure S4.12
# ---------------------------------------------------------------------------
hr("Figure S4.12")
PAL <- c("10th" = "#6BAED6", "20th" = "#2171B5", "30th" = "#08306B")
TS41[, pct_lab := factor(paste0(percentile, "th"), levels = names(PAL))]
TS41[, win_lab := factor(sprintf("%d min", window_min), levels = c("30 min", "20 min", "10 min"))]
TS41[, is_base := percentile == BASE_PCT * 100 & window_min == 2 * BASE_HALF / 60]
airtox_pw <- TS41$airtox_pw_ppb[1]
DW  <- 0.48
thm <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4),
        legend.position = "bottom", legend.key.height = unit(10, "pt"),
        plot.title = element_text(size = 10.5, face = "bold"),
        plot.title.position = "plot")

# --- A: population-weighted benzene on the census blocks --------------------
pA <- ggplot(TS41, aes(x = mobile_pw_ppb, y = win_lab, colour = pct_lab)) +
  geom_vline(xintercept = airtox_pw, linetype = "dashed", colour = "#B2182B") +
  geom_point(aes(shape = is_base), size = 3.1, position = position_dodge(width = DW)) +
  annotate("text", x = airtox_pw - 0.0004, y = 3.42, colour = "#B2182B", hjust = 1,
           size = 3.0, label = sprintf("AirToxScreen  %.3f ppb", airtox_pw)) +
  annotate("text", x = TS41[is_base == TRUE, mobile_pw_ppb], y = 2.02,
           label = "published setting", size = 2.9, hjust = 1.15, colour = "grey25") +
  scale_colour_manual(values = PAL, name = "Percentile") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
  scale_x_continuous(limits = c(0.1425, 0.1655), breaks = seq(0.145, 0.165, 0.005)) +
  labs(title = "A   Population-weighted benzene across the 1,667 common blocks",
       x = "Benzene (ppb, 24-h basis)", y = "Window") + thm

# --- B: organ-system hazard indices -----------------------------------------
him <- melt(TS41, id.vars = c("arm", "pct_lab", "win_lab", "is_base"),
            measure.vars = patterns("^HI_pwmean_"), variable.name = "organ", value.name = "HI")
him[, organ := factor(sub("^HI_pwmean_", "", as.character(organ)),
                      levels = c("Hematological", "Neurological", "Respiratory", "Endocrine"))]
pB <- ggplot(him, aes(x = HI, y = organ, colour = pct_lab)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "#B2182B") +
  geom_point(aes(shape = is_base), size = 2.5, alpha = 0.9,
             position = position_dodge(width = 0.55)) +
  annotate("text", x = 1.05, y = 0.62, label = "HI = 1", colour = "#B2182B", hjust = 0, size = 3.0) +
  scale_x_log10(breaks = c(0.01, 0.03, 0.1, 0.3, 1, 3), limits = c(0.01, 3.2)) +
  scale_colour_manual(values = PAL, name = "Percentile") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
  labs(title = "B   Organ-system hazard index, population-weighted mean exposure",
       x = "Hazard index (log scale)", y = NULL) +
  coord_cartesian(clip = "off") + thm

# --- C: how far the 500 m benzene surface moves -----------------------------
cc <- TS41[is_base == FALSE]
pC <- ggplot(cc, aes(y = win_lab, colour = pct_lab)) +
  geom_linerange(aes(xmin = cells_identical_pct, xmax = cells_within_one_step_pct),
                 linewidth = 1.1, alpha = 0.5, position = position_dodge(width = DW)) +
  geom_point(aes(x = cells_identical_pct), size = 2.0, shape = 21, fill = "white",
             stroke = 1.1, position = position_dodge(width = DW)) +
  geom_point(aes(x = cells_within_one_step_pct), size = 3.0,
             position = position_dodge(width = DW)) +
  scale_colour_manual(values = PAL, name = "Percentile") +
  scale_x_continuous(limits = c(60, 100), breaks = seq(60, 100, 10)) +
  labs(title = "C   500 m benzene cells relative to the published surface",
       subtitle = "open marker: identical value    filled marker: within one 0.05 ppb reporting step",
       x = "Share of the 378 mapped cells (%)", y = "Window") +
  thm + theme(plot.subtitle = element_text(size = 8.5, colour = "grey30"))

out_png <- file.path(BASE, "FinalFig", "FIG_S4.12_background_sensitivity.png")
if (requireNamespace("cowplot", quietly = TRUE)) {
  lg <- cowplot::get_legend(pA + theme(legend.margin = margin(0, 0, 0, 0)))
  g  <- cowplot::plot_grid(pA + theme(legend.position = "none"),
                           pB + theme(legend.position = "none"),
                           pC + theme(legend.position = "none"),
                           ncol = 1, rel_heights = c(1, 1.02, 1.02), align = "v", axis = "lr")
  g  <- cowplot::plot_grid(g, lg, ncol = 1, rel_heights = c(1, 0.06))
  ggsave(out_png, g, width = 7.3, height = 8.2, dpi = 400, bg = "white")
} else ggsave(out_png, pA, width = 7.3, height = 3, dpi = 400, bg = "white")
message("-> ", out_png)

# ---------------------------------------------------------------------------
# claim-by-claim check of the sentences in SI section S4.6
# ---------------------------------------------------------------------------
hr("SI S4.6, claim by claim")
ck <- function(lab, got, claim, tol = 0.01) {
  ok <- is.finite(got) && is.finite(claim) && abs(got - claim) <= tol * max(1, abs(claim))
  cat(sprintf("  [%s] %-52s run %-10s S4.6 says %s\n", if (ok) "OK  " else "EDIT", lab,
              format(signif(got, 4)), format(claim)))
}
ck("lowest population-weighted benzene (ppb)",  min(TS41$mobile_pw_ppb),  0.1464, 0.005)
ck("highest population-weighted benzene (ppb)", max(TS41$mobile_pw_ppb),  0.1535, 0.005)
ck("AirToxScreen on the same blocks (ppb)",     airtox_pw,                0.1611, 0.005)
ck("lowest mobile/AirToxScreen ratio",          min(TS41$ratio_mobile_over_airtox), 0.909, 0.005)
ck("highest mobile/AirToxScreen ratio",         max(TS41$ratio_mobile_over_airtox), 0.953, 0.005)
ck("lowest mobile cases, IUR 5.75",             min(TS41$mobile_cases_low),  0.1065, 0.01)
ck("highest mobile cases, IUR 20.40",           max(TS41$mobile_cases_high), 0.3962, 0.01)
ck("lowest endocrine HI, community metric",     min(TS41$HI_pwmean_Endocrine),   1.586, 0.005)
ck("highest endocrine HI, community metric",    max(TS41$HI_pwmean_Endocrine),   1.612, 0.005)
ck("lowest respiratory HI, community metric",   min(TS41$HI_pwmean_Respiratory), 0.361, 0.01)
ck("highest respiratory HI, community metric",  max(TS41$HI_pwmean_Respiratory), 0.378, 0.01)
ck("highest neurological HI, community metric", max(TS41$HI_pwmean_Neurological), 0.032, 0.05)
ck("lowest endocrine HI, most-exposed block",   min(TS41$HI_maxblock_Endocrine),   8.146, 0.005)
ck("highest endocrine HI, most-exposed block",  max(TS41$HI_maxblock_Endocrine),   9.843, 0.005)
ck("lowest respiratory HI, most-exposed block", min(TS41$HI_maxblock_Respiratory), 4.940, 0.005)
ck("highest respiratory HI, most-exposed block",max(TS41$HI_maxblock_Respiratory), 5.013, 0.005)
ck("largest change in any 500 m benzene cell (ppb)", max(TS41$max_abs_diff_ppb),   0.1165, 0.01)
ck("lowest share within one reporting step (%)",
   min(TS41[is_base == FALSE, cells_within_one_step_pct]), 89.2, 0.02)
ck("highest share within one reporting step (%)",
   max(TS41[is_base == FALSE, cells_within_one_step_pct]), 97.6, 0.02)
.ce <- merge(cellhi[tos == "Endocrine"], data.table(arm = names(arms)), by = "arm")
ck("lowest share of 500 m cells with endocrine HI > 1 (%)",  min(.ce$pct_HI_gt1), 36.4, 0.02)
ck("highest share of 500 m cells with endocrine HI > 1 (%)", max(.ce$pct_HI_gt1), 59.4, 0.02)
ck("HCN reference concentration at site pressure (ppb)", unname(rfc_ppb[["HCN"]]), 0.884, 0.01)
ck("lowest share of 500 m benzene cells unchanged (%)",
   min(TS41[is_base == FALSE, cells_identical_pct]), 64.3, 0.02)
ck("highest share of 500 m benzene cells unchanged (%)",
   max(TS41[is_base == FALSE, cells_identical_pct]), 80.2, 0.02)
cat("\n  EDIT means the run disagrees with the sentence in S4.6 and the SI\n")
cat("  should carry the run's number instead.\n")
message("\nDONE.")
