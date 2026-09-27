# ==============================================================
# 70  SOURCE ATTRIBUTION of the retained H2S plumes: WWTP vs refinery
# --------------------------------------------------------------
# Two-tier chemical fingerprint applied to the H2S plumes that
# already passed the wind-direction + stability funnel in P07
# (the 4 retained plumes in the current run: ids 2, 10, 13, 32).
#
#     AVAILABILITY IS PART OF THE TEST. A co-species that was never
#     measured inside a plume window cannot support either verdict, so
#     the gate is three-valued: co-enhancement seen / looked and saw
#     none / not evaluable. HCN is the binding case - the HCN record
#     begins 2025-01-22 and every retained plume predates it.
#
#   TIER 1 - FINGERPRINT GATE
#     Rationale: wastewater H2S (anaerobic digester / sewer gas) is
#     aromatic- and HCN-POOR. A petroleum refinery co-emits H2S WITH
#     aromatics (BTEX from tankage/flares) and HCN (FCC unit). So an
#     H2S plume with NO co-located aromatic/HCN enhancement is
#     WWTP-consistent; one that co-occurs with a BTEX or HCN plume is
#     refinery-influenced and should not be attributed to the WWTF.
#     Criterion = the SAME 3-sigma rolling-background plume flag used
#     for H2S (plume_Benzene / plume_Toluene / plume_Xylene /
#     plume_Trimethylbenzene / plume_HCN), evaluated at the H2S-plume
#     points. Magnitudes (delta vs baseline) reported for transparency.
#
#   TIER 2 - CH4 CORROBORATION (independent tracer)
#     Rationale: WWTP digester gas is CH4-rich, so a genuine WWTF
#     plume should show co-located CH4 enhancement. CH4 (Picarro G2204,
#     same instrument as H2S) is joined from mobile_methane.csv by
#     (Asset, date); a local CH4 background is built with the SAME
#     rolling 20-min lowest-20th-percentile method as the toxics, and
#     delta-CH4 (and delta-CH4 / delta-H2S) reported per plume.
#
# Inputs  (all produced by rerun_pipeline/RUN_ALL_from_raw.R):
#   mobile_hrrr_windfromwwtf_stability_filtered.RData  (object res_sub)
#   mobile_methane.csv                                 (Asset,date,ch4_ppm,...)
# Outputs:
#   WWTP_H2S_source_attribution.csv        (per-plume table)
#   WWTP_H2S_source_attribution_points.csv (per-point diagnostics)
# Reproducible; heavy diagnostics. No new tuning of the H2S funnel.
# ==============================================================

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate); library(readr); library(tibble)
})

BASE    <- SUNCOR_BASE
STAB_FN <- file.path(BASE, "mobile_hrrr_windfromwwtf_stability_filtered.RData")
CH4_FN  <- file.path(BASE, "mobile_methane.csv")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
hr <- function(s) cat("\n========== ", s, " ==========\n", sep = "")

# ---- P07 funnel settings (MUST match P07 so keep_ids are identical) ----
gap_sec <- 120; min_pts <- 3; min_peak_dh2s <- 1.0
edge_k <- 2; min_rise_dh2s <- 0.3; min_fall_dh2s <- 0.3
min_prom_dh2s <- 0.2; enforce_shape_n <- 5
dur_min_s <- 10; dur_max_s <- 180; max_wind_sd_deg <- 15
keep_stab_levels <- c("B", "C", "D")

# ---- Tier-1 fingerprint thresholds (reported; 3-sigma flag is primary) ----
AROMATICS  <- c("Benzene", "Toluene", "Xylene", "Trimethylbenzene")
# fallback delta thresholds (ppb) used ONLY if a 3-sigma plume_* flag column
# is unavailable for a species; chosen near each species' campaign p95:
delta_thresh <- c(Benzene = 0.8, Toluene = 1.6, Xylene = 1.2,
                  Trimethylbenzene = 1.0, HCN = 7.0)
# CH4 corroboration: delta-CH4 threshold (ppm) AND a 3-sigma rolling test
ch4_delta_ppm_min <- 0.10

circ_sd_deg <- function(theta_deg) {
  th <- theta_deg[is.finite(theta_deg)]; if (length(th) < 2) return(NA_real_)
  r <- th * pi/180; C <- mean(cos(r)); S <- mean(sin(r)); R <- sqrt(C^2 + S^2)
  if (!is.finite(R) || R <= 0) return(180); sqrt(-2*log(R)) * 180/pi
}
# rolling 20-min lowest-20th-pct background + sd (same recipe as script 10)
roll_bg <- function(x, tsec, before = 600, after = 600, min_n = 30) {
  o <- order(tsec); xo <- x[o]; to <- tsec[o]
  b <- rep(NA_real_, length(xo)); s <- rep(NA_real_, length(xo))
  lo <- 1L
  for (i in seq_along(xo)) {
    while (to[i] - to[lo] > before) lo <- lo + 1L
    hi <- i; while (hi < length(xo) && to[hi + 1L] - to[i] <= after) hi <- hi + 1L
    w <- xo[lo:hi]; w <- w[is.finite(w)]
    if (length(w) >= min_n) { b[i] <- as.numeric(quantile(w, 0.20, names = FALSE)); s[i] <- sd(w) }
  }
  bb <- rep(NA_real_, length(x)); ss <- rep(NA_real_, length(x))
  bb[o] <- b; ss[o] <- s; list(bg = bb, sd = ss)
}

# ==============================================================
# 0) LOAD + column resolution
# ==============================================================
hr("LOAD res_sub")
stopifnot(file.exists(STAB_FN))
e <- new.env(); load(STAB_FN, envir = e)
obj <- if (exists("res_sub", envir = e)) "res_sub" else ls(e)[1]
dat <- get(obj, envir = e)
cat("object:", obj, "| rows:", nrow(dat), "| cols:", ncol(dat), "\n")
cat("names:\n"); print(names(dat))

# resolve a column by candidate names (short or *_ppb)
pick <- function(cands) { hit <- cands[cands %in% names(dat)]; if (length(hit)) hit[1] else NA_character_ }
col_H2S   <- pick(c("H2S", "Hydrogen_Sulfide_ppb"))
col_bH2S  <- pick(c("baseline_H2S", "baseline_Hydrogen_Sulfide_ppb"))
col_pH2S  <- pick(c("plume_H2S", "plume_Hydrogen_Sulfide_ppb"))
col_dist  <- pick(c("distance_wwtp"))
col_wspd  <- pick(c("windspd", "ws"))
col_wind  <- pick(c("winddir", "wd"))
col_asset <- pick(c("Asset"))
col_stab  <- pick(c("Stability_Class_simple", "Stability_Class"))
cat(sprintf("\nResolved: H2S=%s base=%s plume=%s dist=%s wspd=%s wind=%s asset=%s stab=%s\n",
            col_H2S, col_bH2S, col_pH2S, col_dist, col_wspd, col_wind, col_asset, col_stab))
stopifnot(!is.na(col_H2S), !is.na(col_bH2S), !is.na(col_pH2S), !is.na(col_dist), !is.na(col_wspd))

.ppb_map <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb", Xylene = "Xylene_ppb",
              Trimethylbenzene = "Trimethylbenzene_ppb", HCN = "Hydrogen_Cyanide_ppb")
sp_col <- function(sp) pick(c(sp, paste0(sp, "_ppb"), .ppb_map[[sp]]))
base_col <- function(sp) { vc <- sp_col(sp)
  pick(c(paste0("baseline_", sp), if (!is.na(vc)) paste0("baseline_", vc))) }
plume_col <- function(sp) { vc <- sp_col(sp)
  pick(c(paste0("plume_", sp), if (!is.na(vc)) paste0("plume_", vc))) }
sd_col <- function(sp) { vc <- sp_col(sp)
  pick(c(paste0("sd_", sp), if (!is.na(vc)) paste0("sd_", vc))) }

cat("\nCo-pollutant column availability:\n")
for (sp in c(AROMATICS, "HCN"))
  cat(sprintf("  %-18s value=%-22s baseline=%-26s sd=%-22s plumeflag=%s\n",
              sp, sp_col(sp) %||% "-", base_col(sp) %||% "-", sd_col(sp) %||% "-",
              plume_col(sp) %||% "-"))

# ==============================================================
# 1) REPRODUCE the retained H2S plumes (P07 logic, verbatim params)
# ==============================================================
hr("REPRODUCE retained H2S plumes")
dat <- dat %>%
  mutate(date = as.POSIXct(date, tz = "UTC"),
         .dist = as.numeric(.data[[col_dist]]),
         .H2S  = suppressWarnings(as.numeric(.data[[col_H2S]])),
         .bH2S = suppressWarnings(as.numeric(.data[[col_bH2S]])),
         .dH2S = .H2S - .bH2S,
         .wspd = suppressWarnings(as.numeric(.data[[col_wspd]])),
         .wind = if (!is.na(col_wind)) suppressWarnings(as.numeric(.data[[col_wind]])) else NA_real_,
         .pH2S = .data[[col_pH2S]]) %>%
  filter(is.finite(date), is.finite(.dist), is.finite(.H2S), is.finite(.bH2S), is.finite(.dH2S),
         .H2S >= 0, .bH2S >= 0)

pts <- dat %>% filter(!is.na(.pH2S) & .pH2S == TRUE) %>% arrange(date) %>%
  mutate(dt = as.numeric(difftime(date, lag(date), units = "secs")),
         plume_id = cumsum(is.na(dt) | dt > gap_sec)) %>% select(-dt)
n_flag <- n_distinct(pts$plume_id)
keep_n <- pts %>% count(plume_id, name = "n") %>% filter(n >= min_pts) %>% pull(plume_id)
pts <- pts %>% filter(plume_id %in% keep_n)

evt <- pts %>% arrange(plume_id, date) %>% group_by(plume_id) %>%
  summarise(start_time = min(date), end_time = max(date),
            duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
            n_pts = n(), n_unique_t = n_distinct(date),
            peak_dH2S = max(.dH2S), time_at_peak = date[which.max(.dH2S)][1],
            dist_at_peak_km = .dist[which.max(.dH2S)][1],
            wind_sd_deg = circ_sd_deg(.wind),
            n_wind_vals = dplyr::n_distinct(.wind[is.finite(.wind)]),
            edge_left  = if (n() >= edge_k) median(head(.dH2S[is.finite(.dH2S)], edge_k)) else NA_real_,
            edge_right = if (n() >= edge_k) median(tail(.dH2S[is.finite(.dH2S)], edge_k)) else NA_real_,
            prom_dH2S = peak_dH2S - median(.dH2S),
            # Stability AT THE PLUME CENTERLINE (the point of peak dH2S), matching
            # what P08_gaussian_plumes_h2s.R uses for the Pasquill-Gifford sigmas.
            # Previously this took the MODAL class across all plume points, which
            # disagreed with the inversion for plumes spanning a stability change
            # (e.g. plume 9: mode B, centerline D).
            stability = if (!is.na(col_stab)) {
              as.character(.data[[col_stab]][which.max(.dH2S)][1])
            } else NA_character_,
            .groups = "drop") %>%
  mutate(rise_dH2S = peak_dH2S - edge_left, fall_dH2S = peak_dH2S - edge_right,
         pass_peak = is.finite(peak_dH2S) & peak_dH2S >= min_peak_dh2s,
         pass_rise = ifelse(n_pts >= enforce_shape_n, is.finite(rise_dH2S) & rise_dH2S >= min_rise_dh2s, TRUE),
         pass_fall = ifelse(n_pts >= enforce_shape_n, is.finite(fall_dH2S) & fall_dH2S >= min_fall_dh2s, TRUE),
         pass_prom = is.finite(prom_dH2S) & prom_dH2S >= min_prom_dh2s,
         # SYNCED WITH P07 (2026-09-27). These three clauses had drifted from
         # P07_identify_and_plot_plumes_for_h2s.R, which is the funnel the paper
         # reports and the one P08 inverts:
         #   pass_dur  was OR-ed with (n_unique_t >= min_pts), a condition every
         #             surviving event satisfies, so the 10-180 s window could
         #             not reject anything (P07 fixed this 2026-08-20).
         #   pass_wind passed an event whose wind SD was NA; P07 now records
         #             whether the criterion was evaluable at all.
         #   pass_stab passed an event with no stability class; P07 drops it
         #             (2026-08-21), because the Briggs sigmas are indexed by
         #             class and P08 cannot invert it.
         # The consequence was 6 retained events here against P07's 4 - and the
         # two extra, plumes 3 and 19, are 4 s spikes. This script is no longer
         # the authority on that set: it re-derives it only to CHECK the file
         # P07 writes, and stops if they disagree.
         pass_dur  = is.finite(duration_s) & duration_s >= dur_min_s & duration_s <= dur_max_s,
         wind_evaluable = is.finite(n_wind_vals) & n_wind_vals >= 2,
         pass_wind = ifelse(wind_evaluable,
                            is.finite(wind_sd_deg) & wind_sd_deg <= max_wind_sd_deg, TRUE),
         pass_stab = !is.na(stability) & stability %in% keep_stab_levels,
         pass_all  = pass_peak & pass_rise & pass_fall & pass_prom & pass_dur & pass_wind & pass_stab)
keep_ids <- evt %>% filter(pass_all) %>% pull(plume_id)
cat(sprintf("plume-flagged=%d  >=min_pts=%d  RETAINED=%d  ids: %s\n",
            n_flag, length(keep_n), length(keep_ids), paste(keep_ids, collapse = ", ")))

# ---- AUTHORITATIVE RETAINED SET: P07's file, not this reimplementation ------
RETAINED_FN <- file.path(BASE, "WWTP_H2S_retained_plumes.csv")
if (!file.exists(RETAINED_FN))
  stop("WWTP_H2S_retained_plumes.csv not found. Run plume_scripts/",
       "P07_identify_and_plot_plumes_for_h2s.R first - this script must not be the\n",
       "  authority on which plumes were retained.")
p07 <- readr::read_csv(RETAINED_FN, show_col_types = FALSE)
.fmt <- function(x) format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
.mine <- evt %>% filter(plume_id %in% keep_ids) %>%
  transmute(plume_id, start_time = .fmt(start_time), end_time = .fmt(end_time)) %>%
  arrange(plume_id)
.theirs <- p07 %>% transmute(plume_id, start_time = .fmt(start_time),
                             end_time = .fmt(end_time)) %>% arrange(plume_id)
hr("CROSS-CHECK against P07's retained set")
cat(sprintf("  P07 retained %d: %s\n", nrow(.theirs), paste(.theirs$plume_id, collapse = ", ")))
cat(sprintf("  this script   %d: %s\n", nrow(.mine),   paste(.mine$plume_id,   collapse = ", ")))
if (!isTRUE(all.equal(as.data.frame(.mine), as.data.frame(.theirs), check.attributes = FALSE))) {
  cat("\n  P07 only:\n"); print(as.data.frame(dplyr::anti_join(.theirs, .mine, by = names(.theirs))), row.names = FALSE)
  cat("  this script only:\n"); print(as.data.frame(dplyr::anti_join(.mine, .theirs, by = names(.mine))), row.names = FALSE)
  stop("The retained plume set here does not match P07's, on identifier or timestamp. ",
       "Do not interpret the attribution until the two funnels agree.")
}
cat("  MATCH: identifiers and timestamps agree exactly.\n")
keep_ids <- .theirs$plume_id
pts_keep <- pts %>% filter(plume_id %in% keep_ids)

# ==============================================================
# 2) TIER 1 - fingerprint: co-located aromatic / HCN enhancement
# ==============================================================
hr("TIER 1: aromatic / HCN fingerprint at each retained H2S plume")
co_species <- c(AROMATICS, "HCN")
per_pt <- pts_keep %>% select(plume_id, date, dplyr::any_of(col_asset), .dH2S)
# BUGFIX (2026-09-27) - ABSENCE OF DATA WAS BEING REPORTED AS ABSENCE OF SIGNAL.
# `any(x == TRUE, na.rm = TRUE)` returns FALSE when x is entirely NA, so a plume
# window in which a co-species was never measured came out as "no co-enhancement"
# and was written into the table as a hard FALSE. Every one of the four retained
# plumes predates 2025-01-22, so NO HCN measurement can exist for any of them,
# and their aromatic channels are NA as well; the fingerprint "WWTP-consistent
# (H2S-only)" and Table S6.2's "None" column were therefore produced entirely by
# missing data. Channel availability is now counted per species and per plume,
# an unmeasured channel yields NA rather than FALSE, and the classification
# distinguishes "evaluated, no co-enhancement" from "not evaluable".
finger <- lapply(keep_ids, function(id) {
  sub <- pts_keep %>% filter(plume_id == id)
  row <- tibble(plume_id = id)
  arom_any <- FALSE; arom_eval <- FALSE
  hcn_any  <- FALSE; hcn_eval  <- FALSE
  for (sp in co_species) {
    vc <- sp_col(sp); bc <- base_col(sp); pc <- plume_col(sp); sc <- sd_col(sp)
    dmax <- NA_real_; coflag <- NA; n_obs <- 0L
    if (!is.na(vc)) {
      v <- suppressWarnings(as.numeric(sub[[vc]]))
      b <- if (!is.na(bc)) suppressWarnings(as.numeric(sub[[bc]])) else NA_real_
      d <- if (!is.na(bc)) v - b else v
      # AVAILABILITY MUST NOT BE READ OFF THE FLAG COLUMN (2026-09-27, second
      # pass). 10_calculating_background_air_pollution_concentrations_rolling_.R
      # preallocates every plume_* column as FALSE and then assigns
      #     !is.na(x) & !is.na(b) & !is.na(s) & (x >= b + 3*s)
      # so the flag is FALSE both where the species was measured and stayed
      # below the threshold AND where it was never measured at all. It is never
      # NA, and counting its non-NA entries just returns the window length -
      # which is what the first version of this fix did, and why it reported
      # every channel as evaluable. A point is testable only where the value,
      # its rolling baseline and its rolling sd are all finite, because that is
      # exactly the condition under which the 3-sigma comparison can fire.
      sdv <- if (!is.na(sc)) suppressWarnings(as.numeric(sub[[sc]])) else rep(NA_real_, nrow(sub))
      testable <- if (!is.na(pc)) is.finite(v) & is.finite(b) & is.finite(sdv)
                  else is.finite(d)
      n_obs <- sum(testable)
      dmax  <- suppressWarnings(max(d, na.rm = TRUE)); if (!is.finite(dmax)) dmax <- NA_real_
      if (n_obs > 0L) {
        if (!is.na(pc)) coflag <- any(sub[[pc]][testable] == TRUE, na.rm = TRUE) # 3-sigma flag (primary)
        else coflag <- is.finite(dmax) && dmax >= (delta_thresh[[sp]] %||% Inf)  # fallback
      }
    }
    row[[paste0("d", sp)]]    <- round(dmax, 3)
    row[[paste0("co_", sp)]]  <- coflag
    row[[paste0("n_", sp)]]   <- n_obs
    # Coverage, not just presence. A channel with one testable point inside a
    # 172 s traverse has not been shown to be free of a co-plume; it has been
    # sampled once. Report the share of the H2S plume's own points at which the
    # co-species could be tested, so "evaluable" can be read with its weight.
    row[[paste0("cov_", sp)]] <- round(n_obs / nrow(sub), 3)
    if (sp %in% AROMATICS) {
      if (n_obs > 0L) arom_eval <- TRUE
      if (isTRUE(coflag)) arom_any <- TRUE
    } else if (sp == "HCN") {
      if (n_obs > 0L) hcn_eval <- TRUE
      if (isTRUE(coflag)) hcn_any <- TRUE
    }
  }
  # three-valued: TRUE = co-enhancement seen; FALSE = looked and saw none;
  # NA = the channel carried no data in this window, so nothing was tested.
  row$aromatic_evaluable  <- arom_eval
  row$hcn_evaluable       <- hcn_eval
  row$aromatic_coenhanced <- if (arom_any) TRUE else if (arom_eval) FALSE else NA
  row$hcn_coenhanced      <- if (hcn_any)  TRUE else if (hcn_eval)  FALSE else NA
  row
}) %>% bind_rows()

# ==============================================================
# 3) TIER 2 - CH4 corroboration
# ==============================================================
hr("TIER 2: CH4 corroboration")
ch4_ok <- file.exists(CH4_FN)
if (ch4_ok) {
  ch4 <- readr::read_csv(CH4_FN, show_col_types = FALSE) %>%
    mutate(date = as.POSIXct(date, tz = "UTC"))
  # PLUME EXEMPTION for CH4, mirroring the H2S plume branch: M01 now averages CH4
  # to its native 5 s cadence for the mapping/hotspot analyses and retains the
  # delivered series as ch4_ppm_raw. A plume intercept lasts only seconds, so the
  # cross-check uses the delivered signal when it is available.
  if ("ch4_ppm_raw" %in% names(ch4)) {
    ch4$ch4_ppm <- ch4$ch4_ppm_raw
    cat("  [CH4 PLUME EXEMPTION] using delivered ch4_ppm_raw (un-averaged) for the cross-check\n")
  } else {
    cat("  [CH4 PLUME EXEMPTION] ch4_ppm_raw absent; using ch4_ppm as delivered\n")
  }
  cat("mobile_methane.csv rows:", nrow(ch4), " assets:", paste(unique(ch4$Asset), collapse=","), "\n")
  # restrict to the Asset x day of the retained plumes (background only needs
  # the local day) -> keeps the rolling computation to a few days, not 2.6M rows
  retain_days   <- unique(as.Date(pts_keep$date))
  retain_assets <- if (!is.na(col_asset)) unique(as.character(pts_keep[[col_asset]])) else unique(ch4$Asset)
  cat("retained plume days:", paste(retain_days, collapse=", "),
      "| assets:", paste(retain_assets, collapse=","), "\n")
  ch4 <- ch4 %>% mutate(day = as.Date(date)) %>%
    filter(day %in% retain_days, Asset %in% retain_assets) %>%
    group_by(Asset, day) %>%
    group_modify(~{ bgs <- roll_bg(.x$ch4_ppm, as.numeric(.x$date))
      .x$ch4_bg <- bgs$bg; .x$ch4_sd <- bgs$sd; .x }) %>% ungroup() %>%
    mutate(dCH4 = ch4_ppm - ch4_bg,
           ch4_plume = is.finite(dCH4) & is.finite(ch4_sd) & (ch4_ppm >= ch4_bg + 3*ch4_sd))
  # ---- TOLERANCE nearest-time join (same Asset, absolute epoch) ----
  # The H2S plume timestamps are delay-corrected; mobile_methane.csv is not
  # necessarily shifted by the same amount, so an exact (Asset,date) join
  # misses by the vehicle delay (~21 s CAT). Match each plume point to the
  # nearest CH4 record within ch4_tol_s and report the offset actually found.
  ch4_tol_s <- 90
  ch4j <- ch4 %>% mutate(Asset = as.character(Asset), e = as.numeric(date)) %>% arrange(Asset, e)
  pj <- pts_keep %>%
    mutate(Asset = if (!is.na(col_asset)) as.character(.data[[col_asset]]) else NA_character_,
           e = as.numeric(date)) %>%
    select(plume_id, Asset, date, e, .dH2S)
  # diagnostic: epoch ranges per retained Asset x day
  for (a in unique(pj$Asset)) {
    pr <- range(pj$e[pj$Asset == a]); cr <- range(ch4j$e[ch4j$Asset == a])
    cat(sprintf("  [CH4 DIAG] Asset %s: plume-pts epoch %.0f..%.0f ; CH4 epoch %.0f..%.0f ; nearest-gap test below\n",
                a, pr[1], pr[2], cr[1], cr[2]))
  }
  nearest_ch4 <- function(a, ei) {
    cc <- ch4j[ch4j$Asset == a, ]
    if (!nrow(cc)) return(c(NA, NA, NA, NA, NA))
    j <- findInterval(ei, cc$e); cand <- unique(pmax(1L, pmin(nrow(cc), c(j, j + 1L))))
    d <- abs(cc$e[cand] - ei); k <- cand[which.min(d)]
    if (min(d) > ch4_tol_s) return(c(NA_real_, NA_real_, NA_real_, NA_real_, min(d)))
    c(cc$ch4_ppm[k], cc$ch4_bg[k], cc$dCH4[k], as.numeric(cc$ch4_plume[k]), min(d))
  }
  mm <- t(mapply(nearest_ch4, pj$Asset, pj$e))
  pj$ch4_ppm <- mm[, 1]; pj$ch4_bg <- mm[, 2]; pj$dCH4 <- mm[, 3]
  pj$ch4_plume <- mm[, 4] == 1; pj$match_dt <- mm[, 5]
  cat(sprintf("  [CH4 JOIN] matched %d / %d plume points within %ds (median |dt| matched = %.1fs; min unmatched gap = %.1fs)\n",
              sum(is.finite(pj$ch4_ppm)), nrow(pj), ch4_tol_s,
              suppressWarnings(median(pj$match_dt[is.finite(pj$ch4_ppm)], na.rm = TRUE)),
              suppressWarnings(min(pj$match_dt[!is.finite(pj$ch4_ppm)], na.rm = TRUE))))
  ch4_sum <- pj %>% group_by(plume_id) %>%
    summarise(n_ch4 = sum(is.finite(ch4_ppm)),
              dCH4_peak = suppressWarnings(max(dCH4, na.rm = TRUE)),
              ch4_coplume = any(ch4_plume == TRUE, na.rm = TRUE),
              dCH4_over_dH2S = { i <- which.max(.dH2S)
                if (length(i) && is.finite(dCH4[i])) round(dCH4[i] / .dH2S[i], 4) else NA_real_ },
              .groups = "drop") %>%
    mutate(dCH4_peak = ifelse(is.finite(dCH4_peak), round(dCH4_peak, 3), NA_real_),
           ch4_corroborates = (is.finite(dCH4_peak) & dCH4_peak >= ch4_delta_ppm_min) | ch4_coplume)
} else {
  cat("[WARN] mobile_methane.csv not found; CH4 tier skipped.\n")
  ch4_sum <- tibble(plume_id = keep_ids, n_ch4 = NA, dCH4_peak = NA_real_,
                    ch4_coplume = NA, dCH4_over_dH2S = NA_real_, ch4_corroborates = NA)
}

# ==============================================================
# 4) COMBINE + CLASSIFY
# ==============================================================
hr("CLASSIFICATION")
out <- evt %>% filter(plume_id %in% keep_ids) %>%
  select(plume_id, start_time, end_time, duration_s, n_pts, peak_dH2S,
         dist_at_peak_km, wind_sd_deg, stability) %>%
  left_join(finger, by = "plume_id") %>%
  left_join(ch4_sum, by = "plume_id") %>%
  mutate(
    # co_any: TRUE if either channel saw a co-plume; FALSE if at least one
    # channel was evaluable and none did; NA if neither channel was evaluable.
    co_any = dplyr::case_when(
      aromatic_coenhanced %in% TRUE | hcn_coenhanced %in% TRUE ~ TRUE,
      aromatic_evaluable | hcn_evaluable                       ~ FALSE,
      TRUE                                                     ~ NA),
    channels_tested = dplyr::case_when(
      aromatic_evaluable &  hcn_evaluable ~ "aromatics + HCN",
      aromatic_evaluable & !hcn_evaluable ~ "aromatics only",
      !aromatic_evaluable &  hcn_evaluable ~ "HCN only",
      TRUE                                 ~ "none"),
    fingerprint = dplyr::case_when(
      co_any %in% TRUE  ~ "refinery-influenced (H2S + aromatic/HCN)",
      co_any %in% FALSE ~ "no co-enhancement in the channels tested",
      TRUE              ~ "not evaluable (no aromatic or HCN data in window)"),
    attribution = dplyr::case_when(
      co_any %in% TRUE                                      ~ "refinery / mixed",
      co_any %in% FALSE &  ch4_corroborates %in% TRUE       ~ "WWTP-consistent (fingerprint + CH4)",
      co_any %in% FALSE & !(ch4_corroborates %in% TRUE)     ~ "WWTP-consistent (fingerprint only)",
      is.na(co_any)     &  ch4_corroborates %in% TRUE       ~ "unattributed by fingerprint; CH4 co-enhancement only",
      TRUE                                                  ~ "unattributed (fingerprint not evaluable)"))

readr::write_csv(out, file.path(BASE, "WWTP_H2S_source_attribution.csv"))
readr::write_csv(per_pt, file.path(BASE, "WWTP_H2S_source_attribution_points.csv"))

hr("SUMMARY")
print(as.data.frame(out), row.names = FALSE)
cat(sprintf("\nRetained H2S plumes: %d\n", nrow(out)))
cat(sprintf("  no co-enhancement in the channels tested:    %d\n", sum(out$co_any %in% FALSE)))
cat(sprintf("  refinery-influenced (aromatic/HCN co-plume): %d\n", sum(out$co_any %in% TRUE)))
cat(sprintf("  NOT EVALUABLE (no aromatic or HCN data):     %d\n", sum(is.na(out$co_any))))
if (ch4_ok) cat(sprintf("  of those with no co-enhancement, CH4-corroborated: %d\n",
                        sum(out$co_any %in% FALSE & out$ch4_corroborates %in% TRUE)))
cat("\nChannel availability per plume. n_<species> = points at which the 3-sigma test\n")
cat("could fire (value, rolling baseline and rolling sd all finite); cov_<species> = that\n")
cat("count as a fraction of the H2S plume's own points.\n")
print(as.data.frame(out[, c("plume_id", "start_time", "n_pts",
                            grep("^n_[A-Z]|^n_HCN", names(out), value = TRUE),
                            grep("^cov_", names(out), value = TRUE),
                            "channels_tested", "fingerprint")]), row.names = FALSE)
.low <- out[out$co_any %in% FALSE &
              pmin(out$cov_Benzene, out$cov_Toluene, out$cov_Xylene,
                   out$cov_Trimethylbenzene, out$cov_HCN, na.rm = TRUE) < 0.5, ]
if (nrow(.low))
  cat("\nNOTE: plume(s) ", paste(.low$plume_id, collapse = ", "),
      " are classified from co-species data covering less than half of the\n",
      "      H2S plume's points. Report the coverage alongside the verdict.\n", sep = "")
if (any(is.na(out$co_any)))
  cat("\n*** WARNING: one or more retained plumes have NO aromatic or HCN data in\n",
      "    the plume window. For those plumes the chemical fingerprint is UNTESTED.\n",
      "    Absence of a co-plume flag is absence of measurement, not absence of signal;\n",
      "    do not report them as aromatic- or HCN-free.\n", sep = "")
cat("\n[Saved] WWTP_H2S_source_attribution.csv  and  _points.csv\n")
