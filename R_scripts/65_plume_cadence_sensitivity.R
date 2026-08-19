# ==============================================================
# 65  PLUME-EMISSIONS SAMPLING-CADENCE SENSITIVITY  (SI)
# Does the ~5-s H2S acquisition (forward-filled to 1 s in the
# delivered record) bias the inferred WWTP H2S emission rate?
#
# The Gaussian inversion is LINEAR in the observed peak enhancement
# dH2S: Q = dH2S * U * 2*pi*sigy*sigz / (crosswind*vertical) * const.
# Averaging the H2S signal over a window of B seconds changes ONLY
# the peak enhancement of each plume (all geometry, wind, stability,
# PBL are unchanged). So we:
#   1. reproduce the exact detection + inversion at 1 s (baseline);
#   2. for B = 2, 5, 10 s, recompute each RETAINED plume's peak
#      enhancement with a centred B-second running mean of H2S (and
#      of the baseline), holding the retained set and all geometry
#      fixed - this isolates the temporal-smear effect on emissions;
#   3. re-run the full scenario grid and compare tons/year.
#
# Note: a centred running mean is phase-independent and mirrors an
# instrument that integrates over B s. Because H2S is already
# acquired ~every 5 s, B<=5 barely moves the peaks; B=10 (coarser
# than native) shows how under-resolving a plume lowers the estimate.
#
# Run scripts' detection settings are copied verbatim from
# Suncor_plume.Rmd so results match the manuscript pipeline.
#
# Outputs (BASE):
#   TABLE_plume_cadence_peaks.csv       per-plume peak by window
#   TABLE_plume_cadence_emissions.csv   emissions summary by window
#   FinalFig/FIG_plume_cadence_sensitivity.png
# Runtime ~1-2 min.
# ==============================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(lubridate); library(zoo); library(ggplot2); library(scales)
  library(patchwork); library(readr)
})
BASE <- "/Users/priyanka/Downloads/Suncor"
WINDOWS <- c(1, 2, 5, 10)   # seconds; 1 = delivered record (baseline)

# ---- detection settings (verbatim from Suncor_plume.Rmd) ------
gap_sec <- 120; min_pts <- 3; min_peak_dh2s <- 1.0
edge_k <- 2; min_rise_dh2s <- 0.3; min_fall_dh2s <- 0.3
min_prom_dh2s <- 0.2; enforce_shape_n <- 5
dur_min_s <- 10; dur_max_s <- 180; max_wind_sd_deg <- 15
keep_stab_levels <- c("B", "C", "D")

# ---- inversion settings (verbatim) ----------------------------
z_m <- 1.5; MW_H2S <- 34.08; mol_m3_air <- 2.7e25 / 6.022e23
seconds_per_year <- 365.25 * 24 * 3600; op_fraction <- 1.0
wwtp_stack_height_m <- 12.2; stack_heights_m <- c(1, 10, 15, 20, 30, 50)
wind_mults <- c(0.8, 1.0, 1.2); x_mults <- c(0.9, 1.0, 1.1)
y_offsets_m <- c(-100, -50, 0, 50, 100)

# ---- functions (verbatim) -------------------------------------
circ_sd_deg <- function(theta_deg) {
  th <- theta_deg[is.finite(theta_deg)]; if (length(th) < 2) return(NA_real_)
  tr <- th * pi/180; C <- mean(cos(tr)); S <- mean(sin(tr)); R <- sqrt(C^2+S^2)
  if (!is.finite(R) || R <= 0) return(180); sqrt(-2*log(R))*180/pi
}
col_exists <- function(df, nm) nm %in% names(df)
pick_first <- function(df, nms) { for (nm in nms) if (col_exists(df, nm))
  return(suppressWarnings(as.numeric(df[[nm]]))); rep(NA_real_, nrow(df)) }
pick_first_chr <- function(df, nms) { for (nm in nms) if (col_exists(df, nm))
  return(as.character(df[[nm]])); rep(NA_character_, nrow(df)) }
kg_s_to_tpy_metric <- function(kg_s, op_fraction = 1.0) kg_s*seconds_per_year*op_fraction/1000
sigma_y_pg <- function(CAT, x_km) { X <- pmax(x_km,1e-6)*1000
  dplyr::case_when(CAT %in% c("A","B") ~ 0.32*X*(1+(0.0004*X))^(-0.5),
    CAT=="C" ~ 0.22*X*(1+(0.0004*X))^(-0.5), CAT=="D" ~ 0.16*X*(1+(0.0004*X))^(-0.5),
    CAT %in% c("E","F") ~ 0.11*X*(1+(0.0004*X))^(-0.5), TRUE ~ NA_real_) }
sigma_z_pg <- function(CAT, x_km) { X <- pmax(x_km,1e-6)*1000
  dplyr::case_when(CAT %in% c("A","B") ~ 0.24*X*(1+(0.001*X))^(0.5),
    CAT=="C" ~ 0.20*X, CAT=="D" ~ 0.14*X*(1+(0.0003*X))^(-0.5),
    CAT %in% c("E","F") ~ 0.08*X*(1+(0.00015*X))^(-0.5), TRUE ~ NA_real_) }
shift_cat <- function(cat, shift=0) { lev <- c("A","B","C","D","E","F")
  idx <- match(cat, lev); ifelse(is.na(idx), NA_character_, lev[pmin(pmax(idx+shift,1),length(lev))]) }
vert_term_vec <- function(z, H, sigz, hpbl, reflections=TRUE) {
  ok <- is.finite(sigz)&sigz>0&is.finite(hpbl)&hpbl>0; out <- rep(NA_real_, length(sigz))
  if (!any(ok)) return(out); s <- sigz[ok]; L <- hpbl[ok]
  a <- exp(-0.5*((z-H)^2)/(s^2)); if (!reflections) { out[ok] <- a; return(out) }
  b <- exp(-0.5*((z+H)^2)/(s^2)); c <- exp(-0.5*((z+H-2*L)^2)/(s^2))
  d <- exp(-0.5*((z-H-2*L)^2)/(s^2)); e <- exp(-0.5*((z-H+2*L)^2)/(s^2))
  out[ok] <- a+b+c+d+e; out }
invert_gaussian <- function(inv_df, reflections, H_m, wind_mult, x_mult, cat_shift, y_m) {
  inv_df %>% dplyr::mutate(
    CAT_s = shift_cat(CAT, cat_shift), u_ms_s = u_ms*wind_mult,
    x_km_s = x_km*x_mult, sigy_s = sigma_y_pg(CAT_s, x_km_s), sigz_s = sigma_z_pg(CAT_s, x_km_s),
    crosswind = exp(-0.5*(y_m^2)/(sigy_s^2)),
    vertical = vert_term_vec(z=z_m, H=H_m, sigz=sigz_s, hpbl=hpbl_m, reflections=reflections),
    denom = crosswind*vertical, dH2S_ppm = dH2S_ppb/1000,
    Q_ppm_m3_s = (dH2S_ppm*u_ms_s*(2*pi*sigy_s*sigz_s))/denom,
    kg_s = Q_ppm_m3_s*mol_m3_air*1e-6*(MW_H2S/1000),
    tpy_metric = kg_s_to_tpy_metric(kg_s, op_fraction=op_fraction)) %>%
  dplyr::filter(is.finite(tpy_metric), tpy_metric>=0, is.finite(kg_s), kg_s>=0,
    is.finite(Q_ppm_m3_s), is.finite(sigy_s), sigy_s>0, is.finite(sigz_s), sigz_s>0,
    is.finite(vertical), vertical>0, is.finite(crosswind), crosswind>0, is.finite(denom), denom>0) }
mean_ci <- function(x) { x <- x[is.finite(x)]; n <- length(x); m <- mean(x); s <- sd(x)
  if (n<2) return(tibble(n=n, mean=m, lci=NA_real_, uci=NA_real_))
  moe <- qt(0.975, df=n-1)*(s/sqrt(n)); tibble(n=n, mean=m, lci=m-moe, uci=m+moe) }

# ---- LOAD + DETECTION (verbatim logic) ------------------------
load(file.path(BASE, "mobile_hrrr_windfromwwtf_stability_filtered.RData"))
if (!exists("res_sub")) stop("Expected object `res_sub`.")
dat <- res_sub
wind_col <- if ("winddir" %in% names(dat)) "winddir" else if ("wd" %in% names(dat)) "wd" else NA_character_
if (is.na(wind_col)) stop("No wind direction column.")
stopifnot(all(c("baseline_H2S","H2S","plume_H2S","distance_wwtp","windspd") %in% names(dat)))
dat <- dat %>% dplyr::mutate(date = as.POSIXct(date, tz="UTC"),
    distance_wwtp = as.numeric(distance_wwtp), H2S = suppressWarnings(as.numeric(H2S)),
    H2S_base = suppressWarnings(as.numeric(baseline_H2S)), dH2S = H2S - H2S_base) %>%
  dplyr::filter(is.finite(date), is.finite(distance_wwtp), is.finite(H2S),
    is.finite(H2S_base), is.finite(dH2S), H2S >= 0, H2S_base >= 0)
h2s_pts_raw <- dat %>% dplyr::filter(!is.na(plume_H2S) & plume_H2S == TRUE) %>%
  dplyr::arrange(date) %>% dplyr::mutate(
    dt_sec = as.numeric(difftime(date, dplyr::lag(date), units="secs")),
    plume_id = cumsum(is.na(dt_sec) | dt_sec > gap_sec)) %>% dplyr::select(-dt_sec)
keep_ids_n <- h2s_pts_raw %>% dplyr::count(plume_id, name="n_pts") %>%
  dplyr::filter(n_pts >= min_pts) %>% dplyr::pull(plume_id)
h2s_pts_all <- h2s_pts_raw %>% dplyr::filter(plume_id %in% keep_ids_n)
stab_col <- dplyr::case_when("Stability_Class_simple" %in% names(h2s_pts_all) ~ "Stability_Class_simple",
  "Stability_Class" %in% names(h2s_pts_all) ~ "Stability_Class", TRUE ~ NA_character_)
h2s_evt_all <- h2s_pts_all %>% dplyr::arrange(plume_id, date) %>% dplyr::group_by(plume_id) %>%
  dplyr::summarise(start_time=min(date), end_time=max(date),
    duration_s=as.numeric(difftime(end_time,start_time,units="secs")),
    n_pts=dplyr::n(), n_unique_t=dplyr::n_distinct(date),
    peak_dH2S=max(dH2S), med_dH2S=median(dH2S), peak_H2S=max(H2S), med_H2S=median(H2S),
    mean_dist_km=mean(distance_wwtp), dist_at_peak_km=distance_wwtp[which.max(dH2S)][1],
    time_at_peak=date[which.max(dH2S)][1], windspd_at_peak=windspd[which.max(dH2S)][1],
    wind_sd_deg=circ_sd_deg(.data[[wind_col]]),
    edge_left={x<-dH2S[is.finite(dH2S)]; if(length(x)>=edge_k) median(head(x,edge_k)) else NA_real_},
    edge_right={x<-dH2S[is.finite(dH2S)]; if(length(x)>=edge_k) median(tail(x,edge_k)) else NA_real_},
    rise_dH2S=peak_dH2S-edge_left, fall_dH2S=peak_dH2S-edge_right, prom_dH2S=peak_dH2S-median(dH2S),
    stability=if(!is.na(stab_col)){x<-.data[[stab_col]]; x<-x[!is.na(x)]
      if(!length(x)) NA_character_ else names(sort(table(x),decreasing=TRUE))[1]} else NA_character_,
    .groups="drop")
h2s_evt_flags <- h2s_evt_all %>% dplyr::mutate(
  pass_peak = is.finite(peak_dH2S) & peak_dH2S >= min_peak_dh2s,
  pass_rise = dplyr::if_else(n_pts>=enforce_shape_n, is.finite(rise_dH2S)&rise_dH2S>=min_rise_dh2s, TRUE),
  pass_fall = dplyr::if_else(n_pts>=enforce_shape_n, is.finite(fall_dH2S)&fall_dH2S>=min_fall_dh2s, TRUE),
  pass_singlepk = is.finite(prom_dH2S) & prom_dH2S >= min_prom_dh2s,
  pass_dur = (is.finite(duration_s)&duration_s>=dur_min_s&duration_s<=dur_max_s)|(n_unique_t>=min_pts),
  pass_wind = is.na(wind_sd_deg) | wind_sd_deg <= max_wind_sd_deg,
  pass_stab = dplyr::if_else(is.na(stability), TRUE, stability %in% keep_stab_levels),
  pass_all = pass_peak&pass_rise&pass_fall&pass_singlepk&pass_dur&pass_wind&pass_stab)
keep_ids <- h2s_evt_flags %>% dplyr::filter(pass_all) %>% dplyr::pull(plume_id)
h2s_pts_keep <- h2s_pts_all %>% dplyr::filter(plume_id %in% keep_ids)
centerline_keep <- h2s_pts_keep %>% dplyr::group_by(plume_id) %>%
  dplyr::slice_max(dH2S, n=1, with_ties=FALSE) %>% dplyr::ungroup()
message("Retained plumes: ", length(keep_ids), " (expected 4)")

# ---- build inv_base from a centerline-like df -----------------
build_inv <- function(df0) {
  df0 <- as.data.frame(df0)
  tibble(plume_id = df0$plume_id,
    dH2S_ppb = suppressWarnings(as.numeric(df0$dH2S)),
    u_ms = pick_first(df0, c("windspd_at_peak","windspd","wind_speed_ms","ws")),
    x_km = pick_first(df0, c("dist_at_peak_km","distance_wwtp","x_km")),
    hpbl_m = pick_first(df0, c("hpbl","hpbl_m","pbl_m")),
    CAT = dplyr::recode(pick_first_chr(df0, c("Stability_Class_simple","Stability_Class","stability","CAT")),
      "A-B"="A","B-C"="B")) %>%
  dplyr::filter(is.finite(dH2S_ppb), dH2S_ppb>0, is.finite(u_ms), u_ms>0,
    is.finite(x_km), x_km>0, is.finite(hpbl_m), hpbl_m>0, CAT %in% c("A","B","C","D","E","F"))
}

# ---- scenario grid (verbatim) ---------------------------------
scenarios <- dplyr::bind_rows(
  tibble(sens_group="baseline", scenario=paste0("baseline_H_",wwtp_stack_height_m,"m"),
    reflections=TRUE, H_m=wwtp_stack_height_m, wind_mult=1, x_mult=1, cat_shift=0, y_m=0),
  tibble(sens_group="reflections", scenario=c("reflections_TRUE","reflections_FALSE"),
    reflections=c(TRUE,FALSE), H_m=wwtp_stack_height_m, wind_mult=1, x_mult=1, cat_shift=0, y_m=0),
  tibble(sens_group="stack_height", scenario=paste0("H_",stack_heights_m,"m"),
    reflections=TRUE, H_m=stack_heights_m, wind_mult=1, x_mult=1, cat_shift=0, y_m=0),
  tibble(sens_group="wind_uncertainty", scenario=paste0("wind_x",wind_mults),
    reflections=TRUE, H_m=wwtp_stack_height_m, wind_mult=wind_mults, x_mult=1, cat_shift=0, y_m=0),
  tibble(sens_group="stability_CAT_shift", scenario=c("CAT_minus1","CAT_0","CAT_plus1"),
    reflections=TRUE, H_m=wwtp_stack_height_m, wind_mult=1, x_mult=1, cat_shift=c(-1,0,1), y_m=0),
  tibble(sens_group="x_distance_uncertainty", scenario=paste0("x_mult_",x_mults),
    reflections=TRUE, H_m=wwtp_stack_height_m, wind_mult=1, x_mult=x_mults, cat_shift=0, y_m=0),
  tibble(sens_group="crosswind_y_uncertainty", scenario=paste0("y_",ifelse(y_offsets_m>=0,"+",""),y_offsets_m,"m"),
    reflections=TRUE, H_m=wwtp_stack_height_m, wind_mult=1, x_mult=1, cat_shift=0, y_m=y_offsets_m))

run_scenarios <- function(inv_df) {
  scenarios %>% dplyr::mutate(data = purrr::pmap(
    list(reflections,H_m,wind_mult,x_mult,cat_shift,y_m),
    ~invert_gaussian(inv_df, reflections=..1, H_m=..2, wind_mult=..3, x_mult=..4, cat_shift=..5, y_m=..6))) %>%
    dplyr::select(sens_group, scenario, data) %>% tidyr::unnest(data)
}

# ---- native-cadence reconstruction ----------------------------
# For each retained plume and window B, bin the (already delay-
# corrected) plume points to B seconds and average EVERY inversion
# input together - H2S enhancement, wind speed, downwind distance,
# PBL height, stability - then take the peak bin. This re-joins the
# averaged wind to the averaged concentration, exactly as "delay ->
# average over B s -> join wind -> invert". B = 5 s is the native
# H2S cadence and is the primary, most-defensible estimate; 1 s is
# the delivered record; 2 and 10 s bracket it.
hp <- as.data.frame(h2s_pts_keep)
hp$u_ms   <- pick_first(hp, c("windspd_at_peak","windspd","wind_speed_ms","ws"))
hp$x_km   <- pick_first(hp, c("dist_at_peak_km","distance_wwtp","x_km"))
hp$hpbl_m <- pick_first(hp, c("hpbl","hpbl_m","pbl_m"))
hp$CAT    <- dplyr::recode(pick_first_chr(hp,
               c("Stability_Class_simple","Stability_Class","stability","CAT")), "A-B"="A","B-C"="B")
hp$epoch  <- as.numeric(hp$date)

bin_plume_peak <- function(pp, B) {
  pp <- pp[order(pp$date), ]
  pp$tb <- if (B <= 1) seq_len(nrow(pp)) else floor(pp$epoch / B)
  a <- pp %>% dplyr::group_by(tb) %>% dplyr::summarise(
    dH2S_ppb = mean(H2S, na.rm=TRUE) - mean(H2S_base, na.rm=TRUE),
    u_ms   = mean(u_ms,   na.rm=TRUE),
    x_km   = mean(x_km,   na.rm=TRUE),
    hpbl_m = mean(hpbl_m, na.rm=TRUE),
    CAT    = { x <- CAT[!is.na(CAT)]; if (length(x)) names(sort(table(x), decreasing=TRUE))[1] else NA_character_ },
    .groups = "drop")
  a[which.max(a$dH2S_ppb), c("dH2S_ppb","u_ms","x_km","hpbl_m","CAT")]
}
inv_for_B <- function(B) {
  hp %>% dplyr::group_by(plume_id) %>% dplyr::group_modify(~ bin_plume_peak(.x, B)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(dH2S_ppb), dH2S_ppb > 0, is.finite(u_ms), u_ms > 0,
      is.finite(x_km), x_km > 0, is.finite(hpbl_m), hpbl_m > 0, CAT %in% c("A","B","C","D","E","F"))
}

peak_tbl <- purrr::map_dfr(WINDOWS, function(B)
  inv_for_B(B) %>% dplyr::transmute(plume_id, window_s = B, peak_dH2S = round(dH2S_ppb, 3)))
peak_wide <- peak_tbl %>% tidyr::pivot_wider(names_from=window_s, values_from=peak_dH2S, names_prefix="peak_") %>%
  dplyr::mutate(factor_5s_vs_1s = round(peak_5/peak_1, 3), factor_10s_vs_1s = round(peak_10/peak_1, 3))
readr::write_csv(peak_wide, file.path(BASE, "TABLE_plume_cadence_peaks.csv"))
message("\nPer-plume peak enhancement (ppb) by native averaging window:"); print(peak_wide)

emis <- purrr::map_dfr(WINDOWS, function(B) {
  res <- run_scenarios(inv_for_B(B)); base_only <- res %>% dplyr::filter(sens_group=="baseline")
  tibble(window_s = B, n_plumes = dplyr::n_distinct(res$plume_id),
    baseline_mean_tpy = round(mean(base_only$tpy_metric), 0),
    baseline_median_tpy = round(median(base_only$tpy_metric), 0),
    allscen_mean_tpy = round(mean(res$tpy_metric), 0),
    allscen_min_tpy = round(min(res$tpy_metric), 0),
    allscen_max_tpy = round(max(res$tpy_metric), 0))
})
emis <- emis %>% dplyr::mutate(
  cadence = dplyr::case_when(window_s==1 ~ "delivered (1 s)", window_s==5 ~ "native H2S (5 s)",
                             TRUE ~ paste0(window_s, " s")),
  pct_vs_1s = round(100*(baseline_mean_tpy/baseline_mean_tpy[window_s==1] - 1), 1))
readr::write_csv(emis, file.path(BASE, "TABLE_plume_cadence_emissions.csv"))
message("\nInferred WWTP H2S emissions by cadence (t/yr; 5 s = native):"); print(emis)

# ---- figure ---------------------------------------------------
p1 <- ggplot(peak_tbl, aes(factor(window_s), peak_dH2S, group=factor(plume_id), color=factor(plume_id))) +
  geom_line() + geom_point(size=2) +
  labs(x="Averaging window (s); 5 = native H2S cadence", y="Peak plume enhancement dH2S (ppb)",
       color="Plume", title="A) Per-plume peak enhancement vs averaging window") +
  theme_bw(base_size=11)
p2 <- ggplot(emis, aes(factor(window_s), baseline_mean_tpy, fill = window_s==5)) +
  geom_col(color="grey25", width=0.6, show.legend=FALSE) +
  scale_fill_manual(values=c(`FALSE`="#9ecae1", `TRUE`="#2171b5")) +
  geom_errorbar(aes(ymin=allscen_min_tpy, ymax=allscen_max_tpy), width=0.2) +
  geom_text(aes(label=scales::comma(baseline_mean_tpy)), vjust=-0.6, size=3) +
  labs(x="Averaging window (s); 5 = native H2S cadence (primary)",
       y="Inferred H2S emissions (t/yr)",
       title="B) Emissions (bar = baseline-scenario mean; whiskers = full scenario range)") +
  theme_bw(base_size=11)
ggsave(file.path(BASE,"FinalFig","FIG_plume_cadence_sensitivity.png"),
       p1/p2, width=9, height=8, dpi=350, bg="white")
message("[Saved] FinalFig/FIG_plume_cadence_sensitivity.png\nDONE.")
