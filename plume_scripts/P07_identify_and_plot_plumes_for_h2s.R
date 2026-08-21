# ==============================================================
# P07  Identify and plot plumes for H2S
# Auto-split from Suncor_plume.Rmd  (section 7 of 10)
# ==============================================================

#Identify and plot plumes for H2S

# ============================================================
# H2S PLUME QA + CENTERLINE SURROGATE + DIAGNOSTICS (WWTP)
# + STEP-COUNT TABLE + FUNNEL FIGURE FOR SI
#
# Uses actual column names in res_sub:
#   H2S, baseline_H2S, plume_H2S
# Uses:
#   distance_wwtp, wind_from_deg_wwtf
# Shows:
#   all kept plumes (no top-N / last-10 restriction)
# Outputs:
#   - WWTP_H2S_plume_step_counts.csv
#   - WWTP_H2S_plume_funnel.csv
#   - FIG_WWTP_H2S_plume_funnel.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(tibble)
  library(readr)
})

# ----------------------------
# SETTINGS
# ----------------------------
gap_sec          <- 120   # new plume if time gap > this (seconds)
min_pts          <- 3     # minimum points per plume
min_peak_dh2s    <- 1.0   # ppb

# Shape diagnostics
edge_k           <- 2
min_rise_dh2s    <- 0.3
min_fall_dh2s    <- 0.3
min_prom_dh2s    <- 0.2
enforce_shape_n  <- 5

# Duration / wind / stability
dur_min_s        <- 10
dur_max_s        <- 180
# (2026-08-20) TRUE = the duration window actually rejects events. FALSE
# restores the previous behaviour, in which the window was unreachable because
# it was OR-ed with a condition every surviving event already satisfied.
DUR_STRICT       <- TRUE
max_wind_sd_deg  <- 15
keep_stab_levels <- c("B", "C", "D")

out_dir <- "/Users/priyanka/Downloads/Suncor"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# LOAD
# ----------------------------
load("/Users/priyanka/Downloads/Suncor/mobile_hrrr_windfromwwtf_stability_filtered.RData")

if (!exists("res_sub")) stop("Expected object `res_sub` in loaded file.")
dat <- res_sub

# ----------------------------
# Helper: circular SD for wind direction (degrees)
# ----------------------------
circ_sd_deg <- function(theta_deg) {
  th <- theta_deg[is.finite(theta_deg)]
  if (length(th) < 2) return(NA_real_)
  th_rad <- th * pi / 180
  C <- mean(cos(th_rad))
  S <- mean(sin(th_rad))
  R <- sqrt(C^2 + S^2)
  if (!is.finite(R) || R <= 0) return(180)
  sd_rad <- sqrt(-2 * log(R))
  sd_rad * 180 / pi
}

# ----------------------------
# Detect wind direction column
# ----------------------------
wind_col <- if ("winddir" %in% names(dat)) {
  "winddir"
} else if ("wd" %in% names(dat)) {
  "wd"
} else {
  NA_character_
}

if (is.na(wind_col)) stop("Could not find wind direction column (`winddir` or `wd`).")

# ----------------------------
# 0) Basic cleaning + REQUIRED enhancement
# ----------------------------
stopifnot("baseline_H2S" %in% names(dat))
stopifnot("H2S" %in% names(dat))
stopifnot("plume_H2S" %in% names(dat))
stopifnot("distance_wwtp" %in% names(dat))
stopifnot("windspd" %in% names(dat))

dat <- dat %>%
  dplyr::mutate(
    date = as.POSIXct(date, tz = "UTC"),
    distance_wwtp = as.numeric(distance_wwtp),
    H2S      = suppressWarnings(as.numeric(H2S)),
    H2S_base = suppressWarnings(as.numeric(baseline_H2S)),
    dH2S     = H2S - H2S_base
  ) %>%
  dplyr::filter(
    is.finite(date),
    is.finite(distance_wwtp),
    is.finite(H2S),
    is.finite(H2S_base),
    is.finite(dH2S)
  ) %>%
  dplyr::filter(H2S >= 0, H2S_base >= 0)

# ----------------------------
# 1) Keep only H2S plume-flagged points and assign plume_id
# ----------------------------
# BUGFIX (2026-08-20): segmentation sorted on `date` alone and started a new
# plume only on a time gap, with no vehicle key. Both mobile laboratories are
# in this record, and when they sample simultaneously their 1-s rows interleave
# with dt_sec = 0, so two vehicles at two different locations were welded into
# a single plume event. That corrupts duration_s, the edge/rise/fall shape
# statistics, the point count, and dist_at_peak_km for any event where it
# fires. Segment within a vehicle: a new plume starts on a time gap OR on a
# change of Asset.
if (!"Asset" %in% names(dat)) {
  message("[SEG] no `Asset` column in the plume input - treating the record as a single vehicle. ",
          "Check that 06_merge_with_wind.R still carries Asset if this is unexpected.")
  dat$Asset <- "UNKNOWN"
}
h2s_pts_raw <- dat %>%
  dplyr::filter(!is.na(plume_H2S) & plume_H2S == TRUE) %>%
  dplyr::arrange(Asset, date) %>%
  dplyr::mutate(
    dt_sec   = as.numeric(difftime(date, dplyr::lag(date), units = "secs")),
    .newplume = is.na(dt_sec) | dt_sec > gap_sec |
                is.na(dplyr::lag(Asset)) | Asset != dplyr::lag(Asset),
    plume_id = cumsum(.newplume)
  ) %>%
  dplyr::select(-dt_sec, -.newplume)

message(sprintf("[SEG] %d plume-flagged seconds segmented into %d events across %d vehicle(s): %s",
                nrow(h2s_pts_raw), dplyr::n_distinct(h2s_pts_raw$plume_id),
                dplyr::n_distinct(h2s_pts_raw$Asset),
                paste(sort(unique(h2s_pts_raw$Asset)), collapse = ", ")))

n_plumes_flagged <- dplyr::n_distinct(h2s_pts_raw$plume_id)

keep_ids_n <- h2s_pts_raw %>%
  dplyr::count(plume_id, name = "n_pts") %>%
  dplyr::filter(n_pts >= min_pts) %>%
  dplyr::pull(plume_id)

h2s_pts_all <- h2s_pts_raw %>%
  dplyr::filter(plume_id %in% keep_ids_n)

n_plumes_minpts <- dplyr::n_distinct(h2s_pts_all$plume_id)

# ----------------------------
# Optional: stability column detection
# ----------------------------
stab_col <- dplyr::case_when(
  "Stability_Class_simple" %in% names(h2s_pts_all) ~ "Stability_Class_simple",
  "Stability_Class" %in% names(h2s_pts_all) ~ "Stability_Class",
  TRUE ~ NA_character_
)

# ----------------------------
# 2) Event-level summary per plume (pre-filter)
# ----------------------------
h2s_evt_all <- h2s_pts_all %>%
  dplyr::arrange(plume_id, date) %>%
  dplyr::group_by(plume_id) %>%
  dplyr::summarise(
    start_time = min(date, na.rm = TRUE),
    end_time   = max(date, na.rm = TRUE),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_pts      = dplyr::n(),
    n_unique_t = dplyr::n_distinct(date),

    peak_dH2S  = max(dH2S, na.rm = TRUE),
    med_dH2S   = stats::median(dH2S, na.rm = TRUE),
    peak_H2S   = max(H2S,  na.rm = TRUE),
    med_H2S    = stats::median(H2S,  na.rm = TRUE),

    mean_dist_km    = mean(distance_wwtp, na.rm = TRUE),
    dist_at_peak_km = distance_wwtp[which.max(dH2S)][1],
    time_at_peak    = date[which.max(dH2S)][1],

    Asset           = dplyr::first(Asset),
    windspd_at_peak = windspd[which.max(dH2S)][1],
    wind_sd_deg     = circ_sd_deg(.data[[wind_col]]),
    # (2026-08-20) How many DISTINCT wind directions the event actually saw.
    # The wind field is HRRR, sampled at the rounded hour and the nearest 3 km
    # grid point, so an event shorter than an hour that stays in one grid cell
    # sees a single constant value - and then circ_sd_deg() returns 0 and the
    # consistency criterion below passes by construction rather than by test.
    # Recording this lets the funnel state how many events the criterion could
    # be evaluated on instead of implying it screened all of them.
    n_wind_vals     = dplyr::n_distinct(.data[[wind_col]][is.finite(.data[[wind_col]])]),

    edge_left = {
      x <- dH2S[is.finite(dH2S)]
      if (length(x) >= edge_k) stats::median(head(x, edge_k), na.rm = TRUE) else NA_real_
    },
    edge_right = {
      x <- dH2S[is.finite(dH2S)]
      if (length(x) >= edge_k) stats::median(tail(x, edge_k), na.rm = TRUE) else NA_real_
    },

    rise_dH2S = peak_dH2S - edge_left,
    fall_dH2S = peak_dH2S - edge_right,
    prom_dH2S = peak_dH2S - stats::median(dH2S, na.rm = TRUE),

    stability = if (!is.na(stab_col)) {
      x <- .data[[stab_col]]
      x <- x[!is.na(x)]
      if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
    } else {
      NA_character_
    },

    .groups = "drop"
  )

# ----------------------------
# 3) Apply filters
# ----------------------------
h2s_evt_flags <- h2s_evt_all %>%
  dplyr::mutate(
    pass_peak = is.finite(peak_dH2S) & peak_dH2S >= min_peak_dh2s,

    pass_rise = dplyr::if_else(
      n_pts >= enforce_shape_n,
      is.finite(rise_dH2S) & rise_dH2S >= min_rise_dh2s,
      TRUE
    ),

    pass_fall = dplyr::if_else(
      n_pts >= enforce_shape_n,
      is.finite(fall_dH2S) & fall_dH2S >= min_fall_dh2s,
      TRUE
    ),

    pass_singlepk = is.finite(prom_dH2S) & prom_dH2S >= min_prom_dh2s,

    # BUGFIX (2026-08-20): pass_dur was
    #   (duration in [dur_min_s, dur_max_s]) | (n_unique_t >= min_pts)
    # and events reaching this point have already been filtered to
    # n_pts >= min_pts. On a 1-s grid n_unique_t therefore equals n_pts and the
    # right-hand clause is TRUE for every surviving event, so the duration
    # window could not reject anything - yet Figure S6.1 presents it as an
    # active quality filter. The window is now binding, which is what
    # dur_min_s and dur_max_s were introduced for: an event lasting under
    # dur_min_s is a spike rather than a traverse, and one lasting over
    # dur_max_s is not a discrete plume. Set DUR_STRICT to FALSE to restore the
    # previous, non-binding behaviour.
    pass_dur = if (DUR_STRICT) {
      is.finite(duration_s) & duration_s >= dur_min_s & duration_s <= dur_max_s
    } else {
      (is.finite(duration_s) & duration_s >= dur_min_s & duration_s <= dur_max_s) |
        (n_unique_t >= min_pts)
    },

    # BUGFIX (2026-08-20): pass_wind tested the circular SD of a wind direction
    # that is constant within the event whenever the event sits inside one HRRR
    # hour and one 3 km grid cell - which is almost always, for events capped at
    # dur_max_s. circ_sd_deg() then returns 0, the test passes, and the funnel
    # reports "0 dropped" as though the criterion had screened the set. The
    # criterion is retained, but an event whose wind field never varied is now
    # marked NOT EVALUABLE rather than silently passed, and the funnel reports
    # how many events it could actually be applied to.
    wind_evaluable = is.finite(n_wind_vals) & n_wind_vals >= 2,
    pass_wind = dplyr::if_else(wind_evaluable,
                               is.finite(wind_sd_deg) & wind_sd_deg <= max_wind_sd_deg,
                               TRUE),

    # BUGFIX (2026-08-21): missing stability used to PASS this filter. That is
    # not a conservative default, it is an inconsistency: the Briggs sigma_y and
    # sigma_z are indexed by stability class, so an event with no class cannot
    # be inverted at all. P08 drops it, and P07's retained count therefore
    # overstated the sample P08 actually inverts. An event with no stability is
    # now dropped here, where the funnel can report it. (P06 now hard-stops if
    # the cloud field is absent, so NA stability should be rare rather than
    # systematic; this guard is what makes that assumption visible if it fails.)
    pass_stab = !is.na(stability) & stability %in% keep_stab_levels,

    pass_all = pass_peak & pass_rise & pass_fall & pass_singlepk & pass_dur & pass_wind & pass_stab
  )

.n_stab_na <- sum(is.na(h2s_evt_flags$stability))
if (.n_stab_na > 0)
  message(sprintf(paste0("[FILTER] %d of %d candidate events have NO stability class and are ",
                         "dropped (they cannot be inverted: the Briggs sigmas are indexed by ",
                         "class). Previously these passed the filter and inflated the retained ",
                         "count relative to what P08 inverts."),
                  .n_stab_na, nrow(h2s_evt_flags)))

keep_ids <- h2s_evt_flags %>%
  dplyr::filter(pass_all) %>%
  dplyr::pull(plume_id)

h2s_pts_keep <- h2s_pts_all %>%
  dplyr::filter(plume_id %in% keep_ids)

h2s_evt_keep <- h2s_evt_flags %>%
  dplyr::filter(plume_id %in% keep_ids)

# ----------------------------
# 4) Centerline surrogate points
# ----------------------------
centerline_keep <- h2s_pts_keep %>%
  dplyr::group_by(plume_id) %>%
  dplyr::slice_max(dH2S, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# ----------------------------
# 5) Stepwise plume counts
# ----------------------------
plume_step_counts <- tibble::tibble(
  step = c(
    "Plume-flagged events after time-gap segmentation",
    paste0("Events with >= ", min_pts, " plume-flagged points"),
    paste0("Pass peak enhancement (>= ", min_peak_dh2s, " ppb)"),
    paste0("Pass rise criterion (>= ", min_rise_dh2s, " ppb, if n >= ", enforce_shape_n, ")"),
    paste0("Pass fall criterion (>= ", min_fall_dh2s, " ppb, if n >= ", enforce_shape_n, ")"),
    paste0("Pass prominence criterion (>= ", min_prom_dh2s, " ppb)"),
    paste0("Pass duration criterion (", dur_min_s, "-", dur_max_s, " s",
           if (DUR_STRICT) ")" else paste0(", or >= ", min_pts, " unique timestamps)")),
    paste0("Pass wind consistency (SD <= ", max_wind_sd_deg, "°; evaluable for ",
           sum(h2s_evt_flags$wind_evaluable, na.rm = TRUE), " of ",
           nrow(h2s_evt_flags), " events)"),
    paste0("Pass stability filter (", paste(keep_stab_levels, collapse = ", "),
           "; ", sum(is.na(h2s_evt_flags$stability)), " events dropped for having no class)"),
    "Pass all filters / retained plumes"
  ),
  n_plumes_remaining = c(
    n_plumes_flagged,
    n_plumes_minpts,
    sum(h2s_evt_flags$pass_peak, na.rm = TRUE),
    sum(h2s_evt_flags$pass_peak & h2s_evt_flags$pass_rise, na.rm = TRUE),
    sum(h2s_evt_flags$pass_peak & h2s_evt_flags$pass_rise & h2s_evt_flags$pass_fall, na.rm = TRUE),
    sum(h2s_evt_flags$pass_peak & h2s_evt_flags$pass_rise & h2s_evt_flags$pass_fall & h2s_evt_flags$pass_singlepk, na.rm = TRUE),
    sum(h2s_evt_flags$pass_peak & h2s_evt_flags$pass_rise & h2s_evt_flags$pass_fall & h2s_evt_flags$pass_singlepk & h2s_evt_flags$pass_dur, na.rm = TRUE),
    sum(h2s_evt_flags$pass_peak & h2s_evt_flags$pass_rise & h2s_evt_flags$pass_fall & h2s_evt_flags$pass_singlepk & h2s_evt_flags$pass_dur & h2s_evt_flags$pass_wind, na.rm = TRUE),
    sum(h2s_evt_flags$pass_peak & h2s_evt_flags$pass_rise & h2s_evt_flags$pass_fall & h2s_evt_flags$pass_singlepk & h2s_evt_flags$pass_dur & h2s_evt_flags$pass_wind & h2s_evt_flags$pass_stab, na.rm = TRUE),
    sum(h2s_evt_flags$pass_all, na.rm = TRUE)
  )
) %>%
  dplyr::mutate(
    n_dropped_at_step = dplyr::lag(n_plumes_remaining) - n_plumes_remaining,
    pct_remaining = round(100 * n_plumes_remaining / dplyr::first(n_plumes_remaining), 1)
  )

.n_eval <- sum(h2s_evt_flags$wind_evaluable, na.rm = TRUE)
message(sprintf("[FUNNEL] duration window binding: %s | wind-consistency evaluable on %d of %d events%s",
                DUR_STRICT, .n_eval, nrow(h2s_evt_flags),
                if (.n_eval == 0) "  <-- the wind criterion screened nothing; report it as not evaluable, not as passed" else ""))
print(plume_step_counts)

out_counts_csv <- file.path(out_dir, "WWTP_H2S_plume_step_counts.csv")
readr::write_csv(plume_step_counts, out_counts_csv)
message("[Saved] ", out_counts_csv)

# ----------------------------
# 6) Exact drops by step (after min_pts)
# ----------------------------
funnel_tbl <- h2s_evt_flags %>%
  dplyr::mutate(
    pass0 = TRUE,
    pass1 = pass0 & pass_peak,
    pass2 = pass1 & pass_rise,
    pass3 = pass2 & pass_fall,
    pass4 = pass3 & pass_singlepk,
    pass5 = pass4 & pass_dur,
    pass6 = pass5 & pass_wind,
    pass7 = pass6 & pass_stab
  ) %>%
  dplyr::summarise(
    n_start         = dplyr::n(),
    dropped_peak    = sum(pass0 & !pass_peak, na.rm = TRUE),
    remaining_peak  = sum(pass1, na.rm = TRUE),
    dropped_rise    = sum(pass1 & !pass_rise, na.rm = TRUE),
    remaining_rise  = sum(pass2, na.rm = TRUE),
    dropped_fall    = sum(pass2 & !pass_fall, na.rm = TRUE),
    remaining_fall  = sum(pass3, na.rm = TRUE),
    dropped_prom    = sum(pass3 & !pass_singlepk, na.rm = TRUE),
    remaining_prom  = sum(pass4, na.rm = TRUE),
    dropped_dur     = sum(pass4 & !pass_dur, na.rm = TRUE),
    remaining_dur   = sum(pass5, na.rm = TRUE),
    dropped_wind    = sum(pass5 & !pass_wind, na.rm = TRUE),
    remaining_wind  = sum(pass6, na.rm = TRUE),
    dropped_stab    = sum(pass6 & !pass_stab, na.rm = TRUE),
    remaining_stab  = sum(pass7, na.rm = TRUE),
    n_keep_all      = sum(pass_all, na.rm = TRUE)
  )

funnel_long <- tibble::tibble(
  step = c(
    "Start (after min_pts filter)",
    "Peak ΔH2S",
    "Rise (edge)",
    "Fall (edge)",
    "Prominence",
    "Duration/unique_ts",
    "Wind SD",
    "Stability",
    "Kept (all)"
  ),
  remaining = c(
    funnel_tbl$n_start,
    funnel_tbl$remaining_peak,
    funnel_tbl$remaining_rise,
    funnel_tbl$remaining_fall,
    funnel_tbl$remaining_prom,
    funnel_tbl$remaining_dur,
    funnel_tbl$remaining_wind,
    funnel_tbl$remaining_stab,
    funnel_tbl$n_keep_all
  ),
  dropped_at_step = c(
    NA_integer_,
    funnel_tbl$dropped_peak,
    funnel_tbl$dropped_rise,
    funnel_tbl$dropped_fall,
    funnel_tbl$dropped_prom,
    funnel_tbl$dropped_dur,
    funnel_tbl$dropped_wind,
    funnel_tbl$dropped_stab,
    0L
  )
)

print(funnel_long)

out_funnel_csv <- file.path(out_dir, "WWTP_H2S_plume_funnel.csv")
readr::write_csv(funnel_long, out_funnel_csv)
message("[Saved] ", out_funnel_csv)

# ----------------------------
# 7) Funnel figure for SI
# ----------------------------
plume_step_counts_plot <- plume_step_counts %>%
  dplyr::mutate(
    step = factor(step, levels = rev(step)),
    label = dplyr::if_else(
      is.na(n_dropped_at_step),
      paste0("n = ", scales::comma(n_plumes_remaining)),
      paste0("n = ", scales::comma(n_plumes_remaining),
             "\n(-", scales::comma(abs(n_dropped_at_step)), ")")
    )
  )

p_funnel <- ggplot2::ggplot(
  plume_step_counts_plot,
  ggplot2::aes(x = n_plumes_remaining, y = step)
) +
  ggplot2::geom_col(fill = "#4C78A8", width = 0.72) +
  ggplot2::geom_text(
    ggplot2::aes(label = label),
    hjust = -0.08,
    size = 3.5,
    lineheight = 0.95
  ) +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.18)),
    labels = scales::comma_format()
  ) +
  ggplot2::labs(
    title = "Stepwise retention of candidate WWTP H2S plume events",
    subtitle = "Counts show plume events remaining after each identification and QA filter",
    x = "Number of plume events",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

out_funnel_png <- file.path(out_dir, "FIG_WWTP_H2S_plume_funnel.png")
ggplot2::ggsave(out_funnel_png, p_funnel, width = 9.5, height = 5.8, dpi = 300, bg = "white")
message("[Saved] ", out_funnel_png)

# ----------------------------
# 8A) Plot ALL initial plume shapes
# ----------------------------
h2s_all_shapes <- h2s_pts_all %>%
  dplyr::arrange(plume_id, date) %>%
  dplyr::group_by(plume_id) %>%
  dplyr::mutate(
    t_rel_s = as.numeric(difftime(date, min(date), units = "secs"))
  ) %>%
  dplyr::ungroup()

p_all_plumes <- ggplot2::ggplot(h2s_all_shapes, ggplot2::aes(x = t_rel_s, y = dH2S)) +
  ggplot2::geom_line(alpha = 0.65, linewidth = 0.5) +
  ggplot2::geom_point(ggplot2::aes(color = distance_wwtp), alpha = 0.85, size = 1.2) +
  ggplot2::scale_color_viridis_c(name = "Distance to WWTP (km)", trans = "log1p") +
  ggplot2::labs(
    title = "All initial H2S plume candidates near WWTP (pre-filters)",
    subtitle = "Each facet is a plume_id; x-axis is seconds since plume start; y = ΔH2S",
    x = "Seconds since plume start",
    y = expression("ΔH"[2]*"S (ppb)")
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "right",
    strip.text = ggplot2::element_text(size = 7)
  ) +
  ggplot2::facet_wrap(~ plume_id, scales = "free_x", ncol = 6)

ggplot2::ggsave(
  file.path(out_dir, "FIG_H2S_ALL_initial_plume_shapes_prefilter_WWTP.png"),
  p_all_plumes, width = 14, height = 9, dpi = 300
)

# ----------------------------
# 8B) Plot ALL kept plumes
# ----------------------------
h2s_kept_shapes <- h2s_pts_keep %>%
  dplyr::arrange(plume_id, date) %>%
  dplyr::group_by(plume_id) %>%
  dplyr::mutate(
    t_rel_s = as.numeric(difftime(date, min(date), units = "secs"))
  ) %>%
  dplyr::ungroup()

p_kept_all <- ggplot2::ggplot(h2s_kept_shapes, ggplot2::aes(x = t_rel_s, y = dH2S)) +
  ggplot2::geom_line(alpha = 0.75, linewidth = 0.6) +
  ggplot2::geom_point(ggplot2::aes(color = distance_wwtp), size = 1.2, alpha = 0.85) +
  ggplot2::scale_color_viridis_c(name = "Distance to WWTP (km)", trans = "log1p") +
  ggplot2::labs(
    title = "All kept H2S plumes near WWTP",
    subtitle = "Each facet is one retained plume; x-axis is seconds since plume start; y = ΔH2S",
    x = "Seconds since plume start",
    y = expression("ΔH"[2]*"S (ppb)")
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "right",
    strip.text = ggplot2::element_text(size = 7)
  ) +
  ggplot2::facet_wrap(~ plume_id, scales = "free_x", ncol = 6)

ggplot2::ggsave(
  file.path(out_dir, "FIG_H2S_all_kept_plume_shapes_WWTP.png"),
  p_kept_all, width = 14, height = 9, dpi = 300
)

kept_table <- h2s_evt_keep %>%
  dplyr::arrange(time_at_peak) %>%
  dplyr::select(
    plume_id, start_time, end_time, time_at_peak, dist_at_peak_km,
    peak_dH2S, n_pts, duration_s, wind_sd_deg, stability
  )
print(kept_table)

# ----------------------------
# PLOT 1: Peak enhancement vs distance
# ----------------------------
p1 <- ggplot2::ggplot(h2s_evt_keep, ggplot2::aes(x = dist_at_peak_km, y = peak_dH2S)) +
  ggplot2::geom_point(ggplot2::aes(shape = stability), alpha = 0.85, size = 2.2) +
  ggplot2::geom_smooth(method = "loess", se = TRUE, linewidth = 0.7) +
  ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  ggplot2::labs(
    title = "H2S plume enhancement vs distance from WWTP (filtered plumes)",
    subtitle = "Each point is one plume; peak enhancement = H2S − baseline at plume maximum",
    x = "Distance from WWTP (km)",
    y = expression("Peak ΔH"[2]*"S (ppb)"),
    shape = "Stability"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "right")

# ----------------------------
# PLOT 2: Plume timeline by distance
# ----------------------------
p2 <- ggplot2::ggplot(h2s_evt_keep, ggplot2::aes(y = dist_at_peak_km)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = start_time, xend = end_time, yend = dist_at_peak_km, color = peak_dH2S),
    linewidth = 2.2, alpha = 0.9, lineend = "round"
  ) +
  ggplot2::scale_color_viridis_c(name = expression("Peak ΔH"[2]*"S (ppb)"), trans = "log1p") +
  ggplot2::labs(
    title = "Filtered H2S plume events near WWTP over time",
    subtitle = "Each segment is one plume (start→end), positioned by distance at peak enhancement",
    x = "Time",
    y = "Distance from WWTP (km)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "right")

# ----------------------------
# PLOT 3: All filtered plume shapes
# ----------------------------
h2s_plot3 <- h2s_pts_keep %>%
  dplyr::arrange(plume_id, date) %>%
  dplyr::group_by(plume_id) %>%
  dplyr::mutate(
    t_rel_s = as.numeric(difftime(date, min(date), units = "secs"))
  ) %>%
  dplyr::ungroup()

p3 <- ggplot2::ggplot(h2s_plot3, ggplot2::aes(x = t_rel_s, y = dH2S)) +
  ggplot2::geom_line(linewidth = 0.6, alpha = 0.75) +
  ggplot2::geom_point(ggplot2::aes(color = distance_wwtp), size = 1.2, alpha = 0.85) +
  ggplot2::scale_color_viridis_c(name = "Distance to WWTP (km)", trans = "log1p") +
  ggplot2::labs(
    title = "Filtered H2S plume shapes near WWTP",
    subtitle = "Lines show ΔH2S = H2S − baseline; all retained plumes shown",
    x = "Seconds since plume start",
    y = expression("ΔH"[2]*"S (ppb)")
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "right",
    strip.text = ggplot2::element_text(size = 7)
  ) +
  ggplot2::facet_wrap(~ plume_id, scales = "free_x", ncol = 6)

# ----------------------------
# QA PLOT 4: Duration distribution
# ----------------------------
p4 <- ggplot2::ggplot(h2s_evt_all, ggplot2::aes(x = duration_s)) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::geom_vline(xintercept = dur_min_s, linetype = "dashed") +
  ggplot2::geom_vline(xintercept = dur_max_s, linetype = "dashed") +
  ggplot2::labs(
    title = "Distribution of plume durations (QA)",
    subtitle = paste0("Dashed lines show duration filter window: ", dur_min_s, "–", dur_max_s, " s"),
    x = "Duration (s)",
    y = "Count"
  ) +
  ggplot2::theme_minimal(base_size = 12)

# ----------------------------
# QA PLOT 5: Wind SD vs peak enhancement
# ----------------------------
p5 <- ggplot2::ggplot(h2s_evt_all, ggplot2::aes(x = wind_sd_deg, y = peak_dH2S)) +
  ggplot2::geom_point(alpha = 0.65) +
  ggplot2::geom_vline(xintercept = max_wind_sd_deg, linetype = "dashed") +
  ggplot2::scale_y_continuous(trans = "log1p") +
  ggplot2::labs(
    title = "Wind-direction consistency during H2S plumes (QA)",
    subtitle = paste0("Dashed line = wind SD threshold (", max_wind_sd_deg, "°); y is peak ΔH2S (log1p)"),
    x = "Circular SD of wind direction during plume (degrees)",
    y = expression("Peak ΔH"[2]*"S (ppb, log1p)")
  ) +
  ggplot2::theme_minimal(base_size = 12)

# ----------------------------
# QA PLOT 6: Angular alignment with WWTP
# ----------------------------
if ("wind_from_deg_wwtf" %in% names(dat)) {
  ang_df <- dat %>%
    dplyr::mutate(
      ang = ((.data[[wind_col]] - wind_from_deg_wwtf + 180) %% 360) - 180
    ) %>%
    dplyr::filter(is.finite(ang), is.finite(dH2S))

  p6 <- ggplot2::ggplot(ang_df, ggplot2::aes(x = ang, y = dH2S)) +
    ggplot2::geom_point(alpha = 0.15) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_y_continuous(trans = "log1p") +
    ggplot2::labs(
      title = "Angular alignment diagnostic (QA)",
      subtitle = "ΔH2S should concentrate near 0° if WWTP dominates",
      x = "Observed wind direction − wind-from-WWTP (degrees; 0 = directly downwind)",
      y = expression("ΔH"[2]*"S (ppb, log1p)")
    ) +
    ggplot2::theme_minimal(base_size = 12)
} else {
  p6 <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "Angular alignment diagnostic not shown",
      subtitle = "Add wind_from_deg_wwtf to enable this plot."
    )
}

# ----------------------------
# Save plots
# ----------------------------
ggplot2::ggsave(file.path(out_dir, "FIG_H2S_peakEnh_vs_distance_filtered_WWTP.png"), p1, width = 7.5, height = 5.0, dpi = 300)
ggplot2::ggsave(file.path(out_dir, "FIG_H2S_plume_timeline_filtered_WWTP.png"), p2, width = 10.0, height = 5.0, dpi = 300)
ggplot2::ggsave(file.path(out_dir, "FIG_H2S_filtered_plume_shapes_all_WWTP.png"), p3, width = 14.0, height = 9.0, dpi = 300)

ggplot2::ggsave(file.path(out_dir, "QA_H2S_duration_hist_WWTP.png"), p4, width = 7.5, height = 5.0, dpi = 300)
ggplot2::ggsave(file.path(out_dir, "QA_H2S_wind_sd_vs_peakEnh_WWTP.png"), p5, width = 7.5, height = 5.0, dpi = 300)
ggplot2::ggsave(file.path(out_dir, "QA_H2S_angular_alignment_WWTP.png"), p6, width = 8.5, height = 5.0, dpi = 300)

# ----------------------------
# Print plots
# ----------------------------
p_funnel
p_all_plumes
p_kept_all
p1; p2; p3; p4; p5; p6

# ----------------------------
# Optional: inspect kept plume table
# ----------------------------
# h2s_evt_keep %>%
# dplyr::select(plume_id, peak_dH2S, dist_at_peak_km, duration_s, n_pts, n_unique_t,
# wind_sd_deg, stability) %>%
#   dplyr::arrange(dplyr::desc(peak_dH2S))
