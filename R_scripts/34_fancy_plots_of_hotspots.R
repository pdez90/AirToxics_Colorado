# ==============================================================
# 34  Fancy plots of hotspots
# Auto-split from Suncor.Rmd  (section 34 of 40)
# ==============================================================

#Fancy plots of hotspots

# ============================================================
# FAST UPDATED POWER FIGURE
# - Panel A fixes:
#     * hotspot 4 label moved off Phillips 66
#     * Refuel labels nudged farther so text is visible
#     * transparent hotspot circles retained
#     * no map title
#     * adds WWTF1 and WWTF2
# - Panel B: chemical fingerprints
# - Panel C: hotspot activity by hour
# - Panel D: source attribution in ratio space
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(ggspatial)
  library(cowplot)
  library(tidyr)
  library(ragg)
  library(tibble)
  library(ggrepel)
  library(scales)
})

# ----------------------------
# USER PATHS
# ----------------------------
in_dir  <- "/Users/priyanka/Downloads/Suncor"
out_dir <- file.path(in_dir, "hotspot_source_fingerprint_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

f_master <- file.path(in_dir, "MASTER_hotspot_group_index.csv")
f_mobile <- file.path(in_dir, "mobile_wswd.RData")

# ----------------------------
# SETTINGS
# ----------------------------
tile_type <- "cartolight"
tile_zoom_regional <- 11

match_dist_m <- 100
global_quantile_for_high <- 0.95

# ----------------------------
# 0) LOAD DATA
# ----------------------------
stopifnot(file.exists(f_master))
master <- readr::read_csv(f_master, show_col_types = FALSE)

stopifnot(file.exists(f_mobile))
load(f_mobile)

if (exists("out")) {
  df <- out
} else if (exists("df")) {
  df <- df
} else {
  stop("mobile_wswd.RData must contain an object named 'out' or 'df'.")
}

# ----------------------------
# 1) BASIC CHECKS / HARMONIZE
# ----------------------------
stopifnot(all(c("Longitude", "Latitude") %in% names(df)))
if (!("date" %in% names(df))) stop("Mobile data must include a 'date' column.")

if (!("ws" %in% names(df))) {
  cand_ws <- c("ws","WS","wind_speed","WindSpeed","windspeed","windSpeed","Wind_Speed")
  ws_hit <- cand_ws[cand_ws %in% names(df)][1]
  if (!is.na(ws_hit)) df$ws <- df[[ws_hit]]
}
if (!("wd" %in% names(df))) {
  cand_wd <- c("wd","WD","wind_dir","WindDir","winddirection","WindDirection","Wind_Direction")
  wd_hit <- cand_wd[cand_wd %in% names(df)][1]
  if (!is.na(wd_hit)) df$wd <- df[[wd_hit]]
}

pollutant_cols <- c(
  benzene = "Benzene_ppb",
  toluene = "Toluene_ppb",
  xylene  = "Xylene_ppb",
  tmb     = "Trimethylbenzene_ppb",
  h2s     = "Hydrogen_Sulfide_ppb",
  hcn     = "Hydrogen_Cyanide_ppb"
)

pretty_name <- function(x) {
  x <- gsub("_ppb$", "", x)
  x <- gsub("^Hydrogen_Sulfide$", "H2S", x)
  x <- gsub("^Hydrogen_Cyanide$", "HCN", x)
  x
}
pretty_pollutants <- setNames(vapply(pollutant_cols, pretty_name, character(1)), pollutant_cols)

pollutant_cols_existing <- names(pretty_pollutants)[names(pretty_pollutants) %in% names(df)]
if (length(pollutant_cols_existing) == 0) {
  stop("None of the expected pollutant columns were found in df.")
}

# ----------------------------
# 2) CLEAN / TIME FEATURES
# ----------------------------
df <- df %>%
  dplyr::mutate(
    Longitude = suppressWarnings(as.numeric(.data$Longitude)),
    Latitude  = suppressWarnings(as.numeric(.data$Latitude)),
    date      = as.POSIXct(.data$date, tz = "UTC"),
    hour      = lubridate::hour(.data$date)
  ) %>%
  dplyr::filter(
    is.finite(.data$Longitude),
    is.finite(.data$Latitude),
    !is.na(.data$date)
  )

# ----------------------------
# 3) SF OBJECTS
# ----------------------------
df_sf_ll <- sf::st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
df_sf_m  <- sf::st_transform(df_sf_ll, 32613)

master_sf_ll <- sf::st_as_sf(master, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
master_sf_m  <- sf::st_transform(master_sf_ll, 32613)

# ----------------------------
# 4) CONTEXTUAL FACILITIES
# ----------------------------
context_facilities <- tibble::tibble(
  label = c(
    "Suncor", "Sinclair", "Phillips 66",
    "WWTF1", "WWTF2", "Woodshop",
    "Refuel", "Refuel", "Refuel", "Refuel"
  ),
  facility_group = c(
    "Covered facility", "Covered facility", "Covered facility",
    "WWTF", "WWTF", "Woodshop",
    "Refuel", "Refuel", "Refuel", "Refuel"
  ),
  Latitude = c(
    39.803333,
    39.8724,
    39.79668,
    39.80822838231637,   # WWTF1
    39.87304794998779,   # WWTF2
    39.791382444842746,
    39.79935581470166,
    39.783338577716854,
    39.886063120868805,
    39.88659578717162
  ),
  Longitude = c(
    -104.945556,
    -104.8861,
    -104.94236,
    -104.9553246877205,   # WWTF1
    -104.91204700295945,  # WWTF2
    -104.94754520948433,
    -104.88376424570843,
    -105.10918240337975,
    -104.84531380337543,
    -104.88371420337545
  ),
  point_fill = c(
    "red", "red", "red",
    "green3", "green3",
    "purple3",
    "dodgerblue3", "dodgerblue3", "dodgerblue3", "dodgerblue3"
  ),
  text_color = c(
    "red4", "red4", "red4",
    "green4", "green4",
    "purple4",
    "dodgerblue4", "dodgerblue4", "dodgerblue4", "dodgerblue4"
  )
)

# Updated label positions for contextual facilities
context_facilities <- context_facilities %>%
  mutate(
    lab_x = c(
      Longitude[1] + 0.0020,  # Suncor
      Longitude[2] + 0.0010,  # Sinclair
      Longitude[3] - 0.0032,  # Phillips 66
      Longitude[4] - 0.0040,  # WWTF1
      Longitude[5] + 0.0012,  # WWTF2
      Longitude[6] - 0.0040,  # Woodshop
      Longitude[7] - 0.0024,  # Refuel (Central Park)
      Longitude[8] - 0.0032,  # Refuel (Kipling)
      Longitude[9] + 0.0018,  # Refuel (12241 E 104th)
      Longitude[10] + 0.0012  # Refuel (8991 E 104th)
    ),
    lab_y = c(
      Latitude[1] + 0.0015,   # Suncor
      Latitude[2] + 0.0015,   # Sinclair
      Latitude[3] - 0.0016,   # Phillips 66
      Latitude[4] + 0.0020,   # WWTF1
      Latitude[5] + 0.0015,   # WWTF2
      Latitude[6] - 0.0020,   # Woodshop
      Latitude[7] - 0.0014,   # Refuel (Central Park)
      Latitude[8] - 0.0012,   # Refuel (Kipling)
      Latitude[9] + 0.0013,   # Refuel (12241 E 104th)
      Latitude[10] + 0.0020   # Refuel (8991 E 104th)
    )
  )

context_sf_ll <- sf::st_as_sf(context_facilities, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
context_sf_m  <- sf::st_transform(context_sf_ll, 32613)

# ----------------------------
# 5) REGIONAL BBOX
# ----------------------------
all_pts_m <- dplyr::bind_rows(
  master_sf_m %>% dplyr::select(geometry),
  context_sf_m %>% dplyr::select(geometry)
)

bb_m <- sf::st_bbox(all_pts_m)
pad_m <- 1200
bb_m[c("xmin", "ymin")] <- bb_m[c("xmin", "ymin")] - pad_m
bb_m[c("xmax", "ymax")] <- bb_m[c("xmax", "ymax")] + pad_m
bb_ll <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_m), 4326))

# ----------------------------
# 6) GLOBAL "HIGH" THRESHOLDS
# ----------------------------
global_thr <- sapply(pollutant_cols_existing, function(cc) {
  suppressWarnings(as.numeric(stats::quantile(df[[cc]], global_quantile_for_high, na.rm = TRUE)))
})

hi <- rep(FALSE, nrow(df))
for (cc in pollutant_cols_existing) {
  x <- df[[cc]]
  thr <- global_thr[[cc]]
  hi <- hi | (is.finite(x) & x > thr)
}
df$any_high_global <- hi

# ----------------------------
# 7) RATIO SUMMARY
# ----------------------------
ratio_summary <- function(df_hot) {
  safe_ratio <- function(num, den) ifelse(is.finite(num) & is.finite(den) & den > 0, num / den, NA_real_)

  cols <- names(df_hot)

  if ("Toluene_ppb" %in% cols && "Benzene_ppb" %in% cols) df_hot$T_B   <- safe_ratio(df_hot$Toluene_ppb, df_hot$Benzene_ppb)
  if ("Xylene_ppb" %in% cols && "Benzene_ppb" %in% cols) df_hot$X_B   <- safe_ratio(df_hot$Xylene_ppb, df_hot$Benzene_ppb)
  if ("Trimethylbenzene_ppb" %in% cols && "Benzene_ppb" %in% cols) df_hot$TMB_B <- safe_ratio(df_hot$Trimethylbenzene_ppb, df_hot$Benzene_ppb)
  if ("Hydrogen_Sulfide_ppb" %in% cols && "Benzene_ppb" %in% cols) df_hot$H2S_B <- safe_ratio(df_hot$Hydrogen_Sulfide_ppb, df_hot$Benzene_ppb)
  if ("Hydrogen_Cyanide_ppb" %in% cols && "Benzene_ppb" %in% cols) df_hot$HCN_B <- safe_ratio(df_hot$Hydrogen_Cyanide_ppb, df_hot$Benzene_ppb)

  ratio_cols <- intersect(c("T_B","X_B","TMB_B","H2S_B","HCN_B"), names(df_hot))
  if (length(ratio_cols) == 0) return(tibble::tibble())

  dplyr::bind_rows(lapply(ratio_cols, function(rc) {
    x <- df_hot[[rc]]
    tibble::tibble(
      ratio = rc,
      n_nonmiss = sum(is.finite(x)),
      median = stats::median(x, na.rm = TRUE),
      p25 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE)),
      p75 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE))
    )
  }))
}

# ----------------------------
# 8) PRECOMPUTE HOTSPOT SUMMARIES
# ----------------------------
all_ratio_summ <- list()
all_hour_activity <- list()

for (gid in sort(unique(master$group_id))) {
  pt_m <- master_sf_m %>% dplyr::filter(group_id == gid)
  if (nrow(pt_m) == 0) next

  d_m <- as.numeric(sf::st_distance(df_sf_m, pt_m))
  idx <- which(d_m <= match_dist_m)
  if (length(idx) == 0) next

  df_hot <- df[idx, , drop = FALSE]

  rs <- ratio_summary(df_hot)
  if (nrow(rs) > 0) {
    rs <- rs %>% dplyr::mutate(group_id = gid, .before = 1)
    all_ratio_summ[[length(all_ratio_summ) + 1]] <- rs
  }

  ha <- df_hot %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(
      n = dplyr::n(),
      frac_any_high = mean(any_high_global, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(group_id = gid, .before = 1)

  all_hour_activity[[length(all_hour_activity) + 1]] <- ha
}

ratio_all <- if (length(all_ratio_summ) > 0) dplyr::bind_rows(all_ratio_summ) else tibble::tibble()
hour_all  <- if (length(all_hour_activity) > 0) dplyr::bind_rows(all_hour_activity) else tibble::tibble()

if (nrow(ratio_all) > 0) {
  readr::write_csv(ratio_all, file.path(out_dir, "hotspot_ratio_summary_ALL.csv"))
}
if (nrow(hour_all) > 0) {
  readr::write_csv(hour_all, file.path(out_dir, "hotspot_hour_activity_ALL.csv"))
}

# ----------------------------
# 9) PANEL A: HOTSPOT MAP
# ----------------------------
make_map_regional_clean <- function() {

  # Manual tweak only for hotspot 4
  master_labels <- master %>%
    mutate(
      lab_x = Longitude,
      lab_y = Latitude
    )

  master_labels$lab_x[master_labels$group_id == 4] <- master_labels$Longitude[master_labels$group_id == 4] - 0.0018
  master_labels$lab_y[master_labels$group_id == 4] <- master_labels$Latitude[master_labels$group_id == 4] - 0.0010

  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = tile_type, zoom = tile_zoom_regional) +
    ggplot2::coord_sf(
      crs = 4326,
      xlim = c(bb_ll["xmin"], bb_ll["xmax"]),
      ylim = c(bb_ll["ymin"], bb_ll["ymax"]),
      expand = FALSE
    ) +

    ggplot2::geom_point(
      data = context_facilities,
      ggplot2::aes(x = Longitude, y = Latitude, fill = facility_group),
      shape = 21, color = "white", stroke = 0.5, size = 3.2, alpha = 0.98
    ) +

    ggplot2::geom_text(
      data = context_facilities,
      ggplot2::aes(x = lab_x, y = lab_y, label = label, color = facility_group),
      fontface = "bold",
      size = 3.0,
      show.legend = FALSE
    ) +

    ggplot2::geom_point(
      data = master,
      ggplot2::aes(x = Longitude, y = Latitude, size = max_n_days),
      shape = 21, fill = alpha("gold", 0.42), color = "black", stroke = 0.55
    ) +

    ggrepel::geom_label_repel(
      data = master_labels,
      ggplot2::aes(x = lab_x, y = lab_y, label = group_id),
      seed = 123,
      size = 3.0,
      fontface = "bold",
      fill = alpha("white", 0.78),
      color = "black",
      label.size = 0.15,
      box.padding = 0.20,
      point.padding = 0.15,
      label.padding = unit(0.10, "lines"),
      min.segment.length = 0,
      segment.color = alpha("black", 0.5),
      segment.size = 0.25,
      max.overlaps = Inf
    ) +

    ggplot2::scale_size_continuous(range = c(4, 11), name = "Max exceedance-days") +
    ggplot2::scale_fill_manual(
      values = c(
        "Covered facility" = "red",
        "WWTF" = "green3",
        "Woodshop" = "purple3",
        "Refuel" = "dodgerblue3"
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Covered facility" = "red4",
        "WWTF" = "green4",
        "Woodshop" = "purple4",
        "Refuel" = "dodgerblue4"
      )
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = NULL, y = NULL)
}

# ----------------------------
# 10) PANEL B: CHEMICAL FINGERPRINTS
# ----------------------------
make_chem_fingerprint <- function(master_df) {
  chem_df <- master_df %>%
    dplyr::mutate(pollutants = tolower(pollutants)) %>%
    tidyr::separate_rows(pollutants, sep = "\\+") %>%
    dplyr::mutate(pollutants = dplyr::case_when(
      pollutants == "h2s" ~ "H2S",
      pollutants == "hcn" ~ "HCN",
      pollutants == "trimethylbenzene" ~ "Trimethylbenzene",
      TRUE ~ stringr::str_to_title(pollutants)
    )) %>%
    dplyr::distinct(group_id, pollutants) %>%
    dplyr::mutate(present = 1)

  ggplot2::ggplot(chem_df, ggplot2::aes(x = factor(group_id), y = present, fill = pollutants)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::labs(
      title = "Hotspot chemical fingerprints",
      x = "Hotspot group",
      y = "Pollutant presence",
      fill = "Pollutant"
    )
}

# ----------------------------
# 11) PANEL C: HOUR ACTIVITY HEATMAP
# ----------------------------
make_hour_heatmap <- function(hour_all_df) {
  if (nrow(hour_all_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  ggplot2::ggplot(hour_all_df, ggplot2::aes(x = hour, y = factor(group_id), fill = frac_any_high)) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, 3)) +
    ggplot2::scale_fill_viridis_c(option = "plasma", name = "Frac. high") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Hotspot activity by hour",
      subtitle = paste0("Fraction of measurements within 100 m with any pollutant > global p",
                        round(global_quantile_for_high * 100), " threshold"),
      x = "Hour of day",
      y = "Hotspot group"
    )
}

# ----------------------------
# 12) PANEL D: SOURCE ATTRIBUTION
# ----------------------------
make_source_panel_clean <- function(ratio_all_df, master_df) {

  ratio_plot_data <- ratio_all_df %>%
    dplyr::select(group_id, ratio, median) %>%
    tidyr::pivot_wider(names_from = ratio, values_from = median) %>%
    dplyr::left_join(master_df, by = "group_id") %>%
    dplyr::mutate(
      source_guess = dplyr::case_when(
        !is.na(H2S_B) & H2S_B > 0.5 ~ "Sulfur/Refinery",
        !is.na(TMB_B) & TMB_B > 1.5 ~ "Petroleum/Industrial",
        !is.na(T_B) & !is.na(X_B) & T_B > 2 & X_B > 1 ~ "Traffic",
        TRUE ~ "Mixed"
      )
    )

  ggplot2::ggplot(ratio_plot_data, ggplot2::aes(x = T_B, y = TMB_B)) +
    ggplot2::geom_point(
      ggplot2::aes(size = persistence_index_weighted, color = source_guess),
      alpha = 0.9
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = group_id),
      seed = 123,
      size = 3.0,
      box.padding = 0.2,
      point.padding = 0.15,
      min.segment.length = 0,
      segment.alpha = 0.5,
      max.overlaps = Inf
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Source attribution in ratio space",
      subtitle = "Hotspots positioned by median aromatic ratios",
      x = "Toluene / Benzene",
      y = "Trimethylbenzene / Benzene",
      color = "Likely source",
      size = "Persistence"
    )
}

# ----------------------------
# 13) BUILD POWER PANEL
# ----------------------------
pA <- make_map_regional_clean()
pB <- make_chem_fingerprint(master)
pC <- make_hour_heatmap(hour_all)
pD <- make_source_panel_clean(ratio_all, master)

power_fig_clean <- cowplot::plot_grid(
  pA, pB,
  pC, pD,
  labels = c("A", "B", "C", "D"),
  ncol = 2,
  rel_widths = c(1.25, 1),
  rel_heights = c(1, 1)
)

out_power <- file.path(out_dir, "FIG_Hotspot_PowerPanel_Clean.png")
ragg::agg_png(out_power, width = 14, height = 11, units = "in", res = 300, background = "white")
print(power_fig_clean)
dev.off()

message("[Saved] ", out_power)
message("DONE. Outputs written to: ", out_dir)
