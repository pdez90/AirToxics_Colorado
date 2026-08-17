# ==============================================================
# 31  Plotting
# Auto-split from Suncor.Rmd  (section 31 of 40)
# ==============================================================

#Plotting

# ============================================================
# Persistent hotspot groups: plots + per-group tables (RUN ALL AT ONCE)
# BASEMAP: uses EXACT same ggspatial tile approach you shared:
#   ggspatial::annotation_map_tile(type="cartolight", zoom=13)
#
# For each persistent hotspot group (centroid):
#   * pollutant "high" table (b99): n_high, n_days_high, which days
#   * pairwise pollutant scatter panel colored by wind direction (wd_sector),
#       point size = wind speed (ws)
#       - NO stat text annotations
#       - axis labels CLEANED:
#           Benzene_ppb -> Benzene, Toluene_ppb -> Toluene, etc.
#           Hydrogen_Sulfide_ppb -> H2S
#           Hydrogen_Cyanide_ppb -> HCN
#   * openair polarPlot (nwr) for all pollutant cols
#   * 2 maps (hotspots ONLY; NO TRI points on map):
#       - regional: all hotspot centroids, highlight current group
#       - local: current group + 100m buffer + mobile points within 100m
#
# TRI:
#   - Uses TRI.csv ONLY to compute nearest facility distance (stored in master index).
#   - TRI facilities are NOT plotted.
#
# OUTPUTS:
#   - MASTER_hotspot_group_index.csv
#   - per-group pollutant table CSV
#   - per-group PNGs: regional map, local map, pairwise panel, polar plot
#
# NOTES:
#   - If tile downloads fail (offline / server hiccup), maps still render without basemap.
#   - Uses ragg::agg_png for sharper PNGs.
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(openair)
  library(cowplot)
  library(ggsci)
  library(tibble)
  library(ggspatial)
  library(ragg)
})

# ----------------------------
# USER PATHS
# ----------------------------
in_dir  <- "/Users/priyanka/Downloads/Suncor"
out_dir <- "/Users/priyanka/Downloads/Suncor/hotspot_group_reports"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

f_groups  <- file.path(in_dir, "group_summary_persistent.csv")
f_thresh  <- file.path(in_dir, "hotspot_thresholds_summary.csv")
f_super3  <- file.path(in_dir, "super_hotspots_3plus_persistent.csv")
f_tri     <- file.path(in_dir, "TRI.csv")

# ----------------------------
# MOBILE DATA
# ----------------------------
load("/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")
if (exists("out")) df <- out
stopifnot(exists("df"))

# ----------------------------
# SETTINGS
# ----------------------------
match_dist_m <- 100
use_super3_only <- TRUE
min_rows_for_pairs <- 25

pad_m <- 500
local_pad_m <- 250

# Basemap (EXACT style you provided)
tile_type <- "cartolight"
tile_zoom_regional <- 13
tile_zoom_local    <- 15

# ============================================================
# 0) Wind sanity + wind-direction sectors
# ============================================================
if (!("ws" %in% names(df))) {
  cand_ws <- c("ws","WS","wind_speed","WindSpeed","windspeed")
  ws_hit <- cand_ws[cand_ws %in% names(df)][1]
  if (!is.na(ws_hit)) df$ws <- df[[ws_hit]]
}
if (!("wd" %in% names(df))) {
  cand_wd <- c("wd","WD","wind_dir","WindDir","winddirection")
  wd_hit <- cand_wd[cand_wd %in% names(df)][1]
  if (!is.na(wd_hit)) df$wd <- df[[wd_hit]]
}

make_wd_sector <- function(wd) {
  out <- cut(
    wd,
    breaks = c(0,45,90,135,180,225,270,315,360),
    labels = c("N","NE","E","SE","S","SW","W","NW"),
    include.lowest = TRUE
  )
  out <- as.character(out)
  out[is.na(out)] <- "Missing"
  factor(out, levels = c("N","NE","E","SE","S","SW","W","NW","Missing"))
}

# ============================================================
# 1) Groups
# ============================================================
stopifnot(file.exists(f_groups))
groups <- readr::read_csv(f_groups, show_col_types = FALSE)

need_g <- c("group_id","Longitude","Latitude","n_pollutants","pollutants")
miss_g <- setdiff(need_g, names(groups))
if (length(miss_g) > 0) stop("group_summary_persistent.csv missing: ", paste(miss_g, collapse=", "))

if (use_super3_only) {
  stopifnot(file.exists(f_super3))
  super3 <- readr::read_csv(f_super3, show_col_types = FALSE)
  if (!("group_id" %in% names(super3))) stop("super_hotspots_3plus_persistent.csv missing group_id")
  groups <- dplyr::semi_join(groups, dplyr::select(super3, group_id), by="group_id")
}

groups_sf_ll <- sf::st_as_sf(groups, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)
groups_sf_m  <- sf::st_transform(groups_sf_ll, 32613)

# shared regional bbox in lon/lat (padded in meters)
bb_m <- sf::st_bbox(groups_sf_m)
bb_pad <- bb_m
bb_pad[c("xmin","ymin")] <- bb_pad[c("xmin","ymin")] - pad_m
bb_pad[c("xmax","ymax")] <- bb_pad[c("xmax","ymax")] + pad_m
bb_ll <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_pad), 4326))

# ============================================================
# 2) Thresholds + pollutant mapping
# ============================================================
stopifnot(file.exists(f_thresh))
thr <- readr::read_csv(f_thresh, show_col_types = FALSE)

thr_names <- names(thr)
poll_col <- thr_names[tolower(thr_names) == "pollutant"][1]
cand_thr <- c("b99_ppb","b99","threshold_ppb","p99_ppb","p99")
thr_col <- cand_thr[cand_thr %in% thr_names][1]
if (is.na(poll_col)) stop("hotspot_thresholds_summary.csv must have a pollutant column")
if (is.na(thr_col)) stop("Could not find threshold column. Available: ", paste(thr_names, collapse=", "))

pollutant_map <- tibble::tribble(
  ~pollutant,            ~col,
  "benzene",             "Benzene_ppb",
  "toluene",             "Toluene_ppb",
  "trimethylbenzene",    "Trimethylbenzene_ppb",
  "xylene",              "Xylene_ppb",
  "h2s",                 "Hydrogen_Sulfide_ppb",
  "hcn",                 "Hydrogen_Cyanide_ppb"
)

b99_tbl <- thr %>%
  transmute(
    pollutant = tolower(.data[[poll_col]]),
    b99 = suppressWarnings(as.numeric(.data[[thr_col]]))
  ) %>%
  right_join(pollutant_map, by="pollutant")

# compute missing b99 from df p99
missing_b99 <- b99_tbl %>% filter(!is.finite(b99))
if (nrow(missing_b99) > 0) {
  cat("\n[Thresholds] Missing b99 -> computing p99 from df:\n")
  for (k in seq_len(nrow(missing_b99))) {
    pol <- missing_b99$pollutant[k]
    col <- missing_b99$col[k]
    if (col %in% names(df)) {
      val <- as.numeric(quantile(df[[col]], 0.99, na.rm = TRUE))
      b99_tbl$b99[b99_tbl$pollutant == pol] <- val
      cat("  - ", pol, " (", col, ") = ", signif(val, 4), "\n", sep="")
    }
  }
}

bad_cols <- b99_tbl %>% filter(!col %in% names(df))
if (nrow(bad_cols) > 0) stop("Missing df columns:\n", paste0(bad_cols$pollutant," -> ",bad_cols$col, collapse="\n"))

# Display labels for pairwise plots (remove _ppb; special cases)
pretty_poll_label <- function(colname) {
  if (colname == "Hydrogen_Sulfide_ppb") return("H2S")
  if (colname == "Hydrogen_Cyanide_ppb") return("HCN")
  gsub("_ppb$", "", colname)
}

# ============================================================
# 3) Prep df + sf
# ============================================================
stopifnot(all(c("Longitude","Latitude","date") %in% names(df)))

df <- df %>%
  mutate(
    date = as.POSIXct(date, tz="UTC"),
    day  = as.Date(date),
    wd_sector = make_wd_sector(wd)
  )

df_sf_ll <- sf::st_as_sf(df, coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)
df_sf_m  <- sf::st_transform(df_sf_ll, 32613)

# ============================================================
# 4) TRI nearest (NOT plotted)
# ============================================================
tri_sf_m <- NULL
tri_id_col <- NA_character_
tri_name_col <- NA_character_

if (file.exists(f_tri)) {
  tri_raw <- readr::read_csv(f_tri, show_col_types = FALSE)
  nm <- names(tri_raw)

  lon_col <- c("Longitude","longitude","LON","lon","LONGITUDE")[c("Longitude","longitude","LON","lon","LONGITUDE") %in% nm][1]
  lat_col <- c("Latitude","latitude","LAT","lat","LATITUDE")[c("Latitude","latitude","LAT","lat","LATITUDE") %in% nm][1]

  if (!is.na(lon_col) && !is.na(lat_col)) {
    tri_sf_ll <- sf::st_as_sf(tri_raw, coords=c(lon_col, lat_col), crs=4326, remove=FALSE)
    tri_sf_m  <- sf::st_transform(tri_sf_ll, 32613)

    tri_id_col   <- c("TRIFID","TRI_ID","TRIID","FacilityID","FACILITY_ID","REGISTRY_ID","ID")[c("TRIFID","TRI_ID","TRIID","FacilityID","FACILITY_ID","REGISTRY_ID","ID") %in% nm][1]
    tri_name_col <- c("FACILITY_NAME","FacilityName","NAME","Name","FACILITY")[c("FACILITY_NAME","FacilityName","NAME","Name","FACILITY") %in% nm][1]
  } else {
    message("[TRI] Could not find TRI lon/lat columns; skipping nearest TRI distance.")
  }
}

nearest_tri <- function(pt_m) {
  if (is.null(tri_sf_m) || nrow(tri_sf_m) == 0) {
    return(list(tri_dist_m=NA_real_, tri_id=NA_character_, tri_name=NA_character_))
  }
  d <- as.numeric(sf::st_distance(tri_sf_m, pt_m))
  k <- which.min(d)
  tri_dist_m <- d[k]
  tri_row <- sf::st_drop_geometry(tri_sf_m[k, , drop=FALSE])

  tri_id <- NA_character_
  tri_name <- NA_character_
  if (!is.na(tri_id_col) && tri_id_col %in% names(tri_row)) tri_id <- as.character(tri_row[[tri_id_col]][1])
  if (!is.na(tri_name_col) && tri_name_col %in% names(tri_row)) tri_name <- as.character(tri_row[[tri_name_col]][1])

  list(tri_dist_m=tri_dist_m, tri_id=tri_id, tri_name=tri_name)
}

# ============================================================
# 5) Helpers
# ============================================================
collapse_days <- function(x) {
  x <- sort(unique(x))
  if (length(x) == 0) return("")
  paste(format(x, "%Y-%m-%d"), collapse=";")
}

safe_add_tiles <- function(p, zoom) {
  # If tile fetching fails, we still return p without crashing.
  tryCatch(
    p + ggspatial::annotation_map_tile(type = tile_type, zoom = zoom),
    error = function(e) {
      message("[Basemap] Tile fetch failed; continuing without basemap. ", conditionMessage(e))
      p
    }
  )
}

make_map_regional <- function(gid) {
  g_this <- groups_sf_ll %>% filter(group_id == gid)

  p <- ggplot()
  p <- safe_add_tiles(p, tile_zoom_regional)

  p +
    coord_sf(
      crs = 4326,
      xlim = c(as.numeric(bb_ll["xmin"]), as.numeric(bb_ll["xmax"])),
      ylim = c(as.numeric(bb_ll["ymin"]), as.numeric(bb_ll["ymax"])),
      expand = FALSE
    ) +
    geom_sf(data = groups_sf_ll, inherit.aes = FALSE, size = 1.3, alpha = 0.9) +
    geom_sf(data = g_this,       inherit.aes = FALSE, size = 4.0) +
    theme_bw() +
    theme(
      plot.margin = margin(10, 10, 10, 10, unit = "pt")
    ) +
    labs(
      title = paste0("Persistent hotspot group ", gid),
      subtitle = "All persistent hotspot centroids (highlighted group in bold)"
    )
}

make_map_local <- function(gid, pt_ll, mobile_ll) {
  pt_m <- sf::st_transform(pt_ll, 32613)

  bb_loc_m <- sf::st_bbox(pt_m)
  bb_loc_m[c("xmin","ymin")] <- bb_loc_m[c("xmin","ymin")] - (match_dist_m + local_pad_m)
  bb_loc_m[c("xmax","ymax")] <- bb_loc_m[c("xmax","ymax")] + (match_dist_m + local_pad_m)
  bb_loc_ll <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_loc_m), 4326))

  buf_ll <- sf::st_transform(sf::st_buffer(pt_m, match_dist_m), 4326)

  p <- ggplot()
  p <- safe_add_tiles(p, tile_zoom_local)

  p +
    coord_sf(
      crs = 4326,
      xlim = c(as.numeric(bb_loc_ll["xmin"]), as.numeric(bb_loc_ll["xmax"])),
      ylim = c(as.numeric(bb_loc_ll["ymin"]), as.numeric(bb_loc_ll["ymax"])),
      expand = FALSE
    ) +
    geom_sf(data = mobile_ll, inherit.aes = FALSE, size = 0.5, alpha = 0.55) +
    geom_sf(data = buf_ll,    inherit.aes = FALSE, fill = NA, linewidth = 0.8) +
    geom_sf(data = pt_ll,     inherit.aes = FALSE, size = 4.0) +
    theme_bw() +
    theme(
      plot.margin = margin(10, 10, 10, 10, unit = "pt")
    ) +
    labs(
      title = paste0("Local view: group ", gid, " (100 m buffer)"),
      subtitle = "Mobile points within 100 m (dots) and 100 m buffer (outline)"
    )
}

make_polar_plot <- function(df_sub, pol_cols, title_prefix) {
  if (!all(c("ws","wd") %in% names(df_sub))) return(NULL)
  pol_cols <- pol_cols[pol_cols %in% names(df_sub)]
  if (length(pol_cols) == 0) return(NULL)
  openair::polarPlot(df_sub, pollutant = pol_cols, statistic = "nwr", main = title_prefix)
}

pair_plot_one <- function(m, xcol, ycol) {
  xlab <- pretty_poll_label(xcol)
  ylab <- pretty_poll_label(ycol)

  ggplot(m, aes(x = .data[[xcol]], y = .data[[ycol]], color = wd_sector)) +
    geom_point(aes(size = ws), alpha = 0.85) +
    ggsci::scale_color_npg(drop = FALSE) +
    scale_size_continuous(range = c(0.6, 2.2), guide = "none") +
    theme_bw() +
    theme(
      legend.position = "right",
      plot.margin = margin(10, 18, 10, 10, unit = "pt"),
      axis.title = element_text(size = 10),
      axis.text  = element_text(size = 9),
      plot.title = element_text(size = 11)
    ) +
    labs(
      title = paste0(xlab, " vs ", ylab),
      x = xlab, y = ylab,
      color = "Wind Direction"
    )
}

make_pairwise_panel <- function(df_g, pol_cols, gid) {
  pol_cols <- pol_cols[pol_cols %in% names(df_g)]
  if (length(pol_cols) < 2) return(NULL)
  if (nrow(df_g) < min_rows_for_pairs) return(NULL)

  ok_cols <- pol_cols[vapply(pol_cols, function(cc) {
    z <- df_g[[cc]]
    is.numeric(z) && sum(is.finite(z)) >= 5 && sd(z, na.rm=TRUE) > 0
  }, logical(1))]
  if (length(ok_cols) < 2) return(NULL)

  pairs <- combn(ok_cols, 2, simplify = FALSE)
  plots <- lapply(pairs, function(pr) pair_plot_one(df_g, pr[1], pr[2]))

  big_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Pairwise pollutant scatterplots colored by wind direction\nwithin ",
             match_dist_m, " m | group ", gid, " | n=", nrow(df_g)),
      fontface = "bold", x = 0.5, hjust = 0.5, size = 12
    )

  panel <- cowplot::plot_grid(plotlist = plots, ncol = 3, align = "hv")

  cowplot::plot_grid(big_title, panel, ncol = 1, rel_heights = c(0.08, 1))
}

# ============================================================
# 6) MAIN LOOP
# ============================================================
master_rows <- list()

for (i in seq_len(nrow(groups_sf_m))) {

  gid <- groups_sf_m$group_id[i]
  cat("\n--- Processing group_id: ", gid, " (", i, "/", nrow(groups_sf_m), ") ---\n", sep="")

  pt_m  <- groups_sf_m[i, ]
  pt_ll <- groups_sf_ll[i, ]

  d_m <- as.numeric(sf::st_distance(df_sf_m, pt_m))
  idx <- which(d_m <= match_dist_m)

  cat("Mobile rows within ", match_dist_m, "m: ", length(idx), "\n", sep="")
  if (length(idx) == 0) next

  df_g_sf_m  <- df_sf_m[idx, ]
  df_g_sf_ll <- sf::st_transform(df_g_sf_m, 4326)
  df_g <- sf::st_drop_geometry(df_g_sf_m)

  tri_info <- nearest_tri(pt_m)

  # pollutant diagnostics
  diag_list <- list()
  n_poll_high_here <- 0L

  for (j in seq_len(nrow(b99_tbl))) {
    pol <- b99_tbl$pollutant[j]
    col <- b99_tbl$col[j]
    b99 <- b99_tbl$b99[j]
    if (!is.finite(b99)) next
    if (!(col %in% names(df_g))) next

    v <- df_g[[col]]
    ok <- is.finite(v) & (v > b99)

    n_high <- sum(ok, na.rm = TRUE)
    days_high <- unique(df_g$day[ok])
    n_days_high <- length(days_high)
    if (n_days_high > 0) n_poll_high_here <- n_poll_high_here + 1L

    diag_list[[pol]] <- data.frame(
      group_id = gid,
      pollutant = pol,
      pollutant_col = col,
      b99_ppb = b99,
      n_high = n_high,
      n_days_high = n_days_high,
      days_high = collapse_days(days_high),
      stringsAsFactors = FALSE
    )
  }
  diag_df <- do.call(rbind, diag_list)

  # outputs
  tag <- paste0("group_", gid)
  out_group_csv     <- file.path(out_dir, paste0(tag, "_pollutant_highday_table.csv"))
  out_map_reg_png   <- file.path(out_dir, paste0(tag, "_map_regional.png"))
  out_map_local_png <- file.path(out_dir, paste0(tag, "_map_local.png"))
  out_pairs_png     <- file.path(out_dir, paste0(tag, "_pairwise_wd.png"))
  out_polar_png     <- file.path(out_dir, paste0(tag, "_polar.png"))

  cat("Writing:\n")
  cat("  table:    ", out_group_csv, "\n", sep="")
  cat("  map(reg): ", out_map_reg_png, "\n", sep="")
  cat("  map(loc): ", out_map_local_png, "\n", sep="")
  cat("  pairs:    ", out_pairs_png, "\n", sep="")
  cat("  polar:    ", out_polar_png, "\n", sep="")

  readr::write_csv(diag_df, out_group_csv)

  # maps (HOTSPOTS ONLY; no TRI)
  p_reg <- make_map_regional(gid)
  ragg::agg_png(out_map_reg_png, width = 6.8, height = 6.8, units = "in", res = 300, background = "white")
  print(p_reg)
  dev.off()

  p_loc <- make_map_local(gid, pt_ll, df_g_sf_ll)
  ragg::agg_png(out_map_local_png, width = 6.8, height = 6.8, units = "in", res = 300, background = "white")
  print(p_loc)
  dev.off()

  # pairwise (no stat text; cleaned labels)
  pol_cols_existing <- b99_tbl$col[b99_tbl$col %in% names(df_g)]
  p_pairs <- make_pairwise_panel(df_g, pol_cols_existing, gid)
  if (!is.null(p_pairs)) {
    ragg::agg_png(out_pairs_png, width = 14, height = 12, units = "in", res = 300, background = "white")
    print(p_pairs)
    dev.off()
  } else {
    out_pairs_png <- ""
    cat("  (pairs) skipped.\n")
  }

  # polar
  pp <- NULL
  try(pp <- make_polar_plot(
    df_g,
    pol_cols_existing,
    paste0("Polar plots (nwr) within ", match_dist_m, " m | group ", gid)
  ), silent = TRUE)

  if (!is.null(pp)) {
    ragg::agg_png(out_polar_png, width = 8, height = 8, units = "in", res = 300, background = "white")
    print(pp)
    dev.off()
  } else {
    out_polar_png <- ""
    cat("  (polar) skipped.\n")
  }

  # master row
  g_row <- groups %>%
    dplyr::filter(group_id == gid) %>%
    dplyr::slice_head(n = 1)

  master_rows[[length(master_rows) + 1]] <- data.frame(
    group_id = gid,
    Longitude = g_row$Longitude,
    Latitude  = g_row$Latitude,
    n_pollutants_in_group_summary = g_row$n_pollutants,
    pollutants_in_group_summary   = g_row$pollutants,
    n_pollutants_high_within_100m = n_poll_high_here,
    n_mobile_rows_within_100m     = nrow(df_g),

    tri_dist_m  = tri_info$tri_dist_m,
    tri_dist_km = if (is.finite(tri_info$tri_dist_m)) tri_info$tri_dist_m/1000 else NA_real_,
    tri_id      = tri_info$tri_id,
    tri_name    = tri_info$tri_name,

    out_group_csv     = out_group_csv,
    out_map_reg_png   = out_map_reg_png,
    out_map_local_png = out_map_local_png,
    out_pairs_png     = out_pairs_png,
    out_polar_png     = out_polar_png,
    stringsAsFactors = FALSE
  )
}

master <- do.call(rbind, master_rows)
out_master <- file.path(out_dir, "MASTER_hotspot_group_index.csv")
readr::write_csv(master, out_master)

cat("\nDONE.\nWrote master index:\n  ", out_master, "\n", sep="")
cat("Per-group outputs in:\n  ", out_dir, "\n", sep="")
