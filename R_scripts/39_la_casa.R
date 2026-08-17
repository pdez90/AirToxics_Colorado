# ==============================================================
# 39  La Casa
# Auto-split from Suncor.Rmd  (section 39 of 40)
# ==============================================================

#La Casa
# The data file with ppb in the name is normalized and background corrected. Drew says the  C8H10H is much larger than it should be due to incorrect normalization.

# The file that has 0709 in the name is in cps. There are more VOCs in this one, but the data is not normalized/background corrected.

# Both files have the HCHO and CDPHE data (some zeros and cals are in there but it does not cause any huge problems for initial looks).

# ============================================================
# La Casa vs Mobile: EXACT-MINUTE joins + Pearson + RMSE + ODR
# for BOTH 100 m and 500 m thresholds
# - Benzene in La Casa set to NA (faulty)
# - Aggregate BOTH datasets to exact minutes
# - Join by exact minute
# - Compute Pearson, RMSE, ODR
# - Save overlap tables for each threshold/year
# - Make 500 m scatterplots colored by distance to La Casa
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(geosphere)
  library(openair)
  library(ggplot2)
  library(cowplot)
})

options(scipen = 9999)

# ----------------------------
# 0) Paths
# ----------------------------
mobile_rdata <- "/Users/priyanka/Downloads/Suncor/mobile_wswd.RData"
lacasa_2023  <- "/Users/priyanka/Downloads/Suncor/ascent_2023.csv"
lacasa_2024  <- "/Users/priyanka/Downloads/Suncor/ascent_2024.csv"
lacasa3_csv  <- "/Users/priyanka/Downloads/Suncor/lacasa3.csv"
out_dir      <- "/Users/priyanka/Downloads/Suncor/FinalFig"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# 1) Load MOBILE data
# ----------------------------
load(mobile_rdata)  # expects `out`
df <- out
rm(out)
setDT(df)

need_mobile <- c("date", "Latitude", "Longitude", "Toluene_ppb", "Xylene_ppb")
miss <- setdiff(need_mobile, names(df))
if (length(miss)) stop("Mobile df missing: ", paste(miss, collapse = ", "))

if (!inherits(df$date, "POSIXct")) {
  df[, date := ymd_hms(date, quiet = TRUE)]
  if (all(is.na(df$date))) df[, date := ymd_hm(date, quiet = TRUE)]
}

df <- df[is.finite(Latitude) & is.finite(Longitude) & !is.na(date)]

# ----------------------------
# 2) Compute distance to La Casa
# ----------------------------
lacasa_lon <- -105.00481
lacasa_lat <-  39.77906

df[, ascent_distance_m := geosphere::distGeo(
  matrix(c(Longitude, Latitude), ncol = 2),
  matrix(c(rep(lacasa_lon, .N), rep(lacasa_lat, .N)), ncol = 2)
)]

# Keep only fields needed downstream
df <- df[, .(
  date,
  Benzene_ppb,
  Toluene_ppb,
  Trimethylbenzene_ppb,
  Xylene_ppb,
  Relative_Humidity_percent,
  Pressure_mb,
  Temperature_F,
  ascent_distance_m
)]

# ----------------------------
# 3) Load La Casa datasets + standardize
# ----------------------------
read_lacasa_ascent <- function(path) {
  x <- fread(path)
  setnames(
    x,
    old = names(x),
    new = c("date_mst","date_mst1","date","date_mdt","benzene","toluene","xylene",
            "wd","ws","temp_far","temp_c","rh")[seq_along(names(x))]
  )
  x[, date := dmy_hm(date)]
  x
}

lacasa1 <- read_lacasa_ascent(lacasa_2023)
lacasa2 <- read_lacasa_ascent(lacasa_2024)

lacasa3 <- fread(lacasa3_csv)
setnames(lacasa3, old = names(lacasa3), new = c("date","toluene","xylene")[seq_along(names(lacasa3))])
lacasa3[, date := mdy_hm(date)]

# ELF = 2023 ascent + lacasa3
lacasa_elf <- rbindlist(list(lacasa1, lacasa3), fill = TRUE)

# 2R = 2024 ascent
lacasa_2r <- copy(lacasa2)

# Benzene faulty at La Casa
lacasa_elf[, benzene := NA_real_]
lacasa_2r[,  benzene := NA_real_]

lacasa_elf <- lacasa_elf[, .(date, toluene, xylene)]
lacasa_2r  <- lacasa_2r[,  .(date, benzene, toluene, xylene)]

setnames(lacasa_elf, c("toluene","xylene"), c("LaCasa_Toluene","LaCasa_Xylene"))
setnames(lacasa_2r,  c("toluene","xylene"), c("LaCasa_Toluene","LaCasa_Xylene"))
setnames(lacasa_2r,  "benzene", "LaCasa_Benzene")

elf_min <- as.data.table(openair::timeAverage(lacasa_elf, avg.time = "min"))
r2_min  <- as.data.table(openair::timeAverage(lacasa_2r,  avg.time = "min"))

# ----------------------------
# 4) Helpers: metrics
# ----------------------------
rmse_fun <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((y[ok] - x[ok])^2))
}

# Linear orthogonal regression / total least squares
# Assumes comparable error structure in x and y
odr_fit <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]

  n <- length(x)
  if (n < 3) {
    return(list(n = n, intercept = NA_real_, slope = NA_real_))
  }

  xbar <- mean(x)
  ybar <- mean(y)
  X <- cbind(x - xbar, y - ybar)
  s <- svd(X)
  v1 <- s$v[, 1]

  if (abs(v1[1]) < .Machine$double.eps) {
    slope <- NA_real_
    intercept <- NA_real_
  } else {
    slope <- v1[2] / v1[1]
    intercept <- ybar - slope * xbar
  }

  list(n = n, intercept = intercept, slope = slope)
}

metric_summary <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  n_ok <- sum(ok)

  if (n_ok < 3) {
    return(data.table(
      n = n_ok,
      pearson_r = NA_real_,
      rmse = NA_real_,
      odr_intercept = NA_real_,
      odr_slope = NA_real_
    ))
  }

  odr <- odr_fit(x, y)

  data.table(
    n = n_ok,
    pearson_r = cor(x[ok], y[ok]),
    rmse = rmse_fun(x, y),
    odr_intercept = odr$intercept,
    odr_slope = odr$slope
  )
}

print_metric_line <- function(dt_row) {
  cat(
    "  threshold_", dt_row$threshold_m, "m | ", dt_row$dataset, " | ", dt_row$pollutant,
    ": n = ", dt_row$n,
    " | Pearson r = ", round(dt_row$pearson_r, 3),
    " | RMSE = ", round(dt_row$rmse, 3),
    " | ODR intercept = ", round(dt_row$odr_intercept, 3),
    " | ODR slope = ", round(dt_row$odr_slope, 3),
    "\n", sep = ""
  )
}

# ----------------------------
# 5) Function to process one threshold
# ----------------------------
run_one_threshold <- function(threshold_m, df_mobile, elf_min, r2_min, out_dir) {

  cat("\n============================================================\n")
  cat("Processing threshold:", threshold_m, "m\n")
  cat("============================================================\n")

  df_near <- df_mobile[is.finite(ascent_distance_m) & ascent_distance_m <= threshold_m]

  cat("Mobile rows within ", threshold_m, " m: ",
      format(nrow(df_near), big.mark = ","), "\n", sep = "")

  if (nrow(df_near) == 0) {
    return(list(
      results_tbl = data.table(),
      join_elf_2023 = data.table(),
      join_2r_2024 = data.table()
    ))
  }

  # minute-average mobile
  mobile_min <- as.data.table(openair::timeAverage(df_near, avg.time = "min"))

  if (!"date" %in% names(mobile_min)) {
    stop("mobile_min is missing 'date' after timeAverage().")
  }

  # minute-level distance summary
  dist_min <- df_near[, .(
    mean_distance_m = mean(ascent_distance_m, na.rm = TRUE),
    min_distance_m  = min(ascent_distance_m, na.rm = TRUE),
    max_distance_m  = max(ascent_distance_m, na.rm = TRUE),
    n_obs_in_minute = .N
  ), by = .(date = floor_date(date, unit = "minute"))]

  mobile_min <- merge(mobile_min, dist_min, by = "date", all.x = TRUE)

  # keep rows with at least one target pollutant present
  if (all(c("Toluene_ppb", "Xylene_ppb") %in% names(mobile_min))) {
    mobile_min <- mobile_min[!(is.na(Toluene_ppb) & is.na(Xylene_ppb))]
  } else if ("Toluene_ppb" %in% names(mobile_min)) {
    mobile_min <- mobile_min[!is.na(Toluene_ppb)]
  } else if ("Xylene_ppb" %in% names(mobile_min)) {
    mobile_min <- mobile_min[!is.na(Xylene_ppb)]
  } else {
    stop("Neither Toluene_ppb nor Xylene_ppb found after timeAverage(). Names are: ",
         paste(names(mobile_min), collapse = ", "))
  }

  cat("Mobile unique minutes (<= ", threshold_m, " m): ",
      format(nrow(mobile_min), big.mark = ","), "\n", sep = "")

  mobile_min[, year := year(date)]

  mobile_2023 <- mobile_min[year == 2023]
  mobile_2024 <- mobile_min[year == 2024]

  join_elf_2023 <- merge(mobile_2023, elf_min, by = "date", all = FALSE)
  join_2r_2024  <- merge(mobile_2024, r2_min,  by = "date", all = FALSE)

  cat("\nExact-minute overlaps:\n")
  cat("  2023 ELF overlap minutes:", nrow(join_elf_2023), "\n")
  cat("  2024 2R  overlap minutes:", nrow(join_2r_2024),  "\n")

  # metrics
  res_2023_t <- metric_summary(join_elf_2023$Toluene_ppb, join_elf_2023$LaCasa_Toluene)
  res_2023_x <- metric_summary(join_elf_2023$Xylene_ppb,  join_elf_2023$LaCasa_Xylene)
  res_2024_t <- metric_summary(join_2r_2024$Toluene_ppb,  join_2r_2024$LaCasa_Toluene)
  res_2024_x <- metric_summary(join_2r_2024$Xylene_ppb,   join_2r_2024$LaCasa_Xylene)

  results_tbl <- rbindlist(list(
    cbind(data.table(threshold_m = threshold_m, dataset = "2023_ELF", pollutant = "Toluene"), res_2023_t),
    cbind(data.table(threshold_m = threshold_m, dataset = "2023_ELF", pollutant = "Xylene"),  res_2023_x),
    cbind(data.table(threshold_m = threshold_m, dataset = "2024_2R",  pollutant = "Toluene"), res_2024_t),
    cbind(data.table(threshold_m = threshold_m, dataset = "2024_2R",  pollutant = "Xylene"),  res_2024_x)
  ), fill = TRUE)

  cat("\nMinute-level comparison metrics (Mobile vs La Casa):\n")
  print(results_tbl)

  # save overlap tables
  out_csv_overlap_2023 <- file.path(
    out_dir, paste0("lacasa_2023_elf_exact_minute_overlaps_", threshold_m, "m.csv")
  )
  out_csv_overlap_2024 <- file.path(
    out_dir, paste0("lacasa_2024_2r_exact_minute_overlaps_", threshold_m, "m.csv")
  )

  fwrite(as.data.table(join_elf_2023), out_csv_overlap_2023)
  fwrite(as.data.table(join_2r_2024),  out_csv_overlap_2024)

  cat("Saved overlap table:\n  ", out_csv_overlap_2023, "\n", sep = "")
  cat("Saved overlap table:\n  ", out_csv_overlap_2024, "\n", sep = "")

  list(
    results_tbl = results_tbl,
    join_elf_2023 = as.data.table(join_elf_2023),
    join_2r_2024 = as.data.table(join_2r_2024)
  )
}

# ----------------------------
# 6) Run thresholds: 100 m and 500 m
# ----------------------------
res_100 <- run_one_threshold(
  threshold_m = 100,
  df_mobile = df,
  elf_min = elf_min,
  r2_min = r2_min,
  out_dir = out_dir
)

res_500 <- run_one_threshold(
  threshold_m = 500,
  df_mobile = df,
  elf_min = elf_min,
  r2_min = r2_min,
  out_dir = out_dir
)

results_all <- rbindlist(list(res_100$results_tbl, res_500$results_tbl), fill = TRUE)

cat("\n============================================================\n")
cat("ALL RESULTS\n")
cat("============================================================\n")
print(results_all)

cat("\nHuman-readable summary:\n")
for (i in seq_len(nrow(results_all))) {
  print_metric_line(results_all[i])
}

out_csv_metrics <- file.path(out_dir, "lacasa_mobile_exact_minute_metrics_100m_500m.csv")
fwrite(results_all, out_csv_metrics)
cat("\nSaved metrics table to:\n", out_csv_metrics, "\n")

# ----------------------------
# 7) Scatter plot helper for 500 m threshold
# ----------------------------
scatter_with_distance <- function(df_join, xvar, yvar, distvar, title_txt, xlab_txt, ylab_txt) {
  if (nrow(df_join) == 0) return(NULL)

  ok <- is.finite(df_join[[xvar]]) & is.finite(df_join[[yvar]]) & is.finite(df_join[[distvar]])
  d2 <- df_join[ok]

  if (nrow(d2) == 0) return(NULL)

  met <- metric_summary(d2[[xvar]], d2[[yvar]])
  lab <- paste0(
    "n = ", met$n,
    "\nPearson r = ", round(met$pearson_r, 2),
    "\nRMSE = ", round(met$rmse, 3),
    "\nODR slope = ", round(met$odr_slope, 2)
  )

  x_rng <- range(c(d2[[xvar]], d2[[yvar]]), na.rm = TRUE)
  lim_lo <- x_rng[1]
  lim_hi <- x_rng[2]

  ggplot(d2, aes(x = .data[[xvar]], y = .data[[yvar]], color = .data[[distvar]])) +
    geom_point(size = 2.5, alpha = 0.9) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = lim_lo + 0.05 * (lim_hi - lim_lo),
      y = lim_hi - 0.05 * (lim_hi - lim_lo),
      label = lab,
      hjust = 0, vjust = 1, size = 4
    ) +
    scale_color_viridis_c(name = "Mean distance (m)") +
    labs(
      title = title_txt,
      x = xlab_txt,
      y = ylab_txt
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

# ----------------------------
# 8) Make 500 m scatter plots colored by distance
# ----------------------------
p_500_2023_t <- scatter_with_distance(
  res_500$join_elf_2023,
  xvar = "LaCasa_Toluene",
  yvar = "Toluene_ppb",
  distvar = "mean_distance_m",
  title_txt = "2023 ELF Toluene (500 m threshold)",
  xlab_txt = "La Casa Toluene (ppb)",
  ylab_txt = "Mobile Toluene (ppb)"
)

p_500_2023_x <- scatter_with_distance(
  res_500$join_elf_2023,
  xvar = "LaCasa_Xylene",
  yvar = "Xylene_ppb",
  distvar = "mean_distance_m",
  title_txt = "2023 ELF Xylene (500 m threshold)",
  xlab_txt = "La Casa Xylene (ppb)",
  ylab_txt = "Mobile Xylene (ppb)"
)

p_500_2024_t <- scatter_with_distance(
  res_500$join_2r_2024,
  xvar = "LaCasa_Toluene",
  yvar = "Toluene_ppb",
  distvar = "mean_distance_m",
  title_txt = "2024 2R Toluene (500 m threshold)",
  xlab_txt = "La Casa Toluene (ppb)",
  ylab_txt = "Mobile Toluene (ppb)"
)

p_500_2024_x <- scatter_with_distance(
  res_500$join_2r_2024,
  xvar = "LaCasa_Xylene",
  yvar = "Xylene_ppb",
  distvar = "mean_distance_m",
  title_txt = "2024 2R Xylene (500 m threshold)",
  xlab_txt = "La Casa Xylene (ppb)",
  ylab_txt = "Mobile Xylene (ppb)"
)

plots_500 <- Filter(Negate(is.null), list(
  p_500_2023_t, p_500_2023_x, p_500_2024_t, p_500_2024_x
))

if (length(plots_500) > 0) {
  out_plot <- file.path(out_dir, "lacasa_mobile_scatterplots_500m_colored_by_distance.png")
  png(out_plot, width = 3200, height = 2600, res = 300)
  print(cowplot::plot_grid(plotlist = plots_500, labels = LETTERS[1:length(plots_500)], ncol = 2))
  dev.off()
  cat("Saved 500 m scatter plots to:\n", out_plot, "\n")
} else {
  cat("No 500 m scatter plots created.\n")
}

# Optional: print plots in session
if (!is.null(p_500_2023_t)) print(p_500_2023_t)
if (!is.null(p_500_2023_x)) print(p_500_2023_x)
if (!is.null(p_500_2024_t)) print(p_500_2024_t)
if (!is.null(p_500_2024_x)) print(p_500_2024_x)
