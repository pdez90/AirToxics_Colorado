# ==============================================================
# 56  WILDFIRE SMOKE CROSS-CHECK (NOAA HMS) — SI
# Downloads the NOAA Hazard Mapping System (HMS) smoke polygons for
# every sampling day, flags days when smoke overlay covered the
# study domain (max density: Light/Medium/Heavy), and compares
# pollutant concentrations on smoke vs smoke-free days.
# HCN is the key species: it is a biomass-burning tracer, so
# elevated campaign-wide HCN on HMS smoke days would indicate
# regional wildfire influence rather than local sources.
# Requires internet. HMS archive:
#   https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/YYYY/MM/hms_smokeYYYYMMDD.zip
# Outputs:
#   TABLE_smoke_days.csv         (per sampling day: density class)
#   TABLE_smoke_comparison.csv   (per pollutant: stats by smoke class)
#   FinalFig/FIG_smoke_comparison.png
# Runtime: ~5-15 min (203 small downloads, cached in hms_cache/)
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
CACHE <- file.path(BASE, "hms_cache")
dir.create(CACHE, showWarnings = FALSE)

message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))
df <- as.data.table(out); rm(out); gc()
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]
days <- sort(unique(df$day))
message(length(days), " sampling days")

dom <- st_as_sfc(st_bbox(c(xmin = min(df$Longitude), xmax = max(df$Longitude),
                           ymin = min(df$Latitude), ymax = max(df$Latitude)),
                         crs = 4326))

# ---- fetch HMS smoke polygons per day -------------------------
get_smoke <- function(d) {
  ymd <- format(d, "%Y%m%d")
  zipf <- file.path(CACHE, paste0("hms_smoke", ymd, ".zip"))
  if (!file.exists(zipf)) {
    url <- sprintf(paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/",
                          "HMS/Smoke_Polygons/Shapefile/%s/%s/hms_smoke%s.zip"),
                   format(d, "%Y"), format(d, "%m"), ymd)
    ok <- tryCatch({
      utils::download.file(url, zipf, mode = "wb", quiet = TRUE); TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) { unlink(zipf); return("no_product") }
  }
  td <- file.path(tempdir(), ymd)
  ok <- tryCatch({ unzip(zipf, exdir = td); TRUE }, error = function(e) FALSE)
  if (!ok) return("read_error")
  shp <- list.files(td, pattern = "\\.shp$", full.names = TRUE)[1]
  if (is.na(shp)) return("read_error")
  sm <- tryCatch(suppressWarnings(st_read(shp, quiet = TRUE)),
                 error = function(e) NULL)
  if (is.null(sm) || nrow(sm) == 0) return("none")
  sm <- suppressWarnings(st_make_valid(st_set_crs(sm, 4326)))
  hit <- tryCatch(suppressMessages(
    lengths(st_intersects(sm, dom)) > 0), error = function(e) rep(FALSE, nrow(sm)))
  if (!any(hit)) return("none")
  dens <- toupper(as.character(sm$Density[hit]))
  # older files use numeric codes 5/16/27
  dens[dens %in% c("5", "5.0")] <- "LIGHT"
  dens[dens %in% c("16", "16.0")] <- "MEDIUM"
  dens[dens %in% c("27", "27.0")] <- "HEAVY"
  if (any(dens == "HEAVY")) "heavy" else
    if (any(dens == "MEDIUM")) "medium" else "light"
}

res <- data.table(day = days, smoke = NA_character_)
t0 <- Sys.time()
for (i in seq_along(days)) {
  res$smoke[i] <- get_smoke(days[i])
  if (i %% 20 == 0 || i == length(days))
    message("  ", i, "/", length(days), " days (",
            round(difftime(Sys.time(), t0, units = "mins"), 1), " min)")
}
print(res[, .N, by = smoke])
fwrite(res, file.path(BASE, "TABLE_smoke_days.csv"))
n_fail <- sum(res$smoke %in% c("no_product", "read_error"))
message(n_fail, " days had no retrievable HMS product (excluded from comparison)")

# ---- compare concentrations by smoke class --------------------
res[, class := fifelse(smoke %in% c("light", "medium", "heavy"), smoke,
                fifelse(smoke == "none", "none", NA_character_))]
df <- merge(df, res[, .(day, class)], by = "day")
df <- df[!is.na(class)]
df[, class := factor(class, levels = c("none", "light", "medium", "heavy"))]
message("Sampling days by class: ",
        paste(capture.output(print(table(unique(df[, .(day, class)])$class))),
              collapse = " "))

POLLS <- c(Benzene = "Benzene_ppb", Toluene = "Toluene_ppb",
           Trimethylbenzene = "Trimethylbenzene_ppb", Xylene = "Xylene_ppb",
           H2S = "Hydrogen_Sulfide_ppb", HCN = "Hydrogen_Cyanide_ppb")

daily <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  df[is.finite(get(col)), .(pollutant = pn, dmed = median(get(col)),
                            dp95 = quantile(get(col), 0.95)),
     by = .(day, class)]
}))
comp <- daily[, .(n_days = .N, median_of_day_medians = round(median(dmed), 3),
                  median_of_day_p95 = round(median(dp95), 2)),
              by = .(pollutant, class)]
setorder(comp, pollutant, class)
# Wilcoxon: any-smoke vs none, day medians
wtests <- daily[, {
  a <- dmed[class == "none"]; b <- dmed[class != "none"]
  if (length(a) > 5 && length(b) > 5) {
    wt <- wilcox.test(b, a)
    .(p_wilcoxon_smoke_vs_none = signif(wt$p.value, 3),
      ratio_smoke_over_none = round(median(b) / max(median(a), 1e-9), 2))
  } else .(p_wilcoxon_smoke_vs_none = NA_real_, ratio_smoke_over_none = NA_real_)
}, by = pollutant]
comp <- merge(comp, wtests, by = "pollutant")
fwrite(comp, file.path(BASE, "TABLE_smoke_comparison.csv"))
print(comp)
print(wtests)

# ---- figure ---------------------------------------------------
p <- ggplot(daily, aes(class, dmed, fill = class)) +
  geom_boxplot(outlier.size = 0.6, linewidth = 0.3, show.legend = FALSE) +
  facet_wrap(~pollutant, scales = "free_y") +
  scale_fill_manual(values = c(none = "grey85", light = "#fee8c8",
                               medium = "#fdbb84", heavy = "#e34a33")) +
  labs(x = "NOAA HMS smoke overlay on the study domain (sampling day)",
       y = "Daily median concentration (ppb)",
       caption = "Each point in a box is one sampling day's campaign-wide median. Smoke classes from NOAA Hazard Mapping System smoke polygons intersecting the study domain (maximum density).") +
  theme_bw(base_size = 11) +
  theme(plot.caption = element_text(size = 8.5, hjust = 0))
ggsave(file.path(BASE, "FinalFig", "FIG_smoke_comparison.png"),
       p, width = 10, height = 6.5, dpi = 400, bg = "white")
message("[Saved] FinalFig/FIG_smoke_comparison.png")
message("DONE.")
