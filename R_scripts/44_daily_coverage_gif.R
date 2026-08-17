# ==============================================================
# 44  DAILY MEASUREMENT ANIMATION (GIF)
# One frame per sampling day showing where the vans measured,
# colored by route, with date and observation count.
# Requires: install.packages("magick")   (binary install, no compile)
# Outputs:
#   FinalFig/daily_frames/frame_###.png   (one per sampling day)
#   FinalFig/daily_measurements.gif
# Runtime: ~10-25 min (203 frames; map tiles cached after first frame)
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggspatial); library(scales)
  library(magick)
})

BASE  <- "/Users/priyanka/Downloads/Suncor"
FRDIR <- file.path(BASE, "FinalFig", "daily_frames")
dir.create(FRDIR, recursive = TRUE, showWarnings = FALSE)
CACHE <- file.path(BASE, "rosm.cache")
dir.create(CACHE, showWarnings = FALSE)

message("Loading mobile data...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out)
df <- df[is.finite(Latitude) & is.finite(Longitude) &
         Site != "Goodrich Corporation (Collins Aerospace)"]
stopifnot("date" %in% names(df))
df[, day := as.Date(date)]
df[, Route := factor(ifelse(grepl("Suncor", Site),
                            "Suncor & Phillips 66 route",
                            "Sinclair Terminal route"),
                     levels = c("Suncor & Phillips 66 route",
                                "Sinclair Terminal route"))]
days <- sort(unique(df$day))
message("  ", length(days), " sampling days, ",
        format(min(days), "%b %d, %Y"), " - ", format(max(days), "%b %d, %Y"))

padx <- 0.012; pady <- 0.012
xlim <- unname(range(df$Longitude)) + c(-padx, padx)
ylim <- unname(range(df$Latitude)) + c(-pady, pady)
stopifnot(xlim[1] > -106, xlim[2] < -104, ylim[1] > 39, ylim[2] < 41)

route_cols <- c("Suncor & Phillips 66 route" = "#2166ac",
                "Sinclair Terminal route"    = "#b2182b")

t0 <- Sys.time()
for (i in seq_along(days)) {
  d <- days[i]
  sub <- df[day == d]
  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoom = 11, cachedir = CACHE) +
    geom_point(data = sub, aes(Longitude, Latitude, color = Route),
               size = 0.35, alpha = 0.5) +
    scale_color_manual(values = route_cols, drop = FALSE,
                       guide = guide_legend(override.aes = list(size = 3, alpha = 1))) +
    coord_sf(crs = 4326, default_crs = 4326, xlim = xlim, ylim = ylim,
             expand = FALSE) +
    labs(title = sprintf("%s  (sampling day %d of %d)",
                         format(d, "%A, %B %d, %Y"), i, length(days)),
         subtitle = sprintf("%s 1-s measurements",
                            comma(nrow(sub))),
         x = NULL, y = NULL, color = NULL,
         caption = "Basemap: CARTO Positron") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(size = 8, hjust = 0))
  ggsave(file.path(FRDIR, sprintf("frame_%03d.png", i)), p,
         width = 7, height = 6.4, dpi = 110, bg = "white")
  if (i %% 10 == 0 || i == length(days))
    message("  frame ", i, "/", length(days), "  (",
            round(difftime(Sys.time(), t0, units = "mins"), 1), " min elapsed)")
}

message("Assembling GIF (this takes a few minutes)...")
ff <- list.files(FRDIR, pattern = "^frame_\\d+\\.png$", full.names = TRUE)
stopifnot(length(ff) == length(days))
gif <- image_animate(image_join(lapply(ff, image_read)), fps = 5, optimize = TRUE)
out_gif <- file.path(BASE, "FinalFig", "daily_measurements.gif")
image_write(gif, out_gif)
message("[Saved] ", out_gif, "  (", round(file.size(out_gif) / 1e6, 1), " MB)")
