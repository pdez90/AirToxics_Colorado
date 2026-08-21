# ==============================================================
# 53  CAMPAIGN WIND ROSE (inset for Figures 2 and 3)
# Builds a compact wind rose from the wind data actually used in
# the analysis (nearest EPA AQS station values merged to each 1-s
# observation): 16 sectors, stacked by wind-speed class, frequency
# in percent. Saved as a small square PNG with a white background,
# sized for use as a corner inset.
# Output: FinalFig/windrose_campaign.png
# ==============================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales)
})

BASE <- "/Users/priyanka/Downloads/Suncor"
message("Loading mobile data (wind columns)...")
load(file.path(BASE, "mobile_wswd.RData"))   # out
df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)" &
         is.finite(ws) & is.finite(wd) & ws >= 0]
message("  wind-valid rows: ", format(nrow(df), big.mark = ","))

sect <- 360 / 16
df[, sector := floor(((wd + sect / 2) %% 360) / sect)]
df[, ws_class := cut(ws, breaks = c(0, 2, 4, 6, Inf),
                     labels = c("0-2", "2-4", "4-6", ">6"),
                     include.lowest = TRUE)]
rose <- df[, .N, by = .(sector, ws_class)]
rose[, pct := 100 * N / sum(N)]
print(data.table::dcast(rose, sector ~ ws_class, value.var = "pct", fill = 0))

p <- ggplot(rose, aes(x = factor(sector, levels = 0:15),
                      y = pct, fill = ws_class)) +
  geom_col(width = 0.95, color = "grey25", linewidth = 0.15) +
  coord_polar(start = -pi / 16) +
  scale_x_discrete(labels = c("N", "", "NE", "", "E", "", "SE", "",
                              "S", "", "SW", "", "W", "", "NW", ""),
                   drop = FALSE) +
  scale_fill_manual(values = c("0-2" = "#c6dbef", "2-4" = "#6baed6",
                               "4-6" = "#2171b5", ">6" = "#08306b"),
                    name = "m/s") +
  labs(x = NULL, y = NULL, title = "Wind (EPA AQS)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, color = "grey80"),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        legend.key.size = unit(0.75, "lines"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        plot.background = element_rect(fill = "white", color = "grey30",
                                       linewidth = 0.6))
ggsave(file.path(BASE, "FinalFig", "windrose_campaign.png"),
       p, width = 3.1, height = 3.4, dpi = 400, bg = "white")
message("[Saved] FinalFig/windrose_campaign.png")
message("DONE.")
