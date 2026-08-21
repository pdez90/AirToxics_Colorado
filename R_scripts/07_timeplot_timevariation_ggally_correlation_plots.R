# ==============================================================
# 07  TimePlot + Timevariation + GGally correlation plots
# Auto-split from Suncor.Rmd  (section 7 of 40)
# ==============================================================

#TimePlot + Timevariation + GGally correlation plots 

library(dplyr)
library(visdat)
library(ggplot2)
library(naniar)
library(tidyr)
library(forcats)
require(ggridges)
library(openair)

load("/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")
df<-out
rm(out)

wind<-subset(df, select=c(Lat_wind, Lon_wind))
wind<-wind[!duplicated(wind),]
write.csv(wind, file="/Users/priyanka/Downloads/Suncor/wind_sites.csv")
rm(wind)
# --- vars you want in the missingness plot
vars_miss <- c(
  "ws", "wd", "ws_mobile", "wd_mobile",
  "Relative_Humidity_percent", "Pressure_mb", "Temperature_F",
  "Benzene_ppb", "Toluene_ppb", "Trimethylbenzene_ppb", "Xylene_ppb",
  "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb"
)

# keep only needed columns + make a row index within Site for plotting
miss_long <- df %>%
  select(Site, all_of(vars_miss)) %>%
  group_by(Site) %>%
  mutate(obs_id = row_number()) %>%
  ungroup() %>%
  pivot_longer(
    cols = all_of(vars_miss),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    missing = is.na(value),
    # nice, stable ordering of variables on the axis
    variable = factor(variable, levels = rev(vars_miss))
  )

# one faceted plot (both sites)
p <- ggplot(miss_long, aes(x = obs_id, y = variable, fill = missing)) +
  geom_raster() +
  facet_wrap(~ Site, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = "grey20", `FALSE` = "grey80"),
                    labels = c(`TRUE` = "Missing", `FALSE` = "Present"),
                    name = NULL) +
  labs(x = "Observations", y = NULL, title = "Missingness by Site") +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    # extra room for long y labels (prevents clipping)
    plot.margin = margin(t = 10, r = 15, b = 10, l = 35),
    legend.position = "bottom"
  )

# Save with sane aspect ratio so it doesn't look squashed
out_dir <- "/Users/priyanka/Downloads/Suncor"
ggsave(
  filename = file.path(out_dir, "missingness_by_site.png"),
  plot = p,
  width = 12, height = 10, dpi = 600, bg = "white"
)


# specify as an object, so we only change it in one place
#https://psyteachr.github.io/quant-fun-v3/07-more-visualisation.html
temp <- df %>% dplyr::select(Site,
  Benzene = Benzene_ppb, Toluene = Toluene_ppb,
  Trimethylbenzene = Trimethylbenzene_ppb, Xylene = Xylene_ppb,
  H2S = Hydrogen_Sulfide_ppb, HCN = Hydrogen_Cyanide_ppb,
  ws, wd, Temp = Temperature_F, Pressure = Pressure_mb,
  RH = Relative_Humidity_percent)   # by NAME (was positional c(1,7:15,20:21))
temp1<- data.table::melt(setDT(temp), id.vars = c("Site"), variable.name = "Pollutant")
temp1<-subset(temp1, temp1$Pollutant!="Temp" & temp1$Pollutant!="Pressure" & temp1$Pollutant!="RH" & temp1$Pollutant!="ws" & temp1$Pollutant!="wd" )
temp1$Pollutant<-as.character(temp1$Pollutant)

temp1<- temp1[temp1$Site!="Goodrich Corporation (Collins Aerospace)",]
temp1$Site<-factor(temp1$Site, levels=c("Suncor and Phillips 66 Terminal", "Holly Energy Partners (Sinclair) Terminal"))

dodge_value <- 0.9
p1<-temp1 %>% 
  drop_na(value) %>%
  ggplot(aes(y = value, x = Site, fill = Site)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width = 0.2, 
               fatten = NULL,
               position = position_dodge(dodge_value)) + 
  stat_summary(fun = "mean", 
               geom = "point",
               position = position_dodge(dodge_value)) +
  stat_summary(fun.data = "mean_cl_boot", 
               geom = "errorbar", 
               width = .1,
               position = position_dodge(dodge_value)) +
  facet_wrap(~ Pollutant) + theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  scale_fill_viridis_d(option = "E") + 
  #scale_y_continuous(name = "Measured Pollutant")+
  scale_y_log10(name="Pollutant (ppb)")

jpeg("/Users/priyanka/Downloads/Suncor/distribution_pollutant_Site.jpeg", width=9000, height=6000, res=600)
p1
dev.off()

#Meteorological
temp <- df %>% dplyr::select(Site,
  Benzene = Benzene_ppb, Toluene = Toluene_ppb,
  Trimethylbenzene = Trimethylbenzene_ppb, Xylene = Xylene_ppb,
  H2S = Hydrogen_Sulfide_ppb, HCN = Hydrogen_Cyanide_ppb,
  ws, wd, Temp = Temperature_F, Pressure = Pressure_mb,
  RH = Relative_Humidity_percent)   # by NAME (was positional c(1,7:15,20:21))
temp1<- data.table::melt(setDT(temp), id.vars = c("Site"), variable.name = "Pollutant")
temp1<-subset(temp1, temp1$Pollutant=="Temp" | temp1$Pollutant=="Pressure" | temp1$Pollutant=="RH" | temp1$Pollutant=="ws" | temp1$Pollutant=="wd")
temp1$Pollutant<-as.character(temp1$Pollutant)

temp1$Site<-factor(temp1$Site, levels=c("Suncor and Phillips 66 Terminal", "Holly Energy Partners (Sinclair) Terminal"))

dodge_value <- 0.9
p2<-temp1 %>% 
  drop_na(value) %>%
  ggplot(aes(y = value, x = Site, fill = Site)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width = 0.2, 
               fatten = NULL,
               position = position_dodge(dodge_value)) + 
  stat_summary(fun = "mean", 
               geom = "point",
               position = position_dodge(dodge_value)) +
  stat_summary(fun.data = "mean_cl_boot", 
               geom = "errorbar", 
               width = .1,
               position = position_dodge(dodge_value)) +
  facet_wrap(~ Pollutant) + theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  scale_fill_viridis_d(option = "E") + 
  #scale_y_continuous(name = "Measured Pollutant")+
  scale_y_log10(name="Meterological Variables")

jpeg("/Users/priyanka/Downloads/Suncor/distribution_meterological_Site.jpeg", width=9000, height=6000, res=600)
p2
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/distribution_Site_pollutant_meteorological.jpeg", width=9000, height=6000, res=600)
cowplot::plot_grid(p1, p2, ncol=1, labels=c("A)", "B)"))
dev.off()

#Correlations/Pairwise plot
# ============================================================
# Scatterplots + pairwise Pearson correlations (by Site)
# - ggpairs: lower = scatter, diag = density, upper = corr + stars
# - saves ONE PNG per Site
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(GGally)
})

vars <- c(
  "ws", "wd", "ws_mobile", "wd_mobile",
  "Relative_Humidity_percent", "Pressure_mb", "Temperature_F",
  "Benzene_ppb", "Toluene_ppb", "Trimethylbenzene_ppb", "Xylene_ppb",
  "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb"
)

out_dir <- "/Users/priyanka/Downloads/Suncor"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

safe_stub <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# --- rename mapping (ONLY for plotting)
nice_names <- c(
  Relative_Humidity_percent = "RH",
  Pressure_mb = "Pressure",
  Temperature_F = "Temperature",
  Benzene_ppb = "Benzene",
  Toluene_ppb = "Toluene",
  Trimethylbenzene_ppb = "Trimethylbenzene",
  Xylene_ppb = "Xylene",
  Hydrogen_Sulfide_ppb = "H2S",
  Hydrogen_Cyanide_ppb = "HCN"
)

# ---- correlation panel
cor_panel <- function(data, mapping, method = "pearson", digits = 3, ...) {
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)

  if (n < 3) {
    lab <- "r=NA\nn<3"
  } else {
    ct <- suppressWarnings(cor.test(x, y, method = method))
    r  <- unname(ct$estimate)
    p  <- ct$p.value
    stars <- ifelse(p < 0.001, "***",
                    ifelse(p < 0.01, "**",
                           ifelse(p < 0.05, "*", "")))
    lab <- paste0("r=", formatC(r, digits = digits, format = "f"),
                  stars, "\n(n=", n, ")")
  }

  ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = lab, size = 3.5) +
    theme_void()
}

scat_panel <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.15, size = 0.25) +
    theme_bw(base_size = 10)
}

diag_panel <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density(linewidth = 0.3, na.rm = TRUE) +
    theme_bw(base_size = 10)
}

for (s in sort(unique(df$Site))) {

  d0 <- df %>%
    filter(Site == s) %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

  # Rename columns for plotting only
  names(d0) <- dplyr::recode(names(d0), !!!nice_names)

  # ggplot2 >= 4.0: ggmatrix no longer accepts `+ ggtitle()`; pass title via
  # ggpairs() and make the cosmetic theme add non-fatal.
  p <- GGally::ggpairs(
    d0,
    title = paste0("Scatterplots + Pearson r: ", s),
    upper = list(continuous = GGally::wrap(cor_panel, method = "pearson")),
    lower = list(continuous = GGally::wrap(scat_panel)),
    diag  = list(continuous = GGally::wrap(diag_panel))
  )
  p <- tryCatch(
    p + theme(plot.title = element_text(face = "bold"),
              strip.text = element_text(size = 9),
              axis.text.x = element_text(angle = 45, hjust = 1)),
    error = function(e) p)

  ggsave(
    filename = file.path(out_dir, paste0("pairs_", safe_stub(s), ".png")),
    plot = p,
    width = 14, height = 14, dpi = 600, bg = "white"
  )
}

#Time Plot
df$year<-year(df$date)
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(openair)   # timePlot()
})
# ---- rename columns in df (in-place)
df <- df %>%
  dplyr::rename(
    Benzene           = Benzene_ppb,
    Toluene           = Toluene_ppb,
    Trimethylbenzene  = Trimethylbenzene_ppb,
    Xylene            = Xylene_ppb,
    H2S               = Hydrogen_Sulfide_ppb,
    HCN               = Hydrogen_Cyanide_ppb
  )
# ---- add year
df$year <- year(df$date)
# ---- timePlot for Suncor site
out_file <- "/Users/priyanka/Downloads/Suncor/TimePlot_Suncor.jpeg"
jpeg(out_file, width = 6000, height = 6000, res = 600, quality = 100)
timePlot(
  df[df$Site == "Suncor and Phillips 66 Terminal", ],
  pollutant  = c("H2S", "HCN", "Benzene", "Toluene", "Trimethylbenzene", "Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE
)
dev.off()

out_file <- "/Users/priyanka/Downloads/Suncor/TimePlot_Terminal.jpeg"
jpeg(out_file, width = 6000, height = 6000, res = 600, quality = 100)
timePlot(
  df[df$Site == "Holly Energy Partners (Sinclair) Terminal", ],
  pollutant  = c("H2S", "HCN", "Benzene", "Toluene", "Trimethylbenzene", "Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE
)
dev.off()

#Timeplots 2
suppressPackageStartupMessages({
  library(openair)
  library(lattice)
})

# Make sure Route exists and is correct
df$Route <- dplyr::recode(
  df$Site,
  "Suncor and Phillips 66 Terminal" = "Route 1",
  "Holly Energy Partners (Sinclair) Terminal" = "Route 2",
  .default = NA_character_
)

df2 <- df[!is.na(df$Route), ]

# Create two lattice objects
p1 <- timePlot(
  df2[df2$Route == "Route 1", ],
  pollutant  = c("H2S","HCN","Benzene","Toluene","Trimethylbenzene","Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE,
  main       = "Route 1"
)

p2 <- timePlot(
  df2[df2$Route == "Route 2", ],
  pollutant  = c("H2S","HCN","Benzene","Toluene","Trimethylbenzene","Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE,
  main       = "Route 2"
)

# Save stacked
jpeg("/Users/priyanka/Downloads/Suncor/TimePlot_BothRoutes_Stacked.jpeg",
     width = 8000, height = 10000, res = 600, quality = 100)

print(p1, split = c(1, 2, 1, 2), more = TRUE)   # top
print(p2, split = c(1, 1, 1, 2), more = FALSE)  # bottom

dev.off()

#Time Variation
suppressPackageStartupMessages({
  library(dplyr)
  library(openair)
})

# --- shorten site labels for plotting (do NOT overwrite original if you don't want to)
df_plot <- df %>%
  dplyr::mutate(
    Route = recode(
      Site,
      "Suncor and Phillips 66 Terminal" = "Route 1",
      "Holly Energy Partners (Sinclair) Terminal" = "Route 2",
      .default = Site
    )
  )
# ----------------------------
# 1) BTEX plot (both routes)
# ----------------------------
jpeg("/Users/priyanka/Downloads/Suncor/TimeVariation_BTEX_BothRoutes.jpeg",
     width = 8000, height = 6000, res = 600, quality = 100)
timeVariation(
  df_plot,
  pollutant = c("Benzene", "Toluene", "Trimethylbenzene", "Xylene"),
  type = "Route"
)
dev.off()

# ----------------------------
# 2) H2S + HCN plot (both routes)
# ----------------------------
jpeg("/Users/priyanka/Downloads/Suncor/TimeVariation_H2S_HCN_BothRoutes.jpeg",
     width = 8000, height = 4500, res = 600, quality = 100)
timeVariation(
  df_plot,
  pollutant = c("H2S", "HCN"),
  type = "Route"
)
dev.off()

#polarPlots
jpeg("/Users/priyanka/Downloads/Suncor/polarplot_benzene_Suncor.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Benzene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_toluene_Suncor.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Toluene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_trimethylbenzene_Suncor.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Trimethylbenzene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_xylene_Suncor.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Xylene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_h2s_Suncor.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="H2S")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_hcn_Suncor.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="HCN")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_benzene_Terminal.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Holly Energy Partners (Sinclair) Terminal",], pollutant="Benzene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_toluene_Terminal.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Holly Energy Partners (Sinclair) Terminal",], pollutant="Toluene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_trimethylbenzene_Terminal.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Holly Energy Partners (Sinclair) Terminal",], pollutant="Trimethylbenzene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_xylene_Terminal.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Holly Energy Partners (Sinclair) Terminal",], pollutant="Xylene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_h2s_Terminal.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Holly Energy Partners (Sinclair) Terminal",], pollutant="H2S")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/polarplot_hcn_Terminal.jpeg", width=6000, height=6000, res=600)
polarPlot(df[df$Site=="Holly Energy Partners (Sinclair) Terminal",], pollutant="HCN")
dev.off()

#Distance from Suncor
df$suncor_Long<-rep(-104.94847, nrow(df))
df$suncor_Lat<-rep(39.80456, nrow(df))

df<-df %>%
  dplyr::mutate(suncor_distance = pmap(list(a = Longitude, 
                              b = Latitude, 
                              x = suncor_Long,
                              y = suncor_Lat), 
                          ~ geosphere::distGeo( c(..1, ..2), c(..3, ..4))))
df$suncor_distance<-as.numeric(df$suncor_distance)

summary(lm(HCN ~ suncor_distance:H2S + H2S, df))

jpeg("/Users/priyanka/Downloads/Suncor/Scatterplot_HCN_H2S_distance.jpeg", width=10000, height=5000, res=600)
scatterPlot(df, x = "HCN", y = "H2S", z = "suncor_distance",  y.relation="free")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_HCN_distance.jpeg", width=10000, height=5000, res=600)
trendLevel(df, "HCN", x = "month", y = "suncor_distance")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_Suncor_HCN.jpeg", width=10000, height=5000, res=600)
trendLevel(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="HCN")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_Suncor_H2S.jpeg", width=10000, height=5000, res=600)
trendLevel(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="H2S")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_Suncor_Benzene.jpeg", width=10000, height=5000, res=600)
trendLevel(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Benzene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_Suncor_Toluene.jpeg", width=10000, height=5000, res=600)
trendLevel(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Toluene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_Suncor_Trimethylbenzene.jpeg", width=10000, height=5000, res=600)
trendLevel(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Trimethylbenzene")
dev.off()

jpeg("/Users/priyanka/Downloads/Suncor/Trendlevel_Suncor_Xylene.jpeg", width=10000, height=5000, res=600)
trendLevel(df[df$Site=="Suncor and Phillips 66 Terminal",], pollutant="Xylene")
dev.off()

temp<-df[df$Site=="Suncor and Phillips 66 Terminal",]
# by NAME (was positional c(7:15) then [,-1] — column order changed after the
# in-place rename above, so positions grabbed non-numeric columns)
temp <- temp %>% dplyr::select(dplyr::any_of(c(
  "Benzene","Toluene","Trimethylbenzene","Xylene","H2S","HCN",
  "ws","wd","Temperature_F","Pressure_mb","Relative_Humidity_percent")))
temp <- as.data.frame(temp)   # GUARD: df comes from mobile_wswd.RData where `out` is a
                              # data.table (script 06 join). On a data.table, the next line's
                              # logical vector would be evaluated in `j` and RETURN THE VECTOR
                              # instead of subsetting columns, so cor() then receives a vector
                              # and fails with "supply both 'x' and 'y' or a matrix-like 'x'".
temp <- temp[, sapply(temp, is.numeric), drop = FALSE]
stopifnot(is.data.frame(temp), ncol(temp) >= 2, nrow(temp) > 0)
jpeg("/Users/priyanka/Downloads/Suncor/Corrplot_Suncor.jpeg", width=5000, height=5000, res=600)
ggcorrplot::ggcorrplot(as.matrix(cor(temp, use="pairwise.complete.obs")),  type = "lower",
   lab = TRUE)
dev.off()

temp<-df[df$Site=="Holly Energy Partners (Sinclair) Terminal",]
# by NAME (was positional c(7:15) then [,-1] — column order changed after the
# in-place rename above, so positions grabbed non-numeric columns)
temp <- temp %>% dplyr::select(dplyr::any_of(c(
  "Benzene","Toluene","Trimethylbenzene","Xylene","H2S","HCN",
  "ws","wd","Temperature_F","Pressure_mb","Relative_Humidity_percent")))
temp <- as.data.frame(temp)   # GUARD: df comes from mobile_wswd.RData where `out` is a
                              # data.table (script 06 join). On a data.table, the next line's
                              # logical vector would be evaluated in `j` and RETURN THE VECTOR
                              # instead of subsetting columns, so cor() then receives a vector
                              # and fails with "supply both 'x' and 'y' or a matrix-like 'x'".
temp <- temp[, sapply(temp, is.numeric), drop = FALSE]
stopifnot(is.data.frame(temp), ncol(temp) >= 2, nrow(temp) > 0)
jpeg("/Users/priyanka/Downloads/Suncor/Corrplot_Terminal.jpeg", width=5000, height=5000, res=600)
ggcorrplot::ggcorrplot(as.matrix(cor(temp, use="pairwise.complete.obs")),  type = "lower",
   lab = TRUE)
dev.off()
