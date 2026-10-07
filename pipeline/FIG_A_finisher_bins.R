# ==============================================================
# FIG_A_finisher_bins.R  (2026-09-30)
# Quick finisher for figure group A after the one-value-per-bin change.
# Runs the first part of script 07 verbatim, i.e. only:
#   missingness_by_site.png       Figure S3.2  (delivered values: coverage)
#   pairs_<Site>.png              Figures S3.3-S3.4 (1-s rows, repeated bin means)
#   TimePlot_Suncor/Terminal.jpeg Figures S3.5-S3.6 (bin values, drawn as bars)
#   TimePlot_BothRoutes_Stacked.jpeg
# The time-variation, trend-level and polar figures of 07 already use the bin
# values and are not redrawn. Needs mobile_wswd.RData with *_rep (R02 after
# the 2026-09-30 fix to 06).
#   Rscript rerun_pipeline/FIG_A_finisher_bins.R
# ==============================================================

#TimePlot + Timevariation + GGally correlation plots 

SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
library(dplyr)
library(visdat)
library(ggplot2)
library(naniar)
library(tidyr)
library(forcats)
require(ggridges)
library(openair)

load(file.path(SUNCOR_BASE, "mobile_wswd.RData"))
df<-out
rm(out)

# ONE VALUE PER BIN (2026-09-30). From 03 on, Hydrogen_Sulfide_ppb and
# Hydrogen_Cyanide_ppb hold one value per 5-s / 2-s bin (on the bin's middle
# second). Two figures here need other series:
#  * the missingness plot (Figure S3.2) describes data COVERAGE, so it uses
#    the delivered values (*_raw), which are present on every delivered second;
#  * the ggpairs scatter/correlation figures (S3.3-S3.4) pair species row by
#    row, so they use the bin means repeated on every delivered second (*_rep).
df_cov  <- df
df_corr <- df
if (all(c("Hydrogen_Sulfide_ppb_raw", "Hydrogen_Cyanide_ppb_raw") %in% names(df))) {
  df_cov$Hydrogen_Sulfide_ppb <- df$Hydrogen_Sulfide_ppb_raw
  df_cov$Hydrogen_Cyanide_ppb <- df$Hydrogen_Cyanide_ppb_raw
}
if (all(c("Hydrogen_Sulfide_ppb_rep", "Hydrogen_Cyanide_ppb_rep") %in% names(df))) {
  df_corr$Hydrogen_Sulfide_ppb <- df$Hydrogen_Sulfide_ppb_rep
  df_corr$Hydrogen_Cyanide_ppb <- df$Hydrogen_Cyanide_ppb_rep
} else warning("mobile_wswd.RData has no *_rep columns: the pairs figures fall back to one value per bin (re-run R02)")

wind<-subset(df, select=c(Lat_wind, Lon_wind))
wind<-wind[!duplicated(wind),]
write.csv(wind, file=file.path(SUNCOR_BASE, "wind_sites.csv"))
rm(wind)
# --- vars you want in the missingness plot
vars_miss <- c(
  "ws", "wd", "ws_mobile", "wd_mobile",
  "Relative_Humidity_percent", "Pressure_mb", "Temperature_F",
  "Benzene_ppb", "Toluene_ppb", "Trimethylbenzene_ppb", "Xylene_ppb",
  "Hydrogen_Sulfide_ppb", "Hydrogen_Cyanide_ppb"
)

# keep only needed columns + make a row index within Site for plotting
miss_long <- df_cov %>%
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
out_dir <- SUNCOR_BASE
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
temp1<- data.table::melt(data.table::setDT(temp), id.vars = c("Site"), variable.name = "Pollutant")
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

jpeg(file.path(SUNCOR_BASE, "distribution_pollutant_Site.jpeg"), width=9000, height=6000, res=600)
p1
dev.off()

#Meteorological
temp <- df %>% dplyr::select(Site,
  Benzene = Benzene_ppb, Toluene = Toluene_ppb,
  Trimethylbenzene = Trimethylbenzene_ppb, Xylene = Xylene_ppb,
  H2S = Hydrogen_Sulfide_ppb, HCN = Hydrogen_Cyanide_ppb,
  ws, wd, Temp = Temperature_F, Pressure = Pressure_mb,
  RH = Relative_Humidity_percent)   # by NAME (was positional c(1,7:15,20:21))
temp1<- data.table::melt(data.table::setDT(temp), id.vars = c("Site"), variable.name = "Pollutant")
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

jpeg(file.path(SUNCOR_BASE, "distribution_meterological_Site.jpeg"), width=9000, height=6000, res=600)
p2
dev.off()

jpeg(file.path(SUNCOR_BASE, "distribution_Site_pollutant_meteorological.jpeg"), width=9000, height=6000, res=600)
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

out_dir <- SUNCOR_BASE
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

for (s in sort(unique(df_corr$Site))) {

  d0 <- df_corr %>%   # 1-s rows with repeated bin means (see ONE VALUE PER BIN above)
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
    columnLabels = sub("^H2S$", "H\u2082S", names(d0)),   # strip labels only
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

rm(df_cov, df_corr); invisible(gc())

#Time Plot
df$year<- lubridate::year(df$date)
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
df$year <- lubridate::year(df$date)
# ---- timePlot for Suncor site
out_file <- file.path(SUNCOR_BASE, "TimePlot_Suncor.jpeg")
jpeg(out_file, width = 6000, height = 6000, res = 600, quality = 100)
timePlot(
  df[df$Site == "Suncor and Phillips 66 Terminal", ],
  pollutant  = c("H2S", "HCN", "Benzene", "Toluene", "Trimethylbenzene", "Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE,
  plot.type  = "h"   # 2026-09-30: vertical bars; H2S/HCN hold one value per bin, and
                     # line segments are not drawn across the NA seconds between bins
)
dev.off()

out_file <- file.path(SUNCOR_BASE, "TimePlot_Terminal.jpeg")
jpeg(out_file, width = 6000, height = 6000, res = 600, quality = 100)
timePlot(
  df[df$Site == "Holly Energy Partners (Sinclair) Terminal", ],
  pollutant  = c("H2S", "HCN", "Benzene", "Toluene", "Trimethylbenzene", "Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE,
  plot.type  = "h"   # 2026-09-30: vertical bars; H2S/HCN hold one value per bin, and
                     # line segments are not drawn across the NA seconds between bins
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
  plot.type  = "h",
  main       = "Route 1"
)

p2 <- timePlot(
  df2[df2$Route == "Route 2", ],
  pollutant  = c("H2S","HCN","Benzene","Toluene","Trimethylbenzene","Xylene"),
  date.pad   = TRUE,
  y.relation = "free",
  key        = FALSE,
  plot.type  = "h",
  main       = "Route 2"
)

# Save stacked
jpeg(file.path(SUNCOR_BASE, "TimePlot_BothRoutes_Stacked.jpeg"),
     width = 8000, height = 10000, res = 600, quality = 100)

print(p1, split = c(1, 2, 1, 2), more = TRUE)   # top
print(p2, split = c(1, 1, 1, 2), more = FALSE)  # bottom

dev.off()


message("FIG_A_finisher_bins: wrote missingness, pairs and time plots")
