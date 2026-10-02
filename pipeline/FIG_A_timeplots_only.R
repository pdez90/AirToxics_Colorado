# FIG_A_timeplots_only.R (2026-10-01): the time-plot part of FIG_A_finisher_bins.R only
# (Figures S3.5-S3.6 and TimePlot_BothRoutes_Stacked), for when the pairs figures are already current.
#   Rscript rerun_pipeline/FIG_A_timeplots_only.R

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
