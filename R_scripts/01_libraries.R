# ==============================================================
# 01  Libraries
# Auto-split from Suncor.Rmd  (section 1 of 40)
# ==============================================================

# --- Original Rmd preamble (YAML front matter + knitr setup) ---
# ---
# title: "Suncor and Phillips 66 Terminal"
# output: html_document
# date: "2023-12-20"
# ---

# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```


#Libraries

require(geosphere)
library(viridis) # nice color palette
library(ggplot2) # plotting
library(ggmap) # ggplot functionality for maps
library(dplyr) # use for fixing up data
library(readr) # reading in data/csv
library(RColorBrewer) # for color palettes
library(purrr) # for mapping over a function
library(magick)
library(geoR)
require(lubridate)
require(nmea)
require(dplyr)
require(maptools)
require(openair)
require(sf)
require(grid)
require(ggstatsplot)
require(patchwork)
require(GGally)
require(openairmaps)
require(leaflet)
require(htmlwidgets)
library(RAQSAPI)
library("keyring")
require(naniar)
require(ggridges)
require(hclust)
require(dplyr)
require(geosphere)
require(sf)
require(sp)
require(rgdal)
#require(rgeos)
require(corrplot)
require(ggcorrplot)
require(splitr)
require(jpeg)
library(geosphere)
library(tidyverse)
require(DescTools)
require(ggsci)
require(sp)
require(rgdal)
require(jpeg)
require(pliman)
require(EBImage)
require(grid)
require(gridExtra)
library(ggpubr)
require(openair)
require(tigris)
require(tidyverse)
require(rjson)
require(RCurl)
require(tigris)
require(acs)
require(choroplethr)
require(choroplethrMaps)
require(tidycensus)
require(zipcode)
require(dplyr)
require(RCurl)
require(jsonlite)
require(FNN)
require(sp)
require(rgdal)
require(readr)
require(ropenaq)
require(arsenal)
require(data.table)
require(revgeo)
require(readr)
require(rgeos)
require(geosphere)
require(sp)
require(maps)
require(maptools)
require(tidycensus)
require(purrr)
require(tidyr)
require(reshape2)
library(tidyverse)
library(ggmap)
library(DT)
require(stargazer)
library(knitr)
require(stringr)
require(stringi)
require(fpc)
require(sf)
require(raster)
require(rgdal)
require(RODBC)
require(schoolmath)
require(MKinfer)
require(lme4)
require(stargazer)
require(sandwich)
require(multiwayvcov)
require(lmtest)
require(spatialreg)
require(spdep)
require(raster)
require(exactextractr)
require(npordtests)
require(PMCMRplus)
require(dunn.test)
require(data.table)
require(dplyr)
require(tidyr)
require(qdap)
require(ggplot2)
require(tm)
require(topicmodels)
require(textmineR)
require(tidytext)
require(lubridate)
require(stringi)
require(plyr)
require(openair)
require(lubridate)
require(tidyverse)
require(stringr)
require(SnowballC)
require(textclean)
require(rainette)
library(tidytext)
library(textmineR)
library(dplyr)
library(tidyr)
library(tm)
library(topicmodels)
library(ggplot2)
require(quanteda)
require(rainette)
require(stm)
require(igraph)
require(dplyr)
require(tidyr)
require(qdap)
require(ggplot2)
require(tm)
require(huge)
require(data.table)
require(lubridate)
require(openair)
require(topicmodels)
require(textmineR)
require(tidytext)
require(lubridate)
require(plyrs)
require(stringi)
require(stringr)
require(SnowballC)
require(textclean)
require(rainette)
require(data.table)
require(DataCombine)
require(stm)
library(ggthemes)
require(purrr)
require(reshape2)
require(ggraph)
require(ggstar)
require(ggrepel)
require(weathermetrics)
require(ranger)
require(scales)
require(data.table)
require(openair)
require(corrplot)
require(hrbrthemes)
require(beepr)
require(purrr)
require(boot)
require(broom)
require(mapview)
require(geosphere)
require(lubridate)
require(tidyverse)
require(openair)
require(data.table)
require(lubridate)
require(geosphere)
require(e1071)
require(rgeos)
require(bbmle)
require(minpack.lm)
require(data.table)
require(openair)
require(splines)
require(zoo)
require(dplyr)
require(tidyr)
require(highfrequency)
require(geosphere)
require(osmdata)
require(tidyverse)
require(sf)
require(ggmap)
require(maptools)
require(nngeo)
require(openair)
require(mgcv)
require(cowplot)
require(egg)
require(rlist)
require(caret)
require(ranger)
require(Metrics)
require(corrplot)
require(exactextractr)
require(raster)
require(openair)
require(ggplot2)
require(cowplot)
require(jpeg)
require(png)
require(caret)
require(gbm)

trajectory_plot_new <- function(x,
                            show_hourly = TRUE,
                            color_scheme = "cycle_hues") {
  
  if (inherits(x, "trajectory_model")) {
    if (!is.null(x$traj_df)) {
      traj_df <- x$traj_df
    } else {
      stop("There is no data available for plotting.")
    }
  }
  
  if (inherits(x, "data.frame")) {
    if (all(c("run", "receptor", "hour_along", "traj_dt",
              "lat", "lon", "height", "traj_dt_i") %in% colnames(x))) {
      traj_df <- x
    } else {
      stop("This tibble does not contain plottable trajectory data.")
    }
  }
  
  dt_runs <- traj_df$traj_dt_i %>% unique() %>% length()
  
  if (color_scheme == "cycle_hues") {
    colors <- scales::hue_pal(c = 90, l = 70)(dt_runs)
  } else if (color_scheme == "increasingly_gray") {
    colors <- scales::grey_pal(0.7, 0.1)(dt_runs)
  }
  
  # Correct longitude values near prime meridian
  traj_df$lon[which(traj_df$lon > 0)] <- 
    traj_df$lon[which(traj_df$lon > 0)] - (180*2)
  
  receptors <-
    traj_df %>%
    dplyr::pull(receptor) %>%
    unique()
  
  dates <-
    traj_df %>%
    dplyr::pull(traj_dt_i) %>%
    unique()
  
  traj_plot <- 
    leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      provider = "OpenStreetMap",
      group = "OpenStreetMap"
    ) %>%
    leaflet::addProviderTiles(
      provider = "CartoDB.DarkMatter",
      group = "CartoDB Dark Matter"
    ) %>%
    leaflet::addProviderTiles(
      provider = "CartoDB.Positron",
      group = "CartoDB Positron"
    ) %>%
    leaflet::addProviderTiles(
      provider = "Esri.WorldTerrain",
      group = "ESRI World Terrain"
    ) %>%
    
    leaflet::fitBounds(
      lng1 = min(traj_df[["lon"]]),
      lat1 = min(traj_df[["lat"]]),
      lng2 = max(traj_df[["lon"]]),
      lat2 = max(traj_df[["lat"]])
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c(
        "CartoDB Positron", "CartoDB Dark Matter",
       "ESRI World Terrain"
      ),
      overlayGroups = c("trajectory_points", "trajectory_paths"),
      position = "topright"
    )
  
  # Get different trajectories by receptor and by date
  for (i in seq_along(receptors)) {
    
    receptor_i <- receptors[i]
    
    for (j in seq_along(dates)) {
      
      date_i <- dates[j]
      
      wind_traj_ij <-
        traj_df %>%
        dplyr::filter(
          receptor == receptor_i,
          traj_dt_i == date_i
        )
      
      popup <- 
        paste0(
          "<strong>trajectory</strong> ", wind_traj_ij[["traj_dt_i"]],
          "<br><strong>at time</strong> ", wind_traj_ij[["traj_dt"]],
          " (", wind_traj_ij[["hour_along"]],
          " h)<br><strong>height</strong> ", wind_traj_ij[["height"]],
          " <font size=\"1\">m AGL</font> / ",
          "<strong>P</strong> ", wind_traj_ij[["pressure"]],
          " <font size=\"1\">hPa</font>"
        )
      
      traj_plot <-
        traj_plot %>%
        leaflet::addPolylines(
          lng = wind_traj_ij[["lon"]],
          lat = wind_traj_ij[["lat"]],
          group = "trajectory_paths",
          weight = 2,
          smoothFactor = 1,
          color = colors[j]
        ) %>%
        leaflet::addCircles(
          lng = wind_traj_ij[["lon"]],
          lat = wind_traj_ij[["lat"]],
          group = "trajectory_points",
          radius = 250,
          fill = TRUE,
          color = colors[j],
          fillColor = colors[j], 
          popup = popup
        )
    }
  }
  
  traj_plot
}
