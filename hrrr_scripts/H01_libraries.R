# ==============================================================
# H01  Libraries
# Auto-split from Suncor_plume2.Rmd  (section 1 of 4)
# ==============================================================

# --- Original Rmd preamble ---
# ---
# title: "Suncor_plume2"
# output: html_document
# date: "2026-02-28"
# ---

# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```


#Libraries

library(data.table)   # like pandas
library(dplyr)        # tidy manipulation
library(lubridate)    # datetime handling
library(ncdf4)        # for NetCDF files
library(raster)       # for grids
library(geosphere)    # haversine distances
library(tidyr)        # reshaping (like pd.melt)
library(ggplot2) 
require(reticulate)
#Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python3.10")
