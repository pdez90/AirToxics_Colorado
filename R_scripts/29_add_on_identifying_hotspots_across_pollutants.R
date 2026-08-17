# ==============================================================
# 29  Add on: identifying hotspots across pollutants
# Auto-split from Suncor.Rmd  (section 29 of 40)
# ==============================================================

#Add on: identifying hotspots across pollutants

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

in_dir  <- "/Users/priyanka/Downloads/Suncor"
out_dir <- "/Users/priyanka/Downloads/Suncor"

match_dist_m <- 100
poll_names <- c("benzene","toluene","trimethylbenzene","xylene","h2s","hcn")

# map internal pollutant name -> filename stem used on disk
file_stem <- c(
  benzene = "benzene",
  toluene = "toluene",
  trimethylbenzene = "trimethylbenzene",
  xylene  = "xylene",
  h2s     = "hydrogen_sulfide",
  hcn     = "hydrogen_cyanide"
)

read_cent <- function(pol, which = c("all","persistent")) {
  which <- match.arg(which)

  stem <- unname(file_stem[pol])
  if (is.na(stem)) stop("No file_stem mapping for pollutant: ", pol)

  f <- file.path(in_dir, paste0("cent_out_", stem, "_", which, ".csv"))
  if (!file.exists(f)) {
    message("Missing file: ", f)
    return(NULL)
  }

  x <- read.csv(f, stringsAsFactors = FALSE)

  need <- c("Longitude","Latitude","n","n_days")
  if (!all(need %in% names(x))) {
    message("Bad columns in ", f, " | missing: ", paste(setdiff(need, names(x)), collapse=", "))
    return(NULL)
  }

  st_as_sf(
    x %>% mutate(pollutant = pol),
    coords = c("Longitude","Latitude"),
    crs = 4326,
    remove = FALSE
  )
}

build_hotspot_sf <- function(which = c("all","persistent")) {
  which <- match.arg(which)

  message("---- Building hotspot sf: ", which, " ----")
  lst <- lapply(poll_names, read_cent, which = which)
  ok  <- !vapply(lst, is.null, logical(1))
  message("Loaded sf tables: ", sum(ok), "/", length(lst))

  lst <- lst[ok]
  hs <- do.call(rbind, lst)

  message("Total points loaded: ", nrow(hs))
  message("By pollutant:\n")
  print(table(hs$pollutant))

  hs
}

# Example:
hs_all <- build_hotspot_sf("all")
hs_persist <- build_hotspot_sf("persistent")
