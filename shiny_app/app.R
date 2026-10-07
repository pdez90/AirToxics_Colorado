# ==============================================================
# CDPHE Mobile Air Toxics Explorer — Shiny app
# Pages: 1 Raw data | 2 AirToxScreen vs Mobile | 3 Plumes |
#        4 Hotspots | 5 Source probability | 6 Study context |
#        7 Health screening | 8 Contact
# Run prep_app_data.R first, then:  shiny::runApp("shiny_app")
# ==============================================================

library(shiny)
library(leaflet)
library(data.table)
library(sf)
library(ggplot2)
library(DT)

DATA <- "data"
cells   <- readRDS(file.path(DATA, "cells_summary.rds"))
summ    <- readRDS(file.path(DATA, "summary_stats.rds"))
events  <- readRDS(file.path(DATA, "events.rds"))
blocks  <- readRDS(file.path(DATA, "blocks.rds"))
plumes  <- readRDS(file.path(DATA, "plumes.rds"))
hs      <- readRDS(file.path(DATA, "hotspots.rds"))
# Population-weighted means quoted on page 2. Computed from the block table
# rather than typed in, so they follow the data through a re-run instead of
# going stale (they were still the pre-exclusion 0.149 / 0.161 before this).
.b_      <- sf::st_drop_geometry(blocks)
.k_      <- is.finite(.b_$sBenzene_med_of_daily_med_scaled) &
            is.finite(.b_$benzene_ppb_airtox) & is.finite(.b_$Population_airtox)
.pw_mob  <- stats::weighted.mean(.b_$sBenzene_med_of_daily_med_scaled[.k_],
                                 .b_$Population_airtox[.k_])
.pw_ats  <- stats::weighted.mean(.b_$benzene_ppb_airtox[.k_],
                                 .b_$Population_airtox[.k_])
rm(.b_, .k_)
ctx     <- readRDS(file.path(DATA, "context.rds"))
camp    <- if (file.exists(file.path(DATA, "campaign.rds")))
             readRDS(file.path(DATA, "campaign.rds")) else NULL
tracks  <- if (file.exists(file.path(DATA, "daily_tracks.rds")))
             readRDS(file.path(DATA, "daily_tracks.rds")) else NULL
haz     <- if (file.exists(file.path(DATA, "hazard.rds")))
             readRDS(file.path(DATA, "hazard.rds")) else NULL
udays   <- if (!is.null(tracks)) sort(unique(tracks$day)) else NULL

# ---- S7.4 temporal-scaling scenarios --------------------------------------
# Mobile sampling is weekday-daytime, so a campaign mean is not a 24-h mean.
# The La Casa stationary monitor measures that gap directly - but only for
# benzene, toluene and the C8 aromatics. 1,2,4-TMB, H2S and HCN have no La
# Casa channel, and H2S and HCN are exactly the two species that drive every
# number on page 7. The toggle exposes that, rather than leaving the reader to
# find it in the SI. Labels here; every value comes from hazard.rds.
SCEN_LAB <- c(
  A_none      = "A - no scaling (the basis used in the paper)",
  B_aromatics = "B - measured aromatics scaled; H2S and HCN unscaled",
  C_borrowed  = "C - B, plus H2S/HCN at the mean aromatic factor",
  D_upper     = "D - B, plus H2S/HCN at the largest aromatic factor")
SCEN_CHOICES <- if (!is.null(haz) && !is.null(haz$scen)) {
  .u <- unique(as.character(haz$scen$scenario))
  stats::setNames(.u, ifelse(.u %in% names(SCEN_LAB), SCEN_LAB[.u], .u))
} else stats::setNames("A_none", SCEN_LAB[["A_none"]])

# The measured-factor range and the break-even factors are quoted in the
# sidebar text. Both are read from the written SI tables at load, so the
# sentence follows a re-run instead of going stale.
.meas <- if (!is.null(haz) && !is.null(haz$scen_poll))
           haz$scen_poll[grepl("^La Casa", factor_source), unique(factor)] else numeric(0)
.be   <- if (!is.null(haz) && !is.null(haz$breakeven)) haz$breakeven else NULL
.bget <- function(org, col) {
  if (is.null(.be) || !org %in% .be$organ) return(NA_real_)
  as.numeric(.be[[col]][match(org, .be$organ)])
}
SCALE_WINDOW_TXT <- if (length(.meas) && !is.null(.be)) sprintf(paste0(
  "How wrong would a borrowed factor have to be to matter? H2S would need a ",
  "24-h factor of %.2f before the community-average respiratory hazard index ",
  "reached 1, and HCN a factor below %.2f before the endocrine index fell to ",
  "1. The measured aromatic factors span %.2f to %.2f, comfortably inside ",
  "that window - which is why no conclusion on this page moves between ",
  "scenarios A and D."),
  .bget("Respiratory", "f_breakeven_pwmean"), .bget("Endocrine", "f_breakeven_pwmean"),
  min(.meas), max(.meas)) else NULL

POLLS <- sort(unique(cells$pollutant))
unit_of <- function(p) if (p == "Methane") "ppm" else "ppb"
WWTP_LL <- c(39.81000447, -104.95562510)

# ---- shared overlay helper --------------------------------------
# Context features are drawn as STARS (SVG icons) so they are visually
# distinct from data markers (circles) on every page.
star_uri <- function(fill, stroke = "black") {
  svg <- sprintf(paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'>",
    "<path d='M12 1.8l3.1 6.5 7.1.8-5.3 4.9 1.4 7-6.3-3.5-6.3 3.5 1.4-7L1.8 9.1l7.1-.8z'",
    " fill='%s' stroke='%s' stroke-width='1.3'/></svg>"), fill, stroke)
  paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(svg)))
}
star_icon <- function(fill, size = 20, stroke = "black")
  makeIcon(iconUrl = star_uri(fill, stroke), iconWidth = size, iconHeight = size,
           iconAnchorX = size / 2, iconAnchorY = size / 2)
add_context <- function(map, layers) {
  k <- ctx$key
  if ("Covered facilities" %in% layers)
    map <- addMarkers(map, data = k[k$type == "Covered facility (HB21-1189)", ],
      ~lon, ~lat, icon = star_icon("red", 24), label = ~name,
      group = "Covered facilities")
  if ("Wastewater treatment" %in% layers)
    map <- addMarkers(map, data = k[k$type == "Wastewater treatment", ],
      ~lon, ~lat, icon = star_icon("green", 24), label = ~name, group = "WWTFs")
  if ("Woodshop" %in% layers)
    map <- addMarkers(map, data = k[k$type == "Woodshop", ],
      ~lon, ~lat, icon = star_icon("purple", 21), label = ~name, group = "Woodshop")
  if ("Refueling stations" %in% layers)
    map <- addMarkers(map, data = k[k$type == "Refueling station", ],
      ~lon, ~lat, icon = star_icon("dodgerblue", 21), label = ~name,
      group = "Refueling")
  if ("TRI facilities" %in% layers)
    map <- addMarkers(map, data = ctx$tri, ~lon, ~lat,
      icon = star_icon("white", 12, stroke = "grey40"), label = ~name,
      group = "TRI")
  if ("Wind sites" %in% layers)
    map <- addMarkers(map, data = ctx$wind, ~lon, ~lat,
      icon = star_icon("orange", 19), label = "EPA AQS wind site",
      group = "Wind sites")
  if ("La Casa" %in% layers)
    map <- addMarkers(map, data = ctx$lacasa, ~lon, ~lat,
      icon = star_icon("gold", 24), label = ~name, group = "La Casa")
  sel <- intersect(names(CTX_COLS), layers)
  if (length(sel)) {
    leg <- paste0(
      "<div style='background:rgba(255,255,255,0.92);padding:6px 10px;",
      "border-radius:5px;box-shadow:0 1px 4px rgba(0,0,0,0.3);",
      "line-height:1.6;font-size:12px'><b>Context (&#9733;)</b><br>",
      paste(sprintf(
        "<span style='color:%s;text-shadow:0 0 1.5px black;font-size:15px'>&#9733;</span> %s",
        CTX_COLS[sel], sel), collapse = "<br>"), "</div>")
    map <- addControl(map, html = leg, position = "bottomleft")
  }
  map
}
CTX_CHOICES <- c("Covered facilities", "Wastewater treatment", "Woodshop",
                 "Refueling stations", "TRI facilities", "Wind sites", "La Casa")
CTX_COLS <- c("Covered facilities" = "red", "Wastewater treatment" = "green",
              "Woodshop" = "purple", "Refueling stations" = "dodgerblue",
              "TRI facilities" = "white", "Wind sites" = "orange",
              "La Casa" = "gold")
# CARTO began watermarking its keyless raster basemap tiles with "API KEY
# REQUIRED", which printed diagonally across every map in the deployed app.
# Esri's light grey canvas needs no key, is equally recessive under a
# sequential ramp, and keeps the data the darkest thing on the page. The
# fallback keeps the app running on any leaflet build that lacks the entry.
BASE_PROVIDER <- if ("Esri.WorldGrayCanvas" %in% names(providers))
                   "Esri.WorldGrayCanvas" else "CartoDB.Positron"
base_map <- function() leaflet() |> addProviderTiles(BASE_PROVIDER) |>
  setView(-104.95, 39.82, zoom = 11)

# ================= UI =================
ui <- navbarPage(
  "CDPHE Mobile Air Toxics Explorer (North Denver / Commerce City, 2023-2025)",
  collapsible = TRUE,

  tabPanel("1. Raw data",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("p1_poll", "Pollutant", POLLS, selected = "Benzene"),
        radioButtons("p1_stat", "Cell statistic",
                     c("Median" = "median", "95th percentile" = "p95",
                       "Maximum" = "max", "Number of measurements" = "n")),
        checkboxGroupInput("p1_ctx", "Show context layers", CTX_CHOICES,
                           selected = c("Covered facilities", "Wastewater treatment",
                                        "Refueling stations")),
        h4("Play through sampling days"),
        checkboxInput("p1_daily", "Show one day at a time", FALSE),
        conditionalPanel("input.p1_daily",
          uiOutput("p1_day_ui"),
          textOutput("p1_day_info"),
          helpText("Press the play button under the slider to animate. Blue ",
                   "= Suncor & Phillips 66 route, dark red = Sinclair ",
                   "Terminal route (points thinned for display).")),
        h4("Campaign summary"), tableOutput("p1_summary"),
        h4("Sampling coverage"), htmlOutput("p1_coverage"),
        h4("Platform and instruments"),
        helpText("Measurements were made by two CDPHE mobile laboratories: ",
                 "the Community Air Toxics (CAT) lab and its duplicate, the ",
                 "Emissions Monitoring Utility (EMU), in service from ",
                 "December 2023 - both Mercedes Sprinter vans operated by ",
                 "CDPHE's Air Toxics and Ozone Precursors (ATOPs) program. ",
                 "Each carries a Vocus Eiger PTR-ToF-MS (benzene, toluene, ",
                 "xylene, trimethylbenzene), a Vocus CI-ToF-MS (HCN), and a ",
                 "Picarro G2204 cavity ring-down spectrometer (H2S and ",
                 "methane). The Eiger acquires once per second; the CI-ToF-MS ",
                 "acquires HCN every 2 s and the Picarro H2S and methane ",
                 "every 5 s."),
        helpText("Because sampled air travels through ~3 m of inlet tubing ",
                 "and instrument response times differ, each measurement was ",
                 "shifted back in time by an instrument- and vehicle-specific ",
                 "inlet delay measured by CDPHE (CAT: 4 s aromatics, 6 s HCN, ",
                 "21 s H2S/CH4; EMU: 5 s aromatics, 3 s HCN, 17 s H2S/CH4), ",
                 "so every value aligns with the GPS position where the ",
                 "sampled air entered the inlet. All data shown are ",
                 "delay-corrected."),
        helpText("CDPHE delivers every channel on a common one-second grid, ",
                 "carrying the most recent reading forward between the ",
                 "acquisitions of the two slower instruments. To avoid ",
                 "treating those repeats as independent measurements, HCN, ",
                 "H2S and methane are averaged to their native acquisition ",
                 "cadence after the delay correction; the aromatics are kept ",
                 "at 1 Hz. Plume detection (page 3) is the one exception and ",
                 "uses the as-delivered H2S signal, because the inversion ",
                 "depends on sub-five-second peak shape."),
        helpText("Measurements taken within 300 m of CDPHE's ATOPs ",
                 "headquarters in Wheat Ridge are excluded throughout. The ",
                 "vehicles are garaged there and run start-up and shut-down ",
                 "procedures, including calibrations, before and after each ",
                 "deployment, so those readings describe warehouse air rather ",
                 "than ambient conditions. The screen uses the delay-corrected ",
                 "position and removes about 1.8% of the record; one ",
                 "multi-pollutant hotspot group reported in earlier versions ",
                 "of this analysis sat on that address and does not survive ",
                 "it."),
        tags$p(tags$a(href = "https://cdphe.colorado.gov/apcd/monitoring",
                      target = "_blank",
                      "More on CDPHE air quality monitoring")),
        helpText("Cells are the 500 m analysis grid; values summarize every ",
                 "measurement in each cell at the native cadence described ",
                 "above. % below MDL is based on CDPHE instrument quality ",
                 "flags.")),
      mainPanel(width = 9, leafletOutput("p1_map", height = 640)))),

  tabPanel("2. AirToxScreen vs Mobile",
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons("p2_layer", "Map layer",
                     c("AirToxScreen benzene" = "ats",
                       "Mobile benzene (scaled)" = "mob",
                       "Ratio mobile / AirToxScreen" = "ratio")),
        h4(paste0("Across ", format(nrow(blocks), big.mark = ","),
                  " common blocks")), tableOutput("p2_stats"),
        h4("How the mobile surface was built"),
        helpText("Every 1-s benzene measurement is assigned to its census ",
                 "block. Within a block, each sampling day is summarized by ",
                 "its median, and the block estimate is the median of those ",
                 "daily medians - a metric robust to brief plume spikes. A ",
                 "rolling-window background computed from the mobile data ",
                 "itself separates the regional background from local ",
                 "enhancements. Because driving occurred mainly on weekday ",
                 "daytimes, block values are scaled to 24-h-equivalent ",
                 "concentrations using the diurnal pattern measured at the ",
                 "La Casa stationary monitoring site. EPA AirToxScreen ",
                 "values are modeled annual-average ambient benzene for the ",
                 "same blocks; the comparison uses only the ",
                 format(nrow(blocks), big.mark = ","), " blocks ",
                 "covered by both datasets."),
        h4("What the comparison shows"),
        helpText("The two datasets agree closely in aggregate - ",
                 sprintf("population-weighted mean benzene of %.3f ppb from the ",
                         .pw_mob),
                 sprintf("mobile data against %.3f ppb from AirToxScreen, an ", .pw_ats),
                 sprintf("aggregate cancer-risk ratio of %.2f - while disagreeing ",
                         .pw_mob / .pw_ats),
                 "almost completely block by block (Pearson r = 0.00). The ",
                 "screening model captures the regional total but misplaces ",
                 "it: modelled values span only a 2.5-fold range across the ",
                 "domain, whereas the mobile surface spans more than an order ",
                 "of magnitude. Because the block metric is a median of daily ",
                 "medians, it deliberately suppresses episodic plumes; ",
                 "metrics weighted toward the upper tail would place ",
                 "mobile-derived exposure above AirToxScreen overall.")),
      mainPanel(width = 9, leafletOutput("p2_map", height = 380),
                plotOutput("p2_scatter", height = 420)))),

  tabPanel("3. H2S plumes",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Retained plume events"),
        helpText("Plumes were identified as transient H2S enhancements above ",
                 "the rolling background, measured while the van was downwind ",
                 "of the wastewater treatment facility (wind direction at the ",
                 "van consistent with transport from the facility). ",
                 "Candidates were segmented by time gaps and retained only ",
                 "with at least three plume-flagged points, a coherent ",
                 "single-peak shape, consistent winds, and a defined ",
                 "atmospheric stability class (Pasquill B-D): 37 candidates, ",
                 "4 retained. ",
                 "Emission rates are inverse Gaussian-plume estimates from ",
                 "the peak enhancement, distance to the facility, wind speed, ",
                 "and Pasquill-Gifford stability, assuming continuous ",
                 "operation. Winds and boundary-layer depth come from NOAA's ",
                 "3-km hourly HRRR model at the measurement times."),
        tableOutput("p3_table")),
      mainPanel(width = 9, leafletOutput("p3_map", height = 640)))),

  tabPanel("4. Hotspots",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxInput("p4_groups",
                      paste(nrow(hs$groups),
                            "persistent multi-pollutant groups"), TRUE),
        selectInput("p4_poll", "Per-pollutant persistent clusters",
                    c("(none)", unique(hs$clusters$pollutant), "methane")),
        checkboxGroupInput("p4_ctx", "Context layers", CTX_CHOICES,
                           selected = c("Covered facilities", "Wastewater treatment",
                                        "Woodshop", "Refueling stations")),
        h4("How hotspots were identified"),
        helpText("For each pollutant, readings above its campaign 99th ",
                 "percentile were clustered on their geographic coordinates ",
                 "with DBSCAN (100 m radius). A cluster was called persistent ",
                 "if it fell in the top decile of that pollutant's own ",
                 "distribution on both the number of high readings and the ",
                 "number of days carrying them (",
                 if (is.null(hs$n_initial) || is.na(hs$n_initial))
                   "the initial clusters reduced to " else
                   paste0(format(hs$n_initial, big.mark = ","),
                          " clusters reduced to "),
                 format(nrow(hs$clusters), big.mark = ","),
                 " persistent). Overlapping persistent clusters of different ",
                 "pollutants were then merged into groups, and the ",
                 nrow(hs$groups), " groups ",
                 "persistent in three or more pollutants are the ",
                 "multi-pollutant hotspots mapped here. Methane, measured ",
                 "alongside H2S, is analyzed the same way and overlaid as a ",
                 "co-elevation class on each group."),
        helpText("Group markers scale with persistence; click for pollutant ",
                 "make-up, exceedance-days, nearest TRI facility, and methane ",
                 "co-elevation class.")),
      mainPanel(width = 9, leafletOutput("p4_map", height = 560),
                h4("Group composition and candidate sources"),
                DT::DTOutput("p4_table")))),

  tabPanel("5. Source probability",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("p5_poll", "Pollutant", unique(events$pollutant),
                    selected = "Benzene"),
        radioButtons("p5_thr", "Event threshold",
                     c("99th percentile" = "p99", "95th percentile" = "p95")),
        sliderInput("p5_ray", "Upwind ray length (km)", 5, 20, 15, step = 5),
        selectInput("p5_sigma", "Smoothing sigma (m)",
                    c(500, 900, 1200, 1800), selected = 900),
        actionButton("p5_go", "Compute surface", class = "btn-primary"),
        h4("How the surface is created"),
        helpText("A wind back-projection: every measurement above the chosen ",
                 "percentile threshold is an exceedance event. From each ",
                 "event location a ray is cast upwind (toward where the wind ",
                 "came from), weighted by the enhancement magnitude and ",
                 "decaying with distance. Weights accumulate on a 250-m grid ",
                 "and are smoothed with a Gaussian kernel; the surface is ",
                 "scaled to its maximum. Bright areas are the places most ",
                 "often upwind of high readings - probable source regions. ",
                 "Winds are taken from the nearest EPA AQS meteorological ",
                 "station. Defaults: p99 threshold, 15 km rays, 900 m ",
                 "smoothing."),
        textOutput("p5_info")),
      mainPanel(width = 9, leafletOutput("p5_map", height = 640)))),

  tabPanel("6. Study context",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxGroupInput("p6_ctx", "Layers", CTX_CHOICES, selected = CTX_CHOICES),
        helpText("All contextual features used in the study: the three ",
                 "covered facilities in the Denver metro area (HB21-1189) - ",
                 "the covered facility in Pueblo is outside this study - two ",
                 "wastewater treatment facilities, refueling stations and a ",
                 "woodshop identified during the campaign, the Toxics Release ",
                 "Inventory (TRI) facilities inside the route domain, the ",
                 "four EPA AQS meteorological stations, and the La Casa ",
                 "stationary monitoring site. TRI is the US EPA's annual ",
                 "public inventory of industrial chemical releases.")),
      mainPanel(width = 9, leafletOutput("p6_map", height = 640)))),

  tabPanel("7. Health screening",
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons("p7_scen", "Temporal scaling of concentrations",
                     choices = SCEN_CHOICES, selected = SCEN_CHOICES[[1]]),
        helpText("Sampling ran on weekday daytimes, so a campaign mean is not ",
                 "a 24-hour mean. The La Casa stationary monitor measures that ",
                 "gap directly - but only for benzene, toluene and the C8 ",
                 "aromatics. There is no La Casa channel for ",
                 "1,2,4-trimethylbenzene, and ", tags$b("none for H2S or HCN"),
                 " - the two species that set every hazard index on this page. ",
                 "Scenario A scales nothing and is what the paper reports. B ",
                 "applies each measured aromatic's own factor and gives ",
                 "1,2,4-TMB the mean of the three. C and D additionally ",
                 tags$i("borrow"), " a factor for H2S and HCN."),
        helpText(tags$b("C and D are bounds, not estimates. "),
                 "Borrowing assumes the unmeasured species share the aromatics' ",
                 "diurnal shape. Within 500 m cells they do not: the aromatics ",
                 "fall across the sampling window while H2S and HCN rise. Read ",
                 "C and D as an upper envelope on what scaling could do, not as ",
                 "a better estimate than A."),
        if (!is.null(SCALE_WINDOW_TXT)) helpText(SCALE_WINDOW_TXT),
        radioButtons("p7_organ", "Organ-system hazard index",
                     choices = if (!is.null(haz) && !is.null(haz$cells))
                                 sort(unique(haz$cells$organ)) else "none"),
        h4("Organ-system hazard indices"), tableOutput("p7_hi"),
        h4("What is shown"),
        helpText("A screening-level cumulative noncancer assessment. Each ",
                 "pollutant is expressed as a hazard quotient - its exposure ",
                 "concentration divided by the EPA IRIS chronic inhalation ",
                 "reference concentration - and the quotients of pollutants ",
                 "acting on the same target organ system are summed into a ",
                 "hazard index. A value at or above 1 flags an exposure above ",
                 "the level judged to be without appreciable risk of that ",
                 "effect over a lifetime. The table gives the two exposure ",
                 "metrics used: the population-weighted community average and ",
                 "the single most-exposed census block. The map resolves the ",
                 "same indices onto the 500 m grid."),
        helpText(tags$b("Read the H2S and HCN values with care. "),
                 "Their reference concentrations (2 and 0.8 ug/m3) lie below ",
                 "the detection limits of the instruments that measured them, ",
                 "so these hazard indices are set by values at or below the ",
                 "detection limit. They indicate a measurement-capability ",
                 "gap - current mobile instrumentation cannot resolve ambient ",
                 "concentrations at the level of the health benchmark - not a ",
                 "demonstrated exceedance."),
        helpText("This is a screening assessment, not a formal exposure or ",
                 "risk assessment: it rests on repeated short visits rather ",
                 "than continuous exposure monitoring, and assumes ",
                 "dose-additivity within an organ system.")),
      mainPanel(width = 9,
                leafletOutput("p7_map", height = 420),
                h4("Chronic hazard quotients by pollutant"),
                helpText("The factor column is the multiplier the selected ",
                         "scenario applies to each species, and its basis says ",
                         "whether that multiplier was measured at La Casa or ",
                         "borrowed from the aromatics."),
                tableOutput("p7_chronic"),
                h4("How much scaling would it take to change a conclusion?"),
                helpText("For each organ system: the hazard index with nothing ",
                         "scaled, with only the measured aromatics scaled, and ",
                         "the factor that would have to apply to the species ",
                         "with no La Casa channel for that index to reach 1. A ",
                         "blank means no unscalable species contributes, so no ",
                         "borrowed factor can move that row."),
                tableOutput("p7_breakeven"),
                h4("Acute screen: short-term peaks vs 1-hour reference exposure levels"),
                helpText("Campaign 99th-percentile and maximum short-term ",
                         "concentrations against the California OEHHA 1-hour ",
                         "acute RELs. The maximum column compares a sub-minute ",
                         "peak with a one-hour guideline, so it is a ",
                         "conservative upper bound rather than an estimate of a ",
                         "realized one-hour exposure. The scaling toggle does ",
                         "not apply here: a 24-h-equivalence factor adjusts a ",
                         "long-term mean, not a short-term peak."),
                tableOutput("p7_acute")))),

  tabPanel("8. Contact",
    fluidRow(column(width = 8, offset = 2,
      h3("Contact"),
      p("Questions about the data, the analysis, or how to use this application:"),
      tags$div(style = "border-left: 4px solid #2c7fb8; padding: 10px 16px; margin: 12px 0;",
        tags$b("Dr. Priyanka deSouza"), tags$br(),
        "Assistant Professor, Department of Urban and Regional Planning", tags$br(),
        "University of Colorado Denver", tags$br(),
        tags$a(href = "mailto:priyanka.desouza@ucdenver.edu", "priyanka.desouza@ucdenver.edu")),
      h4("About this application"),
      p("This application accompanies the manuscript ",
        tags$i("Mobile monitoring of air toxics in North Denver / Commerce City, Colorado"),
        " (deSouza et al.). Every map, table and number shown here is generated by the same ",
        "reproducible pipeline as the paper, from the public CDPHE mobile air-toxics data ",
        "packets, and is redeployed automatically when the analysis code is updated."),
      tags$ul(
        tags$li(tags$a(href = "https://github.com/pdez90/AirToxics_Colorado", target = "_blank",
                       "Analysis code and application source (GitHub)")),
        tags$li(tags$a(href = "https://www.colorado.gov/airquality/air_toxics_repo.aspx", target = "_blank",
                       "CDPHE air toxics data repository (source data)"))),
      helpText("Methane results are secondary analyses: the methane channel is not part of the ",
               "QA/QC'd public repository and is interpreted in relative terms only."))))
)

# ---- concentration colour scale -------------------------------------------
# Sequential, light -> dark, so higher concentration reads as darker and more
# visually dominant. The previous viridis ramp ran dark (low) -> pale yellow
# (high), which inverted that: the lowest cells dominated the map and the
# highest receded, and the legend consequently read upside-down (CDPHE review,
# 2026-09-15). Binned rather than continuous so the legend can be drawn in
# explicit order - leaflet renders a continuous legend minimum-first and gives
# no supported way to flip it without decoupling the labels from the gradient.
#
# Deliberately NOT the US AQI green/yellow/orange/red/purple scheme: these are
# raw pollutant mixing ratios, not AQI categories, and no AQI breakpoints exist
# for most of these species at these levels. Borrowing those colours would imply
# a regulatory category the data does not carry.
CONC_RAMP <- c("#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913",
               "#D94801", "#A63603", "#7F2704")

conc_scale <- function(v, n = length(CONC_RAMP)) {
  v <- v[is.finite(v)]
  if (!length(v) || diff(range(v)) == 0)
    return(list(pal = function(x) CONC_RAMP[1], cols = CONC_RAMP[1], labs = "n/a"))
  brk <- unique(stats::quantile(v, probs = seq(0, 1, length.out = n + 1),
                                na.rm = TRUE, type = 7))
  if (length(brk) < 3) brk <- unique(pretty(v, n))
  if (length(brk) < 3) brk <- range(v)
  k <- length(brk) - 1
  cols <- grDevices::colorRampPalette(CONC_RAMP)(k)
  pal <- leaflet::colorBin(cols, domain = v, bins = brk, na.color = "transparent")
  fmt <- function(x) formatC(signif(x, 3), format = "fg", big.mark = ",")
  labs <- sprintf("%s - %s", fmt(brk[-length(brk)]), fmt(brk[-1]))
  list(pal = pal, cols = cols, labs = labs, brk = brk)
}

# Legend with the LARGEST value at the top, which is how a vertical colour key
# is normally read. addLegend(colors=, labels=) renders in the order supplied.
add_conc_legend <- function(map, sc, title, position = "bottomright")
  leaflet::addLegend(map, position = position, colors = rev(sc$cols),
                     labels = rev(sc$labs), title = title, opacity = 0.9)

# Fixed-domain version, for layers whose scale is pinned rather than data-driven.
conc_scale_fixed <- function(dom, n = 6, digits = 3) {
  brk <- seq(dom[1], dom[2], length.out = n + 1)
  cols <- grDevices::colorRampPalette(CONC_RAMP)(n)
  fmt <- function(x) formatC(signif(x, digits), format = "fg")
  list(pal = leaflet::colorBin(cols, domain = dom, bins = brk,
                               na.color = "transparent"),
       cols = cols,
       labs = sprintf("%s - %s", fmt(brk[-length(brk)]), fmt(brk[-1])),
       brk = brk)
}

# The mobile : AirToxScreen ratio is a POLARITY, not a magnitude: 1 means the
# two agree, and the question is which side a block falls on. That is a
# diverging scale - two hues with a neutral midpoint - not a sequential one.
# Breaks are symmetric in log space about 1 so no bin straddles agreement.
# The IRIS reference concentration for trimethylbenzene applies to ANY TMB
# isomer or mixture (IRIS 2016), and the instruments do not resolve a single
# isomer, so the health tables name the species "Trimethylbenzenes" rather than
# implying the 1,2,4- isomer specifically (CDPHE review, 2026-09-15). The
# pipeline CSVs keep their own column names untouched.
tidy_pollutant <- function(x)
  sub("^1,2,4-Trimethylbenzene$", "Trimethylbenzenes",
      sub("^Trimethylbenzene$", "Trimethylbenzenes", as.character(x)))

RATIO_BRK  <- c(0, 1/3, 1/2, 2/3, 1, 1.5, 2, 3)
RATIO_COLS <- c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0",
                "#FDDBC7", "#EF8A62", "#B2182B")
ratio_scale <- function() {
  fmt <- function(x) formatC(signif(x, 2), format = "fg")
  list(pal = leaflet::colorBin(RATIO_COLS, domain = c(0, 3), bins = RATIO_BRK,
                               na.color = "transparent"),
       cols = RATIO_COLS,
       labs = c(sprintf("%s - %s", fmt(RATIO_BRK[-length(RATIO_BRK)]),
                        fmt(RATIO_BRK[-1]))),
       brk = RATIO_BRK)
}

# ================= SERVER =================
server <- function(input, output, session) {

  # ---- page 1 ----
  output$p1_map <- renderLeaflet({
    d <- cells[pollutant == input$p1_poll]
    v <- d[[input$p1_stat]]
    .vv <- if (input$p1_stat == "n") log10(v) else v
    sc  <- conc_scale(.vv)
    col <- sc$pal(.vv)
    m <- base_map() |>
      addRectangles(d$lon - 0.00292, d$lat - 0.00226, d$lon + 0.00292,
                    d$lat + 0.00226, fillColor = col, fillOpacity = 0.65,
                    weight = 0, popup = sprintf(
                      "n = %s<br>median = %s %s<br>p95 = %s<br>max = %s",
                      format(d$n, big.mark = ","), d$median,
                      unit_of(input$p1_poll), d$p95, d$max))
    m <- add_context(m, input$p1_ctx)
    if (input$p1_stat == "n") {
      .sc2 <- sc
      .sc2$labs <- sprintf("%s - %s",
                           formatC(signif(10^sc$brk[-length(sc$brk)], 2), format = "d", big.mark = ","),
                           formatC(signif(10^sc$brk[-1], 2), format = "d", big.mark = ","))
      add_conc_legend(m, .sc2, "1-s measurements<br>per 500 m cell")
    } else {
      add_conc_legend(m, sc,
                title = sprintf("%s %s (%s)", input$p1_poll, input$p1_stat,
                                unit_of(input$p1_poll)))
    }
  })
  output$p1_summary <- renderTable({
    s <- summ[pollutant == input$p1_poll]
    if (nrow(s) == 0) return(data.frame(note = "campaign stats: see manuscript"))
    data.frame(Metric = c("1-s measurements", "% below MDL", "Median", "p95",
                          "p99", "Max"),
               Value = c(format(s$n, big.mark = ","),
                         paste0(s$pct_below_mdl, "%"), s$median, s$p95, s$p99, s$max))
  }, colnames = FALSE)
  output$p1_day_ui <- renderUI({
    if (is.null(tracks))
      return(helpText("Rerun prep_app_data.R to enable the day slider."))
    sliderInput("p1_day", NULL, min = 1, max = length(udays), value = 1,
                step = 1, ticks = FALSE, width = "100%",
                animate = animationOptions(interval = 600, loop = TRUE))
  })
  observe({
    proxy <- leafletProxy("p1_map")
    if (!isTRUE(input$p1_daily) || is.null(tracks)) {
      clearGroup(proxy, "daily"); return(invisible())
    }
    req(input$p1_day)
    d <- udays[input$p1_day]
    sub <- tracks[day == d]
    proxy <- clearGroup(proxy, "daily")
    addCircleMarkers(proxy, data = sub, ~Longitude, ~Latitude,
      radius = 2.5, stroke = FALSE, group = "daily",
      fillColor = ifelse(sub$Route == "Sinclair Terminal",
                         "#b2182b", "#2166ac"),
      fillOpacity = 0.75)
  })
  output$p1_day_info <- renderText({
    req(isTRUE(input$p1_daily), input$p1_day, !is.null(tracks))
    d <- udays[input$p1_day]
    sprintf("%s — sampling day %d of %d", format(d, "%A, %B %d, %Y"),
            input$p1_day, length(udays))
  })
  output$p1_coverage <- renderUI({
    if (is.null(camp))
      return(helpText("Rerun prep_app_data.R to generate coverage stats."))
    wk <- camp$wk
    HTML(sprintf(paste0(
      "Measurements were collected on <b>%s sampling days</b> between %s ",
      "and %s.<br>%s%% of 1-s measurements were made on weekdays ",
      "(days by weekday: %s).<br>Driving hours: roughly %02d:00&ndash;%02d:00 ",
      "local time."),
      format(camp$n_days, big.mark = ","), camp$first, camp$last,
      camp$pct_weekday,
      paste(sprintf("%s %d", names(wk), as.integer(wk)), collapse = ", "),
      camp$h_lo, camp$h_hi))
  })

  # ---- page 2 ----
  output$p2_map <- renderLeaflet({
    b <- blocks
    val <- switch(input$p2_layer, ats = b$benzene_ppb_airtox,
                  mob = b$sBenzene_med_of_daily_med_scaled, ratio = b$ratio)
    dom <- switch(input$p2_layer,
                  ats = c(0.1, 0.35), mob = c(0, 1), ratio = c(0, 3))
    sc <- if (input$p2_layer == "ratio") ratio_scale() else conc_scale_fixed(dom)
    leaflet(b) |> addProviderTiles(providers$CartoDB.Positron) |>
      setView(-104.93, 39.82, zoom = 11) |>
      addPolygons(fillColor = sc$pal(pmin(pmax(val, dom[1]), dom[2])),
                  fillOpacity = 0.75, weight = 0.3, color = "grey40",
                  popup = ~sprintf(
                    "AirToxScreen: %.3f ppb<br>Mobile (scaled): %.3f ppb<br>Ratio: %.2f<br>Population: %s",
                    benzene_ppb_airtox, sBenzene_med_of_daily_med_scaled,
                    ratio, format(Population_airtox, big.mark = ","))) |>
      add_conc_legend(sc, switch(input$p2_layer,
                ats = "AirToxScreen (ppb)", mob = "Mobile (ppb)",
                ratio = "Mobile : AirToxScreen<br>(1 = agreement)"))
  })
  output$p2_stats <- renderTable({
    b <- st_drop_geometry(blocks)
    ok <- is.finite(b$ratio)
    data.frame(Metric = c("Blocks", "Population",
                          "AirToxScreen range (ppb)", "Mobile range (ppb)",
                          "Blocks >2x AirToxScreen", "Blocks >5x", "Median ratio"),
               Value = c(format(nrow(b), big.mark = ","),
                         format(sum(b$Population_airtox, na.rm = TRUE), big.mark = ","),
                         sprintf("%.3f-%.3f", min(b$benzene_ppb_airtox, na.rm = TRUE),
                                 max(b$benzene_ppb_airtox, na.rm = TRUE)),
                         sprintf("%.2f-%.2f",
                                 min(b$sBenzene_med_of_daily_med_scaled, na.rm = TRUE),
                                 max(b$sBenzene_med_of_daily_med_scaled, na.rm = TRUE)),
                         sum(b$ratio > 2, na.rm = TRUE), sum(b$ratio > 5, na.rm = TRUE),
                         sprintf("%.2f", median(b$ratio[ok]))))
  }, colnames = FALSE)
  output$p2_scatter <- renderPlot({
    b <- st_drop_geometry(blocks)
    ggplot(b, aes(benzene_ppb_airtox, sBenzene_med_of_daily_med_scaled)) +
      geom_point(alpha = 0.25, size = 0.9) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      labs(x = "AirToxScreen benzene (ppb)", y = "Mobile scaled benzene (ppb)",
           subtitle = "Red line = 1:1. Aggregate risk agrees; block-level r ~ 0.") +
      theme_bw()
  })

  # ---- page 3 ----
  output$p3_map <- renderLeaflet({
    m <- base_map() |> setView(WWTP_LL[2], WWTP_LL[1], zoom = 13) |>
      addMarkers(lng = WWTP_LL[2], lat = WWTP_LL[1],
                 icon = star_icon("green", 28),
                 label = "Wastewater treatment facility",
                 labelOptions = labelOptions(permanent = TRUE,
                                             direction = "left"))
    cols <- c("#d73027", "#fc8d59", "#7b3294", "#4575b4")
    has_loc <- all(c("lat", "lon") %in% names(plumes))
    for (i in seq_len(nrow(plumes))) {
      pop <- sprintf(
        "<b>Plume %s</b><br>%s<br>ΔH2S: %s ppb<br>Wind: %s m/s | Stability %s<br>Distance from WWTF: %s km<br><b>Inverse estimate: %s t/yr</b>",
        plumes$plume_id[i], plumes$datetime[i], plumes$dH2S_ppb[i],
        plumes$wind_ms[i], plumes$stability[i], plumes$dist_km[i],
        format(plumes$rate_tpy[i], big.mark = ","))
      if (has_loc && is.finite(plumes$lat[i])) {
        m <- addPolylines(m, lng = c(WWTP_LL[2], plumes$lon[i]),
                          lat = c(WWTP_LL[1], plumes$lat[i]),
                          color = cols[i], weight = 2, dashArray = "5,6")
        m <- addCircleMarkers(m, lng = plumes$lon[i], lat = plumes$lat[i],
          radius = 9, color = "black", weight = 1.5, fillColor = cols[i],
          fillOpacity = 0.95, popup = pop,
          label = sprintf("Plume %s: %s", plumes$plume_id[i], plumes$datetime[i]),
          labelOptions = labelOptions(permanent = TRUE, direction = "auto",
                                      textsize = "11px"))
      } else {
        m <- addCircles(m, lng = WWTP_LL[2], lat = WWTP_LL[1],
                        radius = plumes$dist_km[i] * 1000, weight = 2,
                        fill = FALSE, color = cols[i], popup = pop)
      }
    }
    m
  })
  output$p3_table <- renderTable({
    data.frame(Plume = plumes$plume_id,
               `Date/time` = as.character(plumes$datetime),
               `Rate (t/yr)` = format(plumes$rate_tpy, big.mark = ","),
               check.names = FALSE)
  })

  # ---- page 4 ----
  output$p4_map <- renderLeaflet({
    m <- base_map()
    if (isTRUE(input$p4_groups)) {
      g <- hs$groups
      colv <- if ("ch4_class" %in% names(g))
        c("CH4-enriched" = "red", "CH4-intermediate" = "orange",
          "CH4-quiet" = "steelblue")[g$ch4_class] else "steelblue"
      colv[is.na(colv)] <- "steelblue"
      m <- addCircleMarkers(m, data = g, ~Longitude, ~Latitude,
        radius = ~pmax(6, sqrt(persistence_index_weighted) / 3),
        color = "black", weight = 1.5, fillColor = colv, fillOpacity = 0.85,
        label = ~sprintf("Group %s", group_id),
        labelOptions = labelOptions(direction = "top", textsize = "13px",
                                    style = list("font-weight" = "bold")),
        # addCircleMarkers takes no highlight argument (that is polygons and
        # polylines only); the hover label is what identifies the group.
        popup = ~sprintf(
          "<b>Group %s</b><br>Pollutants: %s<br>Total exceedance-days: %s (max %s)<br>Nearest TRI: %s (%.1f km)%s",
          group_id, gsub("\\+", " + ", pollutants), total_n_days, max_n_days,
          ifelse(is.na(tri_name), "n/a", tri_name), tri_dist_km,
          if ("ch4_class" %in% names(g))
            sprintf("<br>Methane: %s (%.1f%% obs ≥ p95)", ch4_class, pct_ge_p95)
          else ""))
    }
    if (input$p4_poll != "(none)") {
      if (input$p4_poll == "methane" && !is.null(hs$methane)) {
        cl <- hs$methane
        m <- addCircleMarkers(m, data = cl, ~lon, ~lat, radius = 4,
          color = "grey20", fillColor = ifelse(cl$persistent, "red", "grey70"),
          fillOpacity = 0.7, weight = 1,
          popup = ~sprintf("CH4 cluster %s<br>n=%s on %s days<br>max %.1f ppm%s",
                           cluster, n_events, n_days, ch4_max,
                           ifelse(persistent, "<br><b>PERSISTENT</b>", "")))
      } else {
        cl <- hs$clusters[pollutant == input$p4_poll]
        if (nrow(cl) > 0)
          m <- addCircleMarkers(m, data = cl, ~Longitude, ~Latitude, radius = 4,
            color = "grey20", fillColor = "grey60", fillOpacity = 0.7, weight = 1,
            popup = ~sprintf("%s cluster %s<br>n=%s on %s days",
                             pollutant, clust, n, n_days))
      }
    }
    add_context(m, input$p4_ctx)
  })
  output$p4_table <- DT::renderDT({
    g <- as.data.table(hs$groups)
    key <- ctx$key
    has_ch4 <- "ch4_class" %in% names(g)
    near_txt <- character(nrow(g)); src <- character(nrow(g))
    for (i in seq_len(nrow(g))) {
      dkm <- sqrt(((key$lon - g$Longitude[i]) *
                     cos(g$Latitude[i] * pi / 180) * 111.32)^2 +
                  ((key$lat - g$Latitude[i]) * 110.54)^2)
      o <- order(dkm)
      near <- o[dkm[o] <= 1.5]
      near_txt[i] <- if (length(near))
        paste(sprintf("%s (%.2f km)", key$name[near], dkm[near]), collapse = "; ")
      else sprintf("nearest: %s (%.1f km)", key$name[o[1]], dkm[o[1]])
      p <- tolower(g$pollutants[i]); parts <- character()
      if (any(dkm <= 0.6)) parts <- c(parts, key$name[dkm <= 0.6])
      if (grepl("h2s|hydrogen_sulfide", p) &&
          any(dkm[key$type == "Wastewater treatment"] <= 2))
        parts <- c(parts, "wastewater-type (H2S)")
      if (has_ch4 && !is.na(g$ch4_class[i]) && g$ch4_class[i] == "CH4-enriched")
        parts <- c(parts, "methane co-elevated (oil & gas-type)")
      if (!is.na(g$tri_name[i]) && g$tri_dist_km[i] <= 0.75)
        parts <- c(parts, sprintf("TRI: %s", g$tri_name[i]))
      src[i] <- if (length(parts)) paste(unique(parts), collapse = "; ")
                else "unresolved (mixed urban / traffic)"
    }
    out <- data.frame(
      Group = g$group_id,
      Pollutants = gsub("\\+", " + ", g$pollutants),
      `N pollutants` = g$n_pollutants,
      `Exceedance-days` = g$total_n_days,
      Methane = if (has_ch4)
        ifelse(is.na(g$ch4_class), "-",
               sprintf("%s (%.1f%% ≥ p95)", g$ch4_class, g$pct_ge_p95))
        else "-",
      `Nearest TRI` = ifelse(is.na(g$tri_name), "-",
                             sprintf("%s (%.2f km)", g$tri_name, g$tri_dist_km)),
      `Key facilities within 1.5 km` = near_txt,
      `Candidate sources` = src, check.names = FALSE)
    out <- out[order(-g$total_n_days), ]
    DT::datatable(out, rownames = FALSE,
      options = list(pageLength = 20, dom = "t", scrollX = TRUE),
      caption = paste("Composition of the persistent multi-pollutant hotspot",
        "groups. Candidate sources are rule-based and transparent: key",
        "facilities within 0.6 km; wastewater-type if the group includes H2S",
        "and a WWTF lies within 2 km; methane co-elevation class from the",
        "campaign CH4 data; TRI facilities within 0.75 km. Groups matching no",
        "rule are labeled unresolved."))
  })

  # ---- page 5 ----
  surface <- eventReactive(input$p5_go, ignoreNULL = FALSE, {
    withProgress(message = "Computing source-probability surface...", {
      tryCatch({
        ev <- events[pollutant == input$p5_poll]
        thr <- if (input$p5_thr == "p99") ev$thr99[1] else ev$thr95[1]
        ev <- ev[value >= thr & is.finite(wd)]
        if (nrow(ev) < 10) stop("too few events for this pollutant/threshold")
        if (nrow(ev) > 20000)   # cap for server memory; deterministic thinning
          ev <- ev[unique(round(seq(1, .N, length.out = 20000)))]
        n <- nrow(ev)
        lat0 <- median(ev$lat); lon0 <- median(ev$lon)
        ex <- (ev$lon - lon0) * cos(lat0 * pi / 180) * 111320
        ey <- (ev$lat - lat0) * 110540
        w0 <- pmin(ev$value / thr, 5); th <- ev$wd * pi / 180
        steps <- seq(150, input$p5_ray * 1000, by = 150); ns <- length(steps)
        x <- rep(ex, each = ns) + rep(sin(th), each = ns) * steps
        y <- rep(ey, each = ns) + rep(cos(th), each = ns) * steps
        w <- rep(w0, each = ns) * exp(-steps / 12000)
        G <- 250
        gr <- data.table(gx = round(x / G) * G,
                         gy = round(y / G) * G, w = w)[, .(w = sum(w)),
                                                       by = .(gx, gy)]
        xr <- range(gr$gx); yr <- range(gr$gy)
        nx <- (xr[2] - xr[1]) / G + 1; ny <- (yr[2] - yr[1]) / G + 1
        M <- matrix(0, ny, nx)
        M[cbind((gr$gy - yr[1]) / G + 1, (gr$gx - xr[1]) / G + 1)] <- gr$w
        sg <- as.numeric(input$p5_sigma)
        k1 <- dnorm(seq(-3 * sg, 3 * sg, by = G), sd = sg); k1 <- k1 / sum(k1)
        sm <- function(v) as.numeric(stats::filter(
          c(rep(0, length(k1) %/% 2), v, rep(0, length(k1) %/% 2)), k1, sides = 2)
        )[(length(k1) %/% 2 + 1):(length(k1) %/% 2 + length(v))]
        M <- apply(M, 2, sm); M <- t(apply(M, 1, sm))
        M[is.na(M)] <- 0
        if (max(M) <= 0) stop("empty surface (no weighted events)")
        M <- M / max(M)
        list(M = M, xr = xr, yr = yr, G = G, lat0 = lat0, lon0 = lon0,
             n = n, thr = thr, err = NULL)
      }, error = function(e) list(err = conditionMessage(e)))
    })
  })
  output$p5_map <- renderLeaflet({
    s <- surface()
    req(s)
    validate(need(is.null(s$err), paste("Computation failed:", s$err)))
    # Render the surface as an in-memory PNG placed with L.imageOverlay —
    # avoids the raster/terra dependency (terra fails to compile on
    # Connect Cloud); png/base64enc/htmlwidgets are all pre-built there.
    M <- s$M[nrow(s$M):1, , drop = FALSE]                    # row 1 = north
    ramp <- grDevices::colorRampPalette(
      c("#000004", "#420A68", "#932667", "#DD513A",
        "#FCA50A", "#FCFFA4"))(256)                          # inferno
    idx <- pmin(pmax(round(M * 255) + 1L, 1L), 256L)
    rgbm <- grDevices::col2rgb(ramp[idx]) / 255              # column-major
    arr <- array(0, dim = c(nrow(M), ncol(M), 4))
    arr[, , 1] <- matrix(rgbm[1, ], nrow(M))
    arr[, , 2] <- matrix(rgbm[2, ], nrow(M))
    arr[, , 3] <- matrix(rgbm[3, ], nrow(M))
    arr[, , 4] <- 0.75 * sqrt(M)                             # fade near-zero
    f <- tempfile(fileext = ".png")
    png::writePNG(arr, f)
    uri <- base64enc::dataURI(file = f, mime = "image/png")
    west  <- s$lon0 + s$xr[1] / (cos(s$lat0 * pi / 180) * 111320)
    east  <- s$lon0 + s$xr[2] / (cos(s$lat0 * pi / 180) * 111320)
    south <- s$lat0 + s$yr[1] / 110540
    north <- s$lat0 + s$yr[2] / 110540
    pal <- colorNumeric("inferno", c(0, 1), na.color = "transparent")
    m <- base_map() |>
      addLegend(pal = pal, values = c(0, 1), title = "Relative<br>probability")
    m <- add_context(m, c("Covered facilities", "Wastewater treatment",
                          "Woodshop", "Refueling stations"))
    htmlwidgets::onRender(m, sprintf(
      "function(el, x) { L.imageOverlay('%s', [[%.6f, %.6f], [%.6f, %.6f]], {opacity: 1}).addTo(this); }",
      uri, south, west, north, east))
  })
  output$p5_info <- renderText({
    s <- surface(); req(s)
    if (!is.null(s$err)) paste("Error:", s$err)
    else sprintf("%s events ≥ threshold (%.3g %s) with valid wind.",
                 format(s$n, big.mark = ","), s$thr, unit_of(input$p5_poll))
  })

  # ---- page 6 ----
  output$p6_map <- renderLeaflet(add_context(base_map(), input$p6_ctx))

  # ---- page 7: screening health hazard (SI S7) ----
  # Which scenario is live. Falls back to the unscaled baseline whenever
  # hazard.rds predates 77_health_scaling_sensitivity.R, so an app deployed
  # against older data still runs - it just shows scenario A with no toggle.
  p7_scen <- reactive(if (is.null(input$p7_scen)) "A_none" else input$p7_scen)

  output$p7_hi <- renderTable({
    req(haz)
    if (!is.null(haz$scen)) {
      d <- haz$scen[scenario == p7_scen()]
      req(nrow(d) > 0)
      d <- d[order(-HI_pwmean)]
      data.frame(`Organ system` = d$organ,
                 `Community avg` = sprintf("%.3g", d$HI_pwmean),
                 `Most-exposed block` = sprintf("%.3g", d$HI_maxblock),
                 check.names = FALSE)
    } else {
      data.frame(`Organ system` = haz$hi$target_organ,
                 `Community avg` = sprintf("%.3g", haz$hi$HI_pwmean),
                 `Most-exposed block` = sprintf("%.3g", haz$hi$HI_maxblock),
                 check.names = FALSE)
    }
  })

  output$p7_chronic <- renderTable({
    req(haz)
    if (!is.null(haz$scen_poll)) {
      d <- haz$scen_poll[scenario == p7_scen()]
      req(nrow(d) > 0)
      data.frame(Pollutant = tidy_pollutant(d$pollutant),
                 `Target organ` = d$organ,
                 `IRIS RfC (ug/m3)` = format(d$RfC_ugm3, big.mark = ","),
                 `Scaling factor` = sprintf("%.3f", d$factor),
                 `Basis of factor` = d$factor_source,
                 `Community avg (ug/m3)` = sprintf("%.3g", d$pwmean_ugm3),
                 `HQ (community avg)` = sprintf("%.3g", d$HQ_pwmean),
                 `HQ (most-exposed block)` = sprintf("%.3g", d$HQ_maxblock),
                 check.names = FALSE)
    } else {
      d <- haz$chronic
      data.frame(Pollutant = tidy_pollutant(d$pollutant),
                 `Target organ` = d$target_organ,
                 `IRIS RfC (ug/m3)` = format(d$RfC_ugm3, big.mark = ","),
                 `Community avg (ug/m3)` = sprintf("%.3g", d$pwmean_ugm3),
                 `HQ (community avg)` = sprintf("%.3g", d$HQ_pwmean),
                 `HQ (most-exposed block)` = sprintf("%.3g", d$HQ_maxblock),
                 check.names = FALSE)
    }
  })

  output$p7_breakeven <- renderTable({
    req(haz, haz$breakeven)
    d <- haz$breakeven
    fmt <- function(x) ifelse(is.finite(x), sprintf("%.3g", x), "")
    # these two columns hold "A + B + C" lists, so the anchored tidy_pollutant()
    # would not touch them - substitute the token wherever it appears instead
    tidy_list <- function(x) gsub("1,2,4-Trimethylbenzene", "Trimethylbenzenes",
                                  as.character(x), fixed = TRUE)
    data.frame(`Organ system` = d$organ,
               `Driven by` = tidy_list(d$driven_by),
               `No La Casa factor` = tidy_list(d$no_lacasa_factor),
               `HI unscaled` = fmt(d$HI_pwmean_unscaled),
               `HI aromatics scaled` = fmt(d$HI_pwmean_aromscaled),
               `Break-even factor (community avg)` = fmt(d$f_breakeven_pwmean),
               `Break-even factor (most-exposed block)` = fmt(d$f_breakeven_maxblock),
               check.names = FALSE)
  })

  output$p7_acute <- renderTable({
    req(haz)
    d <- haz$acute
    data.frame(Pollutant = tidy_pollutant(d$pollutant),
               `OEHHA 1-h REL (ug/m3)` = format(d$acuteREL_ugm3, big.mark = ","),
               `p99 (ug/m3)` = sprintf("%.3g", d$p99_ugm3),
               `HQ at p99` = sprintf("%.3g", d$HQ_p99),
               `Max (ug/m3)` = sprintf("%.4g", d$max_ugm3),
               `HQ at max` = sprintf("%.3g", d$HQ_max),
               check.names = FALSE)
  })

  # Fixed HI bins with a break exactly at the screening benchmark. The old
  # scale was continuous log10(HI) on reversed magma, which put the darkest
  # colour on the LOWEST cells and made the legend read upside down, and asked
  # the reader to convert a log axis back to a hazard index in their head. The
  # bins are pinned rather than data-driven so that switching scenario or organ
  # moves the map, not the meaning of a colour.
  HI_BRK  <- c(-Inf, 0.1, 0.25, 0.5, 1, 2.5, 5, Inf)
  HI_LABS <- c("under 0.1", "0.1 - 0.25", "0.25 - 0.5", "0.5 - 1",
               "1 - 2.5  (at or above benchmark)",
               "2.5 - 5  (at or above benchmark)",
               "over 5  (at or above benchmark)")

  output$p7_map <- renderLeaflet({
    req(haz, haz$cells)
    d <- haz$cells
    if ("scenario" %in% names(d)) d <- d[scenario == p7_scen()]
    d <- d[organ == input$p7_organ]
    req(nrow(d) > 0)
    hi_col <- function(v) {
      # right = FALSE so an HI of exactly 1 lands in the "at or above the
      # benchmark" bin, which is what "at or above 1" means in S7.
      i <- cut(v, HI_BRK, labels = FALSE, include.lowest = TRUE, right = FALSE)
      out <- CONC_RAMP[i]; out[is.na(i)] <- "#BDBDBD"; out
    }
    base_map() |>
      addRectangles(d$lon - 0.00292, d$lat - 0.00226,
                    d$lon + 0.00292, d$lat + 0.00226,
                    fillColor = hi_col(d$HI), fillOpacity = 0.75, weight = 0,
                    popup = sprintf(
                      "<b>%s hazard index: %.3g</b><br>%s<br>%s sampling days<br><i>%s</i>",
                      d$organ, d$HI, d$pollutants, d$n_days,
                      names(SCEN_CHOICES)[match(p7_scen(), SCEN_CHOICES)])) |>
      add_context(c("Covered facilities", "Wastewater treatment")) |>
      addLegend("bottomright", colors = rev(CONC_RAMP), labels = rev(HI_LABS),
                opacity = 0.9,
                title = sprintf("%s hazard index<br><span style='font-weight:normal'>scenario %s</span>",
                                input$p7_organ, sub("_.*$", "", p7_scen())))
  })
}

shinyApp(ui, server)
