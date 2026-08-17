# ==============================================================
# CDPHE Mobile Air Toxics Explorer — Shiny app
# Pages: 1 Raw data | 2 AirToxScreen vs Mobile | 3 Plumes |
#        4 Hotspots | 5 Source probability | 6 Study context
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
ctx     <- readRDS(file.path(DATA, "context.rds"))

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
base_map <- function() leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
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
        h4("Campaign summary"), tableOutput("p1_summary"),
        helpText("Cells are the 500 m analysis grid; values summarize all 1-s ",
                 "measurements in each cell. %<MDL as reported in Table S3.1 ",
                 "(flag-based; exact MDLs with CDPHE).")),
      mainPanel(width = 9, leafletOutput("p1_map", height = 640)))),

  tabPanel("2. AirToxScreen vs Mobile",
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons("p2_layer", "Map layer",
                     c("AirToxScreen benzene" = "ats",
                       "Mobile benzene (scaled)" = "mob",
                       "Ratio mobile / AirToxScreen" = "ratio")),
        h4("Across 1,668 common blocks"), tableOutput("p2_stats"),
        helpText("Mobile values are background-corrected medians of daily ",
                 "medians, scaled to 24/7 using the La Casa diurnal pattern.")),
      mainPanel(width = 9, leafletOutput("p2_map", height = 420),
                plotOutput("p2_scatter", height = 240)))),

  tabPanel("3. H2S plumes",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Retained plume events"),
        helpText("33 candidate H2S plume events were identified near the ",
                 "wastewater treatment facility; 4 passed all quality filters ",
                 "(shape, wind consistency, stability). Rates are Gaussian ",
                 "plume inverse estimates assuming continuous operation."),
        tableOutput("p3_table")),
      mainPanel(width = 9, leafletOutput("p3_map", height = 640)))),

  tabPanel("4. Hotspots",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxInput("p4_groups", "17 persistent multi-pollutant groups", TRUE),
        selectInput("p4_poll", "Per-pollutant persistent clusters",
                    c("(none)", unique(hs$clusters$pollutant), "methane")),
        checkboxGroupInput("p4_ctx", "Context layers", CTX_CHOICES,
                           selected = c("Covered facilities", "Wastewater treatment",
                                        "Woodshop", "Refueling stations")),
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
        helpText("Reproduces the manuscript's upwind back-projection ",
                 "(concentration-weighted rays, exponential distance kernel, ",
                 "Gaussian smoothing). Manuscript settings: p99, 15 km, 900 m."),
        textOutput("p5_info")),
      mainPanel(width = 9, leafletOutput("p5_map", height = 640)))),

  tabPanel("6. Study context",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxGroupInput("p6_ctx", "Layers", CTX_CHOICES, selected = CTX_CHOICES),
        helpText("All contextual features used in the study: the three ",
                 "covered facilities (HB21-1189), two wastewater treatment ",
                 "facilities, refueling stations and a woodshop identified ",
                 "during the campaign, all TRI facilities in the domain, the ",
                 "four EPA AQS meteorological stations, and the La Casa ",
                 "stationary monitoring site.")),
      mainPanel(width = 9, leafletOutput("p6_map", height = 640))))
)

# ================= SERVER =================
server <- function(input, output, session) {

  # ---- page 1 ----
  output$p1_map <- renderLeaflet({
    d <- cells[pollutant == input$p1_poll]
    v <- d[[input$p1_stat]]
    pal <- colorNumeric("viridis", domain = if (input$p1_stat == "n") log10(v) else v)
    col <- if (input$p1_stat == "n") pal(log10(v)) else pal(v)
    m <- base_map() |>
      addRectangles(d$lon - 0.00292, d$lat - 0.00226, d$lon + 0.00292,
                    d$lat + 0.00226, fillColor = col, fillOpacity = 0.65,
                    weight = 0, popup = sprintf(
                      "n = %s<br>median = %s %s<br>p95 = %s<br>max = %s",
                      format(d$n, big.mark = ","), d$median,
                      unit_of(input$p1_poll), d$p95, d$max))
    m <- add_context(m, input$p1_ctx)
    if (input$p1_stat == "n") {
      addLegend(m, pal = pal, values = log10(v),
                title = "1-s measurements<br>per 500 m cell",
                labFormat = labelFormat(transform = function(x) signif(10^x, 2)))
    } else {
      addLegend(m, pal = pal, values = v,
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

  # ---- page 2 ----
  output$p2_map <- renderLeaflet({
    b <- blocks
    val <- switch(input$p2_layer, ats = b$benzene_ppb_airtox,
                  mob = b$sBenzene_med_of_daily_med_scaled, ratio = b$ratio)
    dom <- switch(input$p2_layer,
                  ats = c(0.1, 0.35), mob = c(0, 1), ratio = c(0, 3))
    pal <- colorNumeric("viridis", domain = dom)
    leaflet(b) |> addProviderTiles(providers$CartoDB.Positron) |>
      setView(-104.93, 39.82, zoom = 11) |>
      addPolygons(fillColor = ~pal(pmin(pmax(val, dom[1]), dom[2])),
                  fillOpacity = 0.75, weight = 0.3, color = "grey40",
                  popup = ~sprintf(
                    "AirToxScreen: %.3f ppb<br>Mobile (scaled): %.3f ppb<br>Ratio: %.2f<br>Population: %s",
                    benzene_ppb_airtox, sBenzene_med_of_daily_med_scaled,
                    ratio, format(Population_airtox, big.mark = ","))) |>
      addLegend(pal = pal, values = dom, title = switch(input$p2_layer,
                ats = "AirToxScreen (ppb)", mob = "Mobile (ppb)", ratio = "Ratio"))
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
}

shinyApp(ui, server)
