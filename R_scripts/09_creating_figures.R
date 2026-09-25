# STANDALONE (2026-09-23): attach what the composites need, so this script
# can be run on its own without re-rendering the polar plots in script 08.
suppressPackageStartupMessages({ library(grid); library(ggplot2); library(cowplot)
  library(jpeg); if (requireNamespace("ragg", quietly = TRUE)) library(ragg) })

# ------------------------------------------------------------------
# TOLERANT COMPOSITES (2026-09-22): this script assembles composite
# figures out of JPEGs. The polar-plot panels are written by script 08,
# but several later composites are built from map images that were
# exported by hand for the original manuscript and were lost with the
# rest of the folder - no script in the pipeline writes them. A missing
# input used to stop the whole group (and with it every composite after
# it). Each composite is now attempted on its own and a missing input is
# reported by name instead of halting the run.
# ------------------------------------------------------------------
.composite <- function(expr) tryCatch(expr, error = function(e) {
  try(while (grDevices::dev.cur() > 1) grDevices::dev.off(), silent = TRUE)
  message("[09] SKIPPED composite: ", conditionMessage(e))
})

.composite({
# ==============================================================
# 09  Creating Figures
# Auto-split from Suncor.Rmd  (section 9 of 40)
# ==============================================================

#Creating Figures

require(jpeg)
require(cowplot)

#Polarplot
pp_suncor_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_Benzene.jpeg")
pp_suncor_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_Toluene.jpeg")
pp_suncor_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_Trimethylbenzene.jpeg")
pp_suncor_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_Xylene.jpeg")
pp_suncor_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_H2S.jpeg")
pp_suncor_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_HCN.jpeg")

library(grid)
library(cowplot)
library(ggplot2)

# Combine plots
p_combined <- cowplot::plot_grid(
  rasterGrob(pp_suncor_toluene),
  rasterGrob(pp_suncor_h2s),
  ncol = 1,
  labels = c("A)", "B)", "C)", "D)", "E)", "F)", "G)")
)

# Save with same dimensions and high resolution
library(grid)
library(cowplot)
library(ggplot2)
library(ragg)

# Combine plots (control label size here)
p_combined <- cowplot::plot_grid(
  rasterGrob(pp_suncor_toluene),
  rasterGrob(pp_suncor_h2s),
  ncol = 1,
  labels = c("A)", "B)", "C)", "D)", "E)", "F)", "G)"),
  label_size = 14,        # <-- make smaller/larger
  label_fontface = "bold" # optional
)

# Save with ragg (sharper)
library(grid)
library(cowplot)
library(ggplot2)
library(ragg)

# Combine plots (control label size here)
p_combined <- cowplot::plot_grid(
    rasterGrob(pp_suncor_benzene),
  rasterGrob(pp_suncor_toluene),
      rasterGrob(pp_suncor_trimethylbenzene),
  rasterGrob(pp_suncor_xylene),
  rasterGrob(pp_suncor_h2s),
  rasterGrob(pp_suncor_hcn),
  ncol = 2,
  labels = c("A)", "B)", "C)", "D)", "E)", "F)"),
  label_size = 10,        # <-- make smaller/larger
  label_fontface = "bold" # optional
)

# Save with ragg (sharper)
out_file <- "/Users/priyanka/Downloads/Suncor/FinalFig/polarplot_main.jpeg"

ragg::agg_jpeg(
  filename = out_file,
  width = 3000,
  height = 4000,
  units = "px",
  res = 800,
  quality = 95,
  background = "white"
)
print(p_combined)
dev.off()
})
.composite({

message("Saved: ", out_file)

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/polarplot_SI.jpeg", res=800, width=4000, height=3500)
cowplot::plot_grid(
  rasterGrob(pp_suncor_benzene),
  rasterGrob(pp_suncor_trimethylbenzene),
  rasterGrob(pp_suncor_xylene),
  rasterGrob(pp_suncor_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"))
dev.off()
})
.composite({


#Persistent Hotspots
hs_suncor_all<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_all.jpeg")
hs_suncor_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_benzene.jpeg")
hs_suncor_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_toluene.jpeg")
hs_suncor_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_trimethylbenzene.jpeg")
hs_suncor_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_xylene.jpeg")
hs_suncor_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_h2s.jpeg")
hs_suncor_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Hotspots_persistent_hcn.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_suncorw.jpeg",  res=800, width=5500, height=6500)
cowplot::plot_grid(
  rasterGrob(hs_suncor_all),
  rasterGrob(hs_suncor_benzene),
  rasterGrob(hs_suncor_toluene),
    rasterGrob(hs_suncor_xylene),
  rasterGrob(hs_suncor_trimethylbenzene),
  rasterGrob(hs_suncor_h2s),
  #rasterGrob(hs_suncor_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
.composite({

#Hotspots
hs_suncor_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillips_Benzene.jpeg")
hs_suncor_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillips_Toluene.jpeg")
hs_suncor_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillips_Trimethylbenzene.jpeg")
hs_suncor_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillips_Xylene.jpeg")
hs_suncor_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillips_H2S.jpeg")
hs_suncor_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillips_HCN.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/hs_suncor.jpeg",  res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(hs_suncor_benzene),
  rasterGrob(hs_suncor_toluene),
  rasterGrob(hs_suncor_trimethylbenzene),
  rasterGrob(hs_suncor_xylene),
  rasterGrob(hs_suncor_h2s),
  rasterGrob(hs_suncor_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"))
dev.off()
})
.composite({

#Zoomed into Suncor
hs_suncor_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillipszoom_Benzene.jpeg")
hs_suncor_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillipszoom_Toluene.jpeg")
hs_suncor_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillipszoom_Trimethylbenzene.jpeg")
hs_suncor_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillipszoom_Xylene.jpeg")
hs_suncor_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillipszoom_H2S.jpeg")
hs_suncor_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorPhillipszoom_HCN.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/hs_suncorzoom.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(hs_suncor_benzene),
  rasterGrob(hs_suncor_toluene),
  rasterGrob(hs_suncor_trimethylbenzene),
  rasterGrob(hs_suncor_xylene),
  rasterGrob(hs_suncor_h2s),
  rasterGrob(hs_suncor_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale = 0.98)
dev.off()
})
.composite({

#https://stackoverflow.com/questions/52175766/draw-border-around-certain-rows-using-cowplot-and-ggplot2

#Zoomed into Suncor & Sinclair terminal
hs_suncor_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorandPhillips_zoomSinclair_Benzene.jpeg")
hs_suncor_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorandPhillips_zoomSinclair_Toluene.jpeg")
hs_suncor_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorandPhillips_zoomSinclair_Trimethylbenzene.jpeg")
hs_suncor_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorandPhillips_zoomSinclair_Xylene.jpeg")
hs_suncor_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorandPhillips_zoomSinclair_H2S.jpeg")
hs_suncor_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HS_SuncorandPhillips_zoomSinclair_HCN.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/hs_suncorzoom_Sinclair.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(hs_suncor_benzene),
  rasterGrob(hs_suncor_toluene),
  rasterGrob(hs_suncor_trimethylbenzene),
  rasterGrob(hs_suncor_xylene),
  rasterGrob(hs_suncor_h2s),
  rasterGrob(hs_suncor_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"))
dev.off()
})
.composite({

#Persistent Hotspots
hs_suncor_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_benzene.jpeg")
hs_suncor_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_toluene.jpeg")
hs_suncor_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_trimethylbenzene.jpeg")
hs_suncor_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_xylene.jpeg")
hs_suncor_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_h2s.jpeg")
hs_suncor_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_hcn.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/hs_persistent_color.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(hs_suncor_benzene),
  rasterGrob(hs_suncor_toluene),
  rasterGrob(hs_suncor_trimethylbenzene),
  rasterGrob(hs_suncor_xylene),
  rasterGrob(hs_suncor_h2s),
  rasterGrob(hs_suncor_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.9)
dev.off()
})
.composite({

#Polarplot Suncor
suncor_pp_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_benzene.jpeg")
suncor_pp_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_toluene.jpeg")
suncor_pp_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_trimethylbenzene.jpeg")
suncor_pp_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_xylene.jpeg")
suncor_pp_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_h2s.jpeg")
suncor_pp_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/polarPlot_suncor_hcn.jpeg")

# EDIT 2026-08-24 (SI Figure S3.9):
# (1) Panel order corrected to match the caption — C) trimethylbenzene,
#     D) xylene. The previous order (xylene third, trimethylbenzene fourth)
#     contradicted the Figure S3.9 caption.
# (2) label_size 9 pt on a full-size canvas — on the old 1500x2000 canvas
#     cowplot's default 14 pt labels rendered enormous relative to the panels.
# (3) The openair panel jpegs are 5400x3000 with ~950 px of white on each
#     side, which left the composite mostly whitespace. Each panel is trimmed
#     to its content (plus a 10 px guard), given a 10% white band on top for
#     the cowplot label, and the canvas height is computed from the trimmed
#     aspect so the 2 x 3 grid fills the page.
trim_white <- function(img, thr = 0.98, pad = 10) {
  g  <- (img[,,1] + img[,,2] + img[,,3]) / 3
  nz <- g < thr
  # MASKING FIX (2026-09-22): apply() is an S4 generic under terra/raster
  # with no matrix method when this runs inside the figure driver.
  rr <- range(which(base::apply(nz, 1, any)))
  cc <- range(which(base::apply(nz, 2, any)))
  img[max(1, rr[1] - pad):min(nrow(g), rr[2] + pad),
      max(1, cc[1] - pad):min(ncol(g), cc[2] + pad), , drop = FALSE]
}
pad_top_white <- function(img, frac = 0.10) {
  ph  <- max(1L, round(dim(img)[1] * frac))
  out <- array(1, dim = c(dim(img)[1] + ph, dim(img)[2], dim(img)[3]))
  out[(ph + 1):dim(out)[1], , ] <- img
  out
}
pp_trim <- lapply(
  list(suncor_pp_benzene, suncor_pp_toluene, suncor_pp_trimethylbenzene,
       suncor_pp_xylene, suncor_pp_h2s, suncor_pp_hcn),
  function(x) pad_top_white(trim_white(x)))
pp_W <- 5000
pp_H <- round(3 * (pp_W / 2) * dim(pp_trim[[1]])[1] / dim(pp_trim[[1]])[2])
jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor_PolarPlot_openairmaps.jpeg", res=800, width=pp_W, height=pp_H)
cowplot::plot_grid(
    plotlist = lapply(pp_trim, rasterGrob),
    ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"),
    label_size = 9, scale=0.99)
dev.off()
})
.composite({

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor_PolarPlot_openairmaps_vertical.jpeg", res=800, width=3500, height=4100)
cowplot::plot_grid(
  rasterGrob(suncor_pp_benzene),
  rasterGrob(suncor_pp_toluene),
  rasterGrob(suncor_pp_trimethylbenzene),
  rasterGrob(suncor_pp_xylene),
  rasterGrob(suncor_pp_h2s),
  rasterGrob(suncor_pp_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
.composite({

#Maps 500 m Suncor
suncor_map_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/median_median_benzene_500m.jpeg")
suncor_map_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/median_median_toluene_500m.jpeg")
suncor_map_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/median_median_trimethylbenzene_500m.jpeg")
suncor_map_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/median_median_xylene_500m.jpeg")
suncor_map_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/median_median_h2s_500m.jpeg")
suncor_map_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/median_median_hcn_500m.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Map500m_main.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(suncor_map_toluene),
  rasterGrob(suncor_map_h2s),
ncol=1, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
.composite({

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Map500m_SI.jpeg", res=800, width=5000, height=4000)
cowplot::plot_grid(
  rasterGrob(suncor_map_benzene),
    rasterGrob(suncor_map_xylene),
  rasterGrob(suncor_map_trimethylbenzene),
  rasterGrob(suncor_map_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
.composite({

#Census blocks
toxscreen_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/airtoxscreen_benzene.jpeg")
mobile_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/mobile_benzene_block.jpeg")
toxscreen_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/airtoxscreen_toluene.jpeg")
mobile_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/mobile_toluene_block.jpeg")
toxscreen_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/airtoxscreen_xylene.jpeg")
mobile_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/mobile_xylene_block.jpeg")

library(grid)
library(cowplot)
library(ragg)

# Combine panels
p_combined <- cowplot::plot_grid(
  rasterGrob(toxscreen_benzene),
  rasterGrob(mobile_benzene),
  rasterGrob(toxscreen_toluene),
  rasterGrob(mobile_toluene),
  rasterGrob(toxscreen_xylene),
  rasterGrob(mobile_xylene),
  ncol = 2,
  labels = c("A)", "B)", "C)", "D)", "E)", "F)"),
  label_size = 10,          # adjust if labels too large
  label_fontface = "bold",
  scale = 0.99
)

# Save with ragg (sharper than base jpeg)
out_file <- "/Users/priyanka/Downloads/Suncor/FinalFig/toxscreen_mobile_block.jpeg"

ragg::agg_jpeg(
  filename = out_file,
  width = 5000,
  height = 6000,
  units = "px",
  res = 800,
  quality = 95,
  background = "white"
)

print(p_combined)
dev.off()
})
.composite({

message("Saved: ", out_file)
#Stable
#suncor_map_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/gen_benzene_500m_stable.jpeg")
suncor_map_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Benzene_500m.jpeg")
suncor_map_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Toluene_500m.jpeg")
suncor_map_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Trimethylbenzene_500m.jpeg")
suncor_map_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Xylene_500m.jpeg")
suncor_map_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/H2S_500m.jpeg")
suncor_map_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/HCN_500m.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor500m/Map500m_Suncor_all.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(suncor_map_benzene),
  rasterGrob(suncor_map_toluene),
    rasterGrob(suncor_map_xylene),
  rasterGrob(suncor_map_trimethylbenzene),
  rasterGrob(suncor_map_h2s),
  rasterGrob(suncor_map_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
.composite({


#Maps 100m
suncor_map_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_benzene.jpeg")
suncor_map_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_toluene.jpeg")
suncor_map_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_trimethylbenzene.jpeg")
suncor_map_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_xylene.jpeg")
suncor_map_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_h2s.jpeg")
suncor_map_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_hcn.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Map100m_Suncor.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(suncor_map_benzene),
  rasterGrob(suncor_map_toluene),
  rasterGrob(suncor_map_trimethylbenzene),
  rasterGrob(suncor_map_xylene),
  rasterGrob(suncor_map_h2s),
  rasterGrob(suncor_map_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
.composite({

#Maps 100m Stable
suncor_map_benzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_benzene_stable.jpeg")
suncor_map_toluene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_toluene_stable.jpeg")
suncor_map_trimethylbenzene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_trimethylbenzene_stable.jpeg")
suncor_map_xylene<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_xylene_stable.jpeg")
suncor_map_h2s<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_h2s_stable.jpeg")
suncor_map_hcn<-readJPEG("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Suncor_100mroute_hcn_stable.jpeg")

jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/Suncor100m/Map100m_Suncor_stable.jpeg", res=800, width=5000, height=6000)
cowplot::plot_grid(
  rasterGrob(suncor_map_benzene),
  rasterGrob(suncor_map_toluene),
  rasterGrob(suncor_map_trimethylbenzene),
  rasterGrob(suncor_map_xylene),
  rasterGrob(suncor_map_h2s),
  rasterGrob(suncor_map_hcn),
             ncol=2, labels=c("A)", "B)", "C)", "D)", "E)", "F)"), scale=0.99)
dev.off()
})
