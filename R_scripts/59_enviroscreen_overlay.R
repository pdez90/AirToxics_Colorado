# ==============================================================
# 59  ENVIRONMENTAL-JUSTICE OVERLAY — Colorado EnviroScreen v2
# Joins the census-block benzene ratios (mobile/AirToxScreen) to
# Colorado EnviroScreen v2 block-group scores (CDPHE) and asks:
# are the blocks where mobile exceeds AirToxScreen located in more
# environmentally burdened / disproportionately impacted (DI)
# communities?
# Data: tries the CDPHE ArcGIS open-data CSV endpoint; if the
# download fails, place the CSV manually at
#   <BASE>/enviroscreen_v2_blockgroup.csv
# (from https://data-cdphe.opendata.arcgis.com -> "ColoradoEnviroScreen
#  v2 BlockGroup" -> Download -> CSV) and rerun.
# Outputs: TABLE_ej_overlay.csv, FinalFig/FIG_ej_overlay.png
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2); library(scales) })
BASE <- "/Users/priyanka/Downloads/Suncor"
esf <- file.path(BASE, "enviroscreen_v2_blockgroup.csv")
if (!file.exists(esf)) {
  url <- paste0("https://opendata.arcgis.com/api/v3/datasets/",
                "218578b0946a44aab0e460343c88069c_0/downloads/data",
                "?format=csv&spatialRefId=4326")
  message("Downloading Colorado EnviroScreen v2 block-group CSV...")
  ok <- tryCatch({ download.file(url, esf, mode="wb", quiet=TRUE); TRUE },
                 error=function(e) FALSE, warning=function(w) FALSE)
  if (!ok || file.size(esf) < 1e5) {
    unlink(esf)
    stop("Automatic download failed. Download the CSV manually from the CDPHE ",
         "open-data portal (ColoradoEnviroScreen v2 BlockGroup) and save as ",
         esf)
  }
}
es <- fread(esf)
message("EnviroScreen columns: ", paste(head(names(es), 40), collapse=", "))
gcol <- grep("GEOID", names(es), value=TRUE, ignore.case=TRUE)[1]
scol <- grep("EnviroScreen.*P|EnviroScreenPctl|EnviroScreen_Score_P|Pctl",
             names(es), value=TRUE, ignore.case=TRUE)[1]
dcol <- grep("^DI|Disproportion", names(es), value=TRUE, ignore.case=TRUE)[1]
message("Using: GEOID='", gcol, "' | score pct='", scol, "' | DI flag='", dcol, "'")
stopifnot(!is.na(gcol), !is.na(scol))
es <- es[, .(bg = sprintf("%012.0f", as.numeric(get(gcol))),
             es_pctl = as.numeric(get(scol)),
             di = if (!is.na(dcol)) as.character(get(dcol)) else NA_character_)]

g <- st_read(file.path(BASE,"censusblocks_suncor_terminal_BINWEIGHTED_AB_COMMONBLOCKS.gpkg"), quiet=TRUE)
idcol <- grep("GEOID", names(g), value=TRUE)[1]
d <- as.data.table(st_drop_geometry(g))
d <- d[, .(block=as.character(get(idcol)), ats=benzene_ppb_airtox,
           mob=sBenzene_med_of_daily_med_scaled, pop=Population_airtox)]
d[, `:=`(bg = substr(block, 1, 12), ratio = fifelse(ats > 0, mob/ats, NA_real_))]
d <- merge(d, es, by="bg", all.x=TRUE)
message("Blocks joined to EnviroScreen: ", sum(!is.na(d$es_pctl)), " of ", nrow(d))
d[, grp := fifelse(ratio > 2, "Mobile >2x AirToxScreen",
            fifelse(is.finite(ratio), "Other blocks", NA_character_))]
res <- d[!is.na(grp) & !is.na(es_pctl),
         .(blocks=.N, population=sum(pop, na.rm=TRUE),
           median_enviroscreen_pctl = round(median(es_pctl),1),
           pct_in_DI = if (!all(is.na(di)))
             round(100*mean(di %in% c("1","TRUE","Yes","yes","Y"), na.rm=TRUE),1)
             else NA_real_), by=grp]
wt <- wilcox.test(es_pctl ~ grp, data=d[!is.na(grp) & !is.na(es_pctl)])
res[, p_wilcoxon := signif(wt$p.value, 3)]
fwrite(res, file.path(BASE,"TABLE_ej_overlay.csv")); print(res)
print(d[!is.na(grp), .N, by=.(grp, di)])
p <- ggplot(d[!is.na(grp) & !is.na(es_pctl)], aes(grp, es_pctl, fill=grp)) +
  geom_boxplot(width=0.5, outlier.size=0.6, show.legend=FALSE) +
  scale_fill_manual(values=c("Mobile >2x AirToxScreen"="#e34a33",
                             "Other blocks"="grey80")) +
  labs(x=NULL, y="Colorado EnviroScreen v2 percentile (block group)",
       caption=sprintf("Wilcoxon p = %.3g. EnviroScreen percentile: higher = greater cumulative environmental and social burden.", wt$p.value)) +
  theme_bw(base_size=12) + theme(plot.caption=element_text(size=9, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_ej_overlay.png"), p,
       width=6.8, height=5.2, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_ej_overlay.png  DONE.")
