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
  # query the CDPHE FeatureServer directly (paginated attribute-only JSON)
  suppressPackageStartupMessages(library(jsonlite))
  svc <- paste0("https://services3.arcgis.com/66aUo8zsujfVXRIT/arcgis/rest/",
                "services/EJP_Main_English_v1_BlockGroup_20230803/",
                "FeatureServer/0/query")
  message("Querying CDPHE EnviroScreen FeatureServer (paginated)...")
  off <- 0; chunks <- list()
  repeat {
    u <- paste0(svc, "?where=1%3D1&outFields=*&returnGeometry=false&f=json",
                "&resultOffset=", off, "&resultRecordCount=2000")
    j <- tryCatch(fromJSON(u), error = function(e) NULL)
    if (is.null(j) || is.null(j$features) || length(j$features) == 0) break
    chunks[[length(chunks) + 1]] <- as.data.table(j$features$attributes)
    n <- nrow(j$features$attributes)
    message("  fetched ", off + n, " block groups")
    if (n < 2000) break
    off <- off + n
  }
  if (length(chunks) == 0)
    stop("FeatureServer query failed. Download the CSV manually from the ",
         "CDPHE open-data portal (ColoradoEnviroScreen v2 BlockGroup) and ",
         "save as ", esf)
  es_all <- rbindlist(chunks, fill = TRUE)
  fwrite(es_all, esf)
  message("Cached ", nrow(es_all), " block groups to ", esf)
}
es <- fread(esf)
# field names from the FeatureServer schema (aliases confirmed):
#   ES_S_  = "EnviroScreen Percentile Score"
#   EnS_S  = "EnviroScreen Score"
#   DIType = "Nov 2024 DI Community Type"
gcol <- "GEOID"; scol <- "ES_S_"; dcol <- "DIType"
stopifnot(all(c(gcol, scol, dcol) %in% names(es)))
message("DIType values: ",
        paste(capture.output(print(table(es[[dcol]], useNA="ifany"))), collapse=" "))
es <- es[, .(bg = sprintf("%012.0f", as.numeric(get(gcol))),
             es_pctl = as.numeric(get(scol)),
             di = as.character(get(dcol)))]

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
# "N/A" in DIType means the block group is NOT a designated DI community
d[, is_di := !is.na(di) & di != "" & !grepl("^N/?A$|^Not", di, ignore.case=TRUE)]
res <- d[!is.na(grp) & !is.na(es_pctl),
         .(blocks=.N, population=sum(pop, na.rm=TRUE),
           median_enviroscreen_pctl = round(median(es_pctl),1),
           pct_in_DI = round(100*mean(is_di),1)), by=grp]
wt <- wilcox.test(es_pctl ~ grp, data=d[!is.na(grp) & !is.na(es_pctl)])
res[, p_wilcoxon := signif(wt$p.value, 3)]
fwrite(res, file.path(BASE,"TABLE_ej_overlay.csv")); print(res)
print(d[!is.na(grp), .N, by=.(grp, di)][order(grp, -N)])
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
