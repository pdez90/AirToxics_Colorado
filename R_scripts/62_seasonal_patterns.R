# ==============================================================
# 62  SEASONAL PATTERNS — concentrations + hotspot activity
# Seasonal (DJF/MAM/JJA/SON) distributions of daily medians per
# pollutant, and the seasonal distribution of high-concentration
# (>p99) events, from the delay-corrected mobile record.
# Outputs: TABLE_seasonal.csv, FinalFig/FIG_seasonal.png
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(scales) })
BASE <- "/Users/priyanka/Downloads/Suncor"
load(file.path(BASE,"mobile_wswd.RData")); df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]
df[, day := as.Date(date)]
df[, season := factor(c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA",
                        "SON","SON","SON","DJF")[month(day)],
                      levels=c("DJF","MAM","JJA","SON"))]
POLLS <- c(Benzene="Benzene_ppb", Toluene="Toluene_ppb",
           Trimethylbenzene="Trimethylbenzene_ppb", Xylene="Xylene_ppb",
           H2S="Hydrogen_Sulfide_ppb", HCN="Hydrogen_Cyanide_ppb")
daily <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  df[is.finite(get(col)), .(pollutant=pn, dmed=median(get(col)),
                            dp95=quantile(get(col),.95)), by=.(day, season)]
}))
tab <- daily[, .(n_days=.N, med=round(median(dmed),3),
                 p95=round(median(dp95),2)), by=.(pollutant, season)]
setorder(tab, pollutant, season)
# seasonal share of >p99 events
ev <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]; v <- df[[col]]
  thr <- quantile(v[is.finite(v)], .99)
  e <- df[is.finite(get(col)) & get(col) > thr, .N, by=season]
  a <- df[is.finite(get(col)), .N, by=season]
  m <- merge(e, a, by="season", suffixes=c("_ev","_all"))
  m[, `:=`(pollutant=pn, ev_rate_pct = round(100*N_ev/N_all, 2))]
  m
}))
tab <- merge(tab, ev[,.(pollutant, season, ev_rate_pct)],
             by=c("pollutant","season"), all.x=TRUE)
fwrite(tab, file.path(BASE,"TABLE_seasonal.csv")); print(tab)
message("Sampling days per season: ",
        paste(capture.output(print(table(unique(df[,.(day,season)])$season))), collapse=" "))
p <- ggplot(daily, aes(season, dmed, fill=season)) +
  geom_boxplot(outlier.size=0.5, linewidth=0.3, show.legend=FALSE) +
  facet_wrap(~pollutant, scales="free_y") +
  scale_fill_brewer(palette="Paired") +
  labs(x=NULL, y="Daily median concentration (ppb)",
       caption="Each point in a box is one sampling day's campaign-wide median. HCN is available from January 22, 2025 only (DJF/MAM 2025).") +
  theme_bw(base_size=11) + theme(plot.caption=element_text(size=8.5, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_seasonal.png"), p,
       width=10, height=6.2, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_seasonal.png  DONE.")
