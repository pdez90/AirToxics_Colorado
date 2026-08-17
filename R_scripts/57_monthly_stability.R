# ==============================================================
# 57  INSTRUMENT-STABILITY (DRIFT PROXY) — monthly stats per van
# Monthly median and p95 per pollutant PER VAN across the campaign.
# Stable monthly medians (especially for background-dominated
# species) argue against instrument drift; step changes should
# align with audit-period boundaries (MDL changes, Table S1.2).
# Outputs: TABLE_monthly_stability.csv, FinalFig/FIG_monthly_stability.png
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(scales) })
BASE <- "/Users/priyanka/Downloads/Suncor"
load(file.path(BASE, "mobile_wswd.RData")); df <- as.data.table(out); rm(out); gc()
df <- df[Site != "Goodrich Corporation (Collins Aerospace)"]
df[, `:=`(month = as.Date(cut(as.Date(date), "month")),
          van = toupper(trimws(as.character(Asset))))]
POLLS <- c(Benzene="Benzene_ppb", Toluene="Toluene_ppb",
           Trimethylbenzene="Trimethylbenzene_ppb", Xylene="Xylene_ppb",
           H2S="Hydrogen_Sulfide_ppb", HCN="Hydrogen_Cyanide_ppb")
ms <- rbindlist(lapply(names(POLLS), function(pn) {
  col <- POLLS[[pn]]
  df[is.finite(get(col)) & van %in% c("CAT","EMU"),
     .(pollutant=pn, n=.N, median=median(get(col)),
       p95=quantile(get(col),.95)), by=.(month, van)]
}))
ms <- ms[n >= 1000]     # skip fragmentary months
fwrite(ms, file.path(BASE, "TABLE_monthly_stability.csv"))
print(dcast(ms[pollutant=="Benzene"], month ~ van, value.var="median"))
# audit boundaries to display (MDL change points, Table S1.2)
bounds <- as.Date(c("2024-10-01", "2025-01-01", "2024-04-01", "2024-07-01"))
p <- ggplot(ms, aes(month, median, color=van)) +
  geom_vline(xintercept=bounds, linetype=3, color="grey60", linewidth=0.3) +
  geom_line(linewidth=0.6) + geom_point(size=1.4) +
  geom_line(aes(y=p95), linetype=2, linewidth=0.4) +
  facet_wrap(~pollutant, scales="free_y") +
  scale_color_manual(values=c(CAT="#2166ac", EMU="#b2182b"), name=NULL) +
  labs(x=NULL, y="Monthly median (solid) and p95 (dashed), ppb",
       caption="Dotted verticals: audit-period boundaries where MDLs changed (Table S1.2). Months with <1,000 valid observations omitted.") +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", plot.caption=element_text(size=8.5, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_monthly_stability.png"), p,
       width=10.5, height=6.5, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_monthly_stability.png  DONE.")
