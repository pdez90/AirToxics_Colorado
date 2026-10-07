# ==============================================================
# 57  INSTRUMENT-STABILITY (DRIFT PROXY) — monthly stats per van
# Monthly median and p95 per pollutant PER VAN across the campaign.
# Stable monthly medians (especially for background-dominated
# species) argue against instrument drift; step changes should
# align with audit-period boundaries (MDL changes, Table S1.2).
# Outputs: TABLE_monthly_stability.csv, FinalFig/FIG_monthly_stability.png
# ==============================================================
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(scales) })
BASE <- SUNCOR_BASE
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
print(data.table::dcast(ms[pollutant=="Benzene"], month ~ van, value.var="median"))
# MDL change points per pollutant (Table S1.2), read from the audit MDL table:
# the first day of each quarter in which either vehicle's MDL differs from the
# previous quarter (a vehicle's first MDL, when it joins the campaign, is not a change).
mdl <- fread(file.path(BASE, "CDPHE_audit_MDLs.csv"))
mdl[, pollutant := ifelse(grepl("HCN", compound), "HCN", ifelse(grepl("H2S", compound), "H2S", compound))]
setorder(mdl, pollutant, from_ym)
mdl[, `:=`(chg = (!is.na(cat_mdl) & !is.na(data.table::shift(cat_mdl)) & cat_mdl != data.table::shift(cat_mdl)) |
                 (!is.na(emu_mdl) & !is.na(data.table::shift(emu_mdl)) & emu_mdl != data.table::shift(emu_mdl))), by = pollutant]
bounds <- mdl[chg == TRUE, .(pollutant, x = as.Date(sprintf("%d-%02d-01", from_ym %/% 100, from_ym %% 100)))]
stopifnot(all(bounds$pollutant %in% names(POLLS)))
print(bounds[, .(dates = paste(format(x, "%Y-%m"), collapse = ", ")), by = pollutant])
p <- ggplot(ms, aes(month, median, color=van)) +
  geom_vline(data=bounds, aes(xintercept=x), linetype=3, color="grey60", linewidth=0.3) +
  geom_line(linewidth=0.6) + geom_point(size=1.4) +
  geom_line(aes(y=p95), linetype=2, linewidth=0.4) +
  facet_wrap(~pollutant, scales="free_y",
             labeller=as_labeller(function(x) sub("^H2S$", "H\u2082S", x))) +   # display only
  scale_color_manual(values=c(CAT="#2166ac", EMU="#b2182b"), name=NULL) +
  labs(x=NULL, y="Monthly median (solid) and p95 (dashed), ppb",
       caption="Dotted verticals: start of each quarter in which that pollutant's MDL changed on either vehicle (Table S1.2). Months with <1,000 valid observations omitted.") +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", plot.caption=element_text(size=8.5, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_monthly_stability.png"), p,
       width=10.5, height=6.5, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_monthly_stability.png  DONE.")
