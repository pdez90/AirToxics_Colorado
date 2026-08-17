# ==============================================================
# 61  LA CASA CPF — independent fixed-site source-direction check
# Conditional probability function at the La Casa stationary site:
# P(concentration > site p90 | wind sector), 16 sectors, using
# La Casa's own wind data (ascent files; ws > 1 m/s). If high
# stationary readings preferentially occur under winds from the
# industrial-corridor sector, that independently corroborates the
# mobile back-projection (Figure 3) with a fixed site.
# Bearings from La Casa: Suncor ~63 deg (ENE), Sinclair ~40 deg,
# Phillips 66 ~52 deg (computed below and printed).
# Outputs: TABLE_lacasa_cpf.csv, FinalFig/FIG_lacasa_cpf.png
# ==============================================================
suppressPackageStartupMessages({ library(data.table); library(lubridate); library(ggplot2); library(scales) })
BASE <- "/Users/priyanka/Downloads/Suncor"
cn12 <- c("date_mst","date_mst1","date","date_mdt","benzene","toluene",
          "xylene","wd","ws","temp_far","temp_c","rh")
rd <- function(f, parser) { x <- read.csv(file.path(BASE,f), stringsAsFactors=FALSE)
  colnames(x) <- cn12; x$date <- parser(x$date); x }
lc <- rbindlist(list(rd("ascent_2023.csv", dmy_hm), rd("ascent_2024.csv", dmy_hm)))
lc <- as.data.table(lc)[is.finite(wd) & is.finite(ws) & ws > 1]
message("La Casa rows with valid wind, ws>1: ", format(nrow(lc), big.mark=","))

# bearings La Casa -> key facilities
lacasa <- c(39.7794, -105.0052)
fac <- data.table(name=c("Suncor","Sinclair","Phillips 66", "WWTF1"),
                  lat=c(39.803333, 39.8724, 39.79668, 39.80822838),
                  lon=c(-104.945556, -104.8861, -104.94236, -104.95532469))
fac[, bearing := (atan2((lon-lacasa[2])*cos(lacasa[1]*pi/180),
                        lat-lacasa[1]) * 180/pi) %% 360]
print(fac[, .(name, bearing=round(bearing))])

sect <- 22.5
lc[, sector := floor(((wd + sect/2) %% 360)/sect)]
cpf <- rbindlist(lapply(c("benzene","toluene","xylene"), function(poll) {
  v <- lc[[poll]]
  fin <- is.finite(v)
  thr <- quantile(v[fin], .90)
  s <- lc[fin, .(n=.N, n_high=sum(get(poll) > thr)), by=sector]
  s[, `:=`(pollutant=poll, cpf=n_high/n, thr=thr)]
  s
}))
fwrite(cpf, file.path(BASE,"TABLE_lacasa_cpf.csv"))
print(dcast(cpf, sector ~ pollutant, value.var="cpf"))
cpf[, mid_deg := sector*sect]
p <- ggplot(cpf, aes(factor(sector, levels=0:15), cpf)) +
  geom_col(fill="#4292c6", color="grey25", linewidth=0.2, width=0.95) +
  geom_vline(data=data.frame(x=fac$bearing/sect + 1),
             aes(xintercept=x), color="red", linetype=2, linewidth=0.4) +
  coord_polar(start=-pi/16) +
  scale_x_discrete(labels=c("N","","NE","","E","","SE","","S","","SW","","W","","NW","")) +
  facet_wrap(~pollutant) +
  labs(x=NULL, y="P(> site p90 | wind sector)",
       caption="Red dashed radials: bearings from La Casa to Sinclair (~40 deg), Phillips 66 (~52 deg), Suncor (~63 deg), and WWTF1. Winds > 1 m/s; La Casa's own meteorology.") +
  theme_bw(base_size=11) +
  theme(axis.text.y=element_blank(), plot.caption=element_text(size=8.5, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_lacasa_cpf.png"), p,
       width=10, height=4.6, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_lacasa_cpf.png  DONE.")
