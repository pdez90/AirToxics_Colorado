# ==============================================================
# 61  LA CASA CPF — independent fixed-site source-direction check
# Conditional probability function at the La Casa stationary site:
# P(concentration > site p90 | wind sector), 16 sectors, using
# La Casa's own wind data (ascent files; ws > 1 m/s). If high
# stationary readings preferentially occur under winds from the
# industrial-corridor sector, that independently corroborates the
# mobile back-projection (Figure 3) with a fixed site.
# Bearings from La Casa (computed below, printed, and written into the figure
# caption): Sinclair ~45 deg, WWTF1 ~53, Suncor ~62, Phillips 66 ~70. The
# caption previously carried hand-typed values (40/52/63) that did not match.
# Outputs: TABLE_lacasa_cpf.csv, FinalFig/FIG_lacasa_cpf.png
# ==============================================================
SUNCOR_BASE <- path.expand(Sys.getenv("SUNCOR_BASE", "~/Downloads/Suncor"))  # analysis root; override with the env var
suppressPackageStartupMessages({ library(data.table); library(lubridate); library(ggplot2); library(scales) })
BASE <- SUNCOR_BASE
cn12 <- c("date_mst","date_mst1","date","date_mdt","benzene","toluene",
          "xylene","wd","ws","temp_far","temp_c","rh")
# TIME CONVENTION (2026-09-22): the ascent files carry FOUR time columns —
# 1: MST clock, 2: MST as YYYYMMDDhhmmss, 3: MDT clock, 4: MDT as YYYYMMDDhhmmss.
# Column 3 was being used as `date`. In ascent_2024.csv column 3 is one hour
# ahead of column 1 (it really is MDT), while the mobile record carries the MST
# wall clock, so the 2024 La Casa deployment was being compared one hour out.
# (ascent_2023.csv was delivered with columns 1 and 3 identical, both MST, so it
# was never affected.) Checked against EPA AQS resultant wind speed at the three
# Denver-area stations within 15 km: hourly correlation peaks at lag 0 for
# column 1 in both years (r = 0.94 in 2023, 0.96 in 2024) and at -1 h for
# column 3 in 2024. La Casa is therefore read from column 1 (MST) below.
rd <- function(f, parser) { x <- read.csv(file.path(BASE,f), stringsAsFactors=FALSE)
  colnames(x) <- cn12
  .mst <- parser(x$date_mst)
  .off <- as.numeric(difftime(parser(x$date), .mst, units = "hours"))
  stopifnot(all(is.na(.off) | .off %in% c(0, 1)))
  x$date <- .mst                            # MST clock, to match the mobile record
  x }
# The summer-2023 deployment is the Vocus Elf, whose benzene is not reported
# (MS 2.2: unit-mass-resolution interference; it averages ~1.8 ppb against ~0.16 ppb
# from the Vocus 2R in summer 2024). La Casa benzene therefore comes from the
# summer-2024 Vocus 2R only; toluene and xylene use all three deployments.
.lc23 <- rd("ascent_2023.csv", dmy_hm); .lc23$benzene <- NA_real_
lc <- rbindlist(list(.lc23, rd("ascent_2024.csv", dmy_hm)))
# The ascent files report wind speed in mph (WS_mph); convert before the 1 m/s cut.
lc <- as.data.table(lc)[, ws := ws * 0.44704][is.finite(wd) & is.finite(ws) & ws > 1]
message("La Casa rows with valid wind, ws > 1 m/s: ", format(nrow(lc), big.mark=","))

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
print(data.table::dcast(cpf, sector ~ pollutant, value.var="cpf"))
cpf[, mid_deg := sector*sect]
p <- ggplot(cpf, aes(factor(sector, levels=0:15), cpf)) +
  geom_col(fill="#4292c6", color="grey25", linewidth=0.2, width=0.95) +
  geom_vline(data=data.frame(x=fac$bearing/sect + 1),
             aes(xintercept=x), color="red", linetype=2, linewidth=0.4) +
  coord_polar(start=-pi/16) +
  # expand = 0 (2026-09-27): the default discrete expansion widened the x range
  # to [0.4, 16.6], so the radials and bars sat ~1-2 degrees off their bearings.
  scale_x_discrete(labels=c("N","","NE","","E","","SE","","S","","SW","","W","","NW",""),
                   expand = c(0, 0)) +
  facet_wrap(~pollutant) +
  labs(x=NULL, y="P(> site p90 | wind sector)",
       caption=paste0("Red dashed radials: bearings from La Casa to ",
                      paste(sprintf("%s (~%d deg)", fac$name[order(fac$bearing)],
                                    round(sort(fac$bearing))), collapse = ", "),
                      ".\nWinds > 1 m/s; La Casa's own meteorology.")) +
  theme_bw(base_size=11) +
  theme(axis.text.y=element_blank(), plot.caption=element_text(size=8.5, hjust=0))
ggsave(file.path(BASE,"FinalFig","FIG_lacasa_cpf.png"), p,
       width=10, height=4.6, dpi=400, bg="white")
message("[Saved] FinalFig/FIG_lacasa_cpf.png  DONE.")
