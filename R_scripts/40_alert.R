# ==============================================================
# 40  Alert
# Auto-split from Suncor.Rmd  (section 40 of 40)
# ==============================================================

#Alert

load("/Users/priyanka/Downloads/Suncor/lacasa_pbl.RData")
alerts<-read.csv("/Users/priyanka/Downloads/Suncor/Suncor_alerts.csv")
alerts$Date<-dmy_hm(alerts$Date)

load("/Users/priyanka/Downloads/Suncor/mobile_wswd.RData")
df<-df[df$Site!="Goodrich Corporation (Collins Aerospace)",]
df<-df[df$Site!="Holly Energy Partners (Sinclair) Terminal",]
df$HCN<-ifelse(df$date> "2025-01-22 00:00:00", df$HCN, NA)

df$day<-as.Date(df$date)
df<- df %>% dplyr::group_by(day, Site) %>% dplyr::arrange(date) %>% dplyr::ungroup()

df<-subset(df, df$date<= max(alerts$Date))

 p_benzene <- ggplot(df, aes(x = date, y = Benzene)) +
      geom_point() + geom_vline(data = alerts, aes(xintercept = Date),
                   color = "red", linetype = "dashed" )+theme_bw()
 p_toluene<- ggplot(df, aes(x = date, y = Toluene)) +
      geom_point() + geom_vline(data = alerts, aes(xintercept = Date),
                   color = "red", linetype = "dashed")+theme_bw()
  p_trimethylbenzene<- ggplot(df, aes(x = date, y = Trimethylbenzene)) +
      geom_point() + geom_vline(data = alerts, aes(xintercept = Date),
                   color = "red", linetype = "dashed")+theme_bw()
  p_xylene<- ggplot(df, aes(x = date, y = Xylene)) +
      geom_point() + geom_vline(data = alerts, aes(xintercept = Date),
                   color = "red", linetype = "dashed")+theme_bw()
    p_h2s<- ggplot(df, aes(x = date, y = H2S)) +
      geom_point() + geom_vline(data = alerts, aes(xintercept = Date),
                   color = "red", linetype = "dashed")+theme_bw()
        p_hcn<- ggplot(df, aes(x = date, y = HCN)) +
      geom_point() + geom_vline(data = alerts, aes(xintercept = Date),
                   color = "red", linetype = "dashed")+theme_bw()
        
jpeg("/Users/priyanka/Downloads/Suncor/FinalFig/alerts.jpeg", width=6500, height=6000, res=600)
cowplot::plot_grid(p_benzene, p_toluene, p_xylene, p_trimethylbenzene, p_h2s)
dev.off()
