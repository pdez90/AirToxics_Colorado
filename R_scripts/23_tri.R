# ==============================================================
# 23  TRI
# Auto-split from Suncor.Rmd  (section 23 of 40)
# ==============================================================

#TRI

tri<-read.csv("/Users/priyanka/Downloads/Suncor/TRI.csv")
tri<-tri[!duplicated(tri[,c(4,5)]),]
write.csv(tri, file="/Users/priyanka/Downloads/Suncor/TRI_subset.csv")
