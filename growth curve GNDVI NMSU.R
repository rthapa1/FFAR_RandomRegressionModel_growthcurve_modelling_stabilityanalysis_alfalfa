
setwd("/Users/rt485/Downloads/Helfer/NMSU/10_4/Normal_irrigation/new_GDD_2_28_2023/GNDVI/new_control_6_7/single_control_11_30_2023/11_16/growthcurve_all")
third_ET <- read.table("gv_GNDVI_third_new_ET", header = T)
#third_ET <- third_ET[,-4]
third_ET$Harvest <- "SITcut3"

fourth_ET <- read.table("gv_GNDVI_fourth_new_ET", header = T)
#fourth_ET <- fourth_ET[,-4]
fourth_ET$Harvest <- "SITcut4"

fifth_ET <- read.table("gv_GNDVI_fifth_new_ET", header = T)
#fifth_ET <- fifth_ET[,-4]
fifth_ET$Harvest <- "SITcut5"

seventh_ET <- read.table("gv_GNDVI_seventh_new_ET", header = T)
#seventh_ET <- seventh_ET[,-4]
seventh_ET$Harvest <- "SITcut7"

third_nor <- read.table("gv_GNDVI_third_new", header = T)
#third_nor <- third_nor[,-4]
third_nor$Harvest <- "NIcut3"

fourth_nor <- read.table("gv_GNDVI_fourth_new", header = T)
#fourth_nor <- fourth_nor[,-4]
fourth_nor$Harvest <- "NIcut4"

fifth_nor <- read.table("gv_GNDVI_fifth_new", header = T)
#fifth_nor <- fifth_nor[,-4]
fifth_nor$Harvest <- "NIcut5"

sixth_nor <- read.table("gv_GNDVI_sixth_new", header = T)
#sixth_nor <- sixth_nor[,-4]
sixth_nor$Harvest <- "NIcut6"

seventh_nor <- read.table("gv_GNDVI_seventh_new", header = T)
#seventh_nor <- seventh_nor[,-4]
seventh_nor$Harvest <- "NIcut7"
d <- rbind(third_ET,fourth_ET,fifth_ET,seventh_ET,third_nor,fourth_nor,fifth_nor,sixth_nor,seventh_nor)

#d <- rbind(fourth_ET,fifth_ET,seventh_ET,third_nor,fourth_nor,fifth_nor,sixth_nor,seventh_nor)
#d <- d[!d$cutting=="first",]
#par(mfrow=c(5,2))
head(d)

stable_G1 <- d[d$genotype=="4",]
dt <- stable_G1 

df.NDVI <- aggregate(ndvi ~ time + genotype, stable_G1, mean)
#df.NDVI$type <- "stable"
df.NDVI$Harvest <- "mean"

dt <- rbind(stable_G1,df.NDVI)


library(ggplot2)
dt$genotype <- as.factor(dt$genotype)
dt$Harvest <- factor(dt$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

#dt$cutting <- factor(dt$cutting, levels = c("third harvest (Early Termination)", "third harvest (Normal Irrigation)", "fourth harvest (Early Termination)", "fourth harvest (Normal Irrigation)", "fifth harvest (Early Termination)", "fifth harvest (Normal Irrigation)", "sixth harvest (Normal Irrigation)", "seventh harvest (Early Termination)", "seventh harvest (Normal Irrigation)", "mean"))
#dt$cutting <- factor(dt$cutting, levels = c("third harvest (Early Termination)", "third harvest (Normal Irrigation)", "fourth harvest (Early Termination)", "fourth harvest (Normal Irrigation)", "fifth harvest (Early Termination)", "fifth harvest (Normal Irrigation)", "sixth harvest (Normal Irrigation)", "seventh harvest (Early Termination)", "seventh harvest (Normal Irrigation)"))



p1 <- ggplot(dt, aes(x = time, y = ndvi, col = Harvest))
p1 <- p1  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")
p1 <- p1 +  ggtitle("\n\nStable line (G4)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
p1 <- p1 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G2 <- d[d$genotype=="5",]


df.NDVI2 <- aggregate(ndvi ~ time + genotype, stable_G2, mean)
#df.NDVI2$type <- "stable"
df.NDVI2$Harvest <- "mean"

dt2 <- rbind(stable_G2,df.NDVI2)


library(ggplot2)
dt2$genotype <- as.factor(dt2$genotype)
dt2$Harvest <- factor(dt2$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

p2 <- ggplot(dt2, aes(x = time, y = ndvi, col = Harvest))
p2 <- p2  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.0001, 0.00021)
p2 <- p2 +  ggtitle("Stable line (G5)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
p2 <- p2 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G3 <- d[d$genotype=="9",]


df.NDVI3 <- aggregate(ndvi ~ time + genotype, stable_G3, mean)
#df.NDVI3$type <- "stable"
df.NDVI3$Harvest <- "mean"

dt3 <- rbind(stable_G3,df.NDVI3)


library(ggplot2)
dt3$genotype <- as.factor(dt3$genotype)
dt3$Harvest <- factor(dt3$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))


p3 <- ggplot(dt3, aes(x = time, y = ndvi, col = Harvest))
p3 <- p3  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.00045, 0.000165)
p3 <- p3 +  ggtitle("Stable line (G9)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
p3 <- p3 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G4 <- d[d$genotype=="14",]


df.NDVI4 <- aggregate(ndvi ~ time + genotype, stable_G4, mean)
#df.NDVI4$type <- "stable"
df.NDVI4$Harvest <- "mean"

dt4 <- rbind(stable_G4,df.NDVI4)


library(ggplot2)
dt4$genotype <- as.factor(dt4$genotype)
dt4$Harvest <- factor(dt4$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))


p4 <- ggplot(dt4, aes(x = time, y = ndvi, col = Harvest))
p4 <- p4  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.00015, 0.00015)
p4 <- p4 +  ggtitle("Stable line (G14)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
p4 <- p4 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G5 <- d[d$genotype=="25",]


df.NDVI5 <- aggregate(ndvi ~ time + genotype, stable_G5, mean)
#df.NDVI5$type <- "stable"
df.NDVI5$Harvest <- "mean"

dt5 <- rbind(stable_G5,df.NDVI5)


library(ggplot2)
dt5$genotype <- as.factor(dt5$genotype)
dt5$Harvest <- factor(dt5$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

p5 <- ggplot(dt5, aes(x = time, y = ndvi, col = Harvest))
p5 <- p5  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.00009, 0.0003)
p5 <- p5 +  ggtitle("Stable line (G25)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
p5 <- p5 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")



ustable_G1 <- d[d$genotype=="1",]


udf.NDVI <- aggregate(ndvi ~ time + genotype, ustable_G1, mean)
#udf.NDVI$type <- "unstable"
udf.NDVI$Harvest <- "mean"

udt <- rbind(ustable_G1,udf.NDVI)


library(ggplot2)
udt$genotype <- as.factor(udt$genotype)
udt$Harvest <- factor(udt$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up1 <- ggplot(udt, aes(x = time, y = ndvi, col = Harvest))
up1 <- up1  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit") 
up1 <- up1 +  ggtitle("Unstable line (G1)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
up1 <- up1 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))

ustable_G2 <- d[d$genotype=="11",]


udf.NDVI2 <- aggregate(ndvi ~ time + genotype, ustable_G2, mean)
#udf.NDVI2$type <- "unstable"
udf.NDVI2$Harvest <- "mean"

udt2 <- rbind(ustable_G2,udf.NDVI2)

# 
library(ggplot2)
udt2$genotype <- as.factor(udt2$genotype)
udt2$Harvest <- factor(udt2$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))


up2 <- ggplot(udt2, aes(x = time, y = ndvi, col = Harvest))
up2 <- up2 + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.0001, 0.00021)
up2 <- up2 +  ggtitle("Unstable line (G11)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
up2 <- up2 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G3 <- d[d$genotype=="17",]


udf.NDVI3 <- aggregate(ndvi ~ time + genotype, ustable_G3, mean)
#udf.NDVI3$type <- "unstable"
udf.NDVI3$Harvest <- "mean"

udt3 <- rbind(ustable_G3,udf.NDVI3)


library(ggplot2)
udt3$genotype <- as.factor(udt3$genotype)
udt3$Harvest <- factor(udt3$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up3 <- ggplot(udt3, aes(x = time, y = ndvi, col = Harvest))
up3 <- up3 + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.00045, 0.000165)
up3 <- up3 +  ggtitle("Unstable line (G17)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
up3 <- up3 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G4 <- d[d$genotype=="21",]


udf.NDVI4 <- aggregate(ndvi ~ time + genotype, ustable_G4, mean)
#udf.NDVI4$type <- "unstable"
udf.NDVI4$Harvest <- "mean"

udt4 <- rbind(ustable_G4,udf.NDVI4)


library(ggplot2)
udt4$genotype <- as.factor(udt4$genotype)

udt4$Harvest <- factor(udt4$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))


up4 <- ggplot(udt4, aes(x = time, y = ndvi, col = Harvest))
up4 <- up4  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.00015, 0.00015)
up4 <- up4 +  ggtitle("Unstable line (G21)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
up4 <- up4 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G5 <- d[d$genotype=="23",]


udf.NDVI5 <- aggregate(ndvi ~ time + genotype, ustable_G5, mean)
#udf.NDVI5$type <- "unstable"
udf.NDVI5$Harvest <- "mean"

udt5 <- rbind(ustable_G5,udf.NDVI5)


library(ggplot2)
udt5$genotype <- as.factor(udt5$genotype)

udt5$Harvest <- factor(udt5$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))


up5 <- ggplot(udt5, aes(x = time, y = ndvi, col = Harvest))
up5 <- up5  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic merit")+  ylim(-0.00009, 0.0003)
up5 <- up5 +  ggtitle("Unstable line (G23)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1", "gray", "sandybrown", "royalblue", "tan1")) 
up5 <- up5 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


library(egg)

arranged_plot <- ggarrange(p1, up1,p2, up2,p3,up3, p4, up4,p5,up5, ncol = 2)
ggsave("arranged_plot2.png", arranged_plot, width = 24, height = 11, units = "in")
