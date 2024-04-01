
setwd("/Users/rt485/Downloads/Helfer/NMSU/10_4/Normal_irrigation/new_GDD_2_28_2023/NDVI/single_control_11_30_2023/11_16/GV-stability_NDVI")

third_nor <- read.table("gv_NDVI_third_nor", header = T)
third_nor$Harvest <- "NIcut3"
third_nor$ndvi <- third_nor$ndvi*10

fourth_nor <- read.table("gv_NDVI_fourth_nor", header = T)
fourth_nor$Harvest <- "NIcut4"
fourth_nor$ndvi <- fourth_nor$ndvi*10

fifth_nor <- read.table("gv_NDVI_fifth_nor", header = T)
fifth_nor$Harvest <- "NIcut5"
fifth_nor$ndvi <- fifth_nor$ndvi*10

sixth_nor <- read.table("gv_NDVI_sixth_nor", header = T)
sixth_nor$Harvest <- "NIcut6"
sixth_nor$ndvi <- sixth_nor$ndvi*10

seventh_nor <- read.table("gv_NDVI_seventh_nor", header = T)
seventh_nor$Harvest <- "NIcut7"
seventh_nor$ndvi <- seventh_nor$ndvi*10

d <- rbind(third_nor,fourth_nor,fifth_nor,sixth_nor,seventh_nor)


third_ET <- read.table("gv_NDVI_thirdET", header = T)
third_ET$Harvest <- "SITcut3"
third_ET$ndvi <- third_ET$ndvi*10

fourth_ET <- read.table("gv_NDVI_fourthET", header = T)
fourth_ET$Harvest <- "SITcut4"
fourth_ET$ndvi <- fourth_ET$ndvi*10

fifth_ET <- read.table("gv_NDVI_fifthET", header = T)
fifth_ET$Harvest <- "SITcut5"
fifth_ET$ndvi <- fifth_ET$ndvi*10

seventh_ET <- read.table("gv_NDVI_seventhET", header = T)
seventh_ET$Harvest <- "SITcut7"
seventh_ET$ndvi <- seventh_ET$ndvi*10


d1 <- rbind(third_ET,fourth_ET,fifth_ET,seventh_ET)


stable_G1 <- d[d$genotype=="4",]


df.NDVI <- aggregate(ndvi ~ time + genotype, stable_G1, mean)
#df.NDVI$type <- "stable"
df.NDVI$Harvest <- "mean"

dt <- rbind(stable_G1,df.NDVI)


library(ggplot2)
dt$genotype <- as.factor(dt$genotype)
dt$Harvest <- factor(dt$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "mean"))

p1 <- ggplot(dt, aes(x = time, y = ndvi, col = Harvest))
p1 <- p1  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit") +  ylim(-0.02, 0.005)
p1 <- p1 +  ggtitle("Stable line (G4)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1")) 
p1 <- p1 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))

stable_G2 <- d[d$genotype=="5",]


df.NDVI2 <- aggregate(ndvi ~ time + genotype, stable_G2, mean)
#df.NDVI2$type <- "stable"
df.NDVI2$Harvest <- "mean"

dt2 <- rbind(stable_G2,df.NDVI2)


library(ggplot2)
dt2$genotype <- as.factor(dt2$genotype)
dt2$Harvest <- factor(dt2$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "mean"))

p2 <- ggplot(dt2, aes(x = time, y = ndvi, col = Harvest))
p2 <- p2  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.001, 0.0025)
p2 <- p2 +  ggtitle("Stable line (G5)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1")) 
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
dt3$Harvest <- factor(dt3$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "mean"))

p3 <- ggplot(dt3, aes(x = time, y = ndvi, col = Harvest))
p3 <- p3  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.003, 0.004)
p3 <- p3 +  ggtitle("Stable line (G9)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1")) 
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
dt4$Harvest <- factor(dt4$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "mean"))

p4 <- ggplot(dt4, aes(x = time, y = ndvi, col = Harvest))
p4 <- p4  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(0.003, -0.003)
p4 <- p4 +  ggtitle("Stable line (G14)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1")) 
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
dt5$Harvest <- factor(dt5$Harvest, levels = c("NIcut3","NIcut4","NIcut5","NIcut6","NIcut7", "mean"))

p5 <- ggplot(dt5, aes(x = time, y = ndvi, col = Harvest))
p5 <- p5  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.001, 0.004)
p5 <- p5 +  ggtitle("Stable line (G25)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "deeppink", "mediumslateblue", "olivedrab1")) 
p5 <- p5 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")











ustable_G1 <- d1[d1$genotype=="4",]


udf.NDVI <- aggregate(ndvi ~ time + genotype, ustable_G1, mean)
#udf.NDVI$type <- "unstable"
udf.NDVI$Harvest <- "mean"

udt <- rbind(ustable_G1,udf.NDVI)


library(ggplot2)
udt$genotype <- as.factor(udt$genotype)
udt$Harvest <- factor(udt$Harvest, levels = c( "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up1 <- ggplot(udt, aes(x = time, y = ndvi, col = Harvest))
up1 <- up1  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.02, 0.005)
up1 <- up1 +  ggtitle("Stable line (G4)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3",  "mediumslateblue", "olivedrab1")) 
up1 <- up1 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))

ustable_G2 <- d1[d1$genotype=="5",]


udf.NDVI2 <- aggregate(ndvi ~ time + genotype, ustable_G2, mean)
#udf.NDVI2$type <- "unstable"
udf.NDVI2$Harvest <- "mean"

udt2 <- rbind(ustable_G2,udf.NDVI2)


library(ggplot2)
udt2$genotype <- as.factor(udt2$genotype)
udt2$Harvest <- factor(udt2$Harvest, levels = c( "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up2 <- ggplot(udt2, aes(x = time, y = ndvi, col = Harvest))
up2 <- up2 + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.001, 0.0025)
up2 <- up2 +  ggtitle("Stable line (G5)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3",  "mediumslateblue", "olivedrab1")) 
up2 <- up2 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G3 <- d1[d1$genotype=="9",]


udf.NDVI3 <- aggregate(ndvi ~ time + genotype, ustable_G3, mean)
#udf.NDVI3$type <- "unstable"
udf.NDVI3$Harvest <- "mean"

udt3 <- rbind(ustable_G3,udf.NDVI3)


library(ggplot2)
udt3$genotype <- as.factor(udt3$genotype)
udt3$Harvest <- factor(udt3$Harvest, levels = c( "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up3 <- ggplot(udt3, aes(x = time, y = ndvi, col = Harvest))
up3 <- up3 + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.003, 0.004)
up3 <- up3 +  ggtitle("Stable line (G9)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3",  "mediumslateblue", "olivedrab1")) 
up3 <- up3 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G4 <- d1[d1$genotype=="14",]


udf.NDVI4 <- aggregate(ndvi ~ time + genotype, ustable_G4, mean)
#udf.NDVI4$type <- "unstable"
udf.NDVI4$Harvest <- "mean"

udt4 <- rbind(ustable_G4,udf.NDVI4)


library(ggplot2)
udt4$genotype <- as.factor(udt4$genotype)

udt4$Harvest <- factor(udt4$Harvest, levels = c( "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up4 <- ggplot(udt4, aes(x = time, y = ndvi, col = Harvest))
up4 <- up4  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(0.003, -0.003)
up4 <- up4 +  ggtitle("Stable line (G14)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "mediumslateblue", "olivedrab1")) 
up4 <- up4 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G5 <- d1[d1$genotype=="25",]


udf.NDVI5 <- aggregate(ndvi ~ time + genotype, ustable_G5, mean)
#udf.NDVI5$type <- "unstable"
udf.NDVI5$Harvest <- "mean"

udt5 <- rbind(ustable_G5,udf.NDVI5)


library(ggplot2)
udt5$genotype <- as.factor(udt5$genotype)

udt5$Harvest <- factor(udt5$Harvest, levels = c( "SITcut3","SITcut4","SITcut5","SITcut7", "mean"))

up5 <- ggplot(udt5, aes(x = time, y = ndvi, col = Harvest))
up5 <- up5  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim(-0.001, 0.004)
up5 <- up5 +  ggtitle("Stable line (G25)")  +
  scale_colour_manual(values=c( "red","black", "aquamarine3", "mediumslateblue", "olivedrab1")) 
up5 <- up5 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


library(egg)

ggarrange(p1, up1,p2,up2, p3,up3, p4, up4,p5,up5, ncol = 2)


