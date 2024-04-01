setwd("/Users/rt485/Downloads/Helfer/Phenotypic_indices/yield/New\ GDD_9_13/GNDVI/2-4-2023/newGDD/growth\ curve_stability/NDVI")

d350 <- read.table("gv_NDVI_350_new_time_3_27_a.txt", header = T)
d350$Harvest <- "2020cut1"

d389 <- read.table("gv_NDVI_389_new_time_3_27_a.txt", header = T)
#d389$Harvest <- "Second Harvest of 2020"
d389$Harvest <- rep("2020cut2", nrow(d389))

d438 <- read.table("gv_NDVI_438_new_time_3_27_a.txt", header = T)
#d438$cutting <- "Third Harvest of 2020"
d438$Harvest <- rep("2020cut3", nrow(d438))

d729 <- read.table("gv_NDVI_729_new_time_3_27_a.txt", header = T)
d729$Harvest <- rep("2021cut1", nrow(d729))

d772 <- read.table("gv_NDVI_772_new_time_3_27_a.txt", header = T)
d772$Harvest <- rep("2021cut2", nrow(d772))

d821 <- read.table("gv_NDVI_821_new_time_3_27_a.txt", header = T)
d821$Harvest <- rep("2021cut3", nrow(d821))

d <- rbind(d350,d389,d438,d729,d772,d821)
d <- d[!d$Harvest=="2020cut1",]
d <- as.data.frame(d)

head(d)

stable_G1 <- d[d$genotype=="1",]


df.NDVI <- aggregate(ndvi ~ time + genotype, stable_G1, mean)
df.NDVI$type <- "stable"
df.NDVI$Harvest <- "mean"

dt <- rbind(stable_G1,df.NDVI)


library(ggplot2)
dt$genotype <- as.factor(dt$genotype)
dt$Harvest <- factor(dt$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))

p1 <- ggplot(dt, aes(x = time, y = ndvi, col = Harvest))
p1 <- p1  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit") +   ylim( -0.0007, 0.0007)
p1 <- p1 +  ggtitle("Stable line (g1)")  +
scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
p1 <- p1 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")

stable_G2 <- d[d$genotype=="2",]


df.NDVI2 <- aggregate(ndvi ~ time + genotype, stable_G2, mean)
df.NDVI2$type <- "stable"
df.NDVI2$Harvest <- "mean"

dt2 <- rbind(stable_G2,df.NDVI2)


library(ggplot2)
dt2$genotype <- as.factor(dt2$genotype)
dt2$Harvest <- factor(dt2$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


p2 <- ggplot(dt2, aes(x = time, y = ndvi, col = Harvest))
p2 <- p2  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim( -0.0007, 0.0007)
p2 <- p2 +  ggtitle("Stable line (g2)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
p2 <- p2 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G3 <- d[d$genotype=="6",]


df.NDVI3 <- aggregate(ndvi ~ time + genotype, stable_G3, mean)
df.NDVI3$type <- "stable"
df.NDVI3$Harvest <- "mean"

dt3 <- rbind(stable_G3,df.NDVI3)


library(ggplot2)
dt3$genotype <- as.factor(dt3$genotype)
dt3$Harvest <- factor(dt3$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


p3 <- ggplot(dt3, aes(x = time, y = ndvi, col = Harvest))
p3 <- p3  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim( -0.0007, 0.0007)
p3 <- p3 +  ggtitle("Stable line (g6)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
p3 <- p3 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G4 <- d[d$genotype=="8",]


df.NDVI4 <- aggregate(ndvi ~ time + genotype, stable_G4, mean)
df.NDVI4$type <- "stable"
df.NDVI4$Harvest <- "mean"

dt4 <- rbind(stable_G4,df.NDVI4)


library(ggplot2)
dt4$genotype <- as.factor(dt4$genotype)
dt4$Harvest <- factor(dt4$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


p4 <- ggplot(dt4, aes(x = time, y = ndvi, col = Harvest))
p4 <- p4  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+   ylim( -0.0007, 0.0007)
p4 <- p4 +  ggtitle("Stable line (g8)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
p4 <- p4 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


stable_G5 <- d[d$genotype=="14",]


df.NDVI5 <- aggregate(ndvi ~ time + genotype, stable_G5, mean)
df.NDVI5$type <- "stable"
df.NDVI5$Harvest <- "mean"

dt5 <- rbind(stable_G5,df.NDVI5)


library(ggplot2)
dt5$genotype <- as.factor(dt5$genotype)
dt5$Harvest <- factor(dt5$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


p5 <- ggplot(dt5, aes(x = time, y = ndvi, col = Harvest))
p5 <- p5  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+   ylim( -0.0007, 0.0007)
p5 <- p5 +  ggtitle("Stable line (g14)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
p5 <- p5 +  theme(legend.position = 'bottom',
                  legend.text = element_text(size = 11),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")



ustable_G1 <- d[d$genotype=="3",]


udf.NDVI <- aggregate(ndvi ~ time + genotype, ustable_G1, mean)
udf.NDVI$type <- "unstable"
udf.NDVI$Harvest <- "mean"

udt <- rbind(ustable_G1,udf.NDVI)


library(ggplot2)
udt$genotype <- as.factor(udt$genotype)
udt$Harvest <- factor(udt$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))

up1 <- ggplot(udt, aes(x = time, y = ndvi, col = Harvest))
up1 <- up1  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim( -0.0009, 0.0009)
up1 <- up1 +  ggtitle("Unstable line (g3)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
up1 <- up1 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))

ustable_G2 <- d[d$genotype=="13",]


udf.NDVI2 <- aggregate(ndvi ~ time + genotype, ustable_G2, mean)
udf.NDVI2$type <- "unstable"
udf.NDVI2$Harvest <- "mean"

udt2 <- rbind(ustable_G2,udf.NDVI2)


library(ggplot2)
udt2$genotype <- as.factor(udt2$genotype)
udt2$Harvest <- factor(udt2$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


up2 <- ggplot(udt2, aes(x = time, y = ndvi, col = Harvest))
up2 <- up2 + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+ ylim( -0.0009, 0.0009)
up2 <- up2 +  ggtitle("Unstable line (g13)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
up2 <- up2 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G3 <- d[d$genotype=="21",]


udf.NDVI3 <- aggregate(ndvi ~ time + genotype, ustable_G3, mean)
udf.NDVI3$type <- "unstable"
udf.NDVI3$Harvest <- "mean"

udt3 <- rbind(ustable_G3,udf.NDVI3)


library(ggplot2)
udt3$genotype <- as.factor(udt3$genotype)
udt3$Harvest <- factor(udt3$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


up3 <- ggplot(udt3, aes(x = time, y = ndvi, col = Harvest))
up3 <- up3 + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+ ylim( -0.0009, 0.0016)
up3 <- up3 +  ggtitle("Unstable line (g21)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
up3 <- up3 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G4 <- d[d$genotype=="22",]


udf.NDVI4 <- aggregate(ndvi ~ time + genotype, ustable_G4, mean)
udf.NDVI4$type <- "unstable"
udf.NDVI4$Harvest <- "mean"

udt4 <- rbind(ustable_G4,udf.NDVI4)


library(ggplot2)
udt4$genotype <- as.factor(udt4$genotype)
udt4$Harvest <- factor(udt4$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


up4 <- ggplot(udt4, aes(x = time, y = ndvi, col = Harvest))
up4 <- up4  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+  ylim( -0.0012, 0.0009)
up4 <- up4 +  ggtitle("Unstable line (g22)")  +
  scale_colour_manual(values=c("#D2691E", "#A9A9A9", "#7FFFD4", "#FFD700", "#8470FF", "#6B8E23", "#D3D3D3"))  # Color-blind friendly colors slightly adjusted
up4 <- up4 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")


ustable_G5 <- d[d$genotype=="35",]


udf.NDVI5 <- aggregate(ndvi ~ time + genotype, ustable_G5, mean)
udf.NDVI5$type <- "unstable"
udf.NDVI5$Harvest <- "mean"

udt5 <- rbind(ustable_G5,udf.NDVI5)


library(ggplot2)
udt5$genotype <- as.factor(udt5$genotype)
dt5$Harvest <- factor(udt5$Harvest, levels = c("2020cut1", "2020cut2", "2020cut3", "2021cut1", "2021cut2", "2021cut3", "mean"))


up5 <- ggplot(udt5, aes(x = time, y = ndvi, col = Harvest))
up5 <- up5  + geom_line()+ xlab("Growing degree days (GDD)") + ylab("Genetic Merit")+   ylim( -0.0012, 0.0009)
up5 <- up5 +  ggtitle("Unstable line (g35)")  +
  scale_colour_manual(values=c( "brown1","black", "aquamarine3", "darkgoldenrod1", "mediumslateblue", "olivedrab1", "gray")) 
up5 <- up5 +  theme(legend.position = 'bottom',
                    legend.text = element_text(size = 11),
                    axis.text = element_text(size = 11),
                    axis.title = element_text(size = 11, face = 'bold')) + theme_light() + theme(axis.text = element_text(size = 11)) + theme(axis.title.y = element_text(size = 11)) + theme(axis.title.x = element_text(size = 11)) + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(size=16)) + theme(legend.text=element_text(size=11))+ theme(legend.position="none")








library(egg)

ggarrange(p1, up1,p2,up2, p3,up3, p4, up4,p5,up5, ncol = 2)


