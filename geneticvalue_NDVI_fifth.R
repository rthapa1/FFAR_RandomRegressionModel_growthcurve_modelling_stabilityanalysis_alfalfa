setwd("/Users/rt485/Downloads/Helfer/NMSU/10_4/Normal_irrigation/new_GDD_2_28_2023/NDVI/single_control_11_30_2023/11_16/fifth/auc")
soln <- fread("solutions", data.table = F)
soln_2 <- soln[soln$`trait/effect`==2,]
soln_3 <- soln[soln$`trait/effect`==3,]
soln_4 <- soln[soln$`trait/effect`==4,]
soln_5 <- soln[soln$`trait/effect`==5,]

soln_all <- cbind(soln_2, soln_3,soln_4,soln_5)


soln_final <- soln_all[,c(4,8,12,16)]

dim(soln_final)

date <- seq(340.85 , 1600, by = 20)
date2 <- as.data.frame(date)

intercept <- rep(1, times = length(date2))
dim1= -1 + (2*(date2$date-min(date2$date))/(max(date2$date)-min(date2$date)))
dim2 = dim1^2
dim3=dim1^3
dim4=dim1^4

df2 <- cbind(intercept, dim1, dim2, dim3, dim4)


A = matrix(c(0.7071, 0.0000, -0.7906, 0.0000, 0.7955,
             0.0000, 1.2247, 0.0000, -2.8067, 0.0000,
             0.0000, 0.0000, 2.3717, 0.0000, -7.9550,
             0.0000, 0.0000, 0.0000, 4.6771, 0.0000,
             0.0000, 0.0000, 0.0000, 0.0000, 9.2808),byrow=TRUE,nrow=5)

M <- as.matrix(df2)

phi <- M%*%A

phi_final <- phi[,1:4]

gvalue <- as.matrix(soln_final)%*%as.matrix(t(phi_final))
g_value <- as.data.frame(gvalue)
colnames(g_value) <- as.character(date2$date)
row.names(g_value) <- c("AFX 149092",	"AFX 779",	"AmeriStand 803T",	"Control",	"Dona Ana",	"Mesquite",	"NM1701PAR",	"NM1702PAR",	"NM1703PAR",	"NM1704PAR",	"NM170506PAR",	"NM1705PAR",	"NM1712PAR",	"NM1713PAR",	"NM1715PAR",	"NuMex Bill Melton CA 2019",	"NuMex Bill Melton FG2011",	"NuMex Bill Melton FG2012",	"SW 1011",	"SW 8421S",	"SW 8888",	"SW 9720",	"SW 9812",	"SW 9813",	"TMA 990")


g_value_t <- t(g_value)
g_value_t <- as.data.frame(g_value_t)
g_value_t$time <- date2$date


library(data.table)
library(reshape2)


library(tidyr)

#pivot the data frame into a long format
gv <- g_value_t %>% pivot_longer(cols=c("AFX 149092",	"AFX 779",	"AmeriStand 803T",	"Control",	"Dona Ana",	"Mesquite",	"NM1701PAR",	"NM1702PAR",	"NM1703PAR",	"NM1704PAR",	"NM170506PAR",	"NM1705PAR",	"NM1712PAR",	"NM1713PAR",	"NM1715PAR",	"NuMex Bill Melton CA 2019",	"NuMex Bill Melton FG2011",	"NuMex Bill Melton FG2012",	"SW 1011",	"SW 8421S",	"SW 8888",	"SW 9720",	"SW 9812",	"SW 9813",	"TMA 990")
                                 , names_to='genotype', values_to='ndvi')

gv <- as.data.frame(gv)

dat_final <- gv
colnames(dat_final) <- c("time", "Genotype", "ndvi")
dat_final$Genotype[dat_final$Genotype =="AFX 149092"]<-1
dat_final$Genotype[dat_final$Genotype =="AFX 779"]<-2
dat_final$Genotype[dat_final$Genotype =="AmeriStand 803T"]<-3
dat_final$Genotype[dat_final$Genotype =="Control"]<-4
dat_final$Genotype[dat_final$Genotype =="Dona Ana"]<-5
dat_final$Genotype[dat_final$Genotype =="Mesquite"]<-6
dat_final$Genotype[dat_final$Genotype =="NM1701PAR"]<-7
dat_final$Genotype[dat_final$Genotype =="NM1702PAR"]<-8
dat_final$Genotype[dat_final$Genotype =="NM1703PAR"]<-9
dat_final$Genotype[dat_final$Genotype =="NM1704PAR"]<-10
dat_final$Genotype[dat_final$Genotype =="NM170506PAR"]<-11
dat_final$Genotype[dat_final$Genotype =="NM1705PAR"]<-12
dat_final$Genotype[dat_final$Genotype =="NM1712PAR"]<-13
dat_final$Genotype[dat_final$Genotype =="NM1713PAR"]<-14
dat_final$Genotype[dat_final$Genotype =="NM1715PAR"]<-15
dat_final$Genotype[dat_final$Genotype =="NuMex Bill Melton CA 2019"]<-16
dat_final$Genotype[dat_final$Genotype =="NuMex Bill Melton FG2011"]<-17
dat_final$Genotype[dat_final$Genotype =="NuMex Bill Melton FG2012"]<-18
dat_final$Genotype[dat_final$Genotype =="SW 1011"]<-19
dat_final$Genotype[dat_final$Genotype =="SW 8421S"]<-20
dat_final$Genotype[dat_final$Genotype =="SW 8888"]<-21
dat_final$Genotype[dat_final$Genotype =="SW 9720"]<-22
dat_final$Genotype[dat_final$Genotype =="SW 9812"]<-23
dat_final$Genotype[dat_final$Genotype =="SW 9813"]<-24
dat_final$Genotype[dat_final$Genotype =="TMA 990"]<-25



write.table(dat_final, file = "gv_NDVI_fifth_auc", row.names = F)



