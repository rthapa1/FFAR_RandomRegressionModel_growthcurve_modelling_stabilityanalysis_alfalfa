
setwd("/Users/rt485/Downloads/Helfer/NMSU/10_4")

############################# metan ##########################################
#install.packages("metan")
library(metan)
#install.packages("ggplot2")
library(ggplot2)
options(max.print = 10000)
############################### set wd ######################################
getwd()
############################## importing data ###############################
#library(readxl)
library(data.table)
MMET <- fread("GGE_NMSU_12_19b.txt")

colnames(MMET) <- c("Genotype", "REP", "YLD", "ENV")

dat_final <- MMET

dat_final$Genotype[dat_final$Genotype =="AFX 149092"]<-"G1"
dat_final$Genotype[dat_final$Genotype =="AFX 779"]<-"G2"
dat_final$Genotype[dat_final$Genotype =="AmeriStand 803T"]<-"G3"
dat_final$Genotype[dat_final$Genotype =="Control1"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control2"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control3"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control4"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control5"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control6"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control7"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control8"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control9"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control10"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control11"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Control12"]<-"G4"
dat_final$Genotype[dat_final$Genotype =="Dona Ana"]<-"G5"
dat_final$Genotype[dat_final$Genotype =="Mesquite"]<-"G6"
dat_final$Genotype[dat_final$Genotype =="NM1701PAR"]<-"G7"
dat_final$Genotype[dat_final$Genotype =="NM1702PAR"]<-"G8"
dat_final$Genotype[dat_final$Genotype =="NM1703PAR"]<-"G9"
dat_final$Genotype[dat_final$Genotype =="NM1704PAR"]<-"G10"
dat_final$Genotype[dat_final$Genotype =="NM170506PAR"]<-"G11"
dat_final$Genotype[dat_final$Genotype =="NM1705PAR"]<-"G12"
dat_final$Genotype[dat_final$Genotype =="NM1712PAR"]<-"G13"
dat_final$Genotype[dat_final$Genotype =="NM1713PAR"]<-"G14"
dat_final$Genotype[dat_final$Genotype =="NM1715PAR"]<-"G15"
dat_final$Genotype[dat_final$Genotype =="NuMex Bill Melton CA 2019"]<-"G16"
dat_final$Genotype[dat_final$Genotype =="NuMex Bill Melton FG2011"]<-"G17"
dat_final$Genotype[dat_final$Genotype =="NuMex Bill Melton FG2012"]<-"G18"
dat_final$Genotype[dat_final$Genotype =="SW 1011"]<-"G19"
dat_final$Genotype[dat_final$Genotype =="SW 8421S"]<-"G20"
dat_final$Genotype[dat_final$Genotype =="SW 8888"]<-"G21"
dat_final$Genotype[dat_final$Genotype =="SW 9720"]<-"G22"
dat_final$Genotype[dat_final$Genotype =="SW 9812"]<-"G23"
dat_final$Genotype[dat_final$Genotype =="SW 9813"]<-"G24"
dat_final$Genotype[dat_final$Genotype =="TMA 990"]<-"G25"

colnames(dat_final) <- c("GEN", "REP", "YLD", "ENV")

MMET <- dat_final

############################# factors with unique levels ####################
MMET$ENV <- factor(MMET$ENV, levels=unique(MMET$ENV))
MMET$GEN <- factor(MMET$GEN, levels=unique(MMET$GEN))
MMET$REP <- factor(MMET$REP, levels=unique(MMET$REP))

############### plotting performace across environments ################
###################### GGE Model ###############################
## yield 
gge_model2 <- gge(MMET, ENV, GEN, YLD)
pgge2 <- predict(gge_model2)

# 1 Basic biplot
# YLD
bbp2 <- plot(gge_model2)
bbp2

# 2 Discriminativeness vs representativeness
dvr2 <- plot(gge_model2, type = 4, plot_theme = theme_gray())
dvr2

############################# svp = genotype ############################
# Yield 
gpg2 <- gge(MMET, ENV, GEN, YLD, svp="genotype")

pgpg2 <- predict(gpg2)

# 5 Mean performance vs stability
## YLD
mvs2 <- plot(gpg2, type=2)
mvs2