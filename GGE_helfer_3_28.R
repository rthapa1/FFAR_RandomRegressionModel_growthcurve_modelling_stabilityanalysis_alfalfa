
############################# metan ##########################################

library(metan)
library(ggplot2)
options(max.print = 10000)
############################### set wd ######################################
#setwd("/Users/rt485/Downloads/Helfer/NMSU/10_4/new")
setwd("/Users/rt485/Downloads/Helfer/Phenotypic_indices/GGE/new")

MMET <- read.csv("/Users/rt485/Downloads/Helfer/Phenotypic_indices/GGE/new/GGE.csv", header =T)

############################# factors with unique levels ####################
MMET$ENV <- factor(MMET$ENV, levels=unique(MMET$ENV))
MMET$GEN <- factor(MMET$GEN, levels=unique(MMET$GEN))
MMET$REP <- factor(MMET$REP, levels=unique(MMET$REP))

###################### data inspection and manipulation #####################
inspect(MMET, plot=TRUE, threshold = 40)
##################### check for outliers ###################################
#find_outliers(MMET, var=HT, plots=TRUE)
find_outliers(MMET, var=YLD, plots=TRUE)
##################### extra clean functions ###############################
##################### remove NA ###########################################
remove_rows_na(MMET)
####################### replace zero #####################################
replace_zero(MMET)
###################### find text in numbers ##############################
#find_text_in_num(MMET$HT)
find_text_in_num(MMET$YLD)

######################### data analysis ##################################
###################### descriptive stats ################################
desc_stat(MMET)
desc_stat(MMET, stats="all")
ds <- desc_stat(MMET, stats="all") 
ds
View(ds)
class(ds)
######################### importing a table ############################
####################### mean of genotypes #############################
mg <- means_by(MMET, GEN)
mg
View(mg)
######################### mean of environments #######################
me <- means_by(MMET, ENV)
me
View(me)
########### mean performance of genotypes across environments ###########
mge <- MMET %>% 
  group_by(ENV, GEN) %>%
  desc_stat(YLD, stats="mean")
mge
View(mge)
############### plotting performance across environments ################

## YLD
pyd <- ge_plot(MMET, ENV, GEN, YLD)
pyd
pyd2 <- ge_plot(MMET, ENV, GEN, YLD, type=2)
pyd2
######################## winners within each env ###########################
win <- ge_winners(MMET, ENV, GEN, resp = everything())
View(win)
####################### ranks of genotypes #################################
ranks <- ge_winners(MMET, ENV, GEN, resp = everything(), type = "ranks")
View(ranks)
##### more details on  performance
ge_details(MMET, ENV, GEN, resp = everything())


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

# Rotate the plot by 180 degrees and adjust axis text orientation and font size
mvs2_rotated <- mvs2 + coord_flip() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)
        )  # Adjust the size as needed

mvs2_rotated


