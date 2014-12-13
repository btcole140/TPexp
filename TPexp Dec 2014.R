setwd("/Users/mac/Google Drive/R Datasets/TP 2013") #home computer
#note need to change user if working on diff computer
setwd("/Users/brittany/Google Drive/R Datasets/TP 2013") #lab computer
TP  <- read.csv("TP 2013.csv")

#Columns
str(TP)
#Factors: TRTMT.No, PLOT, POSITION, DESTZ, ORZ, FAMILY, SURV.TP, SURV.FLW, SURV.FR
TP$TRTMT.No  <- as.factor(TP$TRTMT.No)
class(TP$TRTMT.No)
TP$PLOT  <- as.factor(TP$PLOT)
class(TP$PLOT)
TP$POSITION  <- as.factor(TP$POSITION)
class(TP$POSITION)
TP$DESTZ  <- as.factor(TP$DESTZ)
class(TP$DESTZ)
TP$ORZ  <- as.factor(TP$ORZ)
class(TP$ORZ)
TP$FAMILY  <- as.factor(TP$FAMILY)
class(TP$FAMILY)
TP$SURV.TP  <- as.factor(TP$SURV.TP)
class(TP$SURV.TP)
TP$SURV.FLW  <- as.factor(TP$SURV.FLW)
class(TP$SURV.FLW)
TP$SURV.FR  <- as.factor(TP$SURV.FR)
class(TP$SURV.FR)

#NUM: LEAF.FL, BR.FL, BR.F, J.DATE.FL, J.DATE.F, DIFF.DATE.FL.F, F.NFL
TP$LEAF.FL  <- as.numeric(TP$LEAF.FL)
class(TP$LEAF.FL)
TP$BR.FL  <- as.numeric(TP$BR.FL)
class(TP$BR.FL)
TP$BR.F  <- as.numeric(TP$BR.F)
class(TP$BR.F)
TP$J.Date.FL  <- as.numeric(TP$J.Date.FL)
class(TP$J.Date.FL)
TP$J.Date.F  <- as.numeric(TP$J.Date.F)
class(TP$J.Date.F)
TP$Diff.Date.FL.F  <- as.numeric(TP$Diff.Date.FL.F) 
class(TP$Diff.Date.FL.F)
TP$F.NFL  <- as.numeric(TP$F.NFL)
class(TP$F.NFL)

write.table(TP, file = "TP 2013.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#subset by Dest. site and zone within site
TPDD  <- subset(TP, DESTS == "D")
TPDD1  <-  subset(TP, DSITE.ZONE == "D1")
TPDD2  <-  subset(TP, DSITE.ZONE == "D2")
TPDM  <- subset(TP, DESTS == "M")
TPDM1  <-  subset(TP, DSITE.ZONE == "M1")
TPDM2  <-  subset(TP, DSITE.ZONE == "M2")

#subset by OR. site and zone within site
TPOD  <- subset(TP, ORS == "D")
TPOD1  <-  subset(TP, OSITE.ZONE == "D1")
TPOD2  <-  subset(TP, OSITE.ZONE == "D2")
TPOM  <- subset(TP, ORS == "M")
TPOM1  <-  subset(TP, OSITE.ZONE == "M1")
TPOM2  <-  subset(TP, OSITE.ZONE == "M2")
TPOB  <- subset(TP, ORS == "B")
TPOB1  <-  subset(TP, OSITE.ZONE == "B1")
TPOB2  <-  subset(TP, OSITE.ZONE == "B2")

write.table(TPDD, file = "TP 2013 DD.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDD1, file = "TP 2013 DD1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDD2, file = "TP 2013 DD2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDM, file = "TP 2013 DM.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDM1, file = "TP 2013 DM1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDM2, file = "TP 2013 DM2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD, file = "TP 2013 OD.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD1, file = "TP 2013 OD1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD2, file = "TP 2013 OD2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM, file = "TP 2013 OM.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM1, file = "TP 2013 OM1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM2, file = "TP 2013 OM2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB, file = "TP 2013 OB.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB1, file = "TP 2013 OB1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB2, file = "TP 2013 OB2.csv", sep = ",", col.names = TRUE, row.names = FALSE)


#***********************************************
###FUNCTIONS
#***********************

# Set Fuction to Summarize data for plotting ggplot with standard error bars.
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#   data: a data frame.
#   measurevar: the name of a column that contains the variable to be summariezed
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}


#******************************************
#TRANSFORMATIONS
#**********************************
#variables: GR, STEM.FL, STEM.F, BR.F, J.Date.FL, Diff.Date.FL.F

##GR
#ALL
TP$logGR  <- log10(TP$GR+1)
TP$sqrtGR  <- sqrt(TP$GR+0.5)
TP$rankGR  <- rank(TP$GR, na.last="keep")

#DD
TPDD$logGR  <- log10(TPDD$GR+1)
TPDD$sqrtGR  <- sqrt(TPDD$GR+0.5)
TPDD$rankGR  <- rank(TPDD$GR, na.last="keep")
#DD1
TPDD1$logGR  <- log10(TPDD1$GR+1)
TPDD1$sqrtGR  <- sqrt(TPDD1$GR+0.5)
TPDD1$rankGR  <- rank(TPDD1$GR, na.last="keep")
#DD2
TPDD2$logGR  <- log10(TPDD2$GR+1)
TPDD2$sqrtGR  <- sqrt(TPDD2$GR+0.5)
TPDD2$rankGR  <- rank(TPDD2$GR, na.last="keep")

#DM
TPDM$logGR  <- log10(TPDM$GR+1)
TPDM$sqrtGR  <- sqrt(TPDM$GR+0.5)
TPDM$rankGR  <- rank(TPDM$GR, na.last="keep")
#DM1
TPDM1$logGR  <- log10(TPDM1$GR+1)
TPDM1$sqrtGR  <- sqrt(TPDM1$GR+0.5)
TPDM1$rankGR  <- rank(TPDM1$GR, na.last="keep")
#DM2
TPDM2$logGR  <- log10(TPDM2$GR+1)
TPDM2$sqrtGR  <- sqrt(TPDM2$GR+0.5)
TPDM2$rankGR  <- rank(TPDM2$GR, na.last="keep")

#OD
TPOD$logGR  <- log10(TPOD$GR+1)
TPOD$sqrtGR  <- sqrt(TPOD$GR+0.5)
TPOD$rankGR  <- rank(TPOD$GR, na.last="keep")
#OD1
TPOD1$logGR  <- log10(TPOD1$GR+1)
TPOD1$sqrtGR  <- sqrt(TPOD1$GR+0.5)
TPOD1$rankGR  <- rank(TPOD1$GR, na.last="keep")
#OD2
TPOD2$logGR  <- log10(TPOD2$GR+1)
TPOD2$sqrtGR  <- sqrt(TPOD2$GR+0.5)
TPOD2$rankGR  <- rank(TPOD2$GR, na.last="keep")

#OM
TPOM$logGR  <- log10(TPOM$GR+1)
TPOM$sqrtGR  <- sqrt(TPOM$GR+0.5)
TPOM$rankGR  <- rank(TPOM$GR, na.last="keep")
#OM1
TPOM1$logGR  <- log10(TPOM1$GR+1)
TPOM1$sqrtGR  <- sqrt(TPOM1$GR+0.5)
TPOM1$rankGR  <- rank(TPOM1$GR, na.last="keep")
#OM2
TPOM2$logGR  <- log10(TPOM2$GR+1)
TPOM2$sqrtGR  <- sqrt(TPOM2$GR+0.5)
TPOM2$rankGR  <- rank(TPOM2$GR, na.last="keep")

#OB
TPOB$logGR  <- log10(TPOB$GR+1)
TPOB$sqrtGR  <- sqrt(TPOB$GR+0.5)
TPOB$rankGR  <- rank(TPOB$GR, na.last="keep")
#OB1
TPOB1$logGR  <- log10(TPOB1$GR+1)
TPOB1$sqrtGR  <- sqrt(TPOB1$GR+0.5)
TPOB1$rankGR  <- rank(TPOB1$GR, na.last="keep")
#OB2
TPOB2$logGR  <- log10(TPOB2$GR+1)
TPOB2$sqrtGR  <- sqrt(TPOB2$GR+0.5)
TPOB2$rankGR  <- rank(TPOB2$GR, na.last="keep")


##STEM.FL
#ALL
TP$logSTEM.FL  <- log10(TP$STEM.FL+1)
TP$sqrtSTEM.FL  <- sqrt(TP$STEM.FL+0.5)
TP$rankSTEM.FL  <- rank(TP$STEM.FL, na.last="keep")

#DD
TPDD$logSTEM.FL  <- log10(TPDD$STEM.FL+1)
TPDD$sqrtSTEM.FL  <- sqrt(TPDD$STEM.FL+0.5)
TPDD$rankSTEM.FL  <- rank(TPDD$STEM.FL, na.last="keep")
#DD1
TPDD1$logSTEM.FL  <- log10(TPDD1$STEM.FL+1)
TPDD1$sqrtSTEM.FL  <- sqrt(TPDD1$STEM.FL+0.5)
TPDD1$rankSTEM.FL  <- rank(TPDD1$STEM.FL, na.last="keep")
#DD2
TPDD2$logSTEM.FL  <- log10(TPDD2$STEM.FL+1)
TPDD2$sqrtSTEM.FL  <- sqrt(TPDD2$STEM.FL+0.5)
TPDD2$rankSTEM.FL  <- rank(TPDD2$STEM.FL, na.last="keep")

#DM
TPDM$logSTEM.FL  <- log10(TPDM$STEM.FL+1)
TPDM$sqrtSTEM.FL  <- sqrt(TPDM$STEM.FL+0.5)
TPDM$rankSTEM.FL  <- rank(TPDM$STEM.FL, na.last="keep")
#DM1
TPDM1$logSTEM.FL  <- log10(TPDM1$STEM.FL+1)
TPDM1$sqrtSTEM.FL  <- sqrt(TPDM1$STEM.FL+0.5)
TPDM1$rankSTEM.FL  <- rank(TPDM1$STEM.FL, na.last="keep")
#DM2
TPDM2$logSTEM.FL  <- log10(TPDM2$STEM.FL+1)
TPDM2$sqrtSTEM.FL  <- sqrt(TPDM2$STEM.FL+0.5)
TPDM2$rankSTEM.FL  <- rank(TPDM2$STEM.FL, na.last="keep")

#OD
TPOD$logSTEM.FL  <- log10(TPOD$STEM.FL+1)
TPOD$sqrtSTEM.FL  <- sqrt(TPOD$STEM.FL+0.5)
TPOD$rankSTEM.FL  <- rank(TPOD$STEM.FL, na.last="keep")
#OD1
TPOD1$logSTEM.FL  <- log10(TPOD1$STEM.FL+1)
TPOD1$sqrtSTEM.FL  <- sqrt(TPOD1$STEM.FL+0.5)
TPOD1$rankSTEM.FL  <- rank(TPOD1$STEM.FL, na.last="keep")
#OD2
TPOD2$logSTEM.FL  <- log10(TPOD2$STEM.FL+1)
TPOD2$sqrtSTEM.FL  <- sqrt(TPOD2$STEM.FL+0.5)
TPOD2$rankSTEM.FL  <- rank(TPOD2$STEM.FL, na.last="keep")

#OM
TPOM$logSTEM.FL  <- log10(TPOM$STEM.FL+1)
TPOM$sqrtSTEM.FL  <- sqrt(TPOM$STEM.FL+0.5)
TPOM$rankSTEM.FL  <- rank(TPOM$STEM.FL, na.last="keep")
#OM1
TPOM1$logSTEM.FL  <- log10(TPOM1$STEM.FL+1)
TPOM1$sqrtSTEM.FL  <- sqrt(TPOM1$STEM.FL+0.5)
TPOM1$rankSTEM.FL  <- rank(TPOM1$STEM.FL, na.last="keep")
#OM2
TPOM2$logSTEM.FL  <- log10(TPOM2$STEM.FL+1)
TPOM2$sqrtSTEM.FL  <- sqrt(TPOM2$STEM.FL+0.5)
TPOM2$rankSTEM.FL  <- rank(TPOM2$STEM.FL, na.last="keep")

#OB
TPOB$logSTEM.FL  <- log10(TPOB$STEM.FL+1)
TPOB$sqrtSTEM.FL  <- sqrt(TPOB$STEM.FL+0.5)
TPOB$rankSTEM.FL  <- rank(TPOB$STEM.FL, na.last="keep")
#OB1
TPOB1$logSTEM.FL  <- log10(TPOB1$STEM.FL+1)
TPOB1$sqrtSTEM.FL  <- sqrt(TPOB1$STEM.FL+0.5)
TPOB1$rankSTEM.FL  <- rank(TPOB1$STEM.FL, na.last="keep")
#OB2
TPOB2$logSTEM.FL  <- log10(TPOB2$STEM.FL+1)
TPOB2$sqrtSTEM.FL  <- sqrt(TPOB2$STEM.FL+0.5)
TPOB2$rankSTEM.FL  <- rank(TPOB2$STEM.FL, na.last="keep")


##STEM.F
#ALL
TP$logSTEM.F  <- log10(TP$STEM.F+1)
TP$sqrtSTEM.F  <- sqrt(TP$STEM.F+0.5)
TP$rankSTEM.F  <- rank(TP$STEM.F, na.last="keep")

#DD
TPDD$logSTEM.F  <- log10(TPDD$STEM.F+1)
TPDD$sqrtSTEM.F  <- sqrt(TPDD$STEM.F+0.5)
TPDD$rankSTEM.F  <- rank(TPDD$STEM.F, na.last="keep")
#DD1
TPDD1$logSTEM.F  <- log10(TPDD1$STEM.F+1)
TPDD1$sqrtSTEM.F  <- sqrt(TPDD1$STEM.F+0.5)
TPDD1$rankSTEM.F  <- rank(TPDD1$STEM.F, na.last="keep")
#DD2
TPDD2$logSTEM.F  <- log10(TPDD2$STEM.F+1)
TPDD2$sqrtSTEM.F  <- sqrt(TPDD2$STEM.F+0.5)
TPDD2$rankSTEM.F  <- rank(TPDD2$STEM.F, na.last="keep")

#DM
TPDM$logSTEM.F  <- log10(TPDM$STEM.F+1)
TPDM$sqrtSTEM.F  <- sqrt(TPDM$STEM.F+0.5)
TPDM$rankSTEM.F  <- rank(TPDM$STEM.F, na.last="keep")
#DM1
TPDM1$logSTEM.F  <- log10(TPDM1$STEM.F+1)
TPDM1$sqrtSTEM.F  <- sqrt(TPDM1$STEM.F+0.5)
TPDM1$rankSTEM.F  <- rank(TPDM1$STEM.F, na.last="keep")
#DM2
TPDM2$logSTEM.F  <- log10(TPDM2$STEM.F+1)
TPDM2$sqrtSTEM.F  <- sqrt(TPDM2$STEM.F+0.5)
TPDM2$rankSTEM.F  <- rank(TPDM2$STEM.F, na.last="keep")

#OD
TPOD$logSTEM.F  <- log10(TPOD$STEM.F+1)
TPOD$sqrtSTEM.F  <- sqrt(TPOD$STEM.F+0.5)
TPOD$rankSTEM.F  <- rank(TPOD$STEM.F, na.last="keep")
#OD1
TPOD1$logSTEM.F  <- log10(TPOD1$STEM.F+1)
TPOD1$sqrtSTEM.F  <- sqrt(TPOD1$STEM.F+0.5)
TPOD1$rankSTEM.F  <- rank(TPOD1$STEM.F, na.last="keep")
#OD2
TPOD2$logSTEM.F  <- log10(TPOD2$STEM.F+1)
TPOD2$sqrtSTEM.F  <- sqrt(TPOD2$STEM.F+0.5)
TPOD2$rankSTEM.F  <- rank(TPOD2$STEM.F, na.last="keep")

#OM
TPOM$logSTEM.F  <- log10(TPOM$STEM.F+1)
TPOM$sqrtSTEM.F  <- sqrt(TPOM$STEM.F+0.5)
TPOM$rankSTEM.F  <- rank(TPOM$STEM.F, na.last="keep")
#OM1
TPOM1$logSTEM.F  <- log10(TPOM1$STEM.F+1)
TPOM1$sqrtSTEM.F  <- sqrt(TPOM1$STEM.F+0.5)
TPOM1$rankSTEM.F  <- rank(TPOM1$STEM.F, na.last="keep")
#OM2
TPOM2$logSTEM.F  <- log10(TPOM2$STEM.F+1)
TPOM2$sqrtSTEM.F  <- sqrt(TPOM2$STEM.F+0.5)
TPOM2$rankSTEM.F  <- rank(TPOM2$STEM.F, na.last="keep")

#OB
TPOB$logSTEM.F  <- log10(TPOB$STEM.F+1)
TPOB$sqrtSTEM.F  <- sqrt(TPOB$STEM.F+0.5)
TPOB$rankSTEM.F  <- rank(TPOB$STEM.F, na.last="keep")
#OB1
TPOB1$logSTEM.F  <- log10(TPOB1$STEM.F+1)
TPOB1$sqrtSTEM.F  <- sqrt(TPOB1$STEM.F+0.5)
TPOB1$rankSTEM.F  <- rank(TPOB1$STEM.F, na.last="keep")
#OB2
TPOB2$logSTEM.F <- log10(TPOB2$STEM.F+1)
TPOB2$sqrtSTEM.F  <- sqrt(TPOB2$STEM.F+0.5)
TPOB2$rankSTEM.F  <- rank(TPOB2$STEM.F, na.last="keep")


##BR.F
#ALL
TP$logBR.F  <- log10(TP$BR.F+1)
TP$sqrtBR.F  <- sqrt(TP$BR.F+0.5)
TP$rankBR.F  <- rank(TP$BR.F, na.last="keep")

#DD
TPDD$logBR.F  <- log10(TPDD$BR.F+1)
TPDD$sqrtBR.F  <- sqrt(TPDD$BR.F+0.5)
TPDD$rankBR.F  <- rank(TPDD$BR.F, na.last="keep")
#DD1
TPDD1$logBR.F  <- log10(TPDD1$BR.F+1)
TPDD1$sqrtBR.F  <- sqrt(TPDD1$BR.F+0.5)
TPDD1$rankBR.F  <- rank(TPDD1$BR.F, na.last="keep")
#DD2
TPDD2$logBR.F  <- log10(TPDD2$BR.F+1)
TPDD2$sqrtBR.F  <- sqrt(TPDD2$BR.F+0.5)
TPDD2$rankBR.F  <- rank(TPDD2$BR.F, na.last="keep")

#DM
TPDM$logBR.F  <- log10(TPDM$BR.F+1)
TPDM$sqrtBR.F  <- sqrt(TPDM$BR.F+0.5)
TPDM$rankBR.F  <- rank(TPDM$BR.F, na.last="keep")
#DM1
TPDM1$logBR.F  <- log10(TPDM1$BR.F+1)
TPDM1$sqrtBR.F  <- sqrt(TPDM1$BR.F+0.5)
TPDM1$rankBR.F  <- rank(TPDM1$BR.F, na.last="keep")
#DM2
TPDM2$logBR.F  <- log10(TPDM2$BR.F+1)
TPDM2$sqrtBR.F  <- sqrt(TPDM2$BR.F+0.5)
TPDM2$rankBR.F  <- rank(TPDM2$BR.F, na.last="keep")

#OD
TPOD$logBR.F  <- log10(TPOD$BR.F+1)
TPOD$sqrtBR.F  <- sqrt(TPOD$BR.F+0.5)
TPOD$rankBR.F  <- rank(TPOD$BR.F, na.last="keep")
#OD1
TPOD1$logBR.F  <- log10(TPOD1$BR.F+1)
TPOD1$sqrtBR.F  <- sqrt(TPOD1$BR.F+0.5)
TPOD1$rankBR.F  <- rank(TPOD1$BR.F, na.last="keep")
#OD2
TPOD2$logBR.F  <- log10(TPOD2$BR.F+1)
TPOD2$sqrtBR.F  <- sqrt(TPOD2$BR.F+0.5)
TPOD2$rankBR.F  <- rank(TPOD2$BR.F, na.last="keep")

#OM
TPOM$logBR.F  <- log10(TPOM$BR.F+1)
TPOM$sqrtBR.F  <- sqrt(TPOM$BR.F+0.5)
TPOM$rankBR.F  <- rank(TPOM$BR.F, na.last="keep")
#OM1
TPOM1$logBR.F  <- log10(TPOM1$BR.F+1)
TPOM1$sqrtBR.F  <- sqrt(TPOM1$BR.F+0.5)
TPOM1$rankBR.F  <- rank(TPOM1$BR.F, na.last="keep")
#OM2
TPOM2$logBR.F  <- log10(TPOM2$BR.F+1)
TPOM2$sqrtBR.F  <- sqrt(TPOM2$BR.F+0.5)
TPOM2$rankBR.F  <- rank(TPOM2$BR.F, na.last="keep")

#OB
TPOB$logBR.F  <- log10(TPOB$BR.F+1)
TPOB$sqrtBR.F  <- sqrt(TPOB$BR.F+0.5)
TPOB$rankBR.F  <- rank(TPOB$BR.F, na.last="keep")
#OB1
TPOB1$logBR.F  <- log10(TPOB1$BR.F+1)
TPOB1$sqrtBR.F  <- sqrt(TPOB1$BR.F+0.5)
TPOB1$rankBR.F  <- rank(TPOB1$BR.F, na.last="keep")
#OB2
TPOB2$logBR.F  <- log10(TPOB2$BR.F+1)
TPOB2$sqrtBR.F  <- sqrt(TPOB2$BR.F+0.5)
TPOB2$rankBR.F  <- rank(TPOB2$BR.F, na.last="keep")


##J.Date.FL
#ALL
TP$logJ.Date.FL  <- log10(TP$J.Date.FL+1)
TP$sqrtJ.Date.FL  <- sqrt(TP$J.Date.FL+0.5)
TP$rankJ.Date.FL  <- rank(TP$J.Date.FL, na.last="keep")

#DD
TPDD$logJ.Date.FL  <- log10(TPDD$J.Date.FL+1)
TPDD$sqrtJ.Date.FL  <- sqrt(TPDD$J.Date.FL+0.5)
TPDD$rankJ.Date.FL  <- rank(TPDD$J.Date.FL, na.last="keep")
#DD1
TPDD1$logJ.Date.FL  <- log10(TPDD1$J.Date.FL+1)
TPDD1$sqrtJ.Date.FL  <- sqrt(TPDD1$J.Date.FL+0.5)
TPDD1$rankJ.Date.FL  <- rank(TPDD1$J.Date.FL, na.last="keep")
#DD2
TPDD2$logJ.Date.FL  <- log10(TPDD2$J.Date.FL+1)
TPDD2$sqrtJ.Date.FL  <- sqrt(TPDD2$J.Date.FL+0.5)
TPDD2$rankJ.Date.FL  <- rank(TPDD2$J.Date.FL, na.last="keep")

#DM
TPDM$logJ.Date.FL  <- log10(TPDM$J.Date.FL+1)
TPDM$sqrtJ.Date.FL  <- sqrt(TPDM$J.Date.FL+0.5)
TPDM$rankJ.Date.FL  <- rank(TPDM$J.Date.FL, na.last="keep")
#DM1
TPDM1$logJ.Date.FL  <- log10(TPDM1$J.Date.FL+1)
TPDM1$sqrtJ.Date.FL  <- sqrt(TPDM1$J.Date.FL+0.5)
TPDM1$rankJ.Date.FL  <- rank(TPDM1$J.Date.FL, na.last="keep")
#DM2
TPDM2$logJ.Date.FL  <- log10(TPDM2$J.Date.FL+1)
TPDM2$sqrtJ.Date.FL  <- sqrt(TPDM2$J.Date.FL+0.5)
TPDM2$rankJ.Date.FL  <- rank(TPDM2$J.Date.FL, na.last="keep")

#OD
TPOD$logJ.Date.FL  <- log10(TPOD$J.Date.FL+1)
TPOD$sqrtJ.Date.FL  <- sqrt(TPOD$J.Date.FL+0.5)
TPOD$rankJ.Date.FL  <- rank(TPOD$J.Date.FL, na.last="keep")
#OD1
TPOD1$logJ.Date.FL  <- log10(TPOD1$J.Date.FL+1)
TPOD1$sqrtJ.Date.FL  <- sqrt(TPOD1$J.Date.FL+0.5)
TPOD1$rankJ.Date.FL  <- rank(TPOD1$J.Date.FL, na.last="keep")
#OD2
TPOD2$logJ.Date.FL  <- log10(TPOD2$J.Date.FL+1)
TPOD2$sqrtJ.Date.FL  <- sqrt(TPOD2$J.Date.FL+0.5)
TPOD2$rankJ.Date.FL  <- rank(TPOD2$J.Date.FL, na.last="keep")

#OM
TPOM$logJ.Date.FL  <- log10(TPOM$J.Date.FL+1)
TPOM$sqrtJ.Date.FL  <- sqrt(TPOM$J.Date.FL+0.5)
TPOM$rankJ.Date.FL  <- rank(TPOM$J.Date.FL, na.last="keep")
#OM1
TPOM1$logJ.Date.FL  <- log10(TPOM1$J.Date.FL+1)
TPOM1$sqrtJ.Date.FL  <- sqrt(TPOM1$J.Date.FL+0.5)
TPOM1$rankJ.Date.FL  <- rank(TPOM1$J.Date.FL, na.last="keep")
#OM2
TPOM2$logJ.Date.FL  <- log10(TPOM2$J.Date.FL+1)
TPOM2$sqrtJ.Date.FL  <- sqrt(TPOM2$J.Date.FL+0.5)
TPOM2$rankJ.Date.FL  <- rank(TPOM2$J.Date.FL, na.last="keep")

#OB
TPOB$logJ.Date.FL  <- log10(TPOB$J.Date.FL+1)
TPOB$sqrtJ.Date.FL  <- sqrt(TPOB$J.Date.FL+0.5)
TPOB$rankJ.Date.FL  <- rank(TPOB$J.Date.FL, na.last="keep")
#OB1
TPOB1$logJ.Date.FL  <- log10(TPOB1$J.Date.FL+1)
TPOB1$sqrtJ.Date.FL  <- sqrt(TPOB1$J.Date.FL+0.5)
TPOB1$rankJ.Date.FL  <- rank(TPOB1$J.Date.FL, na.last="keep")
#OB2
TPOB2$logJ.Date.FL  <- log10(TPOB2$J.Date.FL+1)
TPOB2$sqrtJ.Date.FL  <- sqrt(TPOB2$J.Date.FL+0.5)
TPOB2$rankJ.Date.FL  <- rank(TPOB2$J.Date.FL, na.last="keep")


##Diff.Date.FL.F
#ALL
TP$logDiff.Date.FL.F  <- log10(TP$Diff.Date.FL.F+1)
TP$sqrtDiff.Date.FL.F  <- sqrt(TP$Diff.Date.FL.F+0.5)
TP$rankDiff.Date.FL.F  <- rank(TP$Diff.Date.FL.F, na.last="keep")

#DD
TPDD$logDiff.Date.FL.F  <- log10(TPDD$Diff.Date.FL.F+1)
TPDD$sqrtDiff.Date.FL.F  <- sqrt(TPDD$Diff.Date.FL.F+0.5)
TPDD$rankDiff.Date.FL.F  <- rank(TPDD$Diff.Date.FL.F, na.last="keep")
#DD1
TPDD1$logDiff.Date.FL.F  <- log10(TPDD1$Diff.Date.FL.F+1)
TPDD1$sqrtDiff.Date.FL.F  <- sqrt(TPDD1$Diff.Date.FL.F+0.5)
TPDD1$rankDiff.Date.FL.F  <- rank(TPDD1$Diff.Date.FL.F, na.last="keep")
#DD2
TPDD2$logDiff.Date.FL.F  <- log10(TPDD2$Diff.Date.FL.F+1)
TPDD2$sqrtDiff.Date.FL.F  <- sqrt(TPDD2$Diff.Date.FL.F+0.5)
TPDD2$rankDiff.Date.FL.F  <- rank(TPDD2$Diff.Date.FL.F, na.last="keep")

#DM
TPDM$logDiff.Date.FL.F  <- log10(TPDM$Diff.Date.FL.F+1)
TPDM$sqrtDiff.Date.FL.F  <- sqrt(TPDM$Diff.Date.FL.F+0.5)
TPDM$rankDiff.Date.FL.F  <- rank(TPDM$Diff.Date.FL.F, na.last="keep")
#DM1
TPDM1$logDiff.Date.FL.F  <- log10(TPDM1$Diff.Date.FL.F+1)
TPDM1$sqrtDiff.Date.FL.F  <- sqrt(TPDM1$Diff.Date.FL.F+0.5)
TPDM1$rankDiff.Date.FL.F  <- rank(TPDM1$Diff.Date.FL.F, na.last="keep")
#DM2
TPDM2$logDiff.Date.FL.F  <- log10(TPDM2$Diff.Date.FL.F+1)
TPDM2$sqrtDiff.Date.FL.F  <- sqrt(TPDM2$Diff.Date.FL.F+0.5)
TPDM2$rankDiff.Date.FL.F  <- rank(TPDM2$Diff.Date.FL.F, na.last="keep")

#OD
TPOD$logDiff.Date.FL.F  <- log10(TPOD$Diff.Date.FL.F+1)
TPOD$sqrtDiff.Date.FL.F  <- sqrt(TPOD$Diff.Date.FL.F+0.5)
TPOD$rankDiff.Date.FL.F  <- rank(TPOD$Diff.Date.FL.F, na.last="keep")
#OD1
TPOD1$logDiff.Date.FL.F  <- log10(TPOD1$Diff.Date.FL.F+1)
TPOD1$sqrtDiff.Date.FL.F  <- sqrt(TPOD1$Diff.Date.FL.F+0.5)
TPOD1$rankDiff.Date.FL.F  <- rank(TPOD1$Diff.Date.FL.F, na.last="keep")
#OD2
TPOD2$logDiff.Date.FL.F  <- log10(TPOD2$Diff.Date.FL.F+1)
TPOD2$sqrtDiff.Date.FL.F  <- sqrt(TPOD2$Diff.Date.FL.F+0.5)
TPOD2$rankDiff.Date.FL.F  <- rank(TPOD2$Diff.Date.FL.F, na.last="keep")

#OM
TPOM$logDiff.Date.FL.F  <- log10(TPOM$Diff.Date.FL.F+1)
TPOM$sqrtDiff.Date.FL.F  <- sqrt(TPOM$Diff.Date.FL.F+0.5)
TPOM$rankDiff.Date.FL.F  <- rank(TPOM$Diff.Date.FL.F, na.last="keep")
#OM1
TPOM1$logDiff.Date.FL.F  <- log10(TPOM1$Diff.Date.FL.F+1)
TPOM1$sqrtDiff.Date.FL.F  <- sqrt(TPOM1$Diff.Date.FL.F+0.5)
TPOM1$rankDiff.Date.FL.F  <- rank(TPOM1$Diff.Date.FL.F, na.last="keep")
#OM2
TPOM2$logDiff.Date.FL.F  <- log10(TPOM2$Diff.Date.FL.F+1)
TPOM2$sqrtDiff.Date.FL.F  <- sqrt(TPOM2$Diff.Date.FL.F+0.5)
TPOM2$rankDiff.Date.FL.F  <- rank(TPOM2$Diff.Date.FL.F, na.last="keep")

#OB
TPOB$logDiff.Date.FL.F  <- log10(TPOB$Diff.Date.FL.F+1)
TPOB$sqrtDiff.Date.FL.F  <- sqrt(TPOB$Diff.Date.FL.F+0.5)
TPOB$rankDiff.Date.FL.F  <- rank(TPOB$Diff.Date.FL.F, na.last="keep")
#OB1
TPOB1$logDiff.Date.FL.F  <- log10(TPOB1$Diff.Date.FL.F+1)
TPOB1$sqrtDiff.Date.FL.F  <- sqrt(TPOB1$Diff.Date.FL.F+0.5)
TPOB1$rankDiff.Date.FL.F  <- rank(TPOB1$Diff.Date.FL.F, na.last="keep")
#OB2
TPOB2$logDiff.Date.FL.F  <- log10(TPOB2$Diff.Date.FL.F+1)
TPOB2$sqrtDiff.Date.FL.F  <- sqrt(TPOB2$Diff.Date.FL.F+0.5)
TPOB2$rankDiff.Date.FL.F  <- rank(TPOB2$Diff.Date.FL.F, na.last="keep")


write.table(TPDD, file = "TP 2013 DD.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDD1, file = "TP 2013 DD1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDD2, file = "TP 2013 DD2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDM, file = "TP 2013 DM.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDM1, file = "TP 2013 DM1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPDM2, file = "TP 2013 DM2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD, file = "TP 2013 OD.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD1, file = "TP 2013 OD1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD2, file = "TP 2013 OD2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM, file = "TP 2013 OM.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM1, file = "TP 2013 OM1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM2, file = "TP 2013 OM2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB, file = "TP 2013 OB.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB1, file = "TP 2013 OB1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB2, file = "TP 2013 OB2.csv", sep = ",", col.names = TRUE, row.names = FALSE)


