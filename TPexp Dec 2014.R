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

#subset by Dest. zone
TPD1  <- subset(TP, DESTZ == "1")
TPD2  <- subset(TP, DESTZ == "2")
TPD2x  <- subset(TPD2, ORS %in% c("D", "M"))
TPD2x$ORS  <- TPD2x$ORS[, drop=TRUE]

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

write.table(TPD1, file = "TP 2013 D1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPD2x, file = "TP 2013 D2.csv", sep = ",", col.names = TRUE, row.names = FALSE)

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

#D1
TPD1$logGR <- log10(TPD1$GR+1)
TPD1$sqrtGR <- sqrt(TPD1$GR+0.5)
TPD1$rankGR <- rank(TPD1$GR, na.last="keep")
#D2
TPD2x$logGR  <- log10(TPD2x$GR+1)
TPD2x$sqrtGR  <- sqrt(TPD2x$GR+0.5)
TPD2x$rankGR  <- rank(TPD2x$GR, na.last="keep")

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

#D1
TPD1$logSTEM.FL <- log10(TPD1$STEM.FL+1)
TPD1$sqrtSTEM.FL <- sqrt(TPD1$STEM.FL+0.5)
TPD1$rankSTEM.FL <- rank(TPD1$STEM.FL, na.last="keep")
#D2
TPD2x$logSTEM.FL <- log10(TPD2x$STEM.FL+1)
TPD2x$sqrtSTEM.FL <- sqrt(TPD2x$STEM.FL+0.5)
TPD2x$rankSTEM.FL  <- rank(TPD2x$STEM.FL, na.last="keep")

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

#D1
TPD1$logSTEM.F <- log10(TPD1$STEM.F+1)
TPD1$sqrtSTEM.F <- sqrt(TPD1$STEM.F+0.5)
TPD1$rankSTEM.F <- rank(TPD1$STEM.F, na.last="keep")
#D2
TPD2x$logSTEM.F <- log10(TPD2x$STEM.F+1)
TPD2x$sqrtSTEM.F <- sqrt(TPD2x$STEM.F+0.5)
TPD2x$rankSTEM.F  <- rank(TPD2x$STEM.F, na.last="keep")

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

#D1
TPD1$logBR.F  <- log10(TPD1$BR.F+1)
TPD1$sqrtBR.F  <- sqrt(TPD1$BR.F+0.5)
TPD1$rankBR.F  <- rank(TPD1$BR.F, na.last="keep")
#D2
TPD2x$logBR.F  <- log10(TPD2x$BR.F+1)
TPD2x$sqrtBR.F  <- sqrt(TPD2x$BR.F+0.5)
TPD2x$rankBR.F  <- rank(TPD2x$BR.F, na.last="keep")


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

#D1
TPD1$logJ.Date.FL  <- log10(TPD1$J.Date.FL+1)
TPD1$sqrtJ.Date.FL  <- sqrt(TPD1$J.Date.FL+0.5)
TPD1$rankJ.Date.FL  <- rank(TPD1$J.Date.FL, na.last="keep")
#D2
TPD2x$logJ.Date.FL  <- log10(TPD2x$J.Date.FL+1)
TPD2x$sqrtJ.Date.FL  <- sqrt(TPD2x$J.Date.FL+0.5)
TPD2x$rankJ.Date.FL  <- rank(TPD2x$J.Date.FL, na.last="keep")

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

#D1
TPD1$logDiff.Date.FL.F  <- log10(TPD1$Diff.Date.FL.F+1)
TPD1$sqrtDiff.Date.FL.F  <- sqrt(TPD1$Diff.Date.FL.F+0.5)
TPD1$rankDiff.Date.FL.F  <- rank(TPD1$Diff.Date.FL.F, na.last="keep")
#D2
TPD2x$logDiff.Date.FL.F  <- log10(TPD2x$Diff.Date.FL.F+1)
TPD2x$sqrtDiff.Date.FL.F  <- sqrt(TPD2x$Diff.Date.FL.F+0.5)
TPD2x$rankDiff.Date.FL.F  <- rank(TPD2x$Diff.Date.FL.F, na.last="keep")

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

write.table(TPD1, file = "TP 2013 D1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPD2x, file = "TP 2013 D2.csv", sep = ",", col.names = TRUE, row.names = FALSE)

write.table(TPOD, file = "TP 2013 OD.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD1, file = "TP 2013 OD1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOD2, file = "TP 2013 OD2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM, file = "TP 2013 OM.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM1, file = "TP 2013 OM1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOM2, file = "TP 2013 OM2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB, file = "TP 2013 OB.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB1, file = "TP 2013 OB1.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(TPOB2, file = "TP 2013 OB2.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#********************
#Response Variables: GR

#boxplot
ggplot(data=TP, aes(x=DESTZ, y=GR))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Growth Rate (cm/day)") +
  ggtitle("GR by DESTZ")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means... more variation in beach

#boxplot
ggplot(data=TP, aes(x=DESTS, y=GR))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Growth Rate (cm/day)") +
  ggtitle("GR by DESTS")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means, more variation in M

#boxplot
ggplot(data=TP, aes(x=ORZ, y=GR))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Growth Rate (cm/day)") +
  ggtitle("GR by ORZ")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: the same

#boxplot
ggplot(data=TP, aes(x=ORS, y=GR))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Growth Rate (cm/day)") +
  ggtitle("GR by ORS")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means... more variation in D then M

###TP MNS
#distribution
hist(TPDM$sqrtGR) #sqrt skew left... rank may be best
#outliers
mean(TPDM$GR, na.rm=TRUE)
sd(TPDM$GR, na.rm=TRUE)
0.073+(3*0.059) #=0.25, outliers = none

#lmer vs lm
lmegr <- lmer(GR~DESTZ*ORZ+(1+ORZ|ORS), data=TPDM)
lmegra <- lmer(GR~DESTZ*ORZ+(1|ORS), data=TPDM)
anova(lmegr, lmegra) #ORZ not sig p=1 chisq=0 AIC= -151.28, AICa= -155.28
lmgr <- lm(GR~DESTZ*ORZ, data=TPDM)
x <- -2*logLik(lmgr, REML=T) +2*logLik(lmegra, REML=T)
x
pchisq(x, df=5, lower.tail=F)
#logLik= -2.84e-14, p=1, random ORS not sig

#check assumptions of best model
lmgrR <- resid(lmgr) 
lmgrF <- fitted(lmgr)
plot(lmgrF, lmgrR) #raw okay
abline(h=0, col=c("red"))
hist(lmgrR) #raw okay
qqnorm(lmgrR, main="Q-Q plot for residuals") 
qqline(lmgrR) #raw okay

#NOTE: Since random effects are not sig, use the linear model below
#lm - subset by DESTS=MNS
lmgr2  <- lm(GR~DESTZ*(ORS/ORZ)+DESTZ*ORS, data=TPDM)
lmgr3  <- update(lmgr2,~.-DESTZ:ORS)
anova(lmgr3, lmgr2) #DZ:OS not sig p=1, F=0
lmgr4  <- update(lmgr2,~.-DESTZ:(ORS/ORZ))
anova(lmgr4, lmgr2) #DZ:(OS/OZ) not sig p=0.77 f=0.37
lmgr5  <- lm(GR~DESTZ+(ORS/ORZ)+ORS, data=TPDM)
lmgr6  <- update(lmgr5,~.-DESTZ)
anova(lmgr6, lmgr5) #DZ is sig p=0.042 f=4.35 *
lmgr7  <- update(lmgr5,~.-(ORS/ORZ))
anova(lmgr7, lmgr5) #OS/OZ not sig p=0.802 f=0.33
lmgr8  <- lm(GR~DESTZ+ORS, data=TPDM)
lmgr9  <- update(lmgr8,~.-ORS)
anova(lmgr9, lmgr8) #OS not sig p=0.73 f=0.12
lmgr10  <- lm(GR~DESTZ, data=TPDM)
summary(lmgr10) #intercept=0.09, destz= -0.033, R^2=0.062, F=4.68[1,55], p=0.035

lmgrR  <- resid(lmgr10)
par(mfrow = c(2, 2))
plot(lmgr10) #okay
par(mfrow = c(1, 1))
hist(lmgrR) #okay, skew left

grn <- tapply(TPDM$GR, TPDM$DESTZ, length)
grmean <- tapply(TPDM$GR, TPDM$DESTZ, mean)
grsd <- tapply(TPDM$GR, TPDM$DESTZ, sd)
grCV <- (grsd/grmean)*100

TPgr <- summarySE(TPDM, measurevar="GR", groupvars=c("DESTZ")) 
ggplot(data=TPgr, aes(x=DESTZ, y=GR)) +
  geom_errorbar(aes(ymin=GR-se, ymax=GR+se), width=0.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) + geom_point(size=4, position=position_dodge(0.1))+
  xlab("Transplant Zone") + ylab("Growth rate (cm/day)") +
  ggtitle("Mean GR by DESTZ TPDM") +
  annotate("text", x=c(0.85, 2.20), 
           y=c(0.09, 0.057), 
           label=paste("n=",grn), size=6, fontface="bold") +
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18)) +
  theme(strip.text.x = element_text(size=20, face="bold")) +
  theme(strip.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=25), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=20, face="bold")) +
  scale_x_discrete(labels=c("Beach", "Dune")) +
  theme(axis.title.y = element_text(vjust=1, face="bold", size=25),
        axis.text.y  = element_text(size=20, face="bold"))

###TP Dune
#distribution
hist(TPD2x$sqrtGR) #sqrt skew left... rank may be best
#outliers
mean(TPD2x$GR, na.rm=TRUE)
sd(TPD2x$GR, na.rm=TRUE)
0.0405+(3*0.049) #=0.19, outliers = 38

#lm- subset by DESTZ=Dune
lm2gr2  <- lm(GR~DESTS*(ORS/ORZ)+DESTS*ORS, data=TPD2x)
lm2gr3  <- update(lm2gr2,~.-DESTS:ORS)
anova(lm2gr3, lm2gr2) #DS:OS not sig p=1, F=0
lm2gr4  <- update(lm2gr2,~.-DESTS:(ORS/ORZ))
anova(lm2gr4, lm2gr2) #DS:(OS/OZ) not sig p=0.78 f=0.36
lm2gr5  <- lm(GR~DESTS+(ORS/ORZ)+ORS, data=TPD2x)
lm2gr6  <- update(lm2gr5,~.-DESTS)
anova(lm2gr6, lm2gr5) #DS is sig p=0.0055 f=8.34 **
lm2gr7  <- update(lm2gr5,~.-(ORS/ORZ))
anova(lm2gr7, lm2gr5) #OS/OZ not sig p=0.82 f=0.303
lm2gr8  <- lm(GR~DESTS+ORS, data=TPD2x)
lm2gr9  <- update(lm2gr8,~.-ORS)
anova(lm2gr9, lm2gr8) #OS not sig p=0.62 f=0.24
lm2gr10  <- lm(GR~DESTS, data=TPD2x)
summary(lm2gr10)
#intercept=0.024, dests=0.034, R^2=0.103, F=7.77[1,58], p=0.0072

lm2grR  <- resid(lm2gr10)
par(mfrow = c(2, 2))
plot(lm2gr10) #okay
par(mfrow = c(1, 1))
hist(lm2grR) #okay, skew left


gr2n <- tapply(TPD2x$GR, TPD2x$DESTS, length)
gr2mean <- tapply(TPD2x$GR, TPD2x$DESTS, mean)
gr2sd <- tapply(TPD2x$GR, TPD2x$DESTS, sd)
gr2CV <- (gr2sd/gr2mean)*100

TP2gr <- summarySE(TPD2x, measurevar="GR", groupvars=c("DESTS")) 
ggplot(data=TP2gr, aes(x=DESTS, y=GR)) +
  geom_errorbar(aes(ymin=GR-se, ymax=GR+se), width=0.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) + geom_point(size=4, position=position_dodge(0.1))+
  xlab("Transplant Site") + ylab("Growth rate (cm/day)") +
  ggtitle("Mean GR by DESTS TPD2x") +
  annotate("text", x=c(0.85, 2.20), 
           y=c(0.025, 0.057), 
           label=paste("n=",gr2n), size=6, fontface="bold") +
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18)) +
  theme(strip.text.x = element_text(size=20, face="bold")) +
  theme(strip.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=25), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=20, face="bold")) +
  scale_x_discrete(labels=c("Darnley", "Martinique")) +
  theme(axis.title.y = element_text(vjust=1, face="bold", size=25),
        axis.text.y  = element_text(size=20, face="bold"))



#********************
#Response Variables: STEM.FL

#boxplot
ggplot(data=TP, aes(x=DESTZ, y=STEM.FL))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Flowering (cm)") +
  ggtitle("STEM.FL by DESTZ")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means... more variation in beach

#boxplot
ggplot(data=TP, aes(x=DESTS, y=STEM.FL))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Flowering (cm)") +
  ggtitle("STEM.FL by DESTS")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means... more variation in M

#boxplot
ggplot(data=TP, aes(x=ORZ, y=STEM.FL))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Flowering (cm)") +
  ggtitle("STEM.FL by ORZ")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means

#boxplot
ggplot(data=TP, aes(x=ORS, y=STEM.FL))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Flowering (cm)") +
  ggtitle("STEM.FL by ORS")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means B with D and M

#TP DESTS MNS
#distribution
hist(TPDM$sqrtSTEM.FL) #sqrt okay
#outliers
mean(TPDM$STEM.FL, na.rm=TRUE)
sd(TPDM$STEM.FL, na.rm=TRUE)
6.18+(3*2.3) #=13.08, outliers = none

#lmer vs lm
lmesfl <- lmer(STEM.FL~DESTZ*ORZ+(1+ORZ|ORS), data=TPDM)
lmesfla <- lmer(STEM.FL~DESTZ*ORZ+(1|ORS), data=TPDM)
anova(lmesfl, lmesfla) #ORZ not sig p=1 chisq=0 AIC=139.92, AICa=135.92
lmslf <- lm(STEM.FL~DESTZ*ORZ, data=TPDM)
x <- -2*logLik(lmslf, REML=T) +2*logLik(lmesfla, REML=T)
x
pchisq(x, df=5, lower.tail=F)
#logLik= 0, p=1, random ORS not sig

#check assumptions of best model
lmslfR <- resid(lmslf) 
lmslfF <- fitted(lmslf)
plot(lmslfF, lmslfR) #raw okay
abline(h=0, col=c("red"))
hist(lmslfR) #raw okay
qqnorm(lmslfR, main="Q-Q plot for residuals") 
qqline(lmslfR) #raw okay

#NOTE: Since random effects are not sig, use the linear model below
#lm - subset by DESTS=MNS
lmslf2  <- lm(rankSTEM.FL~DESTZ*(ORS/ORZ)+DESTZ*ORS, data=TPDM)
lmslf3  <- update(lmslf2,~.-DESTZ:ORS)
anova(lmslf3, lmslf2) #DZ:OS not sig p=1, F=0
lmslf4  <- update(lmslf2,~.-DESTZ:(ORS/ORZ))
anova(lmslf4, lmslf2) #DZ:(OS/OZ) not sig p=0.38 f=1.0005
lmslf5  <- lm(rankSTEM.FL~DESTZ+(ORS/ORZ)+ORS, data=TPDM)
lmslf6  <- update(lmslf5,~.-DESTZ)
anova(lmslf6, lmslf5) #DZ is sig p=0.023 f=5.82 *
lmslf7  <- update(lmslf5,~.-(ORS/ORZ))
anova(lmslf7, lmslf5) #OS/OZ not sig p=0.69 f=0.49
lmslf8  <- lm(rankSTEM.FL~DESTZ+ORS, data=TPDM)
lmslf9  <- update(lmslf8,~.-ORS)
anova(lmslf9, lmslf8) #OS not sig p=0.96 f=0.0024
lmslf10  <- lm(rankSTEM.FL~DESTZ, data=TPDM)
summary(lmslf10) #intercept=19.16, destz= -7.83, R^2=0.18, F=7.39[1,28], p=0.011

lmslfR  <- resid(lmslf10)
par(mfrow = c(2, 2))
plot(lmslf10) #rank best
par(mfrow = c(1, 1))
hist(lmslfR) #rank best

slfn <- tapply(TPDM[!is.na(TPDM$rankSTEM.FL),]$rankSTEM.FL, TPDM[!is.na(TPDM$rankSTEM.FL),]$DESTZ, length)
slfmean <- tapply(TPDM[!is.na(TPDM$rankSTEM.FL),]$rankSTEM.FL, TPDM[!is.na(TPDM$rankSTEM.FL),]$DESTZ, mean)
slfsd <- tapply(TPDM[!is.na(TPDM$rankSTEM.FL),]$rankSTEM.FL, TPDM[!is.na(TPDM$rankSTEM.FL),]$DESTZ, sd)
slfCV <- (slfsd/slfmean)*100

TPslf <- summarySE(TPDM, measurevar="rankSTEM.FL", groupvars=c("DESTZ")) 
ggplot(data=TPslf, aes(x=DESTZ, y=rankSTEM.FL)) +
  geom_errorbar(aes(ymin=rankSTEM.FL-se, ymax=rankSTEM.FL+se), width=0.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) + geom_point(size=4, position=position_dodge(0.1))+
  xlab("Transplant Zone") + ylab("Ranked Stem Length \nat Flowering (cm)") +
  ggtitle("Mean STEM.FL by DESTZ TPDM") +
  annotate("text", x=c(0.85, 2.20), 
           y=c(19.16, 11.32), 
           label=paste("n=",slfn), size=6, fontface="bold") +
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18)) +
  theme(strip.text.x = element_text(size=20, face="bold")) +
  theme(strip.text.y = element_text(size=20, face="bold")) +
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=25), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=20, face="bold")) +
  scale_x_discrete(labels=c("Beach", "Dune")) +
  theme(axis.title.y = element_text(vjust=1, face="bold", size=25),
        axis.text.y  = element_text(size=20, face="bold"))

###TP Dune
#distribution
hist(TPD2x$sqrtSTEM.FL) #sqrt is best, but tall column at left
#outliers
mean(TPD2x$STEM.FL, na.rm=TRUE)
sd(TPD2x$STEM.FL, na.rm=TRUE)
4.66+(3*1.46) #=9.04, outliers = none

#lm- subset by DESTZ=Dune
lm2slf2  <- lm(STEM.FL~DESTS*(ORS/ORZ)+DESTS*ORS, data=TPD2x, na.action="na.omit")
lm2slf3  <- update(lm2slf2,~.-DESTS:ORS)
anova(lm2slf3, lm2slf2) #DS:OS not sig p=1, F=0
lm2slf4  <- update(lm2slf2,~.-DESTS:(ORS/ORZ))
anova(lm2slf4, lm2slf2) #DS:(OS/OZ) marginally not sig p=0.053 f=2.94 .
lm2slf5  <- lm(STEM.FL~DESTS+(ORS/ORZ)+ORS, data=TPD2x, na.action="na.omit")
lm2slf6  <- update(lm2slf5,~.-DESTS)
anova(lm2slf6, lm2slf5) #DS is sig p=0.27 f=1.26
lm2slf7  <- update(lm2slf5,~.-(ORS/ORZ))
anova(lm2slf7, lm2slf5) #OS/OZ not sig p=0.53 f=0.76
lm2slf8  <- lm(STEM.FL~DESTS+ORS, data=TPD2x, na.action="na.omit")
lm2slf9  <- update(lm2slf8,~.-ORS)
anova(lm2slf9, lm2slf8) #OS not sig p=0.35 f=0.88
lm2slf10  <- lm(STEM.FL~1, data=TPD2x)
summary(lm2slf10)
#intercept=4.66

lm2slfR  <- resid(lm2slf10)
par(mfrow = c(2, 2))
plot(lm2slf10) #okay
par(mfrow = c(1, 1))
hist(lm2slfR) #okay, tall column at left


slf2n <- tapply(TPD2x[!is.na(TPD2x$STEM.FL),]$STEM.FL, TPD2x[!is.na(TPD2x$STEM.FL),]$DESTS, length)
slf2mean <- tapply(TPD2x[!is.na(TPD2x$STEM.FL),]$STEM.FL, TPD2x[!is.na(TPD2x$STEM.FL),]$DESTS, mean)
slf2sd <- tapply(TPD2x[!is.na(TPD2x$STEM.FL),]$STEM.FL, TPD2x[!is.na(TPD2x$STEM.FL),]$DESTS, sd)
slf2CV <- (slf2sd/slf2mean)*100

TP2slf <- summarySE(TPD2x, measurevar="STEM.FL", groupvars=c("DESTS")) 



#********************
#Response Variables: STEM.F

#boxplot
ggplot(data=TP, aes(x=DESTZ, y=STEM.F))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Senescence (cm)") +
  ggtitle("STEM.F by DESTZ")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means.

#boxplot
ggplot(data=TP, aes(x=DESTS, y=STEM.F))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Senescence (cm)") +
  ggtitle("STEM.F by DESTS")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: not much overlap, and different means... more variation in M

#boxplot
ggplot(data=TP, aes(x=ORZ, y=STEM.F))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Senescence (cm)") +
  ggtitle("STEM.F by ORZ")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means

#boxplot
ggplot(data=TP, aes(x=ORS, y=STEM.F))+
  geom_boxplot(width=0.8, position="dodge")+ 
  ylab("Stem Length at Senescence (cm)") +
  ggtitle("STEM.F by ORS")+
  theme_bw() + theme(legend.justification=c(1,0), legend.position="top", 
                     legend.text=element_text(face="bold", size=18), 
                     legend.title=element_text(face="bold", size=18))+
  theme(axis.title.x = element_text(vjust=0.3, face="bold", size=20), 
        axis.text.x  = element_text(vjust=0.3, hjust=0.5, size=18, face="bold"))+
  theme(axis.title.y = element_text(vjust=1, face="bold", size=20),
        axis.text.y  = element_text(size=18, face="bold"))
#NOTE: overlap, but different means B with D and M

#TP DESTS MNS
#distribution
hist(TPDM[!is.na(TPDM$STEM.F),]$sqrtSTEM.F) #sqrt okay
#outliers
mean(TPDM$STEM.F, na.rm=TRUE)
sd(TPDM$STEM.F, na.rm=TRUE)
8.17+(3*4.23) #=20.86, outliers = none

#lmer vs lm
lmesf <- lmer(sqrtSTEM.F~DESTZ*ORZ+(1+ORZ|ORS), data=TPDM)
lmesfa <- lmer(sqrtSTEM.F~DESTZ*ORZ+(1|ORS), data=TPDM)
anova(lmesf, lmesfa) #ORZ not sig p=1 chisq=0 AIC=60.34, AICa=56.34
lmsf <- lm(sqrtSTEM.F~DESTZ*ORZ, data=TPDM)
x <- -2*logLik(lmsf, REML=T) +2*logLik(lmesfa, REML=T)
x
pchisq(x, df=5, lower.tail=F)
#logLik= 2.13e-14, p=1, random ORS not sig

#check assumptions of best model
lmsfR <- resid(lmsf) 
lmsfF <- fitted(lmsf)
plot(lmsfF, lmsfR) #sqrt okay
abline(h=0, col=c("red"))
hist(lmsfR) #sqrt okay
qqnorm(lmsfR, main="Q-Q plot for residuals") 
qqline(lmsfR) #sqrt okay

#NOTE: Since random effects are not sig, use the linear model below
#lm - subset by DESTS=MNS
lmsf2  <- lm(STEM.F~DESTZ*(ORS/ORZ)+DESTZ*ORS, data=TPDM)
lmsf3  <- update(lmsf2,~.-DESTZ:ORS)
anova(lmsf3, lmsf2) #DZ:OS not sig p=1, F=0
lmsf4  <- update(lmsf2,~.-DESTZ:(ORS/ORZ))
anova(lmsf4, lmsf2) #DZ:(OS/OZ) not sig p=0.94 f=0.058
lmsf5  <- lm(STEM.F~DESTZ+(ORS/ORZ)+ORS, data=TPDM)
lmsf6  <- update(lmsf5,~.-DESTZ)
anova(lmsf6, lmsf5) #DZ is sig p=0.15 f=2.18
lmsf7  <- update(lmsf5,~.-(ORS/ORZ))
anova(lmsf7, lmsf5) #OS/OZ not sig p=0.35 f=1.16
lmsf8  <- lm(STEM.F~DESTZ+ORS, data=TPDM)
lmsf9  <- update(lmsf8,~.-ORS)
anova(lmsf9, lmsf8) #OS not sig p=0.87 f=0.026
lmsf10  <- lm(sqrtSTEM.F~1, data=TPDM)
summary(lmsf10) #intercept=8.17

lmsfR  <- resid(lmsf10)
par(mfrow = c(2, 2))
plot(lmsf10) #sqrt okay
par(mfrow = c(1, 1))
hist(lmsfR) #sqrt okay

sfn <- tapply(TPDM[!is.na(TPDM$sqrtSTEM.F),]$sqrtSTEM.F, TPDM[!is.na(TPDM$sqrtSTEM.F),]$DESTZ, length)
sfmean <- tapply(TPDM[!is.na(TPDM$sqrtSTEM.F),]$sqrtSTEM.F, TPDM[!is.na(TPDM$sqrtSTEM.F),]$DESTZ, mean)
sfsd <- tapply(TPDM[!is.na(TPDM$sqrtSTEM.F),]$sqrtSTEM.F, TPDM[!is.na(TPDM$sqrtSTEM.F),]$DESTZ, sd)
sfCV <- (sfsd/sfmean)*100

###TP Dune
#distribution
hist(TPD2x[!is.na(TPDM$STEM.F),]$rankSTEM.F) #rank might be best
#outliers
mean(TPD2x$STEM.F, na.rm=TRUE)
sd(TPD2x$STEM.F, na.rm=TRUE)
5.18+(3*3.28) #=15.02, outliers = none

#lm- subset by DESTZ=Dune
lm2sf2  <- lm(rankSTEM.F~DESTS*(ORS/ORZ)+DESTS*ORS, data=TPD2x, na.action="na.omit")
lm2sf3  <- update(lm2sf2,~.-DESTS:ORS)
anova(lm2sf3, lm2sf2) #DS:OS not sig p=1, F=0
lm2sf4  <- update(lm2sf2,~.-DESTS:(ORS/ORZ))
anova(lm2sf4, lm2sf2) #DS:(OS/OZ) not sig p=1 f=0.4
lm2sf5  <- lm(rankSTEM.F~DESTS+(ORS/ORZ)+ORS, data=TPD2x, na.action="na.omit")
lm2sf6  <- update(lm2sf5,~.-DESTS)
anova(lm2sf6, lm2sf5) #DS is sig p=0.53 f=0.402
lm2sf7  <- update(lm2sf5,~.-(ORS/ORZ))
anova(lm2sf7, lm2sf5) #OS/OZ not sig p=0.64 f=0.57
lm2sf8  <- lm(rankSTEM.F~DESTS+ORS, data=TPD2x, na.action="na.omit")
lm2sf9  <- update(lm2sf8,~.-ORS)
anova(lm2sf9, lm2sf8) #OS not sig p=0.80 f=0.068
lm2sf10  <- lm(rankSTEM.F~1, data=TPD2x, na.action="na.omit")
summary(lm2sf10)
#intercept=14.50

lm2sfR  <- resid(lm2sf10)
par(mfrow = c(2, 2))
plot(lm2sf10) #rank okay, but normQQ plot not great
par(mfrow = c(1, 1))
hist(lm2sfR) #none are great


sf2n <- tapply(TPD2x[!is.na(TPD2x$STEM.FL),]$STEM.FL, TPD2x[!is.na(TPD2x$STEM.FL),]$DESTS, length)
sf2mean <- tapply(TPD2x[!is.na(TPD2x$STEM.FL),]$STEM.FL, TPD2x[!is.na(TPD2x$STEM.FL),]$DESTS, mean)
sf2sd <- tapply(TPD2x[!is.na(TPD2x$STEM.FL),]$STEM.FL, TPD2x[!is.na(TPD2x$STEM.FL),]$DESTS, sd)
sf2CV <- (sf2sd/sf2mean)*100

TP2sf <- summarySE(TPD2x, measurevar="STEM.F", groupvars=c("DESTS")) 

