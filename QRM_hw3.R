
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
  sink()
}

lapply(c("ggplot2", "texreg", "Zelig", "margins", "tidyr", "dplyr"), pkgTest)

# set working directory to parent replication folder
# this shouldn't be impacted where you downloaded 
# the replication files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
finalData <- read.csv("C:/Users/Alkan/Downloads/finalData.csv", stringsAsFactors = T)
finalData$NY.GDP.MKTP.CD <- log(finalData$NY.GDP.MKTP.CD)
finalData <- finalData[, c("year", "country1", "country2", "polity2", "polconiii", "ICSID",
                           "NY.GDP.MKTP.CD","NY.GDP.MKTP.KD.ZG", "NE.TRD.GNFS.ZS","BX.KLT.DINV.WD.GD.ZS","NY.GDP.TOTL.RT.ZS",
                           "timeUntilAnyElec", "demoBin", "anyViolations", "violations", "treatyExistence")]

finalDataCompleteCases <- finalData %>% drop_na()
levels(finalDataCompleteCases$demoBin) <- c("Autocracy", "Anocracy", "Democracy")
finalDataCompleteCases$violations <- ifelse(is.na(finalDataCompleteCases$violations), 0, finalDataCompleteCases$violations)

sign_count <- finalDataCompleteCases %>%
  group_by(year, demoBin) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
sign_count$freq <- sign_count$freq*100

#Figure 2 (Probit) 

prb <- glm(anyViolations ~ polity2*timeUntilAnyElec+ polconiii + log(NY.GDP.MKTP.CD)+ NY.GDP.MKTP.KD.ZG+
             NE.TRD.GNFS.ZS + BX.KLT.DINV.WD.GD.ZS +
             NY.GDP.TOTL.RT.ZS,
           family = binomial(link = "probit"),
           data=finalData)
summary(prb)

###left panel###
interactPlotDemoMEpro <- as.data.frame(cplot(prb, x = "polity2", dx = "timeUntilAnyElec", what = "effect"))
interactPlotDemoMEpro$yvals <- as.numeric(interactPlotDemoMEpro$yvals)


pdf("C:/Users/Alkan/Downloads/Hw3/probittry.pdf")
ggplot(interactPlotDemoMEpro,  aes(x=xvals, y=yvals)) + geom_line(aes(y=yvals)) +
  geom_line(aes(y=upper), color='black', linetype = 4, size=1.25) + 
  geom_line(aes(y=lower), color='black', linetype = 4, size=1.25) + 
  geom_hline(yintercept =0, linetype=2) + ggtitle("Probit")+theme(plot.title = element_text(hjust=0.5))+
  labs(x="Polity", y="Marginal effect of time until election\non Pr(violation=1)") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25),
                          axis.text=element_text(size=24), axis.title=element_text(size=28))

dev.off()


###right panel###
interactPlotDemoMEpro1 <- as.data.frame(cplot(prb, x = "timeUntilAnyElec", dx = "polity2", what = "effect"))
interactPlotDemoMEpro1$yvals <- as.numeric(interactPlotDemoMEpro1$yvals)


pdf("C:/Users/Alkan/Downloads/Hw3/finalprobit2.pdf")
ggplot(interactPlotDemoMEpro1,  aes(x=xvals, y=yvals)) + geom_line(aes(y=yvals)) +
  geom_line(aes(y=upper), color='black', linetype = 4, size=1.25) + 
  geom_line(aes(y=lower), color='black', linetype = 4, size=1.25) + ggtitle("Probit") +
  geom_hline(yintercept =0, linetype=2) +ggtitle("Probit")+theme(plot.title = element_text(hjust=0.5))+
  lims(x=c(-4,0.5), y=c(-0.00015, .0003))+ 
  labs(x="Time until election (years)", y="Marginal effect of level of democracy\non Pr(violation=1)") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25),
                          axis.text=element_text(size=23), axis.title=element_text(size=28))
dev.off()

#Figure 2 (Quasibinomial)

quasi_logit <- glm(anyViolations ~ polity2*timeUntilAnyElec+ polconiii + log(NY.GDP.MKTP.CD)+ NY.GDP.MKTP.KD.ZG+
                     NE.TRD.GNFS.ZS + BX.KLT.DINV.WD.GD.ZS +
                     NY.GDP.TOTL.RT.ZS,
                   data=finalData, family = "quasibinomial")
interactPlotDemoMEq <- as.data.frame(cplot(quasi_logit, x = "polity2", dx = "timeUntilAnyElec", what = "effect"))
interactPlotDemoMEq$yvals <- as.numeric(interactPlotDemoMEq$yvals)

summary(quasi_logit)

###left panel###

pdf("C:/Users/Alkan/Downloads/Hw3/quasi1.pdf")
ggplot(interactPlotDemoMEq,  aes(x=xvals, y=yvals)) + geom_line(aes(y=yvals)) +
  geom_line(aes(y=upper), color='black', linetype = 4, size=1.25) + 
  geom_line(aes(y=lower), color='black', linetype = 4, size=1.25) + ggtitle("Quasibinomial")+theme(plot.title = element_text(hjust=0.5))+
  geom_hline(yintercept =0, linetype=2) +
  labs(x="Polity", y="Marginal effect of time until election\non Pr(violation=1)") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25),
                          axis.text=element_text(size=24), axis.title=element_text(size=28))
dev.off()

###right panel###
interactPlotDemoMEq1 <- as.data.frame(cplot(quasi_logit, x = "timeUntilAnyElec", dx = "polity2", what = "effect"))
interactPlotDemoMEq1$yvals <- as.numeric(interactPlotDemoMEq1$yvals)

pdf("C:/Users/Alkan/Downloads/Hw3/quasi2.pdf")
ggplot(interactPlotDemoMEq1,  aes(x=xvals, y=yvals)) + geom_line(aes(y=yvals)) +
  geom_line(aes(y=upper), color='black', linetype = 4, size=1.25) + 
  geom_line(aes(y=lower), color='black', linetype = 4, size=1.25) + 
  geom_hline(yintercept =0, linetype=2) +ggtitle("Quasibinomial")+theme(plot.title = element_text(hjust=0.5))+
  lims(x=c(-4,0.5), y=c(-0.00015, .0003))+ 
  labs(x="Time until election (years)", y="Marginal effect of level of democracy\non Pr(violation=1)") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25),
                          axis.text=element_text(size=23), axis.title=element_text(size=28))
dev.off()

########
