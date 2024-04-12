# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
#
install.packages("eha")
install.packages("survival")
library(eha)
library(survival)
#
setwd("/Users/isabellameier/Documents/GitHub/StatsII_Spring2024/problemSets/PS04/template copy")
#
data("child", package = "eha")
#
child$sex <- as.factor(child$sex)
#
cph_model <- coxph(Surv(exit, event) ~ m.age + sex, data = child)
#
summary(cph_model)
#
# the estimated coefficient for mother's age indicates the log hazard ratio
# associated with a one unit increase in mother's age 
# exp(coef) is the hazard ratio associated with one year increase in mothers age
# suggests that a one unit increase in mothers age is associated with a 0.76%
# increase in the risk of the event.
# the p value suggests that the coefficient for m.age is significant.
# the estimated coefficient for female sex indicates that being female is
# associated with a decrease in the log hazard ratio compared to male(ref categ)
# the exp(coef) the hazard ratio suggests that female children have a 7.89%
# lower risk of the event.
# the p value suggests that the coefficients are statistically significant
# # the concordance means that the model is just a slightly better predictor
# than chance 