#####################
# load libraries
# set wd
# clear global .envir
#####################

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)
library(stats)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/isabellameier/Documents/GitHub/StatsII_Spring2024/problemSets/PS03/template copy")


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("/Users/isabellameier/Documents/GitHub/StatsII_Spring2024/problemSets/PS03/template copy/gdpChange.csv", stringsAsFactors = F)
head(gdp_data)
# 1

any(gdp_data$GDPWdiff == 0)
gdp_data$GDP_cat <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                           ifelse(gdp_data$GDPWdiff < 0, "negative", "no change"))
gdp_data$GDP_cat <- factor(gdp_data$GDP_cat, ordered = TRUE, levels = c("no change", "positive", "negative"))
gdp_model <- multinom(GDP_cat ~ REG + OIL, data = gdp_data)
summary(gdp_model)
# 1 b
gdp_model2 <- polr(GDP_cat ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(gdp_model2)
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("/Users/isabellameier/Documents/GitHub/StatsII_Spring2024/problemSets/PS03/template copy/MexicoMuniData.csv")
head(mexico_elections)
# a
pan_poimod <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                  data = mexico_elections, family = poisson)
summary(pan_poimod)

# being a swing state is associated with a decrease in the expected number of 
# visits. But the coeff is not statistically significant. So there is not enough
# evidence to suggest that there were more visits to swing states than to safe
# 
#b
# one unit increase in marginality, holding all other variables constant, the 
# the expected number of visits decreases by approx. 2.08 units
# the p value for marginality indicates strong evidence against the null
# marginality is highly significant and impacts the expected number of visits
# when PAN.gov is 1 the expected number of visits decreases by approx. 0.31 units
# compared to when is 0. 
# the pvalue for PAN.gov suggests a not significant impact on the expected 
# number of visits (not statistically significant)
#c
cfs <- coef(pan_poimod)
hyp_distrct <- exp(cfs[1] + cfs[2]*1 + cfs[3]*0 + cfs[4]*1)
# 0.0149

