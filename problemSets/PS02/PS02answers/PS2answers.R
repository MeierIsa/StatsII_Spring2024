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
library(mgcv)
lapply(c(),  pkgTest)
install.packages("gam")
library(gam)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/isabellameier/Documents/GitHub/StatsII_Spring2024/problemSets/PS02/PS02answers")
#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
head(climateSupport)
# Converting the "choice" column into 1s and 0s
climateSupport$choice <- as.numeric(climateSupport$choice == "Supported")
head(climateSupport)
#
sum(is.na(climateSupport$sanctions))
sum(is.infinite(climateSupport$sanctions))
#

climateSupport$countries_num <- as.numeric(sapply(strsplit(as.character(climateSupport$countries), "of"), "[", 1))
head(climateSupport$countries_num)
#
sanctions_mapping <- c("none" = 0, "5%" = 5, "15%" = 15, "20%" = 20)
climateSupport$sanctions_num <- sanctions_mapping[climateSupport$sanctions]
head(climateSupport$sanctions_num)
#
formula <- choice ~ ns(countries_num) + ns(sanctions_num)
model_g <- glm(formula, data = climateSupport, family = binomial(link = "logit"))
summary(model_g)
# Our null hypothesis is that none of the explanatory variables; 
# number of participating countries and the sanctions imposed by it, have an effect 
# on the support given to an international environmental agreement.
# For a 0.05 significance level, p-values smaller than 0.05 suggest that the
# explanatory variable is likely to have a significant effect on the outcome 
# The coefficients estimates indicate that there is a positive association between 
# the number of countries participating in the agreement and it's support and
# a negative association between the the sanctions and the support to an agreement
# Or, a one unit increase in the number of countries is associated with an 
# average change of 0.08 in the log odds of the support taking on a value of 1.
# While a unit increase in the sanctions is associated to with an average change
# of -0.046 in the log odds of the support taking on a value of 1.
# X2 = null dev - res dev with p degrees pf freedom
# X2 = 11783 - 11593
# X2 = 190 with 2 degrees of freedom
# using a pvalue calculator (from the statology.org website) the p value is 0
# which means that the model is useful

# Now I will fit a null model and use ANOVA to compare it to my model to check 
# if what I said above holds

# fitting the model with no explanatory variables to get the null model
null_model <- glm(choice ~ 1, data = climateSupport, family = binomial(link = "logit"))
summary(null_model)
#
anova_res <- anova(null_model, model_g, test = "Chisq")
print(anova_res)
# The pvalue from the ANOVA is very low, near zero, so I believe it supports the
# conclusion that the my model is a better fit compared to the null model

# QUESTION 2
# Defining scenario 1
scenario1 <- data.frame(countries_num = 160, sanctions_num = 5)
# Predicting the log odds of the scenario 1
logodds1 <- predict(model_g, newdata = scenario1, type = "link")
# Exponentiation the log odds to get odds ratio
odds1 <- exp(logodds1)
print(odds1)
# scenario 2
scenario2 <- data.frame(countries_num = 160, sanctions_num = 15)
logodds2 <- predict(model_g, newdata = scenario2, type = "link")
odds2 <- exp(logodds2)
print(odds2)
#

coef_summary <- summary(model_g)$coefficients
coef_countries <- coef_summary["ns(countries_num)", "Estimate"]
coef_sanctions <- coef_summary["ns(sanctions_num)", "Estimate"]
# calculating change in odds for each scenario
oddschange1 <- exp(coef_countries * (scenario1$countries_num - scenario2$countries_num) +
                     coef_sanctions * (scenario1$sanctions_num - scenario2$sanctions_num))
print(oddschange1)
#
oddschange2 <- exp(coef_countries * (scenario2$countries_num - scenario1$countries_num) +
                     coef_sanctions * (scenario2$sanctions_num - scenario1$sanctions_num))
print(oddschange2)

# The odds from the 5% scenario to the 15% scenario is 102.802. Meaning that 
# the odds of support for policies with 5% sanctions is 102.8 tomes higher than 
# to policies with 15% sanction, holding the number of countries constant

# question 2 b
# defining scenario 3
scenario3 <- data.frame(countries_num = 80, sanctions_num = 0)
# predicting the probability of support 
prob_support <- predict(model_g, newdata = scenario3, type = "response")
print(prob_support)
# the probability is 0.537

# question 2 c
modelg_int <- glm(choice ~ ns(countries_num) * ns(sanctions_num), 
                  data = climateSupport, family = binomial(link = "logit"))
#
anova_intest <- anova(modelg_int, model_g, test = "Chisq")
print(anova_intest)

