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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/isabellameier/Documents/GitHub/StatsII_Spring2024/problemSets/PS01/Answers2_PS1")
#
#####################
# Problem 1
#####################

set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# Kolmogorov-Smirnov test function
set.seed(123)
kol_smir_test <- function(sample_size = 1000, location = 0, scale = 1) {
  data <- rcauchy(sample_size, location = location, scale = scale)
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  D <- max(abs(empiricalCDF - pnorm(data)))
  print(paste("Kolmogorov-Smirnov test statistic (D) : ", D))
  return(D)
}
# Calling the function
kol_smir_result <- kol_smir_test() 
print(kol_smir_result)

# Calculating the p-value 
D <- 0.13472806160635
pvalue_3 <- 1- pnorm(sqrt(n) * D)
pvalue_3 <- 1 - pnorm(sqrt(1000) * 0.13472806160635)
print(pvalue_3)
# doing the ks.test to compare results
ks_result3 <- ks.test(data, "pnorm")
print(ks_result3)

#####################
# Problem 2
#####################
# Generating the data
set.seed (123)
data_2 <- data.frame(x = runif(200, 1, 10))
data_2$y <- 0 + 2.75*data_2$x + rnorm(200, 0, 1.5)
# defining the function
OLS_obj <- function(beta, x, y) {
  y_esp <- beta[1] + beta[2]*x
  error <- y - y_esp
  return(sum(error^2))
}
# Beta values at first
beta_um <- c(0, 0)
# Optimising with BFGS
bfg_otimo <- optim(par = beta_um, fn = OLS_obj, x = data_2$x, y = data_2$y, method = "BFGS")
# getting the estimated coefficients 
bfg_estcoef <- bfg_otimo$par
# Comparing the coefficients from BFGS with the lm
lm_comp <- lm(y ~ x, data = data_2)
coef_lm <- coef(lm_comp)
#
print(bfg_estcoef)
print(coef_lm)
