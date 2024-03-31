setwd("/Users/isabellameier/Desktop/Replication stats")
## install packages
library(tidyverse)
library(foreign)
library(gtools)
library(Hmisc)
library(stargazer)
# I wanted to first do a Poisson glm on the Canada set since the "taxnum"variable
# is a count variable
# Canada info
can <- read.csv("data/canreplication.csv")

can$region <- can$province
# fig models
canrelinc <- read.csv("data/CanRelIncs.csv") 
canincs <- read.csv("data/CanIncCats.csv")
canincs$mid <- (canincs$Upp + canincs$Low)/2
#
ocr <- c(4, 6, 7, 8, 9, 11)           
otherchristian <- apply(canrelinc[ocr, 2:ncol(canrelinc)], FUN = sum, 
                        MARGIN = 2)

canrelinc$Religion <- as.character(canrelinc$Religion)
canrelinc <- rbind(canrelinc, c("Other Christian", rep(NA, 33)))
canrelinc[nrow(canrelinc), 2:ncol(canrelinc)] <- otherchristian

avgincome <- rep(NA, nrow(canrelinc))

for(i in 1:length(avgincome)){
  avgincome[i] <- weighted.mean(canincs$mid[1:33], canrelinc[i, 2:34])
}
sapply(canrelinc, class) # IM, because I got an error saying that my argument 
# wasn't numeric. I checked the class of the columns and it returned character
canrelinc[, 2:34] <- lapply(canrelinc[,  2:34], as.numeric) #IM
for(i in 1:length(avgincome)){
  avgincome[i] <- weighted.mean(canincs$mid[1:33], canrelinc[i, 2:34])
}
# List of rich/poor canadian religions

canrelsmall <- data.frame(religion = canrelinc$Religion, relincome = avgincome)
totinc <- subset(canrelsmall, religion == "Total")$relincome
canpoorRels <- canrelsmall$religion[canrelsmall$relincome < totinc]
canRichRels <- canrelsmall$religion[canrelsmall$relincome > totinc]
# trying glm instead of the original lm 
mod13can2 <- glm(taxnum ~ religionlink + union + Male + 
                 agecat +  marital + Bachelor + incnum + taxarg + jl2 + religion, 
                 family = poisson(link = "log"),
                 data = subset(can, religion %in%canpoorRels & religion != "Non-religious"), 
               weights = weight)
# testing assumptions
# residuals dispersion
residuals13_2 <- residuals(mod13can2, type = "deviance")
plot(residuals13_2)
# sum of squared residuals
devquad <- sum(residuals13_2^2)
#598.542
# dispersion test
install.packages("AER")
library(AER)
dispersiontest(mod13can2)
#
print(canpoorRels)
print(canRichRels)
# gave up on the poisson because there are too many categorical variables and
# I wouldn't have sufficient knowledge to choose a base category for each
# So I will remove "marital" to see how does it change Figure 2

mod13Can <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + religion,
               data = subset(can, religion %in%canpoorRels & religion != "Non-religious"), 
               weights = weight)
mod14Can <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + religion,
               data = subset(can, religion %in%canRichRels & religion != "Non-religious"), 
               weights = weight)
# List of poor canadian ethnicities (all but white), leaving out other(Author) 
mod15Can <- lm(taxnum ~ racelink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + race, 
               data = subset(can, race != "White" & race != "Other"), 
               weights = weight)
mod16Can <- lm(taxnum ~ racelink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2, 
               data = subset(can, race =="White"), 
               weights = weight)

## List of poor/rich Canadian provinces
## From RA calulations using LAD

richcanprovs <- c("Alberta", "Ontario", "Saskatchewan")
poorcanprovs <- unique(can$province)[is.na(match(unique(can$province), richcanprovs))]

mod17Can <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat + Bachelor +incnum + jl2 + taxarg + province, 
               data = subset(can, province %in% poorcanprovs), 
               weights = weight)
mod18Can <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat + Bachelor +incnum + jl2 + taxarg + province, 
               data = subset(can, province %in% richcanprovs), 
               weights = weight)
#
stargazer(list(mod15Can, mod16Can, mod13Can, mod14Can, mod17Can, mod18Can),
          out = "Figs/TaxSubgrpsCan4.html",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))
# uk data 
ukpoorRels <- c("Muslim", "Sikh")
ukrichRels <- unique(uk$religion)[
  !(unique(uk$religion) %in% ukpoorRels)]

mod13Uk <- lm(taxnum ~ religionlink + union+ Male + 
                agecat + Bachelor +incnum + taxarg + jl2 + religion, 
              data = subset(uk, religion %in%ukpoorRels), 
              weights = weight)
mod14Uk <- lm(taxnum ~ religionlink + union+ Male + 
                agecat + Bachelor +incnum + taxarg + jl2 + religion, 
              data = subset(uk, religion %in%ukrichRels& religion != "Non-religious" & 
                              religion != "" & religion != "Other"), 
              weights = weight)



## List of poor UK ethnic groups
## See here https://www.ethnicity-facts-figures.service.gov.uk/work-pay-and-benefits/pay-and-income/household-income/latest
## Assuming both Black categories are poorer than avg. Dropping white irish & other white

UKpoorEths <- c("Bangladeshi", "Black African", "Black Caribbean", "Pakistani")
UKRichEths <- c("Chinese", "Indian", 
                "White - British/English/Welsh/Scottish/Northern Irish")


mod15Uk <- lm(taxnum ~ racelink + union+ Male + 
                agecat + Bachelor +incnum + taxarg + jl2 + race, 
              data = subset(uk, race %in% UKpoorEths), 
              weights = weight)
mod16Uk <- lm(taxnum ~ racelink + union+ Male + 
                agecat + Bachelor +incnum + taxarg + jl2 + race, 
              data = subset(uk, race %in% UKRichEths), 
              weights = weight)

## List of poor and rich UK regions come from Eurostat 2015 NUTS 2 data
## https://ec.europa.eu/eurostat/web/regions/data/database

ukRichRegions <- c("East of England", 
                   "South East", 
                   "Greater London") 
ukPoorRegions <- unique(uk$region)[
  !(unique(uk$region) %in% ukRichRegions)]


mod17Uk <- lm(taxnum ~ regionlink + union+ Male + 
                agecat + Bachelor +incnum + jl2 + taxarg + region, 
              data = subset(uk, region %in% ukPoorRegions), 
              weights = weight)
mod18Uk <- lm(taxnum ~ regionlink + union+ Male + 
                agecat + Bachelor +incnum + jl2 + taxarg + region, 
              data = subset(uk, region %in% ukRichRegions), 
              weights = weight)


stargazer(list(mod15Uk, mod16Uk, mod13Uk, mod14Uk, mod17Uk, mod18Uk),
          out = "Figs/TaxSubgrpsUk.html",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

## List of poor and rich German regions come from Eurostat 2015 NUTS 2 data
## https://ec.europa.eu/eurostat/web/regions/data/database

GerRichRegions <- c("Baden-Wurttemberg", 
                    "Bavaria", 
                    "Hamburg", 
                    "Hesse", 
                    "Rhineland-Palatinate") ## Note, there are a few very close to the average, 
## including some that are thought of as rich (NRW)
GerPoorRegions <- unique(ger$region)[
  !(unique(ger$region) %in% GerRichRegions)]


mod17Ger <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(ger, region %in% GerPoorRegions), 
               weights = weight)
mod18Ger <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(ger, region %in% GerRichRegions), 
               weights = weight)

## Data on eth in germany hard to come by. Dropping other and no answer, assuming
## Germans are rich and others are poor

gerpoorEths <- subset(unique(ger$race), unique(ger$race) != "Germans" & 
                        unique(ger$race) != "No information" &
                        unique(ger$race) != "Other")
gerRichEths <- c("Germans")


mod15Ger <- lm(taxnum ~ racelink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + race, 
               data = subset(ger, race %in% gerpoorEths), 
               weights = weight)
mod16Ger <- lm(taxnum ~ racelink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2, 
               data = subset(ger, race %in% gerRichEths), 
               weights = weight)

## Data on German religious income hard to come by as well. 
## Treating only muslims as poor and only Cath/Prot as rich

gerpoorRels <- c("Muslim", "Hindu", "Sikh", "Buddhist")
gerrichRels <- c("Catholic", "Protestant")

mod13Ger <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + religion, 
               data = subset(ger, religion %in%gerpoorRels), 
               weights = weight)
mod14Ger <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + religion, 
               data = subset(ger, religion %in%gerrichRels), 
               weights = weight)

stargazer(list(mod15Ger, mod16Ger, mod13Ger, mod14Ger, mod17Ger, mod18Ger),
          out = "Figs/TaxSubgrpsger.html",
          keep = c("racelink", "religionlink", "regionlink"),  
          type = "html", 
          align = TRUE,
          covariate.labels = c("Ethnic linked fate", "Religious linked fate", 
                               "Regional linked fate"),
          dep.var.labels = c(""),
          column.labels = c("Poor", "Rich", "Poor", "Rich", "Poor", "Rich"),
          dep.var.caption = "",
          digits = 2,
          keep.stat = c("n", "rsq"),
          star.cutoffs = c(0.1, 0.05, 0.01))

#
can$Country <- "Canada"
ger$Country <- "Germany"
uk$Country <- "UK"
varstokeep <- c("racelink", "religionlink", "regionlink", "classlink", 
                "joblossnum", "Bachelor", "employment", "union", 
                "Male", "agecat", "weight", "incnum", 
                "respondent_class", "relfreq", "religion", "region", 
                "moreno", "race", "taxarg", "Country", "taxnum")
#
all <- rbind(subset(ger, select = varstokeep), 
             subset(uk, select = varstokeep), 
             subset(can, select = varstokeep))

## Now turning these into figures and a set of pooled effects
canpoorEths <- levels(can$race)[levels(can$race) != "White" & 
                                  levels(can$race) != "Other"]
canrichEths <- c("White")  


all <- smartbind(uk, ger, can)

mod13All <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + religion + Country, 
               data = subset(all, religion %in% c(gerpoorRels, canpoorRels, ukpoorRels)), 
               weights = weight)
mod14All <- lm(taxnum ~ religionlink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + religion + Country, 
               data = subset(all, religion %in%c(gerrichRels, ukrichRels, canRichRels)), 
               weights = weight)

mod15All <- lm(taxnum ~ racelink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + race + Country, 
               data = subset(all, race %in% c(gerpoorEths, UKpoorEths, canpoorEths)), 
               weights = weight)
mod16All <- lm(taxnum ~ racelink + union+ Male + 
                 agecat + Bachelor +incnum + taxarg + jl2 + Country, 
               data = subset(all, race %in% c(gerRichEths, UKRichEths, canrichEths)), 
               weights = weight)


mod17All <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(all, region %in% c(as.character(GerPoorRegions), 
                                                ukPoorRegions, poorcanprovs)), 
               weights = weight)
mod18All <- lm(taxnum ~ regionlink + union+ Male + 
                 agecat + Bachelor +incnum + jl2 + taxarg + region, 
               data = subset(all, region %in% c(GerRichRegions, ukRichRegions, richcanprovs)), 
               weights = weight)


regeffsub <- data.frame(Country = rep(NA, 24), 
                        Group = rep(NA, 24), 
                        Mean = rep(NA, 24), 
                        SD = rep(NA, 24),
                        Dimension = rep(NA, 24))


allsubMods <- list(mod15Uk, mod16Uk, mod13Uk, mod14Uk, mod17Uk, mod18Uk, 
                   mod15Ger, mod16Ger, mod13Ger, mod14Ger, mod17Ger, mod18Ger, 
                   mod15Can, mod16Can, mod13Can, mod14Can, mod17Can, mod18Can, 
                   mod15All, mod16All, mod13All, mod14All, mod17All, mod18All)

cntrys <- c(rep("UK", 6), rep("Germany", 6), rep("Canada", 6), rep("Pooled", 6))
grps <- rep(c("Poor Groups", "Rich Groups"), 12)
dimens <- rep(c("Ethnic", "Ethnic", "Religious", "Religious", "Regional", "Regional"), 4)

for(i in 1:24){
  thismod <- allsubMods[[i]]
  regeffsub$Country[i] <- cntrys[i]
  regeffsub$Group[i] <- grps[i]
  regeffsub$Mean[i] <- coef(thismod)[2]
  regeffsub$SD[i] <- sqrt(vcov(thismod)[2,2])
  regeffsub$Dimension[i] <- dimens[i]
}


regeffsub$Upp <- regeffsub$Mean + 1.96*regeffsub$SD
regeffsub$Low <- regeffsub$Mean - 1.96*regeffsub$SD
regeffsub$Dimension <- factor(regeffsub$Dimension, levels = c("Regional", "Religious", "Ethnic"))
regeffsub$Country <- factor(regeffsub$Country, levels = c("UK", "Germany", "Canada", "Pooled"))

pdf("Figs/FiGure2.pdf", width = 6, height = 3)
ggplot(regeffsub, aes(y = Mean, x = Dimension, group = Group)) + 
  facet_wrap(vars(Country), nrow = 1) + 
  geom_point(cex = 2, aes(pch = Group), position=position_dodge(width=0.1)) + 
  geom_errorbar(aes(ymin = Low, ymax = Upp, x = Dimension),
                position=position_dodge(width=0.1), 
                width = 0) + 
  xlab("") + theme(legend.position="none") + 
  ylab("Effect of Linked Fate on Redistributive Attitudes") + 
  # xlim(c(1, 4)) + 
  scale_fill_grey() + 
  theme_bw() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = "dashed") + 
  scale_shape_manual(values=c(15, 16)) + 
  theme(legend.title = element_blank()) 

dev.off()

