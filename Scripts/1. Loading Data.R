# data management
require(data.table)
require(dplyr)

# plotting
require(ggplot2)
require(usmap)
require(gridExtra)
require(tidyverse)
require(plyr)

# model fitting
require(geepack)
require(lme4)
require(splines)

# results
require(glmnet)
require(broom.mixed)
require(forestplot)

####################################
# Loading Data
####################################

# Combined and cleaned data
all = read.csv("~/Desktop/Covid disparities/health disparity/Data/all.csv")
dt = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
dt$RuralCont = as.factor(dt$RuralCont)
dt$RuralCont = relevel(dt$RuralCont, ref="1")
dt$RuralCont = relevel(dt$RuralCont, ref="0")
dt$CountyNamew.StateAbbrev = as.character(dt$CountyNamew.StateAbbrev)

# Census 2019 data
census = read.csv("~/Desktop/Covid disparities/health disparity/Data/census2019main.csv")
census$fips = sprintf("%05d", as.numeric(census$fips))

# CHR 2020 data
chr = read.csv("~/Desktop/Covid disparities/health disparity/Data/chr2020.csv")
chr$fips = sprintf("%05d", as.numeric(chr$fipscode))
chr = chr[,c("fips", "v011_rawvalue", "v141_rawvalue", "v142_rawvalue")]

####################################
# Other
####################################

topquartile = function(x){
  cut(x, 
      breaks=c(-Inf, quantile(x, 0.75), Inf),
      labels=c("Bottom", "Top"))
}

remove = which((is.na(dt$Asthma) + is.na(dt$Cancer) + is.na(dt$COPD) +
                  is.na(dt$HF) + is.na(dt$Stroke)) != 0 )
dt.rem = dt[-remove,] 

dt.rem$WNWSegQ4 = topquartile(dt.rem$WNWSeg)


dt.case = dt.rem[which(dt.rem$tot_cases > 0),]

# Miscellaneous processing that's been carried over
dt <- as.data.frame(dt)
dt <- as.data.table(dt)

dt.rem <- as.data.frame(dt.rem)
dt.rem <- as.data.table(dt.rem)

dt.case <- as.data.frame(dt.case)
dt.case <- as.data.table(dt.case)
