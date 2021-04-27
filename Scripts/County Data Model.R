require(lubridate)
require(broom.mixed)

#####################################
# Data processing
#####################################

# Loading data
cases = read.csv("~/Desktop/Covid disparities/health disparity/Data/covid_confirmed_usafacts.csv", fileEncoding="UTF-8-BOM")
deaths = read.csv("~/Desktop/Covid disparities/health disparity/Data/covid_deaths_usafacts.csv", fileEncoding="UTF-8-BOM")
covariates = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")

# Keep only counties with repository covariates
cases = subset(cases, countyFIPS %in% covariates$fips)
deaths = subset(deaths, countyFIPS %in% covariates$fips)

# Daily deaths, 7 day lag
n.col = ncol(cases)
lag.d = 7
deaths.weekly = deaths

for(i in (lag.d+1+4):n.col){
  deaths.weekly[,i] = deaths[,i] - deaths[,i-lag.d]
}

# Daily cases, 7 day lag
n.col = ncol(cases)
lag.d = 7
cases.weekly = cases

for(i in (lag.d+1+4):n.col){
  cases.weekly[,i] = cases[,i] - cases[,i-lag.d]
}

#####################################
# Long form matrix
#####################################

# Beginning and end dates for modeling
ind1 = which(colnames(cases) == "X3.23.20")
ind2 = which(colnames(cases) == "X12.21.20")
ind3 = seq(from=ind1, to=ind2, by=7)    #Change for every day or 7 days
x = seq(as.Date("2020/3/23"), as.Date("2020/12/21"), by="week")    #Change for every day or week
y = as.factor((x >= as.Date("2020/6/20")) + (x >= as.Date("2020/9/22")))

# Constants
n = nrow(deaths.weekly)
m = length(ind3)

# Long form columns
FIPS = rep(cases$countyFIPS, each=m)
total.cases = as.vector(t(as.matrix(cases[, ind3])))
total.deaths = as.vector(t(as.matrix(deaths[, ind3])))
weekly.cases = as.vector(t(as.matrix(cases.weekly[, ind3])))
weekly.cases[weekly.cases < 0] = 0
weekly.deaths = as.vector(t(as.matrix(deaths.weekly[, ind3])))
weekly.deaths[weekly.deaths < 0] = 0
date = rep(x, n)
season = rep(as.factor(y), n)

# Deaths lag 1 week (ex. 5/8 deaths matched with 5/1 cases)
deaths.weekly.lag = as.matrix(deaths.weekly[, ind3])
deaths.weekly.lag[,1:(m-1)] = deaths.weekly.lag[,2:m]
deaths.weekly.lag[,m] = NA
weekly.deaths.lag = as.vector(t(deaths.weekly.lag))
weekly.deaths.lag[weekly.deaths.lag < 0] = 0

# Long form data
data = data.frame(FIPS, total.cases, total.deaths, 
                  weekly.cases, weekly.deaths, weekly.deaths.lag,
                  date, season)
data$weeks_int = rep(1:m, n)
data$fips = sprintf("%05d", as.numeric(data$FIPS))

covariates$fips = sprintf("%05d", as.numeric(covariates$fips))
covariates$RuralCont = as.factor(covariates$RuralCont)
covariates$RuralCont = relevel(covariates$RuralCont, ref="1")
covariates$RuralCont = relevel(covariates$RuralCont, ref="2")
covariates$CensusRegionName = as.factor(covariates$CensusRegionName)
covariates$CensusDivisionName = as.factor(covariates$CensusDivisionName)

colnames(covariates)
covariates$SES = apply(covariates[,c(31:33)], 1, mean)
covariates$Comorbidities = apply(covariates[,c(40:49)], 1, mean)
covariates$lognonWhite = log(100 - exp(covariates$logWhite) + 1)

dm2 = merge(data, covariates, by = 'fips', all = F)
rm(list=setdiff(ls(), "dm2"))
