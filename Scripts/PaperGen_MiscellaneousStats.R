# loading data
dt = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
dt$RuralCont = as.factor(dt$RuralCont)
dt$RuralCont = relevel(dt$RuralCont, ref="1")
dt$RuralCont = relevel(dt$RuralCont, ref="2")

# total cases and deaths
sum(dt$tot_cases)
sum(dt$tot_deaths)

# cases by census region
ind = dt$CensusRegionName == "Midwest"
sum(dt$tot_cases[ind]) / sum(dt$PopSize[ind]) * 100
ind = dt$CensusRegionName == "South"
sum(dt$tot_cases[ind]) / sum(dt$PopSize[ind]) * 100
ind = dt$CensusRegionName == "West"
sum(dt$tot_cases[ind]) / sum(dt$PopSize[ind]) * 100
ind = dt$CensusRegionName == "Northeast"
sum(dt$tot_cases[ind]) / sum(dt$PopSize[ind]) * 100

# deaths by census region
ind = dt$CensusRegionName == "Northeast"
sum(dt$tot_deaths[ind]) / sum(dt$PopSize[ind]) * 10000
ind = dt$CensusRegionName == "Midwest"
sum(dt$tot_deaths[ind]) / sum(dt$PopSize[ind]) * 10000
ind = dt$CensusRegionName == "South"
sum(dt$tot_deaths[ind]) / sum(dt$PopSize[ind]) * 10000
ind = dt$CensusRegionName == "West"
sum(dt$tot_deaths[ind]) / sum(dt$PopSize[ind]) * 10000
