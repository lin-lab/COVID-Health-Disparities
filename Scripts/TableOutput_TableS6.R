source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

# Quartile function
quartile = function(x){
  cut(x, 
      breaks=c(-Inf, quantile(x, 0.25), quantile(x, 0.50), quantile(x, 0.75), Inf),
      labels=c("Q1", "Q2", "Q3", "Q4"))
}

dt$BlackCat = quartile(dt$logBlack)
dt$HispanicCat = quartile(dt$logHispanic)
dt$AmIndCat = quartile(dt$logAmInd)
dt$AsianCat = quartile(dt$logAsian)
dt$NHPICat = quartile(dt$logNHPI)

#############################################
# Case Categorical
#############################################

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                BlackCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                HispanicCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                AmIndCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                AsianCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                NHPICat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State + 
                BlackCat + HispanicCat + AmIndCat + AsianCat + NHPICat +
                scale(WBSeg) + scale(WNWSeg) + 
                scale(logPopDensity) + scale(Pop2029) + scale(Pop6099) +
                scale(Male) +  RuralCont +
                scale(HouseholdSize) + scale(noHealthInsurance) + scale(Poverty) +
                scale(noHighSchool) + scale(PercentEduHealthSoc) +
                scale(Smoking) +  scale(Obesity) + scale(Asthma) + 
                scale(Cancer) + scale(COPD) + scale(HF) + scale(HTN) + 
                scale(Stroke) + scale(ICUBeds) + scale(NursingHomeBeds) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = 15)

#############################################
# Deaths Categorical
#############################################

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                BlackCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                HispanicCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                AmIndCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                AsianCat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                NHPICat + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State + 
                BlackCat + HispanicCat + AmIndCat + AsianCat + NHPICat +
                scale(WBSeg) + scale(WNWSeg) + 
                scale(logPopDensity) + scale(Pop2029) + scale(Pop6099) +
                scale(Male) +  RuralCont +
                scale(HouseholdSize) + scale(noHealthInsurance) + scale(Poverty) +
                scale(noHighSchool) + scale(PercentEduHealthSoc) +
                scale(Smoking) +  scale(Obesity) + scale(Asthma) + 
                scale(Cancer) + scale(COPD) + scale(HF) + scale(HTN) + 
                scale(Stroke) + scale(ICUBeds) + scale(NursingHomeBeds) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = 15)
