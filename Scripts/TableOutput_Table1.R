source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

#############################################
# Cases
#############################################

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                scale(logBlack) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                scale(logHispanic) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                scale(logAmInd) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                scale(logAsian) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                scale(logNHPI) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State + 
                scale(logBlack) + scale(logHispanic) + 
                scale(logAmInd) + scale(logAsian) + scale(logNHPI) +
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
              data = dt.rem)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = 5)

#############################################
# Deaths
#############################################

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logBlack) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logHispanic) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logAmInd) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logAsian) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logNHPI) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State + 
                scale(logBlack) + scale(logHispanic) + 
                scale(logAmInd) + scale(logAsian) + scale(logNHPI) +
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
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = 5)
