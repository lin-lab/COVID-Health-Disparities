################################
# Data and packages
################################

# package to fit poisson mixed model
require(lme4)

# packages to process results
require(broom.mixed)
require(tidyverse)

# loading data
dt = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
dt$RuralCont = as.factor(dt$RuralCont)
dt$RuralCont = relevel(dt$RuralCont, ref="1")
dt$RuralCont = relevel(dt$RuralCont, ref="2")

# counties with 0 cases to 1 cse
ind = which(dt$tot_cases == 0)
dt$tot_cases[ind] = 1

################################
# Univariable results
################################

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Pop2029) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
pop2029 = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Pop6099) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
pop6099 = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Male) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
male = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                RuralCont +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
ruralurban = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(logBlack) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
black = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(logHispanic) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
hispanic = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(logAmInd) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
amind = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(logAsian) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
asian = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(logNHPI) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
nhpi = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(WBSeg) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
wbseg = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(WNWSeg) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
wnwseg = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(HouseholdSize) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
housesize= confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(noHealthInsurance) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
noHI = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Poverty) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
poverty = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(noHighSchool) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
noHS = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(PercentEduHealthSoc) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
EHS = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Smoking) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
smoking = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Obesity) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
obesity = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Asthma) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
asthma = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Cancer) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
cancer = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(COPD) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
COPD = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Diabetes) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
diabetes = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(HF) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
HF = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(HTN) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
HTN = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(KD) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
KD = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Stroke) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
stroke = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(ICUBeds) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
icu = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(NursingHomeBeds) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
nursing = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

univariable = rbind(pop2029, pop6099, male, ruralurban,
                    black, hispanic, amind, asian, nhpi, wbseg, wnwseg,
                    housesize, noHI, poverty, noHS, EHS,
                    smoking, obesity, asthma, cancer, COPD, diabetes, HF, HTN, KD, stroke,
                    icu, nursing) %>% print(n = Inf)

################################
# Multivariable results
################################

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
                scale(Pop2029) + scale(Pop6099) + scale(Male) +  RuralCont +
                scale(logBlack) + scale(logHispanic) + 
                scale(logAmInd) + scale(logAsian) + scale(logNHPI) +
                scale(WBSeg) + scale(WNWSeg) +
                scale(HouseholdSize) + scale(noHealthInsurance) + scale(Poverty) +
                scale(noHighSchool) + scale(PercentEduHealthSoc) +
                scale(Smoking) +  scale(Obesity) + scale(Asthma) + 
                scale(Cancer) + scale(COPD) + scale(Diabetes) + scale(HF) + scale(HTN) +  scale(KD) +
                scale(Stroke) + scale(ICUBeds) + scale(NursingHomeBeds) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
multivariable = confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

################################
# Saving results
################################

write.csv(univariable, 
          file="~/Desktop/Covid disparities/health disparity/Output/CFR Total Univariable.csv",
          row.names=F)

write.csv(multivariable, 
          file="~/Desktop/Covid disparities/health disparity/Output/CFR Total Multivariable.csv",
          row.names=F)
