source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

#############################################
# Native Hawaiian Cases
#############################################

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logNHPI) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(logBlack) + scale(logHispanic) + scale(logAmInd) +
                scale(logAsian) + scale(logNHPI) + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]

model = glmer(formula = tot_deaths ~ offset(log(PopSize)) + State +
                scale(noHealthInsurance) + scale(logNHPI)  + (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
tail(confint)[,c(2, 3, 6, 7, 8)]
