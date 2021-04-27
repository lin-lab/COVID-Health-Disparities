source("~/Desktop/Covid disparities/health disparity/Scripts/County Data Model.R")

library(splines)
require(MASS)
require(nlme)
require(dplyr)

##############################
# Housekeeping
##############################

# function for b-splines
seq_days <- function(x, delta){
  out <- seq(min(x) + delta, max(x), delta)
}

# set <=0 cases to 1 case so log offset always defined
# ind = which(dm2$total.cases == 0)
# dm2$total.cases[ind] = 1
# ind = which(dm2$lag.cases <= 0)
# dm2$lag.cases[ind] = 1

# ensure no lag cases are less than weekly deaths
# ind = which(dm2$lag.cases < dm2$weekly.deaths)
# dm2$lag.cases[ind] = dm2$weekly.deaths[ind]

# formatting for glmmpql
dm2$FIPS = as.factor(dm2$FIPS)

##############################
# GLMM with population size offset
##############################

# model
model = glmmPQL( 
  fixed = weekly.cases ~ offset(log(PopSize)) + State +
    RuralCont*season + scale(lognonWhite)*season + scale(WNWSeg)*season +
    scale(SES)*season + scale(Comorbidities)*season +
    bs(weeks_int, knots = seq_days(weeks_int, 14)) +
    bs(weeks_int, knots = seq_days(weeks_int, 14)):CensusRegionName,
  random = ~ 1 | FIPS, 
  data = dm2, 
  family = poisson,
  correlation=corAR1()
)

# raw results
results = tidy(model,conf.int=T,exponentiate=T,effects="fixed")
results %>% print(n = Inf)

##############################
# effect sizes for each season
##############################

# indices of interest
index1 = c(52:53, 56:59)
p = length(index1)
index2 = matrix(c(65,67,66,68,69:76), nrow=p, byrow=T)[,1]
index3 = matrix(c(65,67,66,68,69:76), nrow=p, byrow=T)[,2]

#see indices match up
rbind(results$term[index1], results$term[index2], results$term[index3])

# season estimates and standard error
cov.matrix = vcov(model)
estimate = numeric(length = 3*p) 
std.error = numeric(length = 3*p)

for(i in 1:p){
  ind1 = index1[i]
  ind2 = index2[i]
  ind3 = index3[i]
  
  estimate[(i-1)*3 + 1] = results$estimate[ind1]
  estimate[(i-1)*3 + 2] = results$estimate[ind1] + results$estimate[ind2]
  estimate[(i-1)*3 + 3] = results$estimate[ind1] + results$estimate[ind3]
  
  std.error[(i-1)*3 + 1] = sqrt(cov.matrix[ind1,ind1])
  std.error[(i-1)*3 + 2] = sqrt(cov.matrix[ind1,ind1] + cov.matrix[ind2,ind2] +
                                  2*cov.matrix[ind1,ind2]) 
  std.error[(i-1)*3 + 3] = sqrt(cov.matrix[ind1,ind1] + cov.matrix[ind3,ind3] +
                                  2*cov.matrix[ind1,ind3]) 
}

# final results
dt = data.frame(variable = rep(results$term[index1], each=3),
                season = rep(c("spring", "summer", "fall"), 6),
                estimate = exp(estimate),
                lower = exp(estimate - 1.96*std.error),
                upper = exp(estimate + 1.96*std.error))
dt

# saving key results
# write.csv(dt, 
#          file="~/Desktop/Covid disparities/health disparity/Output/Case Rate Weekly Multivariable.csv",
#          row.names=F)
