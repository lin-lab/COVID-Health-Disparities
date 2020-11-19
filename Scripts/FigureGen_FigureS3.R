source('~/Desktop/Covid disparities/health disparity/Scripts/8. Estimated County Rates.R')

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

case.obs = dt.rem$rate_cases
case.est.RE = exp(predict(model)) / dt.rem$PopSize * 1e5
case.est.noRE = exp(predict(model, re.form = NA)) / dt.rem$PopSize * 1e5
log.case.obs = log(case.obs + 1)
log.case.est.RE = log(case.est.RE + 1)
log.case.est.noRE = log(case.est.noRE + 1)

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
              data = dt.rem)

death.obs = dt.rem$rate_deaths
death.est.RE = exp(predict(model)) / dt.rem$PopSize * 1e5
death.est.noRE = exp(predict(model, re.form = NA)) / dt.rem$PopSize * 1e5
log.death.obs = log(death.obs + 1)
log.death.est.RE = log(death.est.RE + 1)
log.death.est.noRE = log(death.est.noRE + 1)

png(file="~/Desktop/Covid disparities/health disparity/Output/FigureS3.png",width=900,height=900)
par(mfrow=c(2,2))

cor(log.case.est.RE, log.case.obs)
cor(log.case.est.noRE, log.case.obs)
max1 = max(c(log.case.est.RE, log.case.obs))
max2 = max(c(log.case.est.noRE, log.case.obs))

plot(log.case.est.RE, log.case.obs,
     main="Log Estimated (RE included) vs Log Observed Case Rates, R2 = 0.999",
     xlim=c(0,max1),
     ylim=c(0,max1),
     xlab="log(Estimated Case Rate + 1)",
     ylab="log(Observed Case Rate + 1)",
     col="royalblue2")
abline(0, 1)
plot(log.case.est.noRE, log.case.obs,
     main="Log Estimated (no RE) vs Log Observed Case Rates, R2 = 0.840",
     xlim=c(0,max2),
     ylim=c(0,max2),
     xlab="log(Estimated Case Rate + 1)",
     ylab="log(Observed Case Rate + 1)",
     col="royalblue2")
abline(0, 1)

cor(log.death.est.RE, log.death.obs)
cor(log.death.est.noRE, log.death.obs)
max1 = max(c(log.death.est.RE, log.death.obs))
max2 = max(c(log.death.est.noRE, log.death.obs))
plot(log.death.est.RE, log.death.obs,
     main="Log Estimated (RE included) vs Log Observed Death Rates, R2 = 0.856",
     xlim=c(0,max1),
     ylim=c(0,max1),
     xlab="log(Estimated Death Rate + 1)",
     ylab="log(Observed Death Rate + 1)",
     col="tomato2")
abline(0, 1)
plot(log.death.est.noRE, log.death.obs,
     main="Log Estimated (no RE) vs Log Observed Death Rates, R2 = 0.603",
     xlim=c(0,max2),
     ylim=c(0,max2),
     xlab="log(Estimated Death Rate + 1)",
     ylab="log(Observed Death Rate + 1)",
     col="tomato2")
abline(0, 1)
dev.off()
