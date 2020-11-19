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

mobility = read.csv("~/Desktop/Covid disparities/health disparity/Data/aggregated_daily_mobility_variables_short.csv")
levels(mobility$date)
mobility$days_int = as.numeric(mobility$date)
mobility$fips = sprintf("%05d", as.numeric(mobility$FIPS))

covariates = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
covariates$fips = sprintf("%05d", as.numeric(covariates$fips))
covariates$RuralCont = as.factor(covariates$RuralCont)
covariates$RuralCont = relevel(covariates$RuralCont, ref="1")
covariates$RuralCont = relevel(covariates$RuralCont, ref="0")

fips = covariates$fips
n = length(fips)
covariates$MobMar = covariates$MobApr = covariates$MobMay = 
  covariates$MobJun = covariates$MobJul = covariates$MobAug = 0
c1 = which(levels(mobility$date) == "03/01/20")
c2 = which(levels(mobility$date) == "04/01/20")
c3 = which(levels(mobility$date) == "05/01/20")
c4 = which(levels(mobility$date) == "06/01/20")
c5 = which(levels(mobility$date) == "07/01/20")
c6 = which(levels(mobility$date) == "08/01/20")
c7 = which(levels(mobility$date) == "09/01/20")

index2 = which(mobility$days_int >= c1 & mobility$days_int < c2)
index3 = which(mobility$days_int >= c2 & mobility$days_int < c3)
index4 = which(mobility$days_int >= c3 & mobility$days_int < c4)
index5 = which(mobility$days_int >= c4 & mobility$days_int < c5)
index6 = which(mobility$days_int >= c5 & mobility$days_int < c6)
index7 = which(mobility$days_int >= c6 & mobility$days_int < c7)
index8 = which(mobility$days_int >= c7)

for(i in 1:n){
  index1 = which(mobility$fips == fips[i])
  
  index = intersect(index1, index2)
  covariates$MobMar[i] = mean(mobility$sg_time_home[index], na.rm=T)
  
  index = intersect(index1, index3)
  covariates$MobApr[i] = mean(mobility$sg_time_home[index], na.rm=T)
  
  index = intersect(index1, index4)
  covariates$MobMay[i] = mean(mobility$sg_time_home[index], na.rm=T)
  
  index = intersect(index1, index5)
  covariates$MobJun[i] = mean(mobility$sg_time_home[index], na.rm=T)
  
  index = intersect(index1, index6)
  covariates$MobJul[i] = mean(mobility$sg_time_home[index], na.rm=T)
  
  index = intersect(index1, index7)
  covariates$MobAug[i] = mean(mobility$sg_time_home[index], na.rm=T)
  
  index = intersect(index1, index8)
  covariates$MobSep[i] = mean(mobility$sg_time_home[index], na.rm=T)
}

# Case rates
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
                scale(MobMar) + scale(MobApr) + scale(MobMay) + 
                scale(MobJun) + scale(MobJul) + scale(MobAug) + scale(MobSep) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = covariates)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

index.race = 1:7 + 51
index.dem1 = 8:11 + 51
index.dem2 = 12:13 + 51
index.soc = 14:18 + 51
index.med = 19:28 + 51
index.mob = 29:35 + 51

estimate = format(round(confint$estimate, digits = 2), nsmall=2)
low = format(round(confint$conf.low, digits = 2), nsmall=2)
high = format(round(confint$conf.high, digits = 2), nsmall=2)
values = paste0(estimate, " (", low, ", ", high, ")")
estimates = c("Case Rate","RR (95% CI)", 
              NA, values[index.race],
              NA, values[index.dem1], "Reference", values[index.dem2],
              NA, values[index.soc],
              NA, values[index.med],
              NA, values[index.mob])

mean = c(NA, NA, 
         NA, confint$estimate[index.race],
         NA, confint$estimate[index.dem1], NA, confint$estimate[index.dem2], 
         NA, confint$estimate[index.soc], 
         NA, confint$estimate[index.med],
         NA, confint$estimate[index.mob])
lower = c(NA, NA, 
          NA, confint$conf.low[index.race],
          NA, confint$conf.low[index.dem1], NA, confint$conf.low[index.dem2], 
          NA, confint$conf.low[index.soc], 
          NA, confint$conf.low[index.med],
          NA, confint$conf.low[index.mob])
upper = c(NA, NA, 
          NA, confint$conf.high[index.race],
          NA, confint$conf.high[index.dem1], NA, confint$conf.high[index.dem2], 
          NA, confint$conf.high[index.soc], 
          NA, confint$conf.high[index.med],
          NA, confint$conf.high[index.mob])
variables = c(NA, "County-Level Variable",
              "Race",
              "Black/African American (%)", "Hispanic/Latino (%)", 
              "American Indian/Native Alaskan (%)", "Asian (%)", 
              "Native Hawaiian/Pacific Islander (%)",
              "White/Black Segregation", "White/non-White Segregation",
              "Demographic",
              "Population Density", "Age, 20-29 Years (%)", "Age, 60+ Years (%)",
              "Male (%)", "Nonmetro, <20,000 people", 
              "Metro/near metro, <1 million people", "Metro, >1 million people", 
              "Socioeconomic",
              "Average Household Size", "No Health Insurance (%)", "Poverty (%)",
              "No High School Diploma (%)", "Education/Healthcare/Social Worker (%)",
              "Health",
              "Smokers (%)", "Obesity (%)", "Asthma (%)", "Cancer (%)", 
              "COPD (%)", "Heart Failure (%)", "Hypertension (%)", 
              "Stroke (%)", "ICU Beds", "Nursing Home Beds",
              "Mobility",
              "Average Time Home in March (%)", "Average Time Home in April (%)",
              "Average Time Home in May (%)", "Average Time Home in June (%)",
              "Average Time Home in July (%)", "Average Time Home in August (%)",
              "Average Time Home in September (%)")

tabletext<-cbind(variables, estimates)

png(file="~/Desktop/Covid disparities/health disparity/Output/Case_Forest_Plot_Mob.png",width=1200,height=1200)
forestplot(tabletext, 
           boxsize=0.25,
           mean  = mean, 
           lower = lower,
           upper = upper,
           is.summary=c(rep(TRUE, 3), rep(FALSE, 7), TRUE, rep(FALSE,7), 
                        TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 10), TRUE, rep(FALSE, 7)),
           clip=c(0.70,1.80),  
           vertices=TRUE, 
           xlog=TRUE,
           xticks=c(0.75, 1.0, 1.25, 1.5),
           grid = structure(c(0.75, 1.25, 1.50),
                            gp = gpar(lty=2, col="#CCCCFF")),
           col=fpColors(box="royalblue",line="darkblue", summary="darkred"),
           xlab="Case Rate Relative Risk",
           txt_gp = fpTxtGp(summary=gpar(cex=2.0),
                            label=gpar(cex=2.0),
                            ticks=gpar(cex=2.0),
                            xlab=gpar(cex=2.0)),
           graphwidth=unit(15,"cm"))
dev.off()

# Death rates
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
                scale(MobMar) + scale(MobApr) + scale(MobMay) + 
                scale(MobJun) + scale(MobJul) + scale(MobAug) + scale(MobSep) +
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = covariates)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

estimate = format(round(confint$estimate, digits = 2), nsmall=2)
low = format(round(confint$conf.low, digits = 2), nsmall=2)
high = format(round(confint$conf.high, digits = 2), nsmall=2)
values = paste0(estimate, " (", low, ", ", high, ")")
estimates = c("Death Rate","RR (95% CI)", 
              NA, values[index.race],
              NA, values[index.dem1], "Reference", values[index.dem2],
              NA, values[index.soc],
              NA, values[index.med],
              NA, values[index.mob])

mean = c(NA, NA, 
         NA, confint$estimate[index.race],
         NA, confint$estimate[index.dem1], NA, confint$estimate[index.dem2], 
         NA, confint$estimate[index.soc], 
         NA, confint$estimate[index.med],
         NA, confint$estimate[index.mob])
lower = c(NA, NA, 
          NA, confint$conf.low[index.race],
          NA, confint$conf.low[index.dem1], NA, confint$conf.low[index.dem2], 
          NA, confint$conf.low[index.soc], 
          NA, confint$conf.low[index.med],
          NA, confint$conf.low[index.mob])
upper = c(NA, NA, 
          NA, confint$conf.high[index.race],
          NA, confint$conf.high[index.dem1], NA, confint$conf.high[index.dem2], 
          NA, confint$conf.high[index.soc], 
          NA, confint$conf.high[index.med],
          NA, confint$conf.high[index.mob])

tabletext<-cbind(variables, estimates)

png(file="~/Desktop/Covid disparities/health disparity/Output/Death_Forest_Plot_Mob.png",width=1200,height=1200)
forestplot(tabletext, 
           boxsize=0.25,
           mean  = mean, 
           lower = lower,
           upper = upper,
           is.summary=c(rep(TRUE, 3), rep(FALSE, 7), TRUE, rep(FALSE,7), 
                        TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 10), TRUE, rep(FALSE, 7)),
           clip=c(0.70,1.80), 
           vertices=TRUE, 
           xlog=TRUE,
           xticks=c(0.75, 1.0, 1.25, 1.5),
           grid = structure(c(0.75, 1.25, 1.50),
                            gp = gpar(lty=2, col="#FFCCCC")),
           col=fpColors(box="darkred",line="darkred", summary="darkred"),
           xlab="Death Rate Relative Risk",
           txt_gp = fpTxtGp(summary=gpar(cex=2.0),
                            label=gpar(cex=2.0),
                            ticks=gpar(cex=2.0),
                            xlab=gpar(cex=2.0)),
           graphwidth=unit(15,"cm"))
dev.off()
