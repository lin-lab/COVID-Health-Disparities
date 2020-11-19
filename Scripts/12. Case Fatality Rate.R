source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

model = glmer(formula = tot_deaths ~ offset(log(tot_cases)) + State + 
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
              data = dt.case)
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed")
confint[-(1:51),c(2, 3, 6, 7, 8)] %>% print(n = Inf)

index.race = 1:7 + 51
index.dem1 = 8:11 + 51
index.dem2 = 12:13 + 51
index.soc = 14:18 + 51
index.med = 19:28 + 51

estimate = format(round(confint$estimate, digits = 2), nsmall=2)
low = format(round(confint$conf.low, digits = 2), nsmall=2)
high = format(round(confint$conf.high, digits = 2), nsmall=2)
values = paste0(estimate, " (", low, ",", high, ")")
estimates = c("Case Fatality Rate","RR (95% CI)", 
              NA, values[index.race],
              NA, values[index.dem1], "Reference", values[index.dem2],
              NA, values[index.soc],
              NA, values[index.med])

mean = c(NA, NA, 
         NA, confint$estimate[index.race],
         NA, confint$estimate[index.dem1], NA, confint$estimate[index.dem2], 
         NA, confint$estimate[index.soc], 
         NA, confint$estimate[index.med])
lower = c(NA, NA, 
          NA, confint$conf.low[index.race],
          NA, confint$conf.low[index.dem1], NA, confint$conf.low[index.dem2], 
          NA, confint$conf.low[index.soc], 
          NA, confint$conf.low[index.med])
upper = c(NA, NA, 
          NA, confint$conf.high[index.race],
          NA, confint$conf.high[index.dem1], NA, confint$conf.high[index.dem2], 
          NA, confint$conf.high[index.soc], 
          NA, confint$conf.high[index.med])
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
              "Stroke (%)", "ICU Beds", "Nursing Home Beds")

tabletext<-cbind(variables, estimates)

png(file="~/Desktop/Covid disparities/health disparity/Output/CFR_RR.png",width=1300,height=1200)
forestplot(tabletext, 
           boxsize=0.25,
           mean  = mean, 
           lower = lower,
           upper = upper,
           is.summary=c(rep(TRUE, 3), rep(FALSE, 7), TRUE, rep(FALSE,7), 
                        TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 10)),
           clip=c(0.70,1.80), 
           vertices=TRUE, 
           xlog=TRUE,
           xticks=c(0.80, 0.9, 1.0, 1.1, 1.2),
           grid = structure(c(0.8, 0.9, 1.1, 1.2),
                            gp = gpar(lty=2, col="darkorange")),
           col=fpColors(box="darkorange2",line="darkorange2", summary="darkorange2"),
           xlab="Case Fatality Rate Relative Risk",
           txt_gp = fpTxtGp(summary=gpar(cex=2.0),
                            label=gpar(cex=2.0),
                            ticks=gpar(cex=2.0),
                            xlab=gpar(cex=2.0)),
           graphwidth=unit(15,"cm"))
dev.off()
