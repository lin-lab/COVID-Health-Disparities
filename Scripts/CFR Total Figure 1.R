require(forestplot)

confint = read.csv("~/Desktop/Covid disparities/health disparity/Output/CFR Total Univariable.csv")

index.dem1 = 1:3
index.dem2 = 4:5
index.race = 6:12
index.soc = 13:17
index.med = 18:29

estimate = format(round(confint$estimate, digits = 2), nsmall=2)
low = format(round(confint$conf.low, digits = 2), nsmall=2)
high = format(round(confint$conf.high, digits = 2), nsmall=2)
values = paste0(estimate, " (", low, ", ", high, ")")
estimates = c("Univariable", "Case Fatality Rate","RR (95% CI)", 
              NA, values[index.dem1], "Reference", values[index.dem2],
              NA, values[index.race],
              NA, values[index.soc],
              NA, values[index.med])

mean = c(NA, NA, NA,  
         NA, confint$estimate[index.dem1], NA, confint$estimate[index.dem2],
         NA, confint$estimate[index.race],
         NA, confint$estimate[index.soc], 
         NA, confint$estimate[index.med])
lower = c(NA, NA, NA, 
          NA, confint$conf.low[index.dem1], NA, confint$conf.low[index.dem2],
          NA, confint$conf.low[index.race],
          NA, confint$conf.low[index.soc], 
          NA, confint$conf.low[index.med])
upper = c(NA, NA, NA, 
          NA, confint$conf.high[index.dem1], NA, confint$conf.high[index.dem2],
          NA, confint$conf.high[index.race],
          NA, confint$conf.high[index.soc], 
          NA, confint$conf.high[index.med])

variables = c(NA, NA, "County-Level Variable",
              "Demographic",
              "Age, 20-29 Years (%)", "Age, 60+ Years (%)",
              "Male (%)", "Metro, >1 million people",  
              "Metro/near metro, <1 million people", "Nonmetro, <20,000 people",
              "Race",
              "Black/African American (%)", "Hispanic/Latino (%)", 
              "American Indian/Native Alaskan (%)", "Asian (%)", 
              "Native Hawaiian/Pacific Islander (%)",
              "White/Black Segregation", "White/non-White Segregation",
              "Socioeconomic",
              "Average Household Size", "No Health Insurance (%)", "Poverty (%)",
              "No High School Diploma (%)", "Education/Healthcare/Social Worker (%)",
              "Health",
              "Smokers (%)", "Obesity (%)", "Asthma (%)", "Cancer (%)", 
              "COPD (%)", "Diabetes (%)", "Heart Failure (%)", "Hypertension (%)", 
              "Kidney Disease (%)", "Stroke (%)", "ICU Beds", "Nursing Home Beds")

tabletext<-cbind(variables, estimates)

png(file="~/Desktop/Covid disparities/health disparity/Output/CFR Total Univariable.png",width=1300,height=1200)
forestplot(tabletext, 
           boxsize=0.25,
           mean  = mean, 
           lower = lower,
           upper = upper,
           is.summary=c(rep(TRUE, 4), rep(FALSE, 6), TRUE, rep(FALSE,7), 
                        TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 12)),
           clip=c(0.80,1.80),  
           vertices=TRUE, 
           xlog=TRUE,
           xticks=c(0.85, 1.0, 1.20, 1.40),
           grid = structure(c(0.85, 1.20, 1.40),
                            gp = gpar(lty=2, col="orchid1")),
           col=fpColors(box="orchid4",line="orchid4", summary="orchid4"),
           xlab="Case Fatality Rate Relative Risk",
           txt_gp = fpTxtGp(summary=gpar(cex=2.0),
                            label=gpar(cex=2.0),
                            ticks=gpar(cex=2.0),
                            xlab=gpar(cex=2.0)),
           graphwidth=unit(15,"cm"))
dev.off()
