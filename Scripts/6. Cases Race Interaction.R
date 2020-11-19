source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

model = glmer(formula = tot_cases ~ offset(log(PopSize)) + State +
                scale(WBSeg) + 
                scale(logPopDensity) + scale(Pop2029) + scale(Pop6099) +
                scale(Male) +  RuralCont +
                scale(HouseholdSize) + scale(noHealthInsurance) + scale(Poverty) +
                scale(noHighSchool) + scale(PercentEduHealthSoc) +
                scale(Smoking) +  scale(Obesity) + scale(Asthma) + 
                scale(Cancer) + scale(COPD) + scale(HF) + scale(HTN) + 
                scale(Stroke) + scale(ICUBeds) + scale(NursingHomeBeds) +
                WNWSegQ4 * scale(logBlack) +
                WNWSegQ4 * scale(logHispanic) + 
                WNWSegQ4 * scale(logAmInd) +
                WNWSegQ4 * scale(logAsian) + 
                WNWSegQ4 * scale(logNHPI) + 
                (1 | CountyI),
              family=poisson(),
              nAGQ=0,
              data = dt.rem)
summary(model)
coef = summary(model)$coefficients
index1 = 75:79
index2 = 80:84
estimate.less = coef[c(index1),"Estimate"]
estimate.more = coef[c(index1),"Estimate"] + coef[c(index2),"Estimate"]
estimate.rel = coef[c(index2),"Estimate"]
SE.less = coef[c(index1),"Std. Error"]
SE.more = sqrt(coef[c(index1),"Std. Error"]^2 + coef[c(index2),"Std. Error"]^2 + 
  2 * diag(vcov(model)[index1, index2]))
SE.rel = coef[c(index2),"Std. Error"]

mean.less = c(NA, NA, exp(estimate.less))
lower.less = c(NA, NA, exp(estimate.less - 1.96*SE.less))
upper.less = c(NA, NA, exp(estimate.less + 1.96*SE.less))

mean.more = c(NA, NA, exp(estimate.more))
lower.more = c(NA, NA, exp(estimate.more - 1.96*SE.more))
upper.more = c(NA, NA, exp(estimate.more + 1.96*SE.more))

mean.rel = format(round(exp(estimate.rel), digits=2), nsmall=2)
lower.rel = format(round(exp(estimate.rel - 1.96*SE.rel), digits=2), nsmall=2)
upper.rel = format(round(exp(estimate.rel + 1.96*SE.rel), digits=2), nsmall=2)

mean = c(rbind(mean.less, mean.more))
lower = c(rbind(lower.less, lower.more))
upper = c(rbind(upper.less, upper.more))

variables = c(NA, "County Race/Ethnicity (%)", "Black/African American", "Hispanic/Latino", 
              "American Indian/Native Alaskan", "Asian", "Native Hawaiian/Pacific Islander")
values = paste0(mean.rel, " (", lower.rel, ", ", upper.rel, ")")
estimates = c("Segregation, Case Rate", "Effect Modification (95% CI)", values)
tabletext<-cbind(variables, estimates)

png(file="~/Desktop/Covid disparities/health disparity/Output/Case_Race_Interaction.png",width=1100,height=400)
forestplot(tabletext,
           legend_args = fpLegend(pos = list(x=.50, y=0.90)), 
           boxsize=0.20,
           clip=c(0.9, 1.3), 
           xlog=TRUE,
           legend=c("Less White/non-White Segregation", "More White/non-White Segregation"),
           mean = cbind(mean.less, mean.more),
           lower = cbind(lower.less, lower.more),
           upper = cbind(upper.less, upper.more),
           xticks=c(0.9, 1.0, 1.1, 1.2, 1.3),
           grid = structure(c(0.9, 1.1, 1.2, 1.3),
                            gp = gpar(lty=2, col="#CCCCFF")),
           col=fpColors(box=c("royalblue1", "royalblue4"),
                        line=c("royalblue1", "royalblue4")),
           xlab="Case Rate Relative Risk",
           txt_gp = fpTxtGp(summary=gpar(cex=1.75),
                            label=gpar(cex=1.75),
                            ticks=gpar(cex=1.75),
                            xlab=gpar(cex=1.75),
                            legend=gpar(cex=1.75)),
           is.summary=c(rep(TRUE, 2), rep(FALSE,5)),
           graphwidth=unit(12.5,"cm"),
           lineheight=unit(2,'cm'),
           linewidth=unit(0.5,'cm'))
dev.off()
