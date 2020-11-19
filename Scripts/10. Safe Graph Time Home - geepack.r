source('~/Desktop/Covid disparities/health disparity/Scripts/9. Load Mobility Data.R')

# -------------------------------------------------
#     Analysis of social distancing & mobility 
# -------------------------------------------------

# generate spline knots, fixed interval of days apart
seq_days <- function(x, delta){
	out <- seq(min(x) + delta, max(x), delta)
}

# fit models for behavioural responses
fit_behav_model <- function(varname, data){

	data <- as.data.frame(data)

	# enter covariates as needed below
	model_formula <- as.formula(gsub('\n', ' ', paste(varname, '~', 
	'
		State + 
                scale(logBlack) + 
                scale(logHispanic) + scale(logAmInd) + scale(logAsian) + scale(logNHPI) + 
                scale(WBSeg) + scale(WNWSeg) + 
                scale(logPopDensity) + scale(Pop2029) + scale(Pop6099) +
                scale(Male) + RuralCont +
                scale(HouseholdSize) + scale(noHealthInsurance) + scale(Poverty) +
                scale(noHighSchool) + scale(PercentEduHealthSoc) +
                scale(Smoking) +  scale(Obesity) + scale(Asthma) + scale(Cancer) + scale(COPD) + 
                scale(HF) + scale(HTN) + scale(Stroke) + scale(ICUBeds) + scale(NursingHomeBeds) +
		bs(days_int, knots = seq_days(days_int, 14)) +
		weekday
	'
	)))

	# geepack cannot hanle missing data; remove these
	excl <- is.na(predict(lm(model_formula, data, na.action = na.exclude)))

	data <- data[!excl,]
	data$State = as.factor(as.character(data$State))

	geepack::geeglm( 
		formula = model_formula,
		id = factor(FIPS), 
		corstr = "ar1",
		data = data, 
		family = gaussian(link = "identity"),
		control = geese.control("epsilon" = 1e-4, "maxit" = 50000
		)
	)
}

# list of mobility covariates we want to look at 
mob_list <- c(
  "SafeGraph %Time Home" = "sg_time_home"
)

# fit the models
mob_fits <- lapply(mob_list, fit_behav_model, data = dm2)

# forest plot
model = mob_fits$`SafeGraph %Time Home`
summary(model)
confint = tidy(model,conf.int=TRUE,exponentiate=FALSE,effects="fixed")
confint[52:79,c(1:2, 5:7)] %>% print(n = Inf)

index.race = 1:7 + 51
index.dem1 = 8:11 + 51
index.dem2 = 12:13 + 51
index.soc = 14:18 + 51
index.med = 19:28 + 51

estimate = format(round(confint$estimate, digits = 2), nsmall=2)
low = format(round(confint$conf.low, digits = 2), nsmall=2)
high = format(round(confint$conf.high, digits = 2), nsmall=2)
values = paste0(estimate, " (", low, ", ", high, ")")
estimates = c("% Time Home","Change (95% CI)", 
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

png(file="~/Desktop/Covid disparities/health disparity/Output/Mobility.png",width=1200,height=1200)
forestplot(tabletext, 
           boxsize=0.25,
           mean  = mean, 
           lower = lower,
           upper = upper,
           is.summary=c(rep(TRUE, 3), rep(FALSE, 7), TRUE, rep(FALSE,7), 
                        TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 10)),
           clip=c(-3.5, 4), 
           vertices=TRUE,
           xticks=c(-2, -1, 0, 1, 2, 3),
           grid = structure(c(-2, -1, 0, 1, 2, 3),
                            gp = gpar(lty=2, col="orchid2")),
           col=fpColors(box="orchid4",line="orchid4", summary="orchid4"),
           xlab="Change in % Time Home",
           txt_gp = fpTxtGp(summary=gpar(cex=2.0),
                            label=gpar(cex=2.0),
                            ticks=gpar(cex=2.0),
                            xlab=gpar(cex=2.0)))
dev.off()

