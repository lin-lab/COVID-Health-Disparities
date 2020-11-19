require(geepack)
require(splines)
require(forestplot)
require(reshape2)
require(ggplot2)

# Make sure date format in .csv is 01/01/20, not 1/1/20
mobility = read.csv("~/Desktop/Covid disparities/health disparity/Data/aggregated_daily_mobility_variables_short.csv")
# levels show be in increasing date order
levels(mobility$date)
cutoff = which(levels(mobility$date) == "03/08/20")
mobility$days_int = as.numeric(mobility$date)
mobility = subset(mobility, mobility$days_int >= cutoff)
mobility$days_int = mobility$days_int - cutoff + 1
mobility$fips = sprintf("%05d", as.numeric(mobility$FIPS))

covariates = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
covariates$fips = sprintf("%05d", as.numeric(covariates$fips))
covariates$RuralCont = as.factor(covariates$RuralCont)
covariates$RuralCont = relevel(covariates$RuralCont, ref="1")
covariates$RuralCont = relevel(covariates$RuralCont, ref="0")

dm2 = merge(mobility, covariates, by = 'fips', all = F)
dates = levels(dm2$date)[-(1:(cutoff-1))]
days = rep(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ceiling(length(dates)/7))
dm2$weekday = 0
for(i in 1:length(dates)){
  dm2$weekday[which(dm2$date == dates[i])] = days[i]
}
