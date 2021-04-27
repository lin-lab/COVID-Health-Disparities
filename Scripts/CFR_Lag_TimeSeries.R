source("~/Desktop/Covid disparities/health disparity/Scripts/County Data.R")

library(ggplot2)

# Daily deaths (7 day lag)
n.county = nrow(C.days)
n.days = ncol(C.days)
lag.d = 7
C.days.daily = C.days
for(i in (lag.d+1):n.days){
  C.days.daily[,i] = C.days[,i] - C.days[,i-lag.d]
}
C.days.daily[which(C.days.daily < 0)] = 0

# Data point every 7 days, rate per 10,000 people
n.q = 7   # Change q7 time here
indices = seq(to = n.days, by = n.q, length.out = n.days / n.q)
people = 1e3

# National case rates
date = seq(as.Date("2020/1/22"), as.Date("2020/12/21"), "days")[indices]
T = length(date)
cases = colSums(C.days.daily[,indices]) / sum(pop$population) * people

# Daily deaths (7 day lag)
n.county = nrow(D.days)
n.days = ncol(D.days)
lag.d = 7
D.days.daily = D.days
for(i in (lag.d+1):n.days){
  D.days.daily[,i] = D.days[,i] - D.days[,i-lag.d]
}
D.days.daily[which(D.days.daily < 0)] = 0

# Data point every 7 days, rate per 10,000 people
people = 1e4

# National death rates
date = seq(as.Date("2020/1/22"), as.Date("2020/12/21"), "days")[indices]
T = length(date)
deaths = colSums(D.days.daily[,indices]) / sum(pop$population) * people

k = length(deaths)
lag=1
deaths.lag = rep(NA, k)
deaths.lag[1:(k-lag)] = deaths[(1+lag):k]

dt = data.frame(Date = rep(date, 2),
                Region = c(rep("Cases", T),
                           rep("Deaths Lag", T)),
                CFR = c(cases, 
                        deaths.lag))
dt$Region = factor(dt$Region, levels=c("Cases", "Deaths Lag"))
dt

lag_timeseries = ggplot() + 
  geom_line(data = dt, aes(Date, CFR, color = Region), size = 2) +
  theme_bw() +
  scale_x_date(breaks = scales::pretty_breaks(10)) +
  ggtitle("Weekly cases per thousand people and deaths per ten thousand people") +
  ylab("Rate per population") +
  scale_y_continuous(trans="log1p") +
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title = element_text(size = 35, face = "bold"),
        axis.text = element_text(size = 35),
        legend.title = element_text(size = 35, face = "bold"),
        legend.text = element_text(size = 35),
        legend.key.width = unit(3, "cm"),
        panel.grid.minor = element_blank()) +
  geom_vline(xintercept = as.Date("2020-04-13")) +
  geom_vline(xintercept = as.Date("2020-07-20"))
lag_timeseries
