#############################################
# Observed and standardized county rates
# Run this section first for Fig 3 or Tables S8
#############################################

source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

# Observe rate rankings
counties = c("Bronx, NY", "Kings, NY", "New York, NY", "Queens, NY", "Richmond, NY",
             "Los Angeles, CA", 
             "Cook, IL", "DuPage, IL", 
             "Harris, TX", "Fort Bend, TX", "Montgomery, TX",
             "Philadelphia, PA",
             "San Francisco, CA",
             "King, WA",
             "Suffolk, MA",
             "Wayne, MI",
             "Baltimore, MD",
             "Miami-Dade, FL",
             "Orleans, LA",
             "Navajo, AZ", "Apache, AZ", "San Juan, NM", "McKinley, NM", "Coconino, AZ")
cities = c(rep("NYC", 5),
           "LA", 
           rep("Chicago", 2), 
           rep("Houston", 3), 
           "Philadelphia", 
           "San Francisco",
           "Seattle",
           "Boston",
           "Detroit",
           "Baltimore",
           "Miami",
           "New Orleans",
           rep("Navajo Nation", 5))
cbind(counties, cities)

# Standardized cases
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
X = makeX(model@frame[,3:30])
X[,1] = 1
mm = X[,c(1:62, 64:80)]

RE = ranef(model, condVar = TRUE)
RE.point = RE$CountyI
RE.var = as.numeric(attr( RE$CountyI, "postVar"))

est.point = predict(model)
pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
tvar1 <- pvar1+RE.var

dt.rem$case.adj = log(exp(est.point) / dt.rem$PopSize * 1e5)
dt.rem$case.adj.LL = log(exp(est.point - 1.96 * sqrt(tvar1)) / dt.rem$PopSize * 1e5)
dt.rem$case.adj.UL = log(exp(est.point + 1.96 * sqrt(tvar1))/ dt.rem$PopSize * 1e5)

# Standardized deaths
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
X = makeX(model@frame[,3:30])
X[,1] = 1
mm = X[,c(1:62, 64:80)]

RE = ranef(model, condVar = TRUE)
RE.point = RE$CountyI
RE.var = as.numeric(attr( RE$CountyI, "postVar"))

est.point = predict(model)
pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
tvar1 <- pvar1+RE.var

dt.rem$death.adj = log(exp(est.point) / dt.rem$PopSize * 1e5)
dt.rem$death.adj.LL = log(exp(est.point - 1.96 * sqrt(tvar1)) / dt.rem$PopSize * 1e5)
dt.rem$death.adj.UL = log(exp(est.point + 1.96 * sqrt(tvar1))/ dt.rem$PopSize * 1e5)

#############################################
# Fig 3
#############################################

# Identifying city counties
index = match(counties, dt$CountyNamew.StateAbbrev)
index = match(dt$fips[index], dt.rem$fips)
data = data.frame(county=counties,
                  cities=cities,
                  case.obs=dt.rem$rate_cases[index],
                  case.adj=dt.rem$case.adj[index],
                  case.adj.LL=dt.rem$case.adj.LL[index],
                  case.adj.UL=dt.rem$case.adj.UL[index],
                  death.obs=dt.rem$rate_deaths[index],
                  death.adj=dt.rem$death.adj[index],
                  death.adj.LL=dt.rem$death.adj.LL[index],
                  death.adj.UL=dt.rem$death.adj.UL[index],
                  location=paste0(counties, " (", cities, ")"))

# Standardized case rates
data=data[order(data$case.adj, decreasing=FALSE),]
data$location = factor(data$location, levels=data$location)

select_case = ggplot(data, aes(x=location, y=case.adj)) + 
  geom_point(col="purple", size=5) +   # Draw points
  geom_segment(aes(x=location, 
                   xend=location, 
                   y=min(case.adj.LL), 
                   yend=max(case.adj.UL)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  geom_errorbar(aes(ymin=case.adj.LL, ymax=case.adj.UL),
                size = 0.75,
                width = 0.5) +
  labs(x="County, State (City/Territory)",
       y="Log Cases per 100,000") +  
  coord_flip() + 
  theme_classic() +
  theme(title = element_text(size=20),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20))

# Standardized death rates
data=data[order(data$death.adj, decreasing=FALSE),]
data$location = factor(data$location, levels=data$location)

select_death = ggplot(data, aes(x=location, y=death.adj)) + 
  geom_point(col="royalblue2", size=5) +   # Draw points
  geom_segment(aes(x=location, 
                   xend=location, 
                   y=min(death.adj.LL), 
                   yend=max(death.adj.UL)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  geom_errorbar(aes(ymin=death.adj.LL, ymax=death.adj.UL),
                size = 0.75,
                width = 0.5) +
  labs(x="County, State (City/Territory)",
       y="Log Deaths per 100,000") +  
  coord_flip() + 
  theme_classic() +
  theme(title = element_text(size=20),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20))

#Case map
dt.rem$case_rate <- exp(dt.rem$case.adj)
case_rates <- plot_usmap(regions="county", data=dt.rem, values="case_rate", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "PRGn", trans="log1p", direction = -1,
                       breaks=c(0, 1e1, 1e2, 1e4)) + 
  guides(fill = guide_colorbar(title = "Cases per 100k",
                               barwidth = 25, 
                               barheight = 1.5)) +
  theme(plot.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom")

#Death map
dt.rem$death_rate <- exp(dt.rem$death.adj)
death_rates <- plot_usmap(regions="county", data=dt.rem, values="death_rate", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Blues", trans="log1p", direction=1,
                       breaks=c(0, 10, 25, 1e2)) + 
  guides(fill = guide_colorbar(title = "Deaths per 100k",
                               barwidth = 25, 
                               barheight = 1.5)) +
  theme(plot.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom")


#############################################
# Spreadsheet of county estimated rates
#############################################

#data = data.frame(fips = dt.rem$fips,
#                  county=dt.rem$County.Name,
#                  state = dt.rem$stateName,
#                  case.obs=dt.rem$rate_cases,
#                  case.adj=dt.rem$case.adj,
#                  death.obs=dt.rem$rate_deaths,
#                  death.adj=dt.rem$death.adj,
#                  percent.black=dt.rem$PropBlack2018,
#                  percent.hispanic=dt.rem$PropHispanic2018)
#data=data[order(data$case.adj, decreasing=TRUE),]
#write.csv(data, file="C:/Users/Daniel Li/Desktop/COVID Disparities County/County Rates All.csv",
#          row.names=F)

