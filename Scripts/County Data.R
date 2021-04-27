# Load data
setwd("~/Desktop/Covid disparities/health disparity/Data/")
cases = read.csv("covid_confirmed_usafacts.csv", fileEncoding="UTF-8-BOM")
deaths = read.csv("covid_deaths_usafacts.csv", fileEncoding="UTF-8-BOM")
pop = read.csv("covid_county_population_usafacts.csv", fileEncoding="UTF-8-BOM")

# Remove FIPS 0 or 1 (statewide unallocated counts), 2270 (population 0), 6000 (grand princess cruise ship)
cases = subset(cases, countyFIPS > 1000 & countyFIPS != 6000 & countyFIPS != 2270)
deaths = subset(deaths, countyFIPS > 1000 & countyFIPS != 6000 & countyFIPS != 2270)
pop = subset(pop, countyFIPS > 1000 & countyFIPS != 6000 & countyFIPS != 2270)

# Ensure rows match up 
cases = cases[order(cases$countyFIPS),]
deaths = deaths[order(deaths$countyFIPS),]
summary(cases$countyFIPS == deaths$countyFIPS)

# Census division and region
Division.NE = c("CT", "ME", "MA", "NH", "RI", "VT")
Division.MA = c("NJ", "NY", "PA")
Division.ENC = c("IL", "IN", "MI", "OH", "WI")
Division.WNC = c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
Division.SA = c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV")
Division.ESC = c("AL", "KY", "MS", "TN")
Division.WSC = c("AR", "LA", "OK", "TX")
Division.M = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY")
Division.P = c("AK", "CA", "HI", "OR", "WA")

Region.NE = c(Division.NE, Division.MA)
Region.MW = c(Division.ENC, Division.WNC)
Region.S = c(Division.SA, Division.ESC, Division.WSC)
Region.W = c(Division.M, Division.P)

pop$region = "NE"
pop$region[pop$State %in% Region.MW] = "MW"
pop$region[pop$State %in% Region.S] = "S"
pop$region[pop$State %in% Region.W] = "W"

pop$division = "NE"
pop$division[pop$State %in% Division.MA] = "MA"
pop$division[pop$State %in% Division.ENC] = "ENC"
pop$division[pop$State %in% Division.WNC] = "WNC"
pop$division[pop$State %in% Division.SA] = "SA"
pop$division[pop$State %in% Division.ESC] = "ESC"
pop$division[pop$State %in% Division.WSC] = "WSC"
pop$division[pop$State %in% Division.M] = "M"
pop$division[pop$State %in% Division.P] = "P"

# Data dimensions
#n.col = ncol(cases)
#n.row = nrow(cases)

# View select column names
colnames(cases)[c(1:5, ncol(cases))]

# View county FIPS
# summary(as.factor(cases$countyFIPS))

# Check case and death data rows match
# summary(cases$countyFIPS - deaths$countyFIPS)

# Define study period start and end dates
ind.start = which(colnames(cases) == "X1.22.20")   #Change study period start date here
ind.end = which(colnames(cases) == "X12.21.20")    #Change study period end date here

# Total case and death counts per day
C.days = as.matrix(cases[, ind.start:ind.end])
D.days = as.matrix(deaths[, ind.start:ind.end])
