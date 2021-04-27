source("~/Desktop/Covid disparities/health disparity/Scripts/County Data.R")

library(usmap)
library(ggplot2)
library(gridExtra)

# Checking cumulative deaths
#colSums(D.days)
#subset(colSums(D.days), colSums(D.days) > 1e5)

# Verifying fips identifiers match
#summary(cases$countyFIPS - deaths$countyFIPS)
#summary(cases$countyFIPS - pop$countyFIPS)

# Data frame
# Spring 2020 3/20-6/19
# Summer 2020 6/20-9/21
# Fall 2020 9/22-12/21
dt = data.frame(fips = cases$countyFIPS,
                deaths.spring = deaths$X6.19.20 / pop$population * 1e3,
                deaths.summer = (deaths$X9.21.20 - deaths$X6.19.20) / pop$population * 1e3,
                deaths.fall = (deaths$X12.21.20 - deaths$X9.21.20) / pop$population * 1e3)
dt$deaths.summer[which(dt$deaths.summer<0)] = 0
dt$deaths.fall[which(dt$deaths.fall<0)] = 0
summary(dt)    

# Heatmaps
# Need to change limit to max of fall rate
deathrate_heatmap1 = plot_usmap(regions="county", data=dt, values="deaths.spring", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Reds", trans="log1p", direction = 1,
                       breaks=c(0, 1, 2, 4), limits=c(0,7.6)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 1k\npeople",
         barwidth = 45, 
         barheight = 2)) +
  ggtitle("Spring (through 6/19/20)") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
deathrate_heatmap1

deathrate_heatmap2 = plot_usmap(regions="county", data=dt, values="deaths.summer", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Reds", trans="log1p", direction = 1,
                       breaks=c(0, 1, 2, 4), limits=c(0,7.6)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 1k\npeople",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Summer (6/20/20-9/21/20)") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
deathrate_heatmap2

deathrate_heatmap3 = plot_usmap(regions="county", data=dt, values="deaths.fall", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Reds", trans="log1p", direction = 1,
                       breaks=c(0, 1, 2, 4), limits=c(0,7.6)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 1k\npeople",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Fall (9/22/20-12/21/20)") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
deathrate_heatmap3
        
