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
                cfr.spring = deaths$X6.26.20 / cases$X6.19.20 * 10,
                cfr.summer = (deaths$X9.28.20 - deaths$X6.26.20) / (cases$X9.21.20 - cases$X6.19.20) * 10,
                cfr.fall = (deaths$X12.21.20 - deaths$X9.28.20) / (cases$X12.21.20 - cases$X9.21.20) * 10)
dt$cfr.spring[is.na(dt$cfr.spring)] = 0
dt$cfr.summer[is.na(dt$cfr.summer)] = 0
dt$cfr.fall[is.na(dt$cfr.fall)] = 0
summary(dt)    

# Heatmaps
# Need to change limit to max of fall rate
cfr_heatmap1 = plot_usmap(regions="county", data=dt, values="cfr.spring", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(0, 1, 2, 4), limits=c(0,5)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 10\n cases",
         barwidth = 45, 
         barheight = 2)) +
  ggtitle("Spring (through 6/19/20)") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
cfr_heatmap1

cfr_heatmap2 = plot_usmap(regions="county", data=dt, values="cfr.summer", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(0, 1, 2, 4), limits=c(0,5)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 10\n cases",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Summer (6/20/20-9/21/20)") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
cfr_heatmap2

cfr_heatmap3 = plot_usmap(regions="county", data=dt, values="cfr.fall", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(0, 1, 2, 4), limits=c(0,5)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 10\n cases",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Fall (9/22/20-12/21/20)") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
cfr_heatmap3
        
