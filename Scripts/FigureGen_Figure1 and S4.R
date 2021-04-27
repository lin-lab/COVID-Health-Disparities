################################
# Observed rates heatmaps
################################

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
dt.obs = data.frame(fips = cases$countyFIPS,
                cases.obs = cases$X12.21.20 / pop$population * 1e2,
                deaths.obs = deaths$X12.21.20 / pop$population * 1e3,
                cfr.obs = deaths$X12.21.20 / cases$X12.21.20  * 1e2)
dt.obs$cfr.obs[is.na(dt.obs$cfr.obs)] = 0
summary(dt.obs)    

#Observed rates heatmaps
observe_heatmap1 = plot_usmap(regions="county", data=dt.obs, values="cases.obs", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Blues", trans="log1p", direction = 1,
                       breaks=c(0, 1, 5, 20), limits=c(0,27)) +
  guides(fill = guide_colorbar(title = "Cases\nper 100\npeople",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Observed Case Rates") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
observe_heatmap1

observe_heatmap2 = plot_usmap(regions="county", data=dt.obs, values="deaths.obs", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Reds", trans="log1p", direction = 1,
                       breaks=c(0, 1, 3, 5), limits=c(0,8)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 1k\npeople",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Observed Death Rates") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
observe_heatmap2

observe_heatmap3 = plot_usmap(regions="county", data=dt.obs, values="cfr.obs", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(0, 2, 4, 8), limits=c(0,15)) +
  guides(fill = guide_colorbar(title = "Deaths\nper 100\ncases",
                               barwidth = 45, 
                               barheight = 2)) +
  ggtitle("Observed Case Fatality Rates") +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
observe_heatmap3

library(cowplot)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure 1.png",width=1600,height=750)
plot_grid(observe_heatmap1, observe_heatmap2, labels = NULL, label_size = 50, nrow=1)
dev.off()

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure S4.png",width=800,height=750)
plot_grid(observe_heatmap3, labels = NULL, label_size = 50, nrow=1)
dev.off()
