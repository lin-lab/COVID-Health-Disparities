source("~/Desktop/Covid disparities/health disparity/Scripts/CaseRate_PopulationOffset_Heatmap.R")
source("~/Desktop/Covid disparities/health disparity/Scripts/DeathRate_PopulationOffset_Heatmap.R")

library(cowplot)

top_row = plot_grid(caserate_heatmap1, caserate_heatmap2, caserate_heatmap3, label_size = 50, nrow=1)
middle_row = plot_grid(deathrate_heatmap1, deathrate_heatmap2, deathrate_heatmap3, label_size = 50, nrow=1)
png(file="~/Desktop/Covid disparities/health disparity/Output/Figure 4.png",width=2300,height=1500)
plot_grid(top_row, middle_row, labels = "AUTO", label_size = 50, nrow=2)
dev.off()

