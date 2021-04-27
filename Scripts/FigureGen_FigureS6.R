source("~/Desktop/Covid disparities/health disparity/Scripts/CFR_Lag_TimeSeries.R")
source("~/Desktop/Covid disparities/health disparity/Scripts/CFR_heatmap.R")

library(cowplot)

middle_row = plot_grid(cfr_heatmap1, cfr_heatmap2, cfr_heatmap3, label_size = 50, nrow=1)
png(file="~/Desktop/Covid disparities/health disparity/Output/Figure S6.png",width=2300,height=1500)
plot_grid(lag_timeseries, middle_row, labels = "AUTO", label_size = 50, nrow=2)
dev.off()

