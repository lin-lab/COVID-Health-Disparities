source('~/Desktop/Covid disparities/health disparity/Scripts/2. Rate Heatmaps.R')
source('~/Desktop/Covid disparities/health disparity/Scripts/3. Race Heatmaps.R')

library(cowplot)
top_row = plot_grid(case_rates, death_rates, labels = "AUTO", label_size = 50)
bottom_row = plot_grid(race1, race2, race3, WB_seg, race4, race5, race6, WNW_seg, nrow=2)

race <- grid.arrange(race1, race3, race4, race2, ncol=2)
seg <- grid.arrange(WB_seg, WNW_seg, nrow=2)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure1.png",width=2200,height=2200)
plot_grid(top_row, bottom_row, labels = c("A", "C"), label_size = 50, nrow=2,
 rel_heights=c(1,1.3))
dev.off()

