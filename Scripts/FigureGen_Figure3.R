source('~/Desktop/Covid disparities/health disparity/Scripts/8. Estimated County Rates.R')

library(cowplot)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure3.png",width=1500,height=1400)
plot_grid(case_rates, death_rates, select_case, select_death, labels = "AUTO", label_size = 35,
          rel_heights=c(1,1.5))
dev.off()
