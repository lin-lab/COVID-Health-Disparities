source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

sum(all$population2019)
sum(all$cases11.08)
sum(all$deaths11.08)

sum(dt.rem$PopSize)
sum(dt.rem$tot_cases)
sum(dt.rem$tot_deaths)

sum(dt.rem$PopSize) / sum(all$population2019)

#############################################
# Case death rate heatmaps
#############################################
case_rates <- plot_usmap(regions="county", data=all, values="rate_cases", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdBu", trans="log1p", direction = -1,
                       breaks=c(0, 1e1, 1e2, 1e3, 1e4)) + 
  guides(fill = guide_colorbar(title = "Cases per 100k",
                               barwidth = 40, 
                               barheight = 2)) +
  theme(plot.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")


death_rates <- plot_usmap(regions="county", data=all, values="rate_deaths", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Greens", trans="log1p", direction=1,
                       breaks=c(0, 10, 1e2)) + 
  guides(fill = guide_colorbar(title = "Deaths per 100k",
                               barwidth = 40, 
                               barheight = 2)) +
  theme(plot.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")
