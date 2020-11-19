source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')
source('~/Desktop/Covid disparities/health disparity/Scripts/9. Load Mobility Data.R')

library(magick)
library(cowplot)

index = which(mobility$date == "03/08/20")
sub = mobility[index,]

img1 <- plot_usmap(regions="county", data=sub, values="sg_time_home", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Spectral", direction = -1,
                       breaks=c(25, 50, 75), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 25, 
                               barheight = 2)) +
  ggtitle("Sunday, 3/8/20") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")

index = which(mobility$date == "05/31/20")
sub = mobility[index,]

img2 <- plot_usmap(regions="county", data=sub, values="sg_time_home", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Spectral", direction = -1,
                       breaks=c(25, 50, 75), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 25, 
                               barheight = 2)) +
  ggtitle("Sunday, 5/31/20") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")

index = which(mobility$date == "09/27/20")
sub = mobility[index,]

img3 <- plot_usmap(regions="county", data=sub, values="sg_time_home", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Spectral", direction = -1,
                       breaks=c(25, 50, 75), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 25, 
                               barheight = 2)) +
  ggtitle("Sunday, 9/27/20") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")

mob_map <- grid.arrange(img1, img2, img3, ncol=3)

mob_rr <- image_read("~/Desktop/Covid disparities/health disparity/Output/Mobility.png")
mob_rr1 = ggdraw() + draw_image(mob_rr)

png(file="~/Desktop/Covid disparities/health disparity/Output/FigureS5.png",width=1600,height=2000)
plot_grid(mob_map, mob_rr1, labels = "AUTO", label_size = 50, nrow=2,
          rel_heights=c(1,1.5))
dev.off()
