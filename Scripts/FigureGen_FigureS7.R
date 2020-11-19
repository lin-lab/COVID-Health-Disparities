source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

library(magick)
library(cowplot)

img1 <- plot_usmap(regions="county", data=all, values="rate_fatality", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Oranges", trans="log1p", direction = 1,
                       breaks=c(1, 10), limits=c(0,50)) +  
  guides(fill = guide_colorbar(title = "Deaths per 100 Cases",
                               barwidth = 40, 
                               barheight = 2)) +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")

cfr_rr <- image_read("~/Desktop/Covid disparities/health disparity/Output/CFR_RR.png")
img2 = ggdraw() + draw_image(cfr_rr)

png(file="~/Desktop/Covid disparities/health disparity/Output/FigureS7.png",width=1600,height=2000)
plot_grid(img1, img2, labels = "AUTO", label_size = 50, nrow=2,
          rel_heights=c(1,1.5))
dev.off()
