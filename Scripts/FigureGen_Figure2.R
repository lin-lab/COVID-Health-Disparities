library(magick)
library(cowplot)

case_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Case_Forest_Plot.png")
death_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Death_Forest_Plot.png")

death_crop <- image_crop(death_plot, "725x1200+475")
img <- c(case_plot, death_crop)
combined_img <- image_append(img)

case_int_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Case_Race_Interaction.png")
death_int_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Death_Race_Interaction.png")

death_int_crop <- image_crop(death_int_plot, "725x1200+375")
img <- c(case_int_plot, death_int_crop)
combined_img2 <- image_append(img)

main = ggdraw() + draw_image(combined_img)
int = ggdraw() + draw_image(combined_img2)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure2.png",width=2400,height=2000)
plot_grid(main, int, labels = "AUTO", label_size = 50, nrow=2,
          rel_heights=c(1,0.4))
dev.off()
