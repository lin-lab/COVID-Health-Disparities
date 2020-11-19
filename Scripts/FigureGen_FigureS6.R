library(magick)
library(cowplot)

case_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Case_Forest_Plot_Mob.png")
death_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Death_Forest_Plot_Mob.png")

death_crop <- image_crop(death_plot, "725x1200+475")
img <- c(case_plot, death_crop)
combined_img <- image_append(img)

main = ggdraw() + draw_image(combined_img)

png(file="~/Desktop/Covid disparities/health disparity/Output/FigureS6.png",width=2000,height=1200)
plot_grid(main)
dev.off()
