library(magick)
library(cowplot)

death1_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Death Rate Total Univariable.png")
death2_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Death Rate Total Multivariable.png")

death2_crop <- image_crop(death2_plot, "725x1200+550")
img <- c(death1_plot, death2_crop)
combined_img <- image_append(img)

main = ggdraw() + draw_image(combined_img)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure 3.png",width=2500,height=1500)
main
dev.off()
