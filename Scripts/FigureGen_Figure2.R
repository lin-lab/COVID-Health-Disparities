library(magick)
library(cowplot)

case1_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Case Rate Total Univariable.png")
case2_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/Case Rate Total Multivariable.png")

case2_crop <- image_crop(case2_plot, "725x1200+550")
img <- c(case1_plot, case2_crop)
combined_img <- image_append(img)

main = ggdraw() + draw_image(combined_img)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure 2.png",width=2500,height=1500)
main
dev.off()
