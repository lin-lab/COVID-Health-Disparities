library(magick)
library(cowplot)

cfr1_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/CFR Total Univariable.png")
cfr2_plot <- image_read("~/Desktop/Covid disparities/health disparity/Output/CFR Total Multivariable.png")

cfr2_crop <- image_crop(cfr2_plot, "725x1200+550")
img <- c(cfr1_plot, cfr2_crop)
combined_img <- image_append(img)

main = ggdraw() + draw_image(combined_img)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure S5.png",width=2500,height=1500)
main
dev.off()
