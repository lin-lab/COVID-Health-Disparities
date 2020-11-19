#############################################
# Racial heatmaps
#############################################
source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

sub = data.frame(fips=census$fips, 
                 PropWhite2019 = census$PropWhite2019,
                 PropBlack2019 = census$PropBlack2019,
                 PropAsian2019 = census$PropAsian2019,
                 PropHispanic2019=census$PropHispanic2019,
                 PropAmInd2019 = census$PropAmInd2019,
                 PropNHPI2019 = census$PropNHPI2019)

race1 <- plot_usmap(regions="county", data=sub, values="PropWhite2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("White") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")

race2 <- plot_usmap(regions="county", data=sub, values="PropBlack2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Black/African American") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")


race3 <- plot_usmap(regions="county", data=sub, values="PropHispanic2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Hispanic/Latino") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")

race4 <- plot_usmap(regions="county", data=sub, values="PropAsian2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Asian") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")


race5 <- plot_usmap(regions="county", data=sub, values="PropAmInd2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("American Indian/Native Alaskan") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")

race5 <- plot_usmap(regions="county", data=sub, values="PropAmInd2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("American Indian/Native Alaskan") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")

race6 <- plot_usmap(regions="county", data=sub, values="PropNHPI2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "Purples", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Native Hawaiian/Pacific Islander") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")


#############################################
# Racial heatmaps
#############################################

chr$WBSeg = chr$v141_rawvalue
chr$WNWSeg = chr$v142_rawvalue
chr$WBSeg[is.na(chr$WBSeg)] = 1
chr$WNWSeg[is.na(chr$WNWSeg)] = 1

sub = data.frame(fips=chr$fips, 
                 WBSeg = chr$WBSeg,
                 WNWSeg = chr$WNWSeg)


WB_seg <- plot_usmap(regions="county", data=sub, values="WBSeg", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1,
                       breaks=c(20, 40, 60, 80), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Index",
                               barwidth = 32, 
                               barheight = 2)) +
  ggtitle("White/Black Segregation") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")



WNW_seg <- plot_usmap(regions="county", data=sub, values="WNWSeg", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlOrBr",  direction = 1,
                       breaks=c(20, 40, 60, 80), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Index",
                               barwidth = 32, 
                               barheight = 2)) +
  ggtitle("White/non-White Segregation") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom")

