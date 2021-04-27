dt = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")

dt$RuralCont = as.factor(dt$RuralCont)
dt$RuralCont = relevel(dt$RuralCont, ref="1")
dt$RuralCont = relevel(dt$RuralCont, ref="2")

library(cowplot)
library(usmap)
library(ggplot2)
library(gridExtra)

#############################################
# Demographic heatmaps
#############################################

sub = data.frame(fips=dt$fips, 
                 Pop2029=dt$Pop2029*100,
                 Pop6099=dt$Pop6099*100,
                 Male=dt$Male,
                 RuralCont=dt$RuralCont)
levels(sub$RuralCont) = c("Metro", "Metro/Near Metro", "Nonmetro")
summary(sub)

dem1 <- plot_usmap(regions="county", data=sub, values="Pop2029", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction=1,
                       breaks=c(25, 50), limits=c(0,75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Ages 20-29 Years") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem1

dem2 <- plot_usmap(regions="county", data=sub, values="Pop6099", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction=1,
                       breaks=c(25, 50), limits=c(0,75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Ages 60+ Years") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem2

dem3 <- plot_usmap(regions="county", data=sub, values="Male", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction=1,
                       breaks=c(25, 50), limits=c(0,75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Male") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem3

dem4 <- plot_usmap(regions="county", data=sub, values="RuralCont", color = "white", size = 0.15) +
  labs(fill = "Urban Category") +
  ggtitle("Metro") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom")
dem4

#############################################
# Race/ethnicity heatmaps
#############################################

sub = data.frame(fips=dt$fips, 
                 PropWhite2019 = exp(dt$logWhite) - 1,
                 PropBlack2019 = exp(dt$logBlack) - 1,
                 PropAsian2019 = exp(dt$logAsian) - 1,
                 PropHispanic2019 = exp(dt$logHispanic) - 1,
                 PropAmInd2019 = exp(dt$logAmInd) - 1,
                 PropNHPI2019 = exp(dt$logNHPI) - 1,
                 WBSeg = dt$WBSeg,
                 WNWSeg = dt$WNWSeg)
summary(sub)

race1 <- plot_usmap(regions="county", data=sub, values="PropBlack2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Black/African American") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race1

race2 <- plot_usmap(regions="county", data=sub, values="PropHispanic2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Hispanic/Latino") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race2

race3 <- plot_usmap(regions="county", data=sub, values="PropAsian2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Asian") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race3

race4 <- plot_usmap(regions="county", data=sub, values="PropAmInd2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("American Indian/Native Alaskan") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race4

race5 <- plot_usmap(regions="county", data=sub, values="PropNHPI2019", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", trans="log1p", direction = 1,
                       breaks=c(1, 10, 50), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Native Hawaiian/Pacific Islander") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race5

race6 <- plot_usmap(regions="county", data=sub, values="WBSeg", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", direction = 1,
                       breaks=c(25, 50, 75), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Index",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("White/Black Segregation") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race6

race7 <- plot_usmap(regions="county", data=sub, values="WNWSeg", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "RdPu", direction = 1,
                       breaks=c(25, 50, 75), limits=c(0,100)) +  
  guides(fill = guide_colorbar(title = "Index",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("White/non-White Segregation") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
race7

#############################################
# Socioeconomic heatmaps
#############################################

sub = data.frame(fips=dt$fips, 
                 HouseholdSize=dt$HouseholdSize,
                 noHealthInsurance=dt$noHealthInsurance,
                 Poverty=dt$Poverty,
                 noHighSchool=dt$noHighSchool,
                 EduHealthSocial=dt$PercentEduHealthSoc)
summary(sub)

soc1 <- plot_usmap(regions="county", data=sub, values="HouseholdSize", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(2.5, 3, 3.5), limits=c(1.9, 4)) +  
  guides(fill = guide_colorbar(title = "Average\nSize",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Household Size") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc1

soc2 <- plot_usmap(regions="county", data=sub, values="noHealthInsurance", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 30, 45), limits=c(0, 60)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("No Health Insurance") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc2

soc3 <- plot_usmap(regions="county", data=sub, values="Poverty", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 30, 45), limits=c(0, 60)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Poverty") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc3

soc4 <- plot_usmap(regions="county", data=sub, values="noHighSchool", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 30, 45), limits=c(0, 60)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("No High School Diploma") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc4

soc5 <- plot_usmap(regions="county", data=sub, values="EduHealthSocial", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 30, 45), limits=c(0, 60)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Edu/Health/Soc Workers") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc5
#############################################
# Medical heatmaps
#############################################

sub = data.frame(fips=dt$fips, 
                 Smoking=dt$Smoking,
                 Obesity=dt$Obesity,
                 Asthma=dt$Asthma,
                 Cancer=dt$Cancer,
                 COPD=dt$COPD,
                 Diabetes=dt$Diabetes,
                 HF=dt$HF,
                 HTN=dt$HTN,
                 KD=dt$KD,
                 Stroke=dt$Stroke,
                 ICU=dt$ICUBeds,
                 Nursing=dt$NursingHomeBeds)
summary(sub)

med1 <- plot_usmap(regions="county", data=sub, values="Smoking", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Smoking") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med1

med2 <- plot_usmap(regions="county", data=sub, values="Obesity", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Obesity") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med2

med3 <- plot_usmap(regions="county", data=sub, values="Asthma", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Asthma") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med3

med4 <- plot_usmap(regions="county", data=sub, values="Cancer", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Cancer") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med4

med5 <- plot_usmap(regions="county", data=sub, values="COPD", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("COPD") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med5

med6 <- plot_usmap(regions="county", data=sub, values="Diabetes", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Stroke") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med6

med7 <- plot_usmap(regions="county", data=sub, values="HF", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Heart Failure") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med7

med8 <- plot_usmap(regions="county", data=sub, values="HTN", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Hypertension") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med8

med9 <- plot_usmap(regions="county", data=sub, values="KD", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Kidney Disease") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med9

med10 <- plot_usmap(regions="county", data=sub, values="Stroke", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(25, 50), limits=c(0, 75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Stroke") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med10

med11 <- plot_usmap(regions="county", data=sub, values="ICU", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(10, 100, 1000), limits=c(0, 2.2e3)) +  
  guides(fill = guide_colorbar(title = "Beds",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("ICU Beds") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med11

med12 <- plot_usmap(regions="county", data=sub, values="Nursing", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(10, 100, 1000), limits=c(0, 2.2e3)) +  
  guides(fill = guide_colorbar(title = "Beds",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Nursing Home Beds") +
  theme(plot.title = element_text(size = 28, face = "bold", hjust=0.5),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med12

#############################################
# Putting it all together
#############################################

all <- grid.arrange(dem1, dem2, dem3, dem4,
                    race1, race2, race3, race4, race5, race6, race7, 
                    soc1, soc2, soc3, soc4, soc5,
                    ncol=4)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure S1.png",width=2200,height=2200)
plot_grid(all)
dev.off()

all <- grid.arrange(med1, med2, med3, med4, 
                    med5, med6, med7, med8, 
                    med9, med10, med11, med12,
                    ncol=4)

png(file="~/Desktop/Covid disparities/health disparity/Output/Figure S2.png",width=2200,height=1650)
plot_grid(all)
dev.off()
