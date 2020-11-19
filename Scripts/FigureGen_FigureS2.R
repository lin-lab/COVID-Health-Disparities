source('~/Desktop/Covid disparities/health disparity/Scripts/1. Loading Data.R')

library(cowplot)

#############################################
# Demographic heatmaps - Fig S1
#############################################

sub = data.frame(fips=dt.rem$fips, 
                 PopDensity=dt.rem$PopDensity,
                 Pop2029=dt.rem$Pop2029*100,
                 Pop6099=dt.rem$Pop6099*100,
                 Male=dt.rem$Male,
                 RuralCont=dt.rem$RuralCont)
levels(sub$RuralCont) = c("Nonmetro", "Metro/Near Metro", "Metro")

dem1 <- plot_usmap(regions="county", data=sub, values="PopDensity", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", trans="log", direction=1,
                       breaks=c(1e2, 1e4)) +  
  guides(fill = guide_colorbar(title = "People per Square Mile",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Population Density") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem1

dem2 <- plot_usmap(regions="county", data=sub, values="Pop2029", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction=1,
                       breaks=c(25, 50), limits=c(0,75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Ages 20-29 Years") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem2

dem3 <- plot_usmap(regions="county", data=sub, values="Pop6099", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction=1,
                       breaks=c(25, 50), limits=c(0,75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Ages 60+ Years") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem3

dem4 <- plot_usmap(regions="county", data=sub, values="Male", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction=1,
                       breaks=c(30, 50, 70), limits=c(25,75)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Male") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
dem4

dem5 <- plot_usmap(regions="county", data=sub, values="RuralCont", color = "white", size = 0.15) +
  labs(fill = "Urban Category") +
  ggtitle("Metro") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom")
dem5

#############################################
# Socioeconomic heatmaps - Fig S1
#############################################

sub = data.frame(fips=dt.rem$fips, 
                 HouseholdSize=dt.rem$HouseholdSize,
                 noHealthInsurance=dt.rem$noHealthInsurance,
                 Poverty=dt.rem$Poverty,
                 noHighSchool=dt.rem$noHighSchool,
                 PercentEduHealthSoc=dt.rem$PercentEduHealthSoc)

soc1 <- plot_usmap(regions="county", data=sub, values="HouseholdSize", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(2.5, 3, 3.5)) +  
  guides(fill = guide_colorbar(title = "Size",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Average Household Size") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc1

soc2 <- plot_usmap(regions="county", data=sub, values="noHealthInsurance", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 25, 35)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("No Health Insurance") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc2

soc3 <- plot_usmap(regions="county", data=sub, values="Poverty", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 30, 45)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Poverty") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc3

soc4 <- plot_usmap(regions="county", data=sub, values="noHighSchool", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(15, 30, 45)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("No High School Diploma") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc4

soc5 <- plot_usmap(regions="county", data=sub, values="PercentEduHealthSoc", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "OrRd", direction=1,
                       breaks=c(20, 30, 40)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Education, Healthcare, Social Worker") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
soc5

#############################################
# Medical heatmaps - Fig S1
#############################################

sub = data.frame(fips=dt.rem$fips, 
                 Smoking=dt.rem$Smoking,
                 Obesity=dt.rem$Obesity,
                 Asthma=dt.rem$Asthma,
                 Cancer=dt.rem$Cancer,
                 COPD=dt.rem$COPD,
                 HF=dt.rem$HF,
                 HTN=dt.rem$HTN,
                 Stroke=dt.rem$Stroke,
                 ICUBeds = dt.rem$ICUBeds,
                 NursingHomeBeds=dt.rem$NursingHomeBeds)

med1 <- plot_usmap(regions="county", data=sub, values="Smoking", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(15, 25, 35)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Smoking") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med1

med2 <- plot_usmap(regions="county", data=sub, values="Obesity", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(15, 30, 45)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Obesity") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med2

med3 <- plot_usmap(regions="county", data=sub, values="Asthma", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(3, 6, 9)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Asthma") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med3

med4 <- plot_usmap(regions="county", data=sub, values="Cancer", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(5, 8, 11)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Cancer") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med4

med5 <- plot_usmap(regions="county", data=sub, values="COPD", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(10, 20, 30)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("COPD") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med5

med6 <- plot_usmap(regions="county", data=sub, values="HF", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(10, 20, 30)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Heart Failure") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med6

med7 <- plot_usmap(regions="county", data=sub, values="HTN", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(30, 50, 70)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Hypertension") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med7

med8 <- plot_usmap(regions="county", data=sub, values="Stroke", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1,
                       breaks=c(3, 5, 7)) +  
  guides(fill = guide_colorbar(title = "Percentage",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Stroke") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med8

med9 <- plot_usmap(regions="county", data=sub, values="ICUBeds", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1, trans="log1p",
                       breaks=c(10, 100, 1e3)) +  
  guides(fill = guide_colorbar(title = "Beds",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("ICU Beds Beds") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med9

med10 <- plot_usmap(regions="county", data=sub, values="NursingHomeBeds", color = "white", size = 0.15) +
  scale_fill_distiller(palette = "BuPu", direction=1, trans="log1p",
                       breaks=c(10, 100, 1e3)) +  
  guides(fill = guide_colorbar(title = "Beds",
                               barwidth = 20, 
                               barheight = 2)) +
  ggtitle("Nursing Home Beds") +
  theme(plot.title = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = "bottom")
med10

#############################################
# Putting it all together - Fig S2
#############################################

all <- grid.arrange(dem1, dem2, dem3, dem4, dem5,
                    soc1, soc2, soc3, soc4, soc5,
                    med1, med2, med3, med4, med5, med6, med7, med8, med9, med10,
                    ncol=4)

png(file="~/Desktop/Covid disparities/health disparity/Output/FigureS2.png",width=2200,height=2200)
plot_grid(all)
dev.off()
