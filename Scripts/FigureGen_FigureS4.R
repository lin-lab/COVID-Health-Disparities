library(psychometric)
library(ggplot2)
library(reshape2)

covariates = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
colnames(covariates)
colnames(covariates)[c(15, 17:18, 16, 19:21, 26:30, 36:40, 42:43, 45:47)]
include = which((is.na(covariates$Asthma) +
                   is.na(covariates$Cancer) +
                   is.na(covariates$COPD) +
                   is.na(covariates$HF) +
                   is.na(covariates$Stroke)) == 0)
mydata = covariates[include, c(15, 17:18, 16, 19:21, 26:30, 36:40, 42:43, 45:47)]
summary(mydata)

n = nrow(mydata)
round(CIr(r=cor(mydata$logBlack, mydata$Poverty), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logBlack, mydata$noHighSchool), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logBlack, mydata$HTN), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logHispanic, mydata$HouseholdSize), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logHispanic, mydata$noHealthInsurance), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logAmInd, mydata$noHealthInsurance), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logAsian, mydata$Smoking), n = n, level = .95), digits = 2)
round(CIr(r=cor(mydata$logAsian, mydata$COPD), n = n, level = .95), digits = 2)


#mydata$ICEwnhinc = abs(mydata$ICEwnhinc)
colnames(mydata) = c("Black/AfricanAmerican",
                     "Hispanic/Latino",
                     "AmericanIndian/Alaskan",
                     "Hawaiian/PacificIslander",
                     "Asian",
                     "WB Segregation",
                     "WNW Segregation",
                     "HouseholdSize",
                     "noHealthInsurance",
                     "Poverty",
                     "noHighSchool",
                     "EduHealthSocWorker",
                     "Smoking",
                     "Obesity",
                     "Asthma",
                     "Cancer",
                     "COPD",
                     "HeartFailure",
                     "Hypertension",
                     "Stroke",
                     "ICUBeds",
                     "NursingHomeBeds")
cormat <- round(cor(mydata),2)
melted_cormat <- melt(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
png(file="~/Desktop/Covid disparities/health disparity/Output/FigureS4.png",width=1200,height=1200)
ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 25, hjust = 1),
        axis.text.y = element_text(size = 25),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        ) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 6) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal",
    legend.key.size = unit(3, "line")) +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 2,
                               title.position = "top", title.hjust = 0.5))
dev.off()
