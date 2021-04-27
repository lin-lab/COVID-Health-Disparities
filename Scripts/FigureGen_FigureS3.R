library(psychometric)
library(ggplot2)
library(reshape2)

covariates = read.csv("~/Desktop/Covid disparities/health disparity/Data/covariates.csv")
covariates$RuralCont = -covariates$RuralCont
summary(covariates$RuralCont)

colnames(covariates)
ind = c(15:17, 26, 19, 21:22, 20, 23:25, 30:34, 40:49, 51, 50)
colnames(covariates)[ind]
mydata = covariates[, ind]
summary(mydata)

colnames(mydata) = c("Age20-29",
                     "Age60+",
                     "Male",
                     "Rural",
                     "Black",
                     "Hispanic",
                     "AmericanIndian",
                     "Asian",
                     "NativeHawaiian",
                     "WBSegregation",
                     "WNWSegregation",
                     "HouseholdSize",
                     "noHealthInsurance",
                     "Poverty",
                     "noHighSchool",
                     "EduHealthSocial",
                     "Smoking",
                     "Obesity",
                     "Asthma",
                     "Cancer",
                     "COPD",
                     "Diabetes",
                     "HeartFailure",
                     "Hypertension",
                     "KidneyDisease",
                     "Stroke",
                     "ICUBeds",
                     "NursingHomeBeds")

cormat <- round(cor(mydata, method = "spearman"),2)
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
png(file="~/Desktop/Covid disparities/health disparity/Output/Figure S3.png",width=1200,height=1200)
ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 20, hjust = 1),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
  ) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
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
