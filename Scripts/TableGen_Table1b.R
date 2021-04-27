data = read.csv("~/Desktop/Covid disparities/health disparity/Output/Death Rate Weekly Multivariable.csv")

estimate = format(round(data$estimate, digits = 2), nsmall=2)
low = format(round(data$lower, digits = 2), nsmall=2)
high = format(round(data$upper, digits = 2), nsmall=2)
values = paste0(estimate, " (", low, ", ", high, ")")

data$values = values
table = matrix(values, ncol=3, byrow=TRUE)
rownames(table) = unique(data$variable)
colnames(table) = c("spring", "summer", "fall")

write.csv(table, 
          file = "~/Desktop/Covid disparities/health disparity/Output/Table 1b.csv")
