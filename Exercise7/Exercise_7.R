#

library(stringr)
library(readr)
library(dplyr)
library(psych)
library(caTools)
library(PerformanceAnalytics)


dataset <- read_csv("hprice.csv")

dataset.reg <- subset(dataset, select=c(price,bdrms, lotsize, sqrm,country))

pairs.panels(dataset.reg,col="red")

chart.Correlation(dataset.reg,hist=T)

cor(dataset.reg)


regressor = lm(formula = price ~ .,
               data = dataset.reg)

print(summary(regressor)$coefficient)


hist(residuals(regressor))
boxplot(residuals(regressor))
shapiro.test(residuals(regressor))

plot(density(residuals(regressor)))

hist(residuals(regressor), # histogram
     col="cyan", # column color
     border="red",
     prob = TRUE, # show densities instead of frequencies
     xlab = "range",
     main = "Reseduals")
lines(density(residuals(regressor)), # density plot
      lwd = 2, # thickness of line
      col = "black")

par(mfrow=c(2,2))
plot(regressor)

##Solution for task3
dataset.bedrom <- subset(dataset, select=c(price,bdrms))

chart.Correlation(dataset.bedrom,hist=T)

model <- lm(fromula= price ~ bedrms,data = dataset.bedrom)








