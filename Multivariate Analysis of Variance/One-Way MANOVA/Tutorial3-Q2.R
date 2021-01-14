# Q2
library(car)
Soils_data <- data.frame(Soils)

dim(Soils_data)

head(Soils_data, 5)

##### Checking for linearity
library(psych)
pairs.panels(Soils_data[, 6:14])

##### Checking for multivariate normality & univariate normality
library(MVN) # Using MVN package

result.all <- mvn(data=Soils_data[, c(2, 6:14)], mvnTest = "mardia", subset = "Contour")
result.all

#### Homogeneity of variance-covariance matrices
library(biotools)
#options(scipen = 999) # to stop scientific notation output
boxM(Soils_data[, 6:14], grouping = Soils_data$Contour)

# MANOVA
model2 <- manova(cbind(pH, N, Dens, P, Ca, Mg, K, Na, Conduc) ~ Contour, data = Soils_data)
summary(model2, test = "Pillai")

summary(model2, test = "Wilks")

summary(model2, test = "Hotelling-Lawley")

summary(model2, test = "Roy")

# One-way ANOVA for each dependent variable
summary.aov(model2)

# Pairwise tests using the method 1 discussed in lectures
pairwise.t.test(Soils_data$K, Soils_data$Contour,
                p.adjust.method = "BH")

# Descriptive statistics: K
library(psych)
describeBy(Soils_data$K, group = Soils_data$Contour)

# Pairwise tests using the method 2 discussed in lectures
summary(manova(cbind(pH, N, Dens, P, Ca, Mg, K, Na, Conduc) ~ Contour, data = Soils_data,
               subset = Contour %in% c("Depression", "Slope")))

summary(manova(cbind(pH, N, Dens, P, Ca, Mg, K, Na, Conduc) ~ Contour, data = Soils_data,
               subset = Contour %in% c("Depression", "Top")))

summary(manova(cbind(pH, N, Dens, P, Ca, Mg, K, Na, Conduc) ~ Contour, data = Soils_data,
               subset = Contour %in% c("Slope", "Top")))
