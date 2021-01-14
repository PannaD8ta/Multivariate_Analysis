# Author: Nicholas Tsau 

#Q1
install.packages("carData")
library(car)
dim(iris)

head(iris)

str(iris)

# Check for linearity
library(psych)
pairs.panels(iris[, 1:4])

##### Checking for multivariate normality & univariate normality
library(MVN) # Using MVN package

# Checking multivariate & univariate normality of the four dependent variables
result.all <- mvn(data=iris[, 1:4], mvnTest = "mardia")
result.all

# Checking univariate normality
shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)
shapiro.test(iris$Petal.Length)
shapiro.test(iris$Petal.Width)

library(nortest)
install.packages(nortest)
ad.test(iris$Sepal.Length)
ad.test(iris$Sepal.Width)
ad.test(iris$Petal.Length)
ad.test(iris$Petal.Width)

normtest <- array(0, dim=c(5,4))
rownames(normtest) <- c("Shapiro", "Anderson-Darling", "CVM", "K-S", "SF")
colnames(normtest) <- colnames(iris[, 1:4])
for(i in 1:dim(normtest)[2]){
normtest[1,i] <- shapiro.test(iris[, i])$p.value
normtest[2,i] <- ad.test(iris[, i])$p.value
normtest[3,i] <- cvm.test(iris[, i])$p.value
normtest[4,i] <- lillie.test(iris[, i])$p.value
normtest[5,i] <- sf.test(iris[, i])$p.value
}

normtest

# Transforming Petal.length & Petal.width variables
install.packages('EnvStats')
library(EnvStats)
bx_PtlLength <- boxcox(iris$Petal.Length, optimize = T)
bx_PtlLength$lambda

shapiro.test(iris$Petal.Length^bx_PtlLength$lambda)

bx_PtlWdth <- boxcox(iris$Petal.Width, optimize = T)
bx_PtlWdth$lambda

shapiro.test(iris$Petal.Width^bx_PtlWdth$lambda)

# Checking multivariate & univariate normality of the four dependent variables for each species category
result <- mvn(data = iris[, 1:5], mvnTest = "mardia", subset = "Species",
              univariateTest = "SW", univariatePlot = "histogram")

# Multivariate normality
result$multivariateNormality

# Univariate normality
result$univariateNormality
result$Descriptives


#### Homogeneity of variance-covariance matrices
install.packages("biotools")
library(biotools)

boxM(iris[, 1:4], grouping = iris$Species)

# MANOVA
model1 <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris)
summary(model1, test = "Pillai")

summary(model1, test = "Wilks")

summary(model1, test = "Hotelling-Lawley")

summary(model1, test = "Roy")

# One-way ANOVA for each dependent variable
summary.aov(model1)

# Pairwise tests using the method 1 discussed in lectures
pairwise.t.test(iris$Sepal.Length, iris$Species,
                p.adjust.method = "BH")

pairwise.t.test(iris$Sepal.Width, iris$Species,
                p.adjust.method = "BH")

pairwise.t.test(iris$Petal.Width, iris$Species,
                p.adjust.method = "BH")

pairwise.t.test(iris$Petal.Length, iris$Species,
                p.adjust.method = "BH")

# Pairwise tests using the method 2 discussed in lectures
summary(manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris,
               subset = Species %in% c("setosa", "versicolor")))

summary(manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris,
               subset = Species %in% c("setosa", "virginica")))

summary(manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris,
               subset = Species %in% c("versicolor", "virginica")))

