#install.packages("dplyr","ggplot2","car","hrbrthemes","tidyr","viridis","psych","nortest")
#install.packages("mvnormtest","MVN","MASS","clusterGeneration","corrplot", "rgl", "EnvStats")
#install.packages("biotools", "heplots","lattice","latticeExtra","mnormt")
#load required libraries
library(dplyr)
library(ggplot2)
library(car)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(psych)
library(nortest)
library(mvnormtest)
library(mnormt)
library(MVN) 
library(MASS) 
library(clusterGeneration) 
library(corrplot)
library(rgl)
library(EnvStats)
library(biotools)
library(heplots)
library(lattice)
library(latticeExtra)

#set working directory to where you have saved your income.csv file
dataset<-read.csv("income.csv", header=T)

income.data<-na.omit(dataset)

##check the stucture of the data:
str(income.data)

##have a look at a top few rows of data
head(income.data)

#Compute summary statistics by groups
describeBy(income.data, group="agecat")

# A simple grouped scatterplot:
xyplot(Income ~ hrs, group = agecat, data = income.data, 
       # Define axes:
       xlab = "DV 2", ylab = "DV 1", 
       # Define legend parameters:
       auto.key = list(x = .1, y = .8, corner = c(0, 0)), 
       scales = "free", par.settings=list(superpose.symbol=list(pch=1:4)))

# A grouped scatterplot with 95% data concentration ellipses:
xyplot(Income ~ hrs, groups=agecat, data = income.data,
       # Define axes:
       xlab = "DV 2", ylab = "DV 1",
       # Define legend parameters:
       auto.key = list(x = .1, y = .8, corner = c(0, 0)), scales = "free",
       par.settings = list(superpose.symbol = list(pch=c(1:13)), superpose.line = list(lwd=2, lty=1)),
       # Superimpose data ellipse on the scatterplot:
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.ellipse(x, y, ...)
       }
)

#check correlations between dvs for groups
income.data %>%
  group_by(agecat) %>%
  summarize(cor(Income,hrs ))

#1. two or more dependent variables should be continuous
# plus no univariate outliers.
par(mfrow=c(1,2))
hist(income.data$Income)
hist(income.data$hrs)

levels(income.data$agecat)
#linear relationship between each pair of dependent variables for each group of the independent variable
pairs.panels(income.data[income.data$agecat=="\"18-29\"",1:2])
pairs.panels(income.data[income.data$agecat=="\"30-39\"",1:2])
pairs.panels(income.data[income.data$agecat=="\"40-49\"",1:2])
pairs.panels(income.data[income.data$agecat=="\"50+\"",1:2])

#multivariate normality
result.norm <- mvn(data=income.data, mvnTest = "mardia", subset = "agecat")
result.norm

#apply box-cox transformation
bx_income <- boxcox(income.data$Income, optimize = T)
shapiro.test(income.data$Income^bx_income$lambda)
bx_hrs <- boxcox(income.data$hrs, optimize = T)
shapiro.test(income.data$hrs^bx_hrs$lambda)
new.data <- data.frame(income.data, income.data$Income^bx_income$lambda, income.data$hrs^bx_hrs$lambda)
colnames(new.data) <- c(colnames(income.data), "income_trns", "hrs_trns")
# Checking for MVN for transformed data
result.norm.trns <- mvn(data=new.data[, c(3,4,5)], mvnTest = "mardia", subset = "agecat")
result.norm.trns

#homogeneity of variance-covariance matrices
#The null hypothesis for this test is that the observed covariance matrices for the dependent variables are equal across groups.
boxM(new.data[, c(1,2)], group = new.data$agecat)
boxM(new.data[, c(4,5)], group = new.data$agecat)


#MANOVA: Original data
manova.orig <- manova(cbind(Income, hrs) ~ agecat,
                      data = new.data)
summary(manova.orig, test = "Wilks")
# MANOVA: Transformed data
manova.trns <- manova(cbind(income_trns, hrs_trns) ~ agecat,
                      data = new.data)
summary(manova.trns, test = "Wilks")

summary.aov(manova.orig)

#Pair-wise comparisons: post-hoc test results
# Post hoc tests
pairwise.t.test(new.data$Income, new.data$agecat,
                p.adjust.method = "BH")

summary(manova.orig, test = "Wilks")
# Calculating partial eta-square using the Wilks lambda
etasq(manova.orig, test = "Wilks")

# Calculating partial eta-square for ANOVA: Income
etasq(aov(Income ~ agecat, data=income.data), anova = T)

# partial eta square = 0.085
# Calculating partial eta-square for ANOVA: hrs
etasq(aov(hrs ~ agecat, data=income.data), anova = T)
# partial eta square = 0.001
