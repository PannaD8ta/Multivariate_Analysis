#install.packages("dplyr","ggplot2","car","hrbrthemes","tidyr","viridis","psych","nortest")
#install.packages("mvnormtest","MVN","MASS","clusterGeneration","corrplot", "rgl", "EnvStats")
#install.packages("biotools", "heplots")
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
library(MVN) 
library(MASS) 
library(clusterGeneration) 
library(corrplot)
library(rgl)
library(EnvStats)
library(biotools)
library(heplots)
#set working directory to where you have saved your school.RData file
manova_data<-read.csv("manova_data.csv", header=T)

##check the stucture of the data:
str(manova_data)

#Compute summary statistics by groups
describeBy(manova_data[,1:2], group=manova_data$iv1)

# A simple grouped scatterplot:
xyplot(dv1 ~ dv2, group = iv1, data = manova_data, 
       # Define axes:
       xlab = "DV 2", ylab = "DV 1", 
       # Define legend parameters:
       auto.key = list(x = .1, y = .8, corner = c(0, 0)), 
       scales = "free", par.settings=list(superpose.symbol=list(pch=1:3)))

# A grouped scatterplot with 95% data concentration ellipses:
xyplot(dv1 ~ dv2, groups=iv1, data = manova_data,
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
manova_data %>%
  group_by(iv1) %>%
  summarize(cor(dv1,dv2))

#1. two or more dependent variables should be continuous
# plus no univariate outliers.
par(mfrow=c(1,2))
hist(manova_data$dv1)
hist(manova_data$dv2)
# plus  multivariate normality and no multivariate outliers
ggplot(manova_data[manova_data$iv1=="A",], aes(x=dv1, y=dv2) ) +
  geom_density_2d()+ labs(x = "DV 1", y="DV 2") + labs(title = "Contour plot for group A")+
  theme_ipsum()

ggplot(manova_data[manova_data$iv1=="B",], aes(x=dv1, y=dv2) ) +
  geom_density_2d()+ labs(x = "DV 1", y="DV 2") + labs(title = "Contour plot for group B")+
  theme_ipsum()

ggplot(manova_data[manova_data$iv1=="C",], aes(x=dv1, y=dv2) ) +
  geom_density_2d()+ labs(x = "DV 1", y="DV 2") + labs(title = "Contour plot for group C")+
  theme_ipsum()

#linear relationship between each pair of dependent variables for each group of the independent variable
pairs.panels(manova_data[manova_data$iv1=="A",1:2])
pairs.panels(manova_data[manova_data$iv1=="B",1:2])
pairs.panels(manova_data[manova_data$iv1=="C",1:2])

#homogeneity of variance-covariance matrices
#The null hypothesis for this test is that the observed covariance matrices for the dependent variables are equal across groups.
boxM(manova_data[,1:2], group = manova_data$iv1)

#MANOVA: Original data
manova.orig <- manova(cbind(dv1, dv2) ~ iv1,
                      data = manova_data)
summary(manova.orig, test = "Wilks")

#Pair-wise comparisons: post-hoc test results
# Post hoc tests
pairwise.t.test(manova_data$dv1, manova_data$iv1,
                p.adjust.method = "BH")
pairwise.t.test(manova_data$dv2, manova_data$iv1,
                p.adjust.method = "BH")

summary(manova.orig, test = "Wilks")
# Calculating partial eta-square using the Wilks lambda
etasq(manova.orig, test = "Wilks")

# Calculating partial eta-square for ANOVA: Income
etasq(aov(dv1 ~ iv1, data=manova_data), anova = T)
# partial eta square = 0.0054
# Calculating partial eta-square for ANOVA: hrs
etasq(aov(dv2 ~ iv1, data=manova_data), anova = T)
# partial eta square = 0.0035