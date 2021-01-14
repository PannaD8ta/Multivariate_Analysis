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

#set working directory to where you have saved your income.csv file
data(OBrienKaiser, package="carData")
dataEx1<-OBrienKaiser[,c(1,3,8,13)]


##check the stucture of the data:
str(dataEx1)

##have a look at a top few rows of data
head(dataEx1)

#reshaping the data
library(reshape)
dataFinal <- melt(dataEx1, id = "treatment", measured = c("pre.1", "post.1", "fup.1"))
head(dataFinal, 3)

#Compute summary statistics by groups
describeBy(dataFinal$value, list(dataFinal$treatment, dataFinal$variable))

#A visual plot of the treatments across the three test points (pre, post, follow-up)
Treatment <- matrix(rep(cbind("control", "A", "B"), c(3)))
Variable <- matrix(rep(cbind("1.pre", "2.post", "3.fup"), c(3,3,3)))
Avg <- matrix(cbind(3.6, 4.25, 3.71, 3.4, 5.5, 6, 4.2, 7, 6.86))
newData <- data.frame(Treatment, Variable, Avg)
head(newData, 4)

ggplot(newData, aes(x=Variable, y=Avg, group = Treatment)) + geom_line(aes(linetype=Treatment)) +
  labs(x="Dependent variables", y = "Mean")

#one-way MANOVA. 
newData1 <- data.frame(dataFinal$treatment, dataEx1$pre.1 - dataEx1$post.1,
                         dataEx1$post.1-dataEx1$fup.1)
colnames(newData1) <- c("Treatment", "pre_post", "post_folwup")
model <- manova(cbind(pre_post, post_folwup) ~ Treatment, data = newData1)
summ <- summary(model, test = "Wilks")
summ