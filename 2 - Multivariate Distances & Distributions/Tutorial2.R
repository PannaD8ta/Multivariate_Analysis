# Author: Nicholas Tsau 

# QUESTION 1

# Write Euclidean distance between seven canine groups
X1 <- c(9.7, 8.1, 13.5, 11.5, 10.7, 9.6, 10.3)
X2 <- c(21, 16.7, 27.3, 24.3, 23.5, 22.6, 22.1)
X3 <- c(19.4, 18.3, 26.8, 24.5, 21.4, 21.1, 19.1)
X4 <- c(7.7, 7, 10.6, 9.3, 8.5, 8.3, 8.1)
X5 <- c(32, 30.3, 41.9, 40, 28.8, 34.4, 32.2)
X6 <- c(36.5, 32.9, 48.1, 44.6, 37.6, 43.1, 35)

canine <- data.frame(X1, X2, X3, X4, X5, X6, row.names = c("Modern", "Golden", "Chinese", "Indian", "Cuon", "Dingo", "Prehistoric"))

# Show canine table
canine

# Euclidean distance with raw variables
# ?dist
dist(canine, method = "euclidean")
round(dist, 3)

# Next, we will calculate the distance for standardised variables
stand.canine <- scale(canine)
round(dist(stand.canine, method = "euclidean"), 2)

# ?mahalanobis
cov1 <- cov(canine)
cov1
means <- apply(canine, 2, mean) # 1 = row, 2 = column
means

# now we can calculate the mahalanobis distance for 7 observations based on 6 variables
dist.m <- mahalanobis(canine, center=means, cov=cov1)
dist.m

library("chi")

critical.value <- qchi(0.999, df=6)
critical.value

#str(dist.m)

library(clusterGeneration)
mu <- c(2, 10) # Define means
set.seed(30005)
pos.def.mat <- genPositiveDefMat(2) # Creating a PD Matrix
pos.def.mat

# Extract the variance covariance matrix
Sigma <- pos.def.mat$Sigma 

#Generate values from MVN Distribution
mvn <- mvnorm(100, mu = mu, Sigma = Sigma)

dist.m <- mahalanobis(mvn, center = mu, cov=Sigma)

mvn <- as.data.frame(mvn)
head(mvn)
binary <- mvn$dist.m>13.82
mvn <- cbind(mvn, binary)
mvn
?subset

# First we keep the outliers and do the analysis, and remove the outliers and do the analysis to justify what we had done. Do the comparison whenever you found a difference. 

# QUESTION 2  

ozone <- read.csv(file.choose(), header=T)
head(ozone)
dim(ozone)
ozone1 <- ozone[, -2] # 2nd column will be removed from the dataset, then we can find out he mahalanobis 
head(ozone1) 
# Next compute the critical value for this dataset
# Different values influences the results
means.oz <- apply(ozone1, 2, mean)
means.oz

cov.oz <- cov(ozone1)
cov.oz

dist.oz <- mahalanobis(ozone1, center=means.oz, cov=cov.oz) #computes the mahanabos distance for this subset

# Now to find unusual values in the dataset, df= number of variables
critical.value <- qchisq(0.999, df=9)
critical.value 
outliers <- dist.oz>27.88
table(outliers) # table suggest that there are two unsual values (TRUE), then we need to detect the values.

ozone1 <- cbind(ozone1, outliers)
?which
which(ozone1$outliers=="TRUE")

ozone1 <- ozone1[c(-33, -92), ] # the two records will be removed from the analysis
dim(ozone1)
