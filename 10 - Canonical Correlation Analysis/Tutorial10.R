# Load the data (this is a base data set for R)
data("iris")

# Show the data
View(iris)

# Define the first variate (sepal) based on columns 1 and 2
sepal<-iris[,1:2]

# Define the second variate (petal) based on columns 3 and 4
petal<-iris[,1:2]

# Install / Load the required packages
install.packages("yacca")
library(yacca)
install.packages("CCA")
library(CCA)

# Check the bivariate correlations
round(cor(iris[c(1:4)]),2)

# Define and create your canonical correlation model
cc1<-cca(sepal,petal)

# Show the canonical correlation values for your model
summary(cc1)

# Install / Load the required packages
install.packages("CCP")
library(CCP)

# Check Wilk’s Lambda (or any other method)
p.asym(cc1$corr,150,2,2,tstat = "Wilks")

# You can re-run if you lost the previous output
summary(cc1)

# Create helio plot
helio.plot(cc1)
