# 1) Determine the population principal component Y and Y for the covariance matrix
# 5, 2, 2, 2 by row
matrix1 <-matrix(c(5,2,2,2), ncol=2)
eigen(matrix1)
eigen(matrix1)$values
eigen(matrix1)$vectors
## also calculate the proportion of the total variance explained by the first principal component.
eigen(matrix1)$values[1]/sum(eigen(matrix1)$values) 

# 2) Let X1,X2. …,, X5 denote observed weekly rates of return for JP Morgan, Citybank, Wells Fargo, Royan Ducth Shel and ExxonMobil respectively. 
matrix2<-matrix(c(1,0.632,0.511,0.115,0.155,0.632,1,0.574,0.322,0.213,0.511,0.574,1,0.183,0.146,0.115,0.322,0.183,1,0.683,0.155,0.213,0.146,0.683,1), ncol=5, byrow=F)

## a. Compute the eigenvalues and eigenvectors for this correlation matrix 
eigen(matrix2)
eigen(matrix2)$values
eigen(matrix2)$vectors
## b. How many PCs to be retained?
eigen(matrix2)$values
## c. What percentage of total variance is explained by the first PC?
eigen(matrix2)$values[1]/sum(eigen(matrix2)$values)  
# Check that eigen(matrix2)$values[1]/sum(eigen(matrix2)$values)  = 2.4376148 + 1.4062612 + 0.5001857 + 0.4000328 + 0.2559055
  
# Install packages: Psych, MASS and parallel

# 3) This exercise aims to develop a Consumer Price Index (CPI) of US based on the estimated retail food prices by cites. For each of 23 cities, the prices of five kinds of food are measured. The developed CPI should reflect most information given by the five prices, of Bread Burger Milk Oranges Tomatoes, but contain a lower number of variables;
# The data, which are reported by US Bureau of Labour Statistics (1973), and is given as a text file with file “LabourStat1973.csv”.

## a. Load the dataset into R
setwd("/Users/nictsau/Projects/Dataset")
example1<-read.csv("LabourStat1973.csv", header=T)
example1<-read.csv(file.choose(), header=T)
head(example1)
str(example1)
View(example1)
## b. Compute the principal components from the covariance matrix (mean-correlated data).
data<-scale(example1[,2:6])
head(data)
pca<-principal(data,nfactor=5,rotate="none") 
str(pca) # to see the structure of the output
## c. How many PCs to be retained
pca # 2 PCs
## d. What percentage of total variance is explained by the first PC?
pca # 48%
## e. Obtain scree plot and discuss.
plot(pca$values, type="l") # to decide how many PCAs to be retained
abline(h=1, lty=2)

dim(data)
hornpa(k=5,size=23,reps=500,seed=100)


# 4) The data for this exercise come from 200 specimens of a certain type of crabs which is availabe the MASS package. The crabs come in two colours (blue and orange). In the experiment 100 of each type were collected, 50 males and 50 females, and for each of the 200 crabs, five quantities were measured: The carapace/shell length (CL), carapace/shell width (CW), size of frontal lobe (FL), rear width (RW), and body depth (BD). 

## a. Install MASS package and load the dataset.
install.packages("MASS")
library(MASS)
crabs <- crabs
head(crabs)
dim(crabs)

## b. Compute the principal components from the covariance matrix (mean-correlated data).
vars<-crabs[, 4:8]
vars<-scale(vars); vars
pca <- principal(vars, nfactor=5, rotate="none")

## c. How many PCs to be retained
pca # 1 PC

## d. What percentage of total variance is explained by the first PCA?
pca # 96%

## e. Obtain scree plot and discuss.
plot(pca$value, type="l")
abline(h=1, lty=2) # abline(h=1, lty=1, lwd=10) -> lwd=10 will give you a thick line
hornpa(k=5,size=200,reps=500,seed=100)

# 5) The data ” MAGIC Gamma Telescope Data Set”are MC generated to simulate registration of high energy gamma particles in a ground-based atmospheric Cherenkov gamma telescope using the imaging technique. Cherenkov gamma telescope observes high energy gamma rays, taking advantage of the radiation emitted by charged particles produced inside the electromagnetic showers initiated by the gammas, and developing in the atmosphere. This Cherenkov radiation (of visible to UV wavelengths) leaks through the atmosphere and gets recorded in the detector, allowing reconstruction of the shower parameters. The available information consists of pulses left by the incoming Cherenkov photons on the photomultiplier tubes, arranged in a plane, the camera. Depending on the energy of the primary gamma, a total of few hundreds to some 10000 Cherenkov photons get collected, in patterns (called the shower image), allowing to discriminate statistically those caused by primary gammas (signal) from the images of hadronic showers initiated by cosmic rays in the upper atmosphere (background). 

# The data set was generated by a Monte Carlo program, Corsika, described in: 
  # D. Heck et al., CORSIKA, A Monte Carlo code to simulate extensive air showers, 
# The variables are 
# fLength:	continuous # major axis of ellipse [mm] 
# fWidth:	continuous # minor axis of ellipse [mm] 
# fSize:	continuous # 10-log of sum of content of all pixels [in #phot] 
# fConc:	continuous # ratio of sum of two highest pixels over fSize [ratio] 
# fConc1:	continuous # ratio of highest pixel over fSize [ratio] 
# fAsym:	continuous # distance from highest pixel to center, projected onto major axis [mm] 
# fM3Long: 	continuous # 3rd root of third moment along major axis [mm] 
# fM3Trans:	continuous # 3rd root of third moment along minor axis [mm] 
# fAlpha:	continuous # angle of major axis with vector to origin [deg] 
# fDist:	continuous # distance from origin to center of ellipse [mm] 
# class: g,h # gamma (signal), hadron (background) 

## a. Load the dataset “MTD.csv”
example1<-read.csv("MTD.csv", header=T)
dim(example1)
head(example1)

## b. Compute the principal components from the covariance matrix (mean-correlated data).
vars<-example1[, 2:11]
vars<-scale(vars); vars
dim(vars)
pca <- principal(vars, nfactor=10, rotate="none"); pca

## c. How many PCs to be retained
## d. What percentage of total variance is explained by the first PCA?
## e. Obtain scree plot and discuss. 
