perfac<-read.csv(file.choose(), header=T)
head(perfac)
names(perfac)

perfac1<-perfac[,c(9:23)]
head(perfac1)
summary(perfac1)

install.packages("psych")
library("psych")
install.packages("GPArotation")
library(GPArotation)
cor_mat<-round(cor(perfac1),3)
cor_mat
#KMO test
KMO(perfac1)
#0.5 so it is satisfied
#Bartlett test, 
bartlett.test(perfac1)
# P value is less than 0.05, there is a correlation between vars
output_fa<-fa(perfac1, nfactors = 15, rotate = "none")
print(output_fa)
fa.parallel(perfac1, fa="fa")
#from parallel analysis we have decided to retain 5 factors
output_fa<-fa(perfac1, nfactors = 5, rotate = "none")
print(output_fa)
str(output_fa)
loadings<-output_fa$loadings
loadings
# we did not reach a simple structure so we apply rotation
output_fa<-fa(perfac1, nfactors = 5, rotate = "oblimin")
loadings<-output_fa$loadings
loadings
write.csv(loadings, file="loadgins1.csv")

output_fa<-fa(perfac1, nfactors = 4, rotate = "oblimin")
loadings<-output_fa$loadings
write.csv(loadings, file="loadgins1.csv")

output_fa<-fa(perfac1, nfactors = 4, rotate = "varimax")
loadings<-output_fa$loadings
write.csv(loadings, file="loadgins2.csv")
# sD4 TO BE REMOVED DUE TO CROSS  LOADING
perfac2<-perfac1[, -4]
head(perfac2)
output_fa<-fa(perfac1[, -4], nfactors = 4, rotate = "varimax")
loadings<-output_fa$loadings
write.csv(loadings, file="loadgins3.csv")

output_fa<-fa(perfac1[,-4], nfactors = 3, rotate = "varimax")
output_fa
loadings<-output_fa$loadings
write.csv(loadings, file="loadgins4.csv")
dim(perfac1)

############################################################################


lab2<-read.csv(file.choose(), header=T)
head(lab2)

lab21<-lab2[,c(2:21)]
head(lab21)
#Screeing
summary(lab21)
cor_mat<-round(cor(lab21),3)
cor_mat
#KMO test
KMO(lab21)
#0.5 so it is satisfied
#Bartlett test, 
bartlett.test(lab21)
# P value is less than 0.05, there is a correlation between vars
output_fa<-fa(lab21, nfactors = 20, rotate = "none")
print(output_fa)
fa.parallel(lab21, fa="fa")
#from parallel analysis we have decided to retain 5 factors
output_fa<-fa(lab21, nfactors = 3, rotate = "none")
print(output_fa)
str(output_fa)
loadings<-output_fa$loadings
loadings
write.csv(loadings, file="loadgins1.csv")
#we have not reached a simple structure

output_fa<-fa(lab21, nfactors = 3, rotate = "oblimin")
loadings<-output_fa$loadings
write.csv(loadings, file="loadgins1.csv")

output_fa<-fa(lab21, nfactors = 3, rotate = "varimax")
loadings<-output_fa$loadings
write.csv(loadings, file="loadings2.csv")

# Item2 TO BE REMOVED DUE TO CROSS  LOADING

output_fa<-fa(lab21[, -2], nfactors = 3, rotate = "varimax")
loadings<-output_fa$loadings
write.csv(loadings, file="loadings3.csv")

# Item2 and item3 TO BE REMOVED DUE TO CROSS  LOADING

output_fa<-fa(lab21[, c(-2,-3)], nfactors = 3, rotate = "varimax")
loadings<-output_fa$loadings
write.csv(loadings, file="loadings3.csv")

output_fa<-fa(lab21[, c(-2,-3)], nfactors = 3, rotate = "varimax")
output_fa
loadings<-output_fa$loadings
write.csv(loadings, file="loadings4.csv")

#Imputation will be dicussed later on



