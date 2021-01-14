install.packages('psych')
install.packages('GPArotation')
library('GPArotation')
library('psych')

teach<-read.csv( file.choose(), header=T)
head(teach)
dim(teach)
head(teach[, 3:23])
teach<-teach[,3:23]
sum(is.na(teach))
head(teach)
summary(teach)

#***** Without loops
sum(is.na(teach[,1]))
sum(is.na(teach[,2]))
sum(is.na(teach[,3]))
sum(is.na(teach[,4]))
sum(is.na(teach[,5]))
temp<-median(teach[,5], na.rm=T)
temp
temp1<-teach[,5]
temp1[is.na(temp1)]
temp1[is.na(temp1)]<-temp
teach[,5]<-temp1
sum(is.na(teach[,5]))
# needs to be run 21 times
#******************************
n<-dim(teach)[2]
for(i in 1:n){
  counts<-sum(is.na(teach[,i]))
  if(counts >0){
    temp<-teach[,i]
    temp[is.na(temp)]<-median(teach[,i], na.rm=T)
    teach[,i]<-temp
  }
}

sum(is.na(teach))
# FA analysis
correlation<-round(cor(teach),2)   #corrleation to see whether all items are corrleated at with one other
KMO(teach)    # sample adequacy 
bartlett.test(teach)  # corrleation test
output<-fa(teach, nfactors = 21, rotate="none")   # factor analysis
output    # see the output

fa.parallel(teach, fa="fa")   #parallel analysis
output<-fa(teach, nfactors = 4, rotate="none")   # factor analysis
output$loadings

output<-fa(teach, nfactors = 4, rotate="varimax")   # factor analysis
output$loadings

output<-fa(teach, nfactors = 4, rotate="oblimin")   # factor analysis
output$loadings
teach<-teach[, -20]  # variable 20 droppped
output<-fa(teach, nfactors = 4, rotate="oblimin")   # factor analysis
output$loadings

output<-fa(teach, nfactors = 3, rotate="oblimin")   # 3 factor analysis
output$loadings

temp<-round(output$residual,2)

write.csv(temp, file="residual.csv")

teach<-teach[, -4]  # variable 4 droppped
output<-fa(teach, nfactors = 3, rotate="oblimin")   # factor analysis
output$loadings
temp<-round(output$residual,2)

write.csv(temp, file="residual.csv")

head(teach)
fac1<-teach[, c(2,8,11,12,14,17,18,19)]
alpha$fac1



