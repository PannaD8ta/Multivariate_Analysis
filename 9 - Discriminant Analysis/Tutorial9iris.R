# Load libraries
library(tidyverse)
library(MASS)
library(klaR)
library(datasets)

# Attaching Iris dataset
data(iris)
View(iris)

# Set a seed value
set.seed(123)

# Split the data 50/50
training_sample <- sample(c(TRUE, FALSE), nrow(iris), replace = T, prob = c(0.5,0.5))

# Define the Train Data
train <- iris[training_sample, ]

# Define the Testing Data
test <- iris[!training_sample, ]

# Create an initial LDA model (m1) based upon the data
# the "." is taking everything for the IV, and we have assigned everything to m1, 
# the ld function will be included in the calculation
m1 <- lda(Species ~ ., train); m1
# use variable.names(iris) to get the accurate measurements
m1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train); m1

# Plot LD1 & LD2
plot(m1, col=as.integer(train$Species))

# Plot LD1 only
plot(m1, dimen = 1, type ="b")

# Compare model against train set
lda.train <- predict(m1)
train$lda <- lda.train$class
table(train$lda, train$Species)

# Compare model against test set
lda.test <- predict(m1, test)
test$lda <- lda.test$class
table2 <- table(test$lda, test$Species)
table2









