movie <- read.delim("movie.txt")
setwd("/Users/nictsau/Desktop")
View(movie)

library(tidyverse)
library(MASS)
install.packages("MASS")
install.packages("klaR")
library(klaR)

# Set a seed value
set.seed(123)

# Split the data 50/50
training_sample <- sample(c(TRUE, FALSE), nrow(movie), replace = T, prob = c(0.5,0.5))

# Define the Train Data
train <- movie[training_sample, ]

# Define the Testing Data
test <- movie[!training_sample, ]

# Create an initial LDA model (m1) based upon the data
# Results: Comedy (47%), Drama (42%), Horror (10%)
# Compare mean differences in Runtime and Metascore first
# LD1 explains 99.1% of the variation
m1 <- lda(Genre ~ Runtime + Metascore + imdbRating, train); m1

# Plot LD1 & LD2
plot(m1, col=as.integer(train$Genre))

# Plot LD1 only
plot(m1, dimen = 1, type ="b")

# Compare model against train set
lda.train <- predict(m1)
train$lda <- lda.train$class
table <- table(train$lda, train$Genre)
accuracy <- sum(table[1],table[5]) / sum(table)
accuracy
(319+213)/(319+156+78+92+213+9)

# The total number of correctly predicted observations is the sum of diagonal (328 + 226 + 0)
# So this model fit the training data correctly for 63.97% of the case
# Verifying the training set doesn't prove accuracy,
# but a poor fit to the training data could be a sign that the model isn't a good one.

# Compare model against test set
lda.test <- predict(m1, test)
test$lda <- lda.test$class
table2 <- table(test$lda, test$Genre)
table2

accuracy2 <- sum(table2[1],table2[5]) / sum(table2)
accuracy2





