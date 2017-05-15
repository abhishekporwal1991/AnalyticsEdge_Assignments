setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment 4_Trees")
getwd()


letters = read.csv("letters_ABPR.csv")

# Adding new variable to the data frame
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)

split = sample.split(letters$isB, SplitRatio = 0.5)

train = subset(letters, split == TRUE)
test  = subset(letters, split == FALSE)

# Baseline model
table(train$isB)              # most frequent outcome is 'not B'.

table(test$isB)

# Classification model
library(rpart)
library(rpart.plot)

CARTb = rpart(isB ~ . - letter, data = train, method = "class")
summary(CARTb)
prp(CARTb)

predCART  = predict(CARTb, newdata = test, type = "class")

#accuracy
table(test$isB, predCART)

# Randon forest model
library(randomForest)
set.seed(1000)

RFb = randomForest(isB ~ . - letter, data = train)

predRF = predict(RFb, newdata = test, type = "class")

table(test$isB, predRF)

# --------------------------------------------
# Multi class classification
str(letters)
letters$letter = as.factor(letters$letter)
str(letters)

set.seed(2000)
Rsplit = sample.split(letters$letter, SplitRatio = 0.5)

Rtrain = subset(letters, Rsplit == TRUE)
Rtest  = subset(letters, Rsplit == FALSE)

table(Rtrain$letter)        # most frequent outcome is letter - P
table(Rtest$letter)

CART2 = rpart(letter ~ . - isB, data = Rtrain, method = "class")

predCART2 = predict(CART2, newdata = Rtest, type = "class")

table(Rtest$letter, predCART2)

# Random forest model
set.seed(1000)
RF2 = randomForest(letter ~ . - isB, data = Rtrain)

predRF2 = predict(RF2, newdata = Rtest, type = "class")

table(Rtest$letter, predRF2)
