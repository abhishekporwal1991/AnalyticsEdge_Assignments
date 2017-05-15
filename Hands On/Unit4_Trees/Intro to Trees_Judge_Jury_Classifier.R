setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit4_Trees")
getwd()

#read data file in R
stevens = read.csv("stevens.csv")
str(stevens)

# Splitting the data in train and test
# Load the the caTools library
library(caTools)
set.seed(3000)

spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, spl  == TRUE)
Test  = subset(stevens, spl  == FALSE)

# Load libraries for modelling purpose
library(rpart)
library(rpart.plot)

# CART model for classification problems
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner +  Respondent + LowerCourt + Unconst, data = Train, method = "class",  minbucket = 25)

# Plotting the split
prp(StevensTree)
prp(StevensTree, type = 1, extra = 1)

predictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, predictCART)

library(ROCR)
predictROC = predict(StevensTree, newdata = Test)  
predictROC

pred = prediction(predictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# Area under the curve (AUC) of ROCR curve
as.numeric(performance(pred, "auc")@y.values)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner +  Respondent + LowerCourt + Unconst, data = Train, method = "class",  minbucket = 25)
prp(StevensTree, extra = 1)


# Building a Random Forest model
library(randomForest)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + Unconst, data = Train, nodesize = 25, ntree = 200)

# The outcome variable should be "factor" if building classification model using Random Forest
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse  = as.factor(Test$Reverse) 

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + Unconst, data = Train, nodesize = 25, ntree = 200)

# Predicting the accuracy
predictForest = predict(StevensForest, newdata = Test)

# Confusion matrix
table(Test$Reverse, predictForest)

set.seed(100)
StevensForest1 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
predictForest1 = predict(StevensForest1, newdata = Test)
table(Test$Reverse, predictForest1)

set.seed(200)
StevensForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
predictForest2 = predict(StevensForest2, newdata = Test)
table(Test$Reverse, predictForest2)

#install.packages("e1071")
library(e1071)
library(caret)

# an argument to be passed in 'train' function to control the computational nuances 
numFolds = trainControl(method = "cv", number = 10)

# creating a data frame
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

# Build CART model again using the cp parameters instead of minbucket i.e. using cross validation
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp = 0.18 )
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")

# Confusion matrix
table(Test$Reverse, PredictCV)

# Accuracy
#(59+64)/(59+18+29+64) = 0.7235294 > previous CART model

# plotting the tree
prp(StevensTreeCV)
