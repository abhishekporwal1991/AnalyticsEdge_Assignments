setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment 4_Trees")
getwd()

census = read.csv("census.csv")

library(caTools)
set.seed(2000)

split = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, split == TRUE)
test  = subset(census, split == FALSE)

# Logistic regression  model
logreg = glm(over50k ~ ., data = train, family = binomial)
summary(logreg)

predLog = predict(logreg, newdata = test, type = "response")

table(test$over50k, predLog >= 0.5)

# baseline accuracy
# most significant outcome
table(train$over50k)

# test set prediction
table(test$over50k)

# Area under the curve (AUC)
library(ROCR)
ROCRpred = prediction(predLog, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# CART classification model
library(rpart)
library(rpart.plot)

CARTtree = rpart(over50k ~ ., data = train, method = "class")
prp(CARTtree)

# accuracy
predCART = predict(CARTtree, newdata = test, type = "class")

# confusion matrix
table(test$over50k, predCART)

# Plotting ROC curve for CART model
predCART_ROC = predict(CARTtree, newdata = test)
ROCR_CARTpred = prediction(predCART_ROC[,2], test$over50k)
x = performance(ROCR_CARTpred, "tpr","fpr")
plot(x)

as.numeric(performance(ROCR_CARTpred, "auc")@y.values)


# Randon Forest model
set.seed(1)

# reducing the size of training set
trainSmall = train[sample(nrow(train), 2000),]

library(randomForest)
set.seed(1)
RFtree = randomForest(over50k ~ ., data =trainSmall, method = "class")

predRF = predict(RFtree, newdata = test)

table(test$over50k, predRF)

vu = varUsed(RFtree, count =TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RFtree$forest$xlevels[vusorted$ix]))

varImpPlot(RFtree)

#-----------------------------------
library(e1071)
library(caret)

set.seed(2)
numFolds = trainControl(method = "cv", number = 10)
CARTgrid = expand.grid(.cp = seq(0.002, 0.1, 0.002))
train(over50k ~., data = train, method = "rpart", trControl = numFolds, tuneGrid = CARTgrid)

CARTtree_cp = rpart(over50k ~ ., data=train, method = "class", cp = 0.002)
pred_cp  = predict(CARTtree_cp, newdata = test, type = "class")

table(test$over50k, pred_cp)
prp(CARTtree_cp)
