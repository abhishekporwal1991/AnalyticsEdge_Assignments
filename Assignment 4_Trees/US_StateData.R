data(state)
statedata = data.frame(state.x77)
str(statedata)

# Linear regression model
LinModel = lm(Life.Exp ~ ., data = statedata)
summary(LinModel)

pred = predict(LinModel)

SSE = sum((pred - statedata$Life.Exp)^2)
SSE

SSE1 = sum((LinModel$residuals)^2)
SSE1

LinModel2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LinModel2)

SSE2 = sum((LinModel2$residuals)^2)
SSE2
-------------------------------------------------------
  
# CART model
library(rpart)
library(rpart.plot)

CARTModel = rpart(Life.Exp ~ ., data = statedata)
prp(CARTModel)

predtree = predict(CARTModel)

TreeSSE = sum((predtree - statedata$Life.Exp)^2)
TreeSSE

CARTModel2 = rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(CARTModel2)

predtree2 = predict(CARTModel2)
TreeSSE2 = sum((predtree2 - statedata$Life.Exp)^2)
TreeSSE2


CARTModel3 = rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
predtree3 = predict(CARTModel3)

TreeSSE3 = sum((predtree3 - statedata$Life.Exp)^2)
TreeSSE3
prp(CARTModel3)
#---------------------------------------------

# Model using cross vaildation
library(caret)
set.seed(111)

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.50, 0.01))
train(Life.Exp ~ ., data=statedata, method = "rpart" , trControl = numFolds, tuneGrid = cpGrid)

CARTModel_cp = rpart(Life.Exp ~ ., data = statedata, cp = 0.12)
prp(CARTModel_cp)

predtree_cp = predict(CARTModel_cp)

TreeSSE_cp = sum((predtree_cp - statedata$Life.Exp)^2)
TreeSSE_cp

set.seed(111)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.50, 0.01))
train(Life.Exp ~ Area, data=statedata, method = "rpart" , trControl = numFolds, tuneGrid = cpGrid)

CARTModel_cp1 = rpart(Life.Exp ~ Area, data = statedata, cp = 0.02)
prp(CARTModel_cp1)

predtree_cp1 = predict(CARTModel_cp1)
TreeSSE_cp1 = sum((predtree_cp1 - statedata$Life.Exp)^2)
TreeSSE_cp1
