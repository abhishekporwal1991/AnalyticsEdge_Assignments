setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit4_Trees")
getwd()

# load the claim data
Claims = read.csv("ClaimsData.csv")
str(Claims)

table(Claims$bucket2009)/nrow(Claims)

library(caTools)
set.seed(88)

# Splitting the data
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)

ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest  = subset(Claims, spl == FALSE)

# average age of patients
summary(ClaimsTrain)
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain)


# Baseline model- Classification matrix
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

# Accuracy of base model - 0.685

#creating penalty matrix as created by D2Hawkeye
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
PenaltyMatrix

# Calculating the penalty error of baseline model
(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))* PenaltyMatrix)

(sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))* PenaltyMatrix))/nrow(ClaimsTest)
# Penalty Error - 0.7347

# If baseline model is computed using most frequent outcome then the model
table(ClaimsTest$bucket2009)

# Accuracy - 0.67127

# Penalty error
(0*0.1222978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(ClaimsTest) # 1.044301


# Building CART model
# Cross validation to find out the optimal parameter

#library(caret)
#library(e1071)
#numFolds = trainControl(method = "cv", number = 10)
#cpGrid = expand.grid(.cp = seq())
#train(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = ClaimsTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = ClaimsTrain, method = "class", cp = 0.00005 )
prp(ClaimsTree)

PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, PredictTest)

# Accuracy of CART model - 0.71336

# Penalty error rate
as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix
(sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix))/nrow(ClaimsTest) # 0.75926 - this is pretty higher than the baseline model

# use parameter loss parameter of 'rpart' to reduce the penalty error
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = ClaimsTrain, method = "class", cp = 0.00005, parms = list(loss = PenaltyMatrix) )

PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, PredictTest)

# Accouracy - 0.6414613

# Penalty error rate
(sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMatrix))/nrow(ClaimsTest) # 0.6409755 - lower than the previous model 
