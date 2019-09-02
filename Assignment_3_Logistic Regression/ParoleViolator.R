setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignments_Unit3_Logistic Regression")
getwd()

parole = read.csv("parole.csv")
str(parole)
summary(parole)

table(parole$violator)

# Conversion in factor
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)
summary(parole)

# Modelling- Logistic regression
set.seed(144)
library(caTools)
split = sample.split(parole$violator,SplitRatio = 0.7)
str(split)
table(split)

# Train and Test sample
train = subset(parole, split==TRUE)
test  = subset(parole, split==FALSE)
str(train)

# LR model 
Model = glm(violator ~ ., data=train, family = binomial)
summary(Model)

# Predicting test data
pred = predict(Model, newdata = test, type = "response")

max(pred)
summary(pred)

table(test$violator, pred >= 0.5)
   
    FALSE TRUE
  0   167   12
  1    11   12
  
# Accuracy  
(167+12)/(167+12+12+11)

# Sensitivity
12/(12+11)

# Specificity
167/(167+12)

table(test$violator)

  0   1 
179  23 

# Accuracy of base model
179/202

table(test$violator, pred <= 0.5)
   
  FALSE TRUE
0    12  167
1    12   11

# Sensitivity
11/(11+12)

library(ROCR)
prediction = prediction(pred, test$violator)
as.numeric(performance(prediction,"auc")@y.values)
