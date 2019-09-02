setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignments_Unit3_Logistic Regression")
getwd()

loans = read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
missingV  = subset(loans,is.na(loans$log.annual.inc) |is.na(loans$days.with.cr.line) | is.na(loans$revol.util) | is.na(loans$inq.last.6mths) | is.na(loans$delinq.2yrs) | is.na(loans$pub.rec))
View(missingV)
table(missingV$not.fully.paid)

# Imputing the missing values
library(mice)
set.seed(144)

# Removing target variable for the list
vars.for.imputation = setdiff(names(loans), "not.fully.paid")

# Imputation
imputed = complete(mice(loans[vars.for.imputation]))

# Copying all the data to actual data frame
loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train = subset(loans, split == TRUE)
test  = subset(loans, split == FALSE)

# Modelling
Model = glm(not.fully.paid ~ ., data=train, family = binomial)
summary(Model)

# Predicting model for test set
test$predicted.risk = predict(Model, newdata=test, type = "response")

table(test$not.fully.paid, test$predicted.risk >= 0.5)

# Baseline model
table(test$not.fully.paid)

str(test)

# Loading the library
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

# Bi-variate model
Model_int.rate = glm(not.fully.paid ~ int.rate, data=train, family = binomial)
summary(Model_int.rate)

cor(train$int.rate, train$fico)

# Test set prediction
pred_BV = predict(Model_int.rate, newdata = test, type = "response")
str(pred_BV)

# Max probability of loan not being paid in full
max(pred_BV)

table(test$not.fully.paid, pred_BV >= 0.5)

# Test set area under the curve (auc)
# prepare a prediction class object
pred_smart = prediction(pred_BV, test$not.fully.paid)
as.numeric(performance(pred_smart,"auc")@y.values)

# Profit if loan is paid in full - c*exp(rt)
# profit if loan is not paid in full - (-c)

# Profit for 1$ investment for 3 years
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
str(test)

max(test$profit)

# Subset of high interest loan
highInterest = subset(test, int.rate >= 0.15)
summary(highInterest)

# Proportion of loan not paid in full
table(highInterest$not.fully.paid)

# Sorting the list of predicted risk
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

# subsetting using "cutoff"
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
summary(selectedLoans)
sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)
