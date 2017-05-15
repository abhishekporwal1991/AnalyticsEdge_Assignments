setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment 4_Trees")
getwd()

gerber = read.csv("gerber.csv")
str(gerber)

# proportion of people voted in the election
table(gerber$voting)          #  - 0.316

# Subsetting the data
CvD = subset(gerber, civicduty == 1)
HwT = subset(gerber, hawthorne == 1)
NeB = subset(gerber, neighbors == 1)
Self = subset(gerber, self == 1)

summary(CvD)
summary(HwT)
summary(NeB)
summary(Self)

# another simpler way to find out the largrest voting % in the treatment group
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)

# Logistic regression model
logreg = glm(voting ~ civicduty + hawthorne + neighbors + self, data = gerber, family = binomial)
summary(logreg)

pred = predict(logreg, type = "response")

table(gerber$voting, pred >= 0.3)
table(gerber$voting, pred >= 0.5)

# Area under the curve of model
library(ROCR)
RORCpred = prediction(pred, gerber$voting)
as.numeric(performance(RORCpred, "auc")@y.values)

# CART regression model
library(rpart)
library(rpart.plot)

CARTModel = rpart(voting ~ civicduty + hawthorne + neighbors + self, data = gerber)
summary(CARTModel)
prp(CARTModel)

CARTModel2 = rpart(voting ~ civicduty + hawthorne + neighbors + self, data = gerber, cp = 0.0)
prp(CARTModel2)

CARTModel3 = rpart(voting ~ civicduty + hawthorne + neighbors + self + sex, data = gerber, cp = 0.0)
prp(CARTModel3)

# Male voter are more likely to vote in Civicduty & control group. 

CARTcontrol = rpart(voting ~ control, data = gerber, cp = 0.0)
CARTsex = rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(CARTcontrol, digits = 6)
prp(CARTsex, digits = 6)

Regcontrol = glm(voting ~ control + sex, data = gerber, family = binomial)
summary(Regcontrol)

Possibilities = data.frame(sex = c(0,0,1,1), control = c(0,1,0,1))
predict(Regcontrol, newdata = Possibilities, type = "response")

# Combine two variables in regression model
LogModel2 = glm(voting ~ sex + control + sex:control, data = gerber, family = binomial)
summary(LogModel2)
predict(LogModel2, newdata = Possibilities, type = "response")
