setwd("D:/Study/Data Science/The Analytics Edge-edX//Assignment 2")
getwd()
pisaTrain =read.csv("pisa2009Train.csv")
str(pisaTrain)
pisaTest =read.csv("pisa2009test.csv")
str(pisaTest)
table(pisaTrain$raceeth)
pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")
pisaTest$raceeth = relevel(pisaTest$raceeth,"White")
str(pisaTrain)
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
lmScore$residuals
SSE = sum(lmScore$residuals^2)
MSE = SSE/(nrow(pisaTrain))
RMSE = sqrt(MSE)
RMSE1 = sqrt(mean(lmScore$residuals^2))
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)
SSE_Pred = sum((predTest - pisaTest$readingScore)^2)
SSE_Pred
RMSE_Pred = sqrt(mean((predTest-pisaTest$readingScore)^2))
RMSE_Pred
mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST
7802354
R2 = 1 -(SSE_Pred/SST)
R2
