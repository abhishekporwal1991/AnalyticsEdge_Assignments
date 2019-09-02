setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment 2")
getwd()
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)
which.max(FluTrain$ILI
FluTrain$Week[303]
which.max(FluTrain$Queries)

hist(FluTrain$ILI,ylab = "Freq", main = "Histogram of ILI")
plot(FluTrain$Queries,FluTrain$ILI)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 =  lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
Corel = cor(FluTrain$Queries, FluTrain$ILI)
Corel
x = Corel^2
Corel = cor(FluTrain$Queries, log(FluTrain$ILI))
Corel
x = Corel^2
log(1/Corel)
exp(-0.5*Corel)

FluTest = read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata = FluTest)
summary(PredTest1)
str(PredTest1)
PredTest2 =exp(predict(FluTrend1, newdata=FluTest))
summary(PredTest2)
str(PredTest2)
?which
PredTest2
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest2[11] 

RE = (FluTest$ILI[11] -PredTest2[11])/(FluTest$ILI[11])
RE 
Error = (FluTest$ILI - PredTest2)
SE = Error^2
SE 
SSE = sum(SE)
SSE
MSE = mean(SE)
MSE
RMSE =sqrt(MSE)
RMSE

install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI),-2,na.pad =TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
str(FluTrain)
summary(FluTrain)      
table(is.na(FluTrain))

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

plot(FluTrain$Queries,FluTrain$ILILag2)
plot(FluTrain$Queries,log(FluTrain$ILILag2))

ILILag2 = lag(zoo(FluTest$ILI),-2,na.pad =TRUE)
FluTest$ILILag2 = coredata(ILILag2)
str(FluTest)
summary(FluTest) 
str(FluTrain)
nrow(FluTrain)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[1]
FluTrain$ILI[416]

FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[2]
FluTrain$ILI[417]

str(FluTest)
table(is.na(FluTest))
summary(FluTest)                                     

summary(FluTrend2)
PredTest3 = exp(predict(FluTrend2, newdata = FluTest))
error2 = (PredTest3 - FluTest$ILI)
SE2 = error2^2
MSE2 = mean(SE2)
RMSE2 = sqrt(MSE2)
RMSE2  = sqrt(mean((PredTest3-FluTest$ILI)^2))

RMSE
RMSE2

?arima
