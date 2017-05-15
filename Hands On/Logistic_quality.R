setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On")
getwd()
quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split
table(split)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)
qualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family = binomial)
summary(qualityLog)
predictTrain = predict(qualityLog, type = "response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)
str(predictTrain)
predictTrain
QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain , family =binomial)
summary(QualityLog2)
table(qualityTrain$PoorCare,predictTrain > 0.5)

install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)
ROCRpref = performance(ROCRpred,"tpr","fpr")
ROCRpred
ROCRpref
plot(ROCRpref)
plot(ROCRpref, colorize =TRUE)
plot(ROCRpref, colorize =TRUE, print.cutoffs.at = seq(0,1,0.1))
plot(ROCRpref, colorize =TRUE, print.cutoffs.at = seq(0,1,0.1),text.adj = c(-2,1.7))
?prediction
?performance
