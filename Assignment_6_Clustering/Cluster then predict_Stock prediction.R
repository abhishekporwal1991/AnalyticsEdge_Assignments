setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment6_Clustering")
getwd()

# Load the data set
stocks = read.csv("stockscluster.csv", header = TRUE)
str(stocks)
summary(stocks)

# Correlation matrix
cor(stocks)

# Logistic regression
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

stocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)

predTrain = predict(stocksModel, type = "response")

# Accuracy
table(stocksTrain$PositiveDec, predTrain >= 0.5)      # accuracy - 0.571

# Testing set accuracy
predTest = predict(stocksModel, newdata = stocksTest, type = "response")

table(stocksTest$PositiveDec, predTest > 0.5)     # accuracy - 0.5671

# Baseline accuracy - predict most frequent outcome
table(stocksTest$PositiveDec)           # Accuracy - 0.5461
#------------------------------------------------------------------

# Clustering
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL    # Removing the dependent variable

limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# normalization of variable
library(caret)
preProc = preProcess(limitedTrain)

normTrain = predict(preProc, limitedTrain)
normTest  = predict(preProc, limitedTest)

# mean value of variables
summary(normTrain)
summary(normTest)

mean(stocksTrain$ReturnJan)
mean(stocksTest$ReturnJan)

# k-means Clustering
set.seed(144)
km = kmeans(normTrain, centers = 3)

kmClust1 = subset(normTrain, km$cluster == 1)
kmClust2 = subset(normTrain, km$cluster == 2)
kmClust3 = subset(normTrain, km$cluster == 3)

table(km$cluster)
km$size

# Creating training and testing Clusters
library(flexclust)
km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)
ClusterTest  = predict(km.kcca, newdata = normTest)

table(ClusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, ClusterTest == 1)
stocksTest2 = subset(stocksTest, ClusterTest == 2)
stocksTest3 = subset(stocksTest, ClusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# Logistic regression model seperated for the specific clusters
stocksModel1 = glm(PositiveDec ~., data = stocksTrain1, family = binomial)
stocksModel2 = glm(PositiveDec ~., data = stocksTrain2, family = binomial)
stocksModel3 = glm(PositiveDec ~., data = stocksTrain3, family = binomial)

summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel3)

PredictTest1 = predict(stocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(stocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(stocksModel3, newdata = stocksTest3, type = "response")

# Accuracy
table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)   # Accuracy - 0.6194
table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)   # Accuracy - 0.5504
table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)   # Accuracy - 0.6458

# Combined prediction
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes    = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions >= 0.5)   # Accuracy - 0.5788
