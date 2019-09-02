library(ggmap)
library(ggplot2)

statesMap = map_data("state")
str(statesMap)

table(statesMap$group)

# Plot the map of US in the R console
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment7_Visualization")
getwd()

polling = read.csv("PollingImputed.csv")
str(polling)

train = subset(polling, Year <= 2008)
test  = subset(polling, Year > 2008)

str(train)
str(test)

# Logistic regression model
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data =train, family = binomial)

TestPrediction = predict(mod2, newdata = test, type = "response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, test$State)

table(predictionDataFrame$TestPredictionBinary)
summary(predictionDataFrame)
str(predictionDataFrame)

# converting the states name in all lower case letters before merging the 2 data sets
predictionDataFrame$region = tolower(predictionDataFrame$test.State)
str(predictionDataFrame)

# Merge the data sets
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
str(predictionMap)

# arranging the order again
predictionMap = predictionMap[order(predictionMap$group,predictionMap$order),]

# Mapping the predictions on the US map
ggplot(predictionMap, aes(x = long, y = lat, group =group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group =group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black", alpha = 0.6) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks =c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012") 
  
# Map as per predicted probabilities
ggplot(predictionMap, aes(x = long, y = lat, group =group, fill = TestPrediction)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

