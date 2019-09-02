setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit4_Trees/Recitation")
getwd()

boston = read.csv("boston.csv")
str(boston)

# plot the data
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = "blue", pch = 19)
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = "red", pch = 19)

# distribution of air pollution
summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.5547], boston$LAT[boston$NOX >= 0.5547], col = "green", pch = 19)

plot(boston$LON,boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.20], col = "red", pch = 19)

# Building linear regression model
plot(boston$LON,boston$MEDV)
plot(boston$LAT,boston$MEDV)

latlonlm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.20], col = "red", pch = 19)

latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.20], col = "blue", pch = "$")


# Building a regressio tree
library(rpart)
library(rpart.plot)

latlonTree = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlonTree)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.20], col = "red", pch = 19)
fittedValue = predict(latlonTree)

points(boston$LON[fittedValue >= 21.2], boston$LAT[fittedValue >= 21.20], col = "blue", pch = "$")

# Simpler regression tree
latlonTree = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlonTree)
text(latlonTree)

plot(boston$LON,boston$LAT)
abline(v = -71.07)
abline(h = 42.21)
abline(h = 42.17)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.20], col = "red", pch = 19)

# Building models using all the other variables
library(caTools)
set.seed(123)

split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test  = subset(boston, split == FALSE)

linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)

lin.pred = predict(linreg, newdata = test)
lin.sse = sum((lin.pred - test$MEDV)^2)
lin.sse

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)

tree.pred = predict(tree, newdata = test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse

library(caret)
library(e1071)

tr.control = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = (0:10)*0.001)

tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cpGrid)
tr

best.tree = tr$finalModel
prp(best.tree)

best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse =sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
