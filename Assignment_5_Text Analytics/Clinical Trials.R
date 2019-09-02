setwd("D:/Study/Data Science/The Analytics Edge-edX/Assigment 5_Text Analytics")
getwd()

trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
summary(trials)
str(trials)
x = nchar(trials$abstract)
which.max(x)
x[664]

max(nchar(trials$abstract))
summary(nchar(trials$abstract))

x = subset(trials, nchar(trials$abstract) == 0)
table(nchar(trials$abstract) == 0)
sum(nchar(trials$abstract) == 0)

min(nchar(trials$title))
trials$title[which.min(nchar(trials$title))]

library(tm)

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

# pre-processing the corpus
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# Document term matrices
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# removing sparsity
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# converting into data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# Most frequent word stem across the abstract
csAbstract = colSums(dtmAbstract)
which.max(csAbstract)

# Combining the two data frame
x = cbind(dtmTitle, dtmAbstract)

# There are duplicate column names 
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial = trials$trial
str(dtm)

# Splitting the data
library(caTools)
set.seed(144)

split = sample.split(dtm$trial, SplitRatio = 0.7)

train = subset(dtm, split == TRUE)
test = subset(dtm, split == FALSE)

# baseline accuracy
table(train$trial) # accuracy - 0.5606

# building CART model
library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)

# predicting probability of training set
predTrain = predict(trialCART)
predTrain = predTrain[,2]
max(predTrain)
summary(predTrain)

# Training set accuracy of CART model
table(train$trial, predTrain >= 0.5)   # Accuracy - 0.8233; sensitivity -  0.771; specificity - 0.864

# Testing set accuracy
predTest = predict(trialCART, newdata = test)

table(test$trial, predTest[,2] >= 0.5)   # accuracy - 0.758

# Area under the curve
library(ROCR)

predict = prediction(predTest[,2], test$trial)
as.numeric(performance(predict, "auc")@y.values)  # AUC - 0.8371

plot(performance(predict, "tpr", "fnr"), colorize = TRUE)
