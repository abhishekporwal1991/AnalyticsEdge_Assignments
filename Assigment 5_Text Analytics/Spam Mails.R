setwd("D:/study/Data Science/The Analytics Edge-edX/Assigment 5_Text Analytics")
getwd()

emails = read.csv("emails.csv", stringsAsFactors = FALSE)
str(emails)
nrow(emails)

# spam emails
table(emails$spam)

# Longest email
max(nchar(emails$text))
which.max(nchar(emails$text))

# Shortest email
min(nchar(emails$text))
which.min(nchar(emails$text))

# pre-processing the text
library(tm)

corpus = Corpus(VectorSource(emails$text))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

# Document term matrix
dtm = DocumentTermMatrix(corpus)
dtm

# sparse matrix
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# creating data frame
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))
sort(colSums(emailsSparse), decreasing = TRUE)

# Adding the dependent variable to the data set
emailsSparse$spam = emails$spam

x = subset(emailsSparse, spam == 0)
y = colSums(x)
sort(y, decreasing = TRUE)

x = subset(emailsSparse, spam == 1)
y = colSums(x)
sort(y, decreasing = TRUE)

# Building models
library(caTools)
set.seed(123)

library(rpart)
library(rpart.plot)

library(randomForest)

# converting dependent variable into factor
emailsSparse$spam = as.factor(emailsSparse$spam)

# Splitting the data
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

# logistic regression model
spamLog = glm(spam ~., data = train, family = binomial)

# CART model
spamCART = rpart(spam ~., data = train, method = "class")

# Random forest model
set.seed(123)
spamRF = randomForest(spam ~., data = train)

# predicted probabilities of model for train set
predLog  = predict(spamLog, type = "response")
predCART = predict(spamCART)[,2]
predRF   = predict(spamRF, type = "prob")[,2]

table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

summary(spamLog)
prp(spamCART)
--------------------------
# Training set accuracy of logistic regression
table(train$spam, predLog >= 0.5)   # accuracy - 0.999

# AUC of training set
library(ROCR)
ROCRpredLog = prediction(predLog, train$spam)
as.numeric(performance(ROCRpredLog, "auc")@y.values)    # AUC - 0.9999959
--------------------------
# Training set accuracy of CART model
table(train$spam, predCART >= 0.5)   # accuracy - 0.942394

# AUC of training set
ROCRpredCART = prediction(predCART, train$spam)
as.numeric(performance(ROCRpredCART, "auc")@y.values)    # AUC - 0.9696
---------------
# Training set accuracy of random forest model
table(train$spam, predRF >= 0.5)   # accuracy - 0.9793017

# AUC of training set
ROCRpredRF = prediction(predRF, train$spam)
as.numeric(performance(ROCRpredRF, "auc")@y.values)    # AUC - 0.9979 

--------------
# Testing set accuracy
predLog_test = predict(spamLog, newdata = test, type = "response")
table(test$spam, predLog_test >= 0.5)   # accuracy - 0.95052

# AUC of test set
ROCRpredLog_test = prediction(predLog_test, test$spam)
as.numeric(performance(ROCRpredLog_test, "auc")@y.values)   # AUC - 0.9627

--------------
# Testing set accuracy
predCART_test = predict(spamCART, newdata = test)[,2]
table(test$spam, predCART_test >= 0.5)    # accuracy - 0.93946

# AUC of test set
ROCRpredCART_test = prediction(predCART_test, test$spam)
as.numeric(performance(ROCRpredCART_test, "auc")@y.values)   # AUC - 0.9631

-------------------
# Testing set accuracy
predRF_test = predict(spamRF, newdata = test, type = "prob")[,2]
table(test$spam, predRF_test >= 0.5)    # accuracy - 0.975553

# AUC of test set
ROCRpredRF_test = prediction(predRF_test, test$spam)
as.numeric(performance(ROCRpredRF_test, "auc")@y.values)   # AUC - 0.9975

wordCount = rowSums(as.matrix(dtm))

# Histogram 
hist(wordCount)
hist(log(wordCount))

# Creating new variable in data frame
emailsSparse$logWordCount = log(wordCount)

# Boxplot
boxplot(emailsSparse$logWordCount~emailsSparse$spam)

# Checking the use of new variable into the model
train2 = subset(emailsSparse, split == TRUE)
test2 = subset(emailsSparse, split == FALSE)

spam2CART = rpart(spam ~ ., data = train2, method = "class")
prp(spam2CART)

# Random forest model
set.seed(123)
spam2RF = randomForest(spam ~ ., data = train2)

# predictions om test set data
predCART_test2 = predict(spam2CART, newdata = test2)[,2]
predRF_test2   = predict(spam2RF, newdata = test2, type = "prob")[,2]

----------------
# Accuracy CART model
table(test2$spam, predCART_test2 >= 0.5)    # accuracy - 0.9301

ROCRpredCART_test2 = prediction(predCART_test2, test2$spam)
as.numeric(performance(ROCRpredCART_test2, "auc")@y.values)   # AUC - 0.9582

----------------
# Accuracy random forest model
table(test2$spam, predRF_test2 >= 0.5)     # accuracy - 0.9772

ROCRpredRF_test2 = prediction(predRF_test2, test2$spam)
as.numeric(performance(ROCRpredRF_test2, "auc")@y.values)   # AUC - 0.9981
