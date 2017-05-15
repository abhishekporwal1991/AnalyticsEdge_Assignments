setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit5_Text Analytics")
getwd()

#install.packages("SnowballC")
library(tm)
library(SnowballC)

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

# Adding new variable to data
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

# Pre-processing the tweets

# 1. Convert the data to "corpus - a collection of documents"
corpus = Corpus(VectorSource(tweets$Tweet))
corpus

corpus[[1]]$content

corpus = tm_map(corpus, tolower)
corpus[[1]]$content

corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]$content

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

stopwords("english")[1:10]

# Removing "apple" as well, cause the tweets are related to Apple only
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# Words frequency
frequencies = DocumentTermMatrix(corpus)
frequencies

# Looking into the documents
inspect(frequencies[1000:1005, 505:515])

# Most popular function in the document term matrix
findFreqTerms(frequencies, lowfreq = 20)

# There are many ZEROs/sparse in our matrix, Let's remove them
sparse = removeSparseTerms(frequencies, 0.995)    # only terms that are 0.5% in the tweets
sparse

# Converting the sparse matrix to data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

# Naming the columns of data frame
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Adding dependent variable to the data frame
tweetsSparse$Negative = tweets$Negative

# Splitting the data into train & test
library(caTools)
set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split == TRUE)
testSparse  = subset(tweetsSparse, split == FALSE)

# Words appeared 100 times in the document term matrix
findFreqTerms(frequencies, lowfreq = 100)

# Building CART model
library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)

predCART = predict(tweetCART, newdata = testSparse, type = "class")

# confusion matrix
table(testSparse$Negative, predCART) # accuracy = 0.8788

# baseline model accuracy
table(testSparse$Negative) # accuracy = 300/355 = 0.8450

# Building random forest model
library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data = trainSparse)

predRF = predict(tweetRF, newdata = testSparse)

# Confusion matrix
table(testSparse$Negative, predRF) # accuracy = 0.8845

# CART model is better than RF due to the interpretability of the model. If done through the cross validation approach the accuracy will be more or less equal to accuracy of RF model.

# Building logistic regression model
tweetLog = glm(Negative ~ ., data = trainSparse, family = binomial)

predLog = predict(tweetLog, newdata = testSparse, type = "response")

# Confusion matrix
table(testSparse$Negative, predLog >= 0.5) # accuracy = 0.8056

# Model perform worse than the baseline model due to the overfitting.
# A logistic regression model with a large number of variables is particularly at risk for overfitting.
predLog1 = predict(tweetLog, newdata = trainSparse, type = "response")
table(trainSparse$Negative, predLog >= 0.5) # accuracy = 0.9576