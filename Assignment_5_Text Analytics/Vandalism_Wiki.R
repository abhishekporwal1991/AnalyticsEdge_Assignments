setwd("D:/Study/Data Science/The Analytics Edge-edX/Assigment 5_Text Analytics")
getwd()

wiki =read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)

wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

table(wiki$Vandal)

# build model
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
 
# removing English stopwrods
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

# stemming
corpusAdded = tm_map(corpusAdded, stemDocument)

# Document term matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# Removing sparse entries
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Converting sparse matrix to data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))

# Prepending words with letter A
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Pre-processing the column "Removed"
corpusRemoved = Corpus(VectorSource(wiki$Removed))

# data is already in lower case and punctuation is also removed already
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

# creating document term matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

# Creating sparse matrix
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# Creating data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

# Prepending the data frame with letter "R"
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# combining two data frame
wikiWords = cbind(wordsAdded, wordsRemoved)

# Adding the dependent variable "Vandal"
wikiWords$Vandal = wiki$Vandal

# Creating training and testing sets
library(caTools)
set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

wikiTrain = subset(wikiWords, split == TRUE)
wikiTest = subset(wikiWords, split == FALSE)

# Baseline accuracy
table(wikiTest$Vandal)  # accuracy - 0.5313

# building CART model
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data = wikiTrain, method = "class")

predWiki = predict(wikiCART, newdata = wikiTest, type = "class")

# accuracy
table(wikiTest$Vandal, predWiki)  # accuracy - 0.5417

# plotting the model
prp(wikiCART)

# checking for overfitting
pred = predict(wikiCART, newdata = wikiTrain, type = "class")
table(wikiTrain$Vandal, pred) # Accuracy - 5440
#-------------------------------------------------------------------------------
# Creating more useful bag of words

# create the copy of original data
wikiWords2 = wikiWords

# New column
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

# splitting the data using similar split
wikiTrain2 = subset(wikiWords2, split == TRUE)
wikiTest2  = subset(wikiWords2, split == FALSE)

# building CART model again
wikiCART2 = rpart(Vandal ~., data = wikiTrain2, method = "class")

predWiki2 = predict(wikiCART2, newdata = wikiTest2, type = "class")

# accuracy
table(wikiTest2$Vandal, predWiki2)  # accuracy - 0.5726

# Adding the rows
wikiWords2$NumWordsAdded   = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

# splitting the data using similar split
wikiTrain3 = subset(wikiWords2, split == TRUE)
wikiTest3  = subset(wikiWords2, split == FALSE)

# building CART model again
wikiCART3 = rpart(Vandal ~., data = wikiTrain3, method = "class")

predWiki3 = predict(wikiCART3, newdata = wikiTest3, type = "class")

# accuracy
table(wikiTest3$Vandal, predWiki3)  # accuracy - 0.6552
# --------------------------------------------------------------------

# Adding two original variable to the data set
wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

# splitting the data using similar split
wikiTrain4 = subset(wikiWords3, split == TRUE)
wikiTest4  = subset(wikiWords3, split == FALSE)

# building CART model again
wikiCART4 = rpart(Vandal ~., data = wikiTrain4, method = "class")

predWiki4 = predict(wikiCART4, newdata = wikiTest4, type = "class")

# accuracy
table(wikiTest4$Vandal, predWiki4)  # accuracy - 0.7188

# plotting the CART model
prp(wikiCART4)
