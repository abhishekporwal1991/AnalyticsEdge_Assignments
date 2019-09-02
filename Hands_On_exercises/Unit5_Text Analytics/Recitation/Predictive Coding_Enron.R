setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit5_Text Analytics/Recitation")
getwd()

emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)

# Quite a long email
emails$email[1]

# Simpler format for reading
strwrap(emails$email[1])
emails$responsive[1]

strwrap(emails$email[2])
emails$responsive[2]

table(emails$responsive)

# pre-processing steps
library(tm)

corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords ("english"))
corpus = tm_map(corpus, stemDocument)

strwrap(corpus[[1]])

# bag of words
dtm = DocumentTermMatrix(corpus)
dtm

# remove the sparse terms
dtm = removeSparseTerms(dtm, 0.97)
dtm

# creating a data frame
labeledterms = as.data.frame(as.matrix(dtm))

# adding the dependent variable to the data frame
labeledterms$responsive = emails$responsive
str(labeledterms)

# Spliting the data
library(caTools)
set.seed(144)

split = sample.split(labeledterms$responsive, SplitRatio = 0.7)

train = subset(labeledterms, split == TRUE)
test = subset(labeledterms, split == FALSE)

# Building a CART model
library(rpart)
library(rpart.plot)

emailCART = rpart(responsive ~ ., data = train, method = "class")
prp(emailCART)

# predicting the model
pred = predict(emailCART, newdata = test)
pred[1:10,]
pred.prob = pred[,2]

# Accuracy
table(test$responsive, pred.prob >= 0.5) # accuracy - 0.8560

# baseline model - Always predict the doc as Non-responsive
table(test$responsive) # accuracy - 0.8365

# In this case, fpr is less harmful then the fnr i.e. predict a responsive doc as non-responsive
# plotting ROCR curve
library(ROCR)

predROCR = prediction(pred.prob, test$responsive)
prefROCR = performance(predROCR,"tpr", "fpr")
plot(prefROCR, colorize = TRUE)

# area under the curve
performance(predROCR, "auc")@y.values # AUC - 0.7936  i.e.  that our model can differentiate
# between a randomly selected responsive and non-responsive document about 80% of the time.

