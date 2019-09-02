setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment7_Visualization")
getwd()

# Load data
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))

# pre-processing the corpus
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

# Document term matrix
dtm = DocumentTermMatrix(corpus)

# Converting the dtm to data frame
allTweets = as.data.frame(as.matrix(dtm))
class(allTweets)
str(allTweets)

# loading wordcloud package

# install.packages("wordcloud")
library(wordcloud)
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(4,0.5))
# -----------------------------

corpus = Corpus(VectorSource(tweets$Tweet))

# pre-processing the corpus
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

# Document term matrix
dtm = DocumentTermMatrix(corpus)

# Converting the dtm to data frame
allTweets = as.data.frame(as.matrix(dtm))

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2,0.25))

# Word clous with negative sentiments
negativeTweets = subset(allTweets, tweets$Avg <= -1)

wordcloud(colnames(negativeTweets), colSums(negativeTweets))

wordcloud(colnames(allTweets), colSums(allTweets), random.order = FALSE, rot.per = 0.5)

library(RColorBrewer)
brewer.pal(8, "Set2")
brewer.pal(8, "Accent")

display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), colors = brewer.pal(9, "Blues"))
wordcloud(colnames(allTweets), colSums(allTweets), colors = brewer.pal(9, "Blues")[c(5,6,7,8,9)])
# wordcloud(colnames(allTweets), colSums(allTweets), colors = brewer.pal(9, "Blues")[c(-1,-2,-3,-4)])
