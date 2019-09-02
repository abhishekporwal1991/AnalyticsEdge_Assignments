setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignments_Unit3_Logistic Regression")
getwd()

songs = read.csv("songs.csv")
str(songs)
summary(songs)
x = subset(songs$year, songs$year == "2010")
str(x)
table(songs$year)
table(songs$artistname, songs$artistname == "Michael Jackson")
table(songs$artistname)
summary(songs)
str(songs)
MJ = subset(songs, songs$artistname == "Michael Jackson")
str(MJ)
summary(MJ)
table(MJ$Top10)
subset(MJ$songtitle, MJ$Top10 == TRUE)
MJ$songtitle
MJ[c("songtitle", "Top10")]

table(songs$timesignature)

which.max(songs$tempo)
songs$songtitle[6206]

SongsTrain = subset(songs, year <= 2009)
str(SongsTrain)

SongsTest = subset(songs, year == 2010)
str(SongsTest)

nonvars = c("songtitle", "year", "songID", "artistname", "artistID")

SongsTrain = SongsTrain[,!(names(SongsTrain) %in% nonvars)]
SongsTest  = SongsTest[,!(names(SongsTest) %in% nonvars)]
str(SongsTrain)
str(SongsTest)

Model1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)
cor(SongsTrain$loudness,SongsTrain$energy)

Model2 = glm(Top10 ~ .-loudness, data=SongsTrain, family=binomial)
Model3 = glm(Top10 ~ .-energy, data=SongsTrain, family=binomial)

summary(Model2)
summary(Model3)

pred = predict(Model3, newdata=SongsTest, type = "response")
table(SongsTest$Top10, pred >= 0.45)

#Accuracy
(309+19)/(309+5+40+19)

#BaseLine model
table(SongsTest$Top10)

# Accuracy of Base line model
314/(314+59)

# Sensitivity of model3
19/(19+40)

# Specificity of Model3
309/(309+5)