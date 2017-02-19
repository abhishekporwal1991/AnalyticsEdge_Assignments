setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignments_Unit3_Logistic Regression")
getwd()

baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)

# team/year pair in the dataset
table(baseball$Year)
table(baseball$Team)
#tapply(baseball$Team, baseball$Year, sum(baseball$Team, na.rm=TRUE))
unique(baseball$Year)

# No. of years in the dataset
length(unique(baseball$Year))

# New dataframe with Playoff == 1
baseball = subset(baseball, baseball$Playoffs == 1)

tapply(baseball$Playoffs, baseball$Team, sum)

table(baseball$Year)
table(table(baseball$Year))

# Storing the counts of number of playoff team from each year
PlayoffTable = table(baseball$Year)

# Use of "names" function in getting names of entries in a table
names(PlayoffTable)
str(names(PlayoffTable))

PlayoffTable[c("1990", "2001")]

# Adding new predictor to the data set
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
str(baseball)

table(baseball$NumCompetitors)

# Adding WorldSeries variable to the dataset
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
str(baseball)

table(baseball$WorldSeries)

# Building bivariate models to predict Winner
M1 = glm(WorldSeries ~ Year, data = baseball, family = binomial)
summary(M1)

M2 = glm(WorldSeries ~ RS, data = baseball, family = binomial)
summary(M2)

M3 = glm(WorldSeries ~ RA, data = baseball, family = binomial)
summary(M3)

M4 = glm(WorldSeries ~ W, data = baseball, family = binomial)
summary(M4)

M5 = glm(WorldSeries ~ OBP, data = baseball, family = binomial)
summary(M5)

M6 = glm(WorldSeries ~ SLG, data = baseball, family = binomial)
summary(M6)

M7 = glm(WorldSeries ~ BA, data = baseball, family = binomial)
summary(M7)

M8 = glm(WorldSeries ~ RankSeason, data = baseball, family = binomial)
summary(M8)

M9 = glm(WorldSeries ~ OOBP, data = baseball, family = binomial)
summary(M9)

M10 = glm(WorldSeries ~ OSLG, data = baseball, family = binomial)
summary(M10)

M11 = glm(WorldSeries ~ NumCompetitors, data = baseball, family = binomial)
summary(M11)

M12 = glm(WorldSeries ~ League, data = baseball, family = binomial)
summary(M12)

# Multivariate model using signiciant variables only(Year, RA, RankSeason, NumCompetitors)
Winner = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(Winner)

# Correlation b/w the variables
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

# Two variable model
M21 = glm(WorldSeries ~ Year + RA, data = baseball, family = binomial)
summary(M21)

M22 = glm(WorldSeries ~ Year + RankSeason, data = baseball, family = binomial)
summary(M22)

M23 = glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial)
summary(M23)

M24 = glm(WorldSeries ~ RA + RankSeason, data = baseball, family = binomial)
summary(M24)

M25 = glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = binomial)
summary(M25)

M26 = glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(M26)
