setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment7_Visualization")
getwd()

# load the data
parole = read.csv("parole.csv")
str(parole)

# converting the variables into factor
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

str(parole)

# Female violator
table(parole$male, parole$violator)

# Common crime in Kentucky
table(parole$crime, parole$state)  

# plotting histogram using ggplot2
library(ggplot2)

ggplot(data = parole, aes(x = age)) + geom_histogram()
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color = "blue")

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color = "blue") + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color = "blue") + facet_grid(.~ male)

# Coloring different groups differntly
ggplot(data = parole, aes(x=age, fill = male)) + geom_histogram(binwidth = 5, color = "blue")

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values = colorPalette)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) + scale_fill_manual(values = colorPalette)

# basic histogram with time served
ggplot(parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1)

ggplot(parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime~ .)

ggplot(parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) #+ scale_fill_manual(values = colorPalette)
