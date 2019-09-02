setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit7_Visualization")
getwd()

WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)

# Scatterplot
plot(WHO$GNI, WHO$FertilityRate)

library(ggplot2)

# ggplot object
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))

# Adding geometric object to the plot
scatterplot + geom_point()
scatterplot + geom_line()

# Playing with plots
scatterplot + geom_point(color = "green", size = 3, shape = 11)

# adding title to the plot
scatterplot + geom_point(color = "darkred", size = 1.5, shape = 16) + ggtitle("Fertility Rate Vs Gross National Income")

# saving plot to a variable
fertilityGNIplot = scatterplot + geom_point(color = "darkred", size = 1.5, shape = 16) + ggtitle("Fertility Rate Vs Gross National Income")

# create a file to save plot
pdf("MyPlot.pdf")

# print plot to the created file
print(fertilityGNIplot)

# Close the file
dev.off()
# ------------------------------------------------------------------

# plot the points as per the variable
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# Visualing a variable is a good predictor of other using the plot
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Linear regression model
model = lm(Under15 ~ FertilityRate, data = WHO)
summary(model)

model1 = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model1)

# Adding regression line to the plot
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")
# plot is showing regression line with default 95% confidence interval

# Regression line with 99% confidence interval 
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# Regression line without confidence interval i.e. no shaded region around the line
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

# Change the color of the line
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE, color = "orange")

ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()

# More clearer points
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette = "Dark2")
