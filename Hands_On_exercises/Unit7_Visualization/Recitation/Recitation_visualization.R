setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit7_Visualization/Recitation")
getwd()

# Load data
intl = read.csv("intl.csv")
str(intl)

# Visualizing the data as a bar chart
library(ggplot2)
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") +
  geom_text(aes(label = PercentOfIntl))

# Re-ordering the region value
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))

# Changing the scale of the no. on Y axis
intl$PercentOfIntl = intl$PercentOfIntl * 100
str(intl)

# Improving the aesthetic of the plot
ggplot(intl, aes(x=Region, y = PercentOfIntl)) + geom_bar(stat = "identity", fill = "dark blue") +
  geom_text(aes(label = PercentOfIntl), vjust = -0.4) + ylab("Percent of International Students") +
  theme(axis.title.x  =element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
# ----------------------------------------------------------------------------------------------

# Plotting the international students data on the world map
library(ggmap)
world_map = map_data("world")
str(world_map)

intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
str(intlall)
head(intlall)

# NA is equivalant to the no. of students so it is equal to ZERO.
intlall[is.na(intlall)] = 0

# Merging the two data sets to plot
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)  # Observations are reduced as the countries with no students at MIT has been dropped

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

# Due to the merger the order of long & lat have been disturbed, have to rearrange them to get correct world map
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

table(intlall$Citizenship)
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"
table(intlall$Citizenship)

# merge again
world_map = merge(map_data("world"),intlall, by.x = "region", by.y = "Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), color = "black") + coord_map("mercator")

# Orthogonal projection
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), color = "black") + coord_map("ortho", orientation = c(37,55,0))
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# Load household data set
households = read.csv("households.csv")
str(households)

library(ggplot2)

# The plot is to show change in the status with the year, so we want the other variables as columns not rows
# This can be done by the 'melt' function of reshape2 package.

library(reshape2)
households[,1:2]
melt(households, id.vars = "Year")[,1:2]

ggplot(melt(households, id.vars = "Year"), aes(x = Year, y = value, color = variable)) + geom_line(size=2) +
  geom_point(size = 5) + ylab("Percentage of Household")
