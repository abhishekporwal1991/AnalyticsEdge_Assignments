setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit7_Visualization")
getwd()

mvt =read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)

# Converting the date
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

# Total no. of crime in weekdays
table(mvt$weekday)

# Converting the info to the data frame to pass on to ggplot
WeekdayCount = as.data.frame(table(mvt$weekday))
str(WeekdayCount)

library(ggplot2)
ggplot(WeekdayCount, aes(x = Var1, y = Freq)) + geom_line(aes(group=1))

# The plot obtained is having name of weekdays into alphabatic order but we want them in chronological order.
# So converting weekdays into the ordered factor variable
WeekdayCount$Var1 = factor(WeekdayCount$Var1, ordered = TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
str(WeekdayCount)

# Plotting again
ggplot(WeekdayCount, aes(x = Var1, y = Freq)) + geom_line(aes(group=1))

ggplot(WeekdayCount, aes(x = Var1, y = Freq)) + geom_line(aes(group=1)) + xlab (" Day of the week") + ylab ("Total motor vehicle theft")

ggplot(WeekdayCount, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), linetype = 2) + xlab (" Day of the week") + ylab ("Total motor vehicle theft")

ggplot(WeekdayCount, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), alpha = 0.3) + xlab (" Day of the week") + ylab ("Total motor vehicle theft")
# -----------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting the graph for hour of the day
table(mvt$weekday, mvt$Hour)

# converting the table into data frame
DayHourCounts = as.data.frame(table(mvt$weekday, mvt$Hour))
str(DayHourCounts)

# Converting the hour data i.e. Var2 into numeric vector
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
str(DayHourCounts)

ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1))
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1), size = 2)

# Heat-map
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())

# Change color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low = "white", high = "black") + theme(axis.title.y = element_blank())

pdf("HeatMap.pdf")  
HeatMap = ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low = "white", high = "red") + theme(axis.title.y = element_blank())
print(HeatMap)
dev.off()
# ------------------------------------------------------------------------------------------

# Heat map on Geographical map
#install.packages("maps")
#install.packages("ggmap")
library(maps)
library(ggmap)
chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)  

# Plot 1st 100 MVT 
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

# Rounding the Longitude and Latitude into 2 digit
LatLoncounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLoncounts)                          

# Converting Long & Lat into numeric vector and add them to data frame
LatLoncounts$Long = as.numeric(as.character(LatLoncounts$Var1))
LatLoncounts$Lat  = as.numeric(as.character(LatLoncounts$Var2))
str(LatLoncounts)

ggmap(chicago) + geom_point(data = LatLoncounts, aes(x = Long, y = Lat, color = Freq, size =Freq))
ggmap(chicago) + geom_point(data = LatLoncounts, aes(x = Long, y = Lat, color = Freq, size =Freq)) + scale_color_gradient(low = "yellow", high = "red")

ggmap(chicago) + geom_tile(data = LatLoncounts, aes(x = Long, y = Lat, alpha = Freq), fill = "red")

# pdf("HeatMap_Chicago.pdf")
# HeatMap_Chicago = ggmap(chicago) + geom_tile(data = LatLoncounts, aes(x = Long, y = Lat, alpha = Freq), fill = "red")
# print(HeatMap_Chicago)
# dev.off()

# removing the area with no motor vehicle theft
LatLoncounts2 = subset(LatLoncounts, Freq > 0)
str(LatLoncounts2)
ggmap(chicago) + geom_tile(data = LatLoncounts2, aes(x = Long, y = Lat, alpha = Freq), fill = "red")
# ------------------------------------------------------------------------------------------------------------------------------

murders = read.csv("murders.csv")
str(murders)

library(maps)
library(ggmap)

# Data frame how to draw United States
statesMap = map_data("state")
str(statesMap)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# adding new variable with states name in all lower cases to merge the two data sets
murders$region = tolower(murders$State)
str(murders)

# Adding two data frames
murderMap = merge(statesMap, murders, by = "region")
str(murderMap)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Murder rate
murderMap$MurderRate = murderMap$Murders/murderMap$Population * 100000
str(murderMap)
 
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Plot is maroon looking i.e. there is a n outlier in the data - Washington DC
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + scale_fill_gradient(low = "yellow", high = "red", guide = "legend")
