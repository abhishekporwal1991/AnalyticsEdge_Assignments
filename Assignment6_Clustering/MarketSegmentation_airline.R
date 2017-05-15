setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment6_Clustering")
getwd()

# Load the data set
airlines = read.csv("airlinesCluster.csv", header = TRUE)
str(airlines)
summary(airlines)

# normalizing the data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
str(airlinesNorm)
summary(airlinesNorm)

# Hierarchical clustering
distances = dist(airlinesNorm, method = "euclidean")
airCluster = hclust(distances, method = "ward.D")

# Plotting dendrogram
plot(airCluster)

Grp_HierClust = cutree(airCluster, k = 5)
table(Grp_HierClust)

# Cluster centroid
tapply(airlines$Balance, Grp_HierClust, mean)
tapply(airlines$QualMiles, Grp_HierClust, mean)
tapply(airlines$BonusMiles, Grp_HierClust, mean)
tapply(airlines$BonusTrans, Grp_HierClust, mean)
tapply(airlines$FlightTrans, Grp_HierClust, mean)
tapply(airlines$FlightMiles, Grp_HierClust, mean)
tapply(airlines$DaysSinceEnroll, Grp_HierClust, mean)


# k-means clustering
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
table(KMC$cluster)
KMC$centers
tapply(airlines$Balance, KMC$cluster, mean)
tapply(airlines$QualMiles, KMC$cluster, mean)
tapply(airlines$BonusMiles, KMC$cluster, mean)
tapply(airlines$BonusTrans, KMC$cluster, mean)
tapply(airlines$FlightTrans, KMC$cluster, mean)
tapply(airlines$FlightMiles, KMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, KMC$cluster, mean)
