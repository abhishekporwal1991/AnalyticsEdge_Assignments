setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit6_Clustering/Recitation")
getwd()

# Loading the data
flower = read.csv("flower.csv", header = FALSE)
str(flower)

# Converting the data into matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# Converting the matrix into vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

# Converting data frame directly to the vector do not resolve our purpose
flowerVector2 = as.vector(flower)
str(flowerVector2)

# Hierarchical clustering
# Calc the distances
distance = dist(flowerVector, method = "euclidean")
ClusterIntensity = hclust(distance, method = "ward.D")   # ward method -"Minimun variance method" 
                                                         # try to find out the clusters with minimum variance

# plotting cluster dendrogram
plot(ClusterIntensity)

# visualising the cluster using rectangles
rect.hclust(ClusterIntensity, k = 3, border = "red")

# cut the tree into 3 clusters
flowerClusters = cutree(ClusterIntensity, k = 3)    # this assigns the name of the cluster against each data points in the vector
flowerClusters

# average value in each cluster
tapply(flowerVector, flowerClusters, mean) # clust 1 - darkest shades nd clust 3 - brightest shade

# Output the image using 'image' function which required i/p as matrix
dim(flowerClusters) = c(50,50)

image(flowerClusters, axes = FALSE)

# Grey scale image
image(flowerMatrix, axes = FALSE, col = grey(seq(0,1, length = 256)))
# -----------------------------------------------------------------------------

# use of Hierarchical clustering is difficult if the image is in high resolution
# let's see the example

healthy = read.csv("healthy.csv", header = FALSE)
str(healthy)

# converting data frame into matrix
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# output the image
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length=256)))

# converting matrix to the vector
healthyVector = as.vector(healthyMatrix)

# Do not create distance matrix it is huge- due to which Hierarchical clustering cannot be used here.

# no. of combination = n*(n-1)/2 
n = 365636
n*(n-1)/2
# --------------------------------------------

# k- means clustering
k = 5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyCluster = KMC$cluster
KMC$centers[2]

dim(healthyCluster) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyCluster, axes = FALSE, col = rainbow(k))

# SCREE plot to select the no. of clusters in clustering method

# KMC algo with two clusters
KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)
str(KMC2)

KMC3 = kmeans(healthyVector, centers = 3, iter.max = 1000)
KMC4 = kmeans(healthyVector, centers = 4, iter.max = 1000)
KMC5 = kmeans(healthyVector, centers = 5, iter.max = 1000)
KMC6 = kmeans(healthyVector, centers = 6, iter.max = 1000)
KMC7 = kmeans(healthyVector, centers = 7, iter.max = 1000)
KMC8 = kmeans(healthyVector, centers = 8, iter.max = 1000)
KMC9 = kmeans(healthyVector, centers = 9, iter.max = 1000)
KMC10 = kmeans(healthyVector, centers = 10, iter.max = 1000)

NumClusters = seq(2,10,1)
SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC5$withinss), sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss), sum(KMC10$withinss))

 # Plot Scree plot
plot(NumClusters, SumWithinss, type = "b")

# we have to choose a elbow point after which the decrease in within cluster sum of squares is not significant.

# Another simplistic approach to calc SumWithinss
Sumwithinss2 = sapply(2:10, function(x) sum(kmeans(healthyVector, centers = x, iter.max = 1000)$withinss))

plot(NumClusters, Sumwithinss2, type = "b")
#---------------------------------------------------------

# Loading the tumor data
tumor =read.csv("tumor.csv", header = FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# here the tumorVector be used as test set for k-means clustering
#install.packages("flexclust")
library(flexclust)       # the 'flexclust' package contains the object class KCCA - k-means centroids cluster analysis 

KMC.kccc = as.kcca(KMC, healthyVector)

# Predicting tumor data
tumorClusters = predict(KMC.kccc, newdata = tumorVector)

# Converting the data into matrix
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

# Visualizing the tumor image
image(tumorClusters, axes = FALSE, col = rainbow(k))
