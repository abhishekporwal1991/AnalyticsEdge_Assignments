setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment6_Clustering")
getwd()

# load the data
doc = read.csv("dailykos.csv", header = TRUE)
str(doc)

#docMatrix = as.matrix(doc)
#docVector = as.vector(docMatrix)

#n = 5299350
#n*(n-1)/2
distances = dist(doc, method = "euclidean")
docClusters = hclust(distances, method = "ward.D")

# Dendrogram
plot(docClusters)

# splitting the tree into 7 clusters
hierGroups = cutree(docClusters, k=7)

Hierclust1 = subset(doc, hierGroups == 1)
Hierclust2 = subset(doc, hierGroups == 2)
Hierclust3 = subset(doc, hierGroups == 3)
Hierclust4 = subset(doc, hierGroups == 4)
Hierclust5 = subset(doc, hierGroups == 5)
Hierclust6 = subset(doc, hierGroups == 6)
Hierclust7 = subset(doc, hierGroups == 7)

table(hierGroups)

# Anothor approach
HierCluster = split(doc, hierGroups)
HierCluster[[1]]

# Top 6 words
tail(sort(colMeans(Hierclust1)))     # computed the mean frequency value of each word
tail(sort(colMeans(Hierclust2))) 
tail(sort(colMeans(Hierclust3))) 
tail(sort(colMeans(Hierclust4))) 
tail(sort(colMeans(Hierclust5))) 
tail(sort(colMeans(Hierclust6))) 
tail(sort(colMeans(Hierclust7))) 
# -----------------------------------------
# k-means clustering

# Mistake done again
# docMatrix = as.matrix(doc)
# docVector = as.vector(docMatrix)

set.seed(1000)
KMC = kmeans(doc, centers = 7)
#KMCCluster = KMC$cluster

KMClust1 = subset(doc, KMC$cluster == 1 )
KMClust2 = subset(doc, KMC$cluster == 2 )
KMClust3 = subset(doc, KMC$cluster == 3 )
KMClust4 = subset(doc, KMC$cluster == 4 )
KMClust5 = subset(doc, KMC$cluster == 5 )
KMClust6 = subset(doc, KMC$cluster == 6 )
KMClust7 = subset(doc, KMC$cluster == 7 )

table(KMC$cluster)

KMC = split(doc, KMC$cluster)
KMC[[1]]

# Top 6 words 
tail(sort(colMeans(KMClust1)))
tail(sort(colMeans(KMClust2)))
tail(sort(colMeans(KMClust3)))
tail(sort(colMeans(KMClust4)))
tail(sort(colMeans(KMClust5)))
tail(sort(colMeans(KMClust6)))
tail(sort(colMeans(KMClust7)))

# Comparing the two clustering methods
table(hierGroups, KMC$cluster)
