setwd("D:/Study/Data Science/The Analytics Edge-edX/Hands On/Unit6_Clustering")
getwd()

# Loading the data in R
movies = read.table("movieLens.txt", header = FALSE , sep = "|", quote = "\"")
str(movies)

# Adding columns name in the data set
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War", "Western")
str(movies)

# Removing un-useful variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

str(movies)

# removing duplicate entries
movies = unique(movies)
str(movies)

table(movies$Comedy)
table(movies$Western)
table(movies$Drama, movies$Romance)

# creating the clusters
distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D")   # ward method cares about the distance b/w cluster using centroid dist & variance in each cluster.

# Plotting dendrogram
plot(clusterMovies)

# As the dendrogram & our knowledge about the prob, we can say the 10 clusters are good option.

# Label each of the data point according to the what cluster it belongs to...
clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)  # Action is binary variable so calculating its mean => calc its %age in each cluster

tapply(movies$Unknown, clusterGroups, mean)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Adventure, clusterGroups, mean)
tapply(movies$Animation, clusterGroups, mean)
tapply(movies$Childrens, clusterGroups, mean)
tapply(movies$Comedy, clusterGroups, mean)
tapply(movies$Crime, clusterGroups, mean)
tapply(movies$Documentary, clusterGroups, mean)
tapply(movies$Drama, clusterGroups, mean)
tapply(movies$Fantasy, clusterGroups, mean)
tapply(movies$FilmNoir, clusterGroups, mean)
tapply(movies$Horror, clusterGroups, mean)
tapply(movies$Musical, clusterGroups, mean)
tapply(movies$Mystery, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
tapply(movies$SciFi, clusterGroups, mean)
tapply(movies$Thriller, clusterGroups, mean)
tapply(movies$War, clusterGroups, mean)
tapply(movies$Western, clusterGroups, mean)

# Recommandations for others using these clusters
subset(movies, Title == "Men in Black (1997)")

clusterGroups[257]  # i.e. cluster 2 which is cluster for Action Adventure SciFi movies

# Movies can be recommand to others
cluster2 = subset(movies, clusterGroups == 2)

cluster2$Title[1:10]
#------------------------

clusterGroups = cutree(clusterMovies, k = 2)

#--------------------------------------
# Approaches to find out cluster centroids
colMeans(subset(movies[2:20], clusterGroups == 1))
colMeans(subset(movies[2:20], clusterGroups == 2))
colMeans(subset(movies[2:20], clusterGroups == 3))
colMeans(subset(movies[2:20], clusterGroups == 4))
colMeans(subset(movies[2:20], clusterGroups == 5))
colMeans(subset(movies[2:20], clusterGroups == 6))
colMeans(subset(movies[2:20], clusterGroups == 7))
colMeans(subset(movies[2:20], clusterGroups == 8))
colMeans(subset(movies[2:20], clusterGroups == 9))
colMeans(subset(movies[2:20], clusterGroups == 10))

# Another more simplestic approach using lapply
spl = split(movies[2:20], clusterGroups)
colMeans(spl[[1]])

lapply(spl, colMeans)
