setwd("D:/Study/Data Science/The Analytics Edge-edX/Assignment7_Visualization")
getwd()

edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(users)
str(edges)

unique(edges)
table(edges$V2)
table(users$school)
table(users$locale)
table(users$locale, users$school)
table(users$school, users$gender)

# installing package
# install.packages("igraph")
library(igraph)
g = graph.data.frame(edges, directed = FALSE , users)
plot(g, vertex.size = 5, vertex.label = NA)
degree(g)

table(degree(g))
table(degree(g) >= 10)

# Marking the important nodes i.e.with higher degree/higher no. of nodes
V(g)$size = degree(g)/2 + 2

# plotting again without specifying the vertex size
plot(g, vertex.label = NA)

max(degree(g))

# Coloring the graph
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label = NA)

# Coloring based on the school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"

plot(g, vertex.label = NA)

# Coloring based on the locale
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "blue"

plot(g, vertex.label = NA)

?igraph.plotting

# Plotting the graph in 3-D
rglplot(g, vertex.label = NA)
plot(g, vertex.label = NA, edge.width = 2)
