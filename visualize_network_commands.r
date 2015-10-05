edges = read.csv("edges.csv")
users = read.csv("users.csv")

#find num of users

str(users) 
#or
nrow(users)

#find average num of friends per user

str(edges) # 146 pairs of users

292/59 = 4.95 #avg num of friends

#find most common locale of users who listed a school

table(users$locale,users$school)

#possibility of school a or b being all boy/girl
#both genders attend both schools

table(users$gender,users$school)

#create a new graph object

install.packages("igraph")
library(igraph)

#user graph.data.fram function, and based on ?graph.data.frame, which creates a graph describing the social network

# a directed graph is one where the edges only go one way (one vertex to another). an undirected graph means the relationship between the vertecies are symmetric

g = graph.data.frame(edges,FALSE,users)

#directed parameter is false since its an undirected graph
# edges is first since the edges are expected first
# users is last because the vertices parameter expects data where the first column is a vertex id and the rest are properies of vertices in the graph

#plot the graph

plot(g,vertex.size=5,vertex.label=NA)

#in the graph, the degree is the number of friends
# degree 0 is no connections, degree 10 is 10 connections

table(degree(g)) 
#or
table(degree(g)>=10)

#draw attention to important nodes in a network. for example, those with more connections in this case of fb friends
#visually draw attention by changing the size attribute of the vertices of the graph in proportion to their degrees

V(g)$size=degree(g)/2+2
plot(g,vertex.label=NA)

#find the largest and smallest size assigned to a node

table(degree(g))
#or
sumary(degree(g))

#shows maximum is 18, minimum is 0. max = 18/9+2 = 11
#min is 0/2+2 =2

#change the color of verticies. obtained the verticies by V(g) then size by V(g)$size. 

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "grey"

#change color for school
plot(g,vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "grey"


plot(g,vertex.label=NA)


#change color for locale

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "grey"


plot(g,vertex.label=NA)


#other plotting options
#use rglplot to plot in 3d
rgplot(g,vertex.label=NA)
#use edge.width to change edge width
plot(g,edge.width=2,vertex.label=NA)