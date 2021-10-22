#this is a new method with some special operators
#Let's start with a network of state borders
library(network)
library(sna)
library(ggnetwork)

#we want to work with this as an edge list which is directed
n<-network(g, directed = TRUE, matrix.type = "edgelist")
#this is called an edge list, it shows where the edges will go
#there are other network structures as well...
as.sociomatrix(n)

#for me, edge lists are really easy to think about. 

#how many state borders are there?
e <- network.edgecount(n) 
#what is e?

#you can assign things that are relevant
#THESE ARE NOT INNERJOINS BUT ROW JOINS!!!!
set.vertex.attribute(n, "count", degree(n))
set.vertex.attribute(n, "between", betweenness(n))
set.vertex.attribute(n, "close", closeness(n))

#how to check things
network.vertex.names(n)
get.vertex.attribute(n, "count")
list.vertex.attributes(n)
list.network.attributes(n)

#statistical features of the network
important<-data.frame(get.vertex.attribute(n, "vertex.names"),  betweenness(n), closeness(n), degree(n))
View(important)

#this is really just a ggplot that throws dots places
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "orange") +
  geom_nodes(aes(color = count)) +
  theme_blank()

#and size by something also facet
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "orange") +
  geom_nodes(aes(size = count, color = count)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank()+
  theme(legend.position = "bottom")

#there are lots of shapes we can use too...
#you can assign a variety of network shapes
K<-ggnetwork(n, layout = "kamadakawai")
L<-ggnetwork(n, layout = "circrand")
M<-ggnetwork(n, layout = "geodist")
N<-ggnetwork(n, layout = "hall")
O<-ggnetwork(n, layout = "mds")
P<-ggnetwork(n, layout = "target")
Q<-ggnetwork(n, layout = "princoord")
R<-ggnetwork(n, layout = "random")
S<-ggnetwork(n, layout = "rmds")
U<-ggnetwork(n, layout = "segeo")
V<-ggnetwork(n, layout = "seham")
W<-ggnetwork(n, layout = "spring")
B<-ggnetwork(n, layout = "springrepulse")
C<-ggnetwork(n, layout = "eigen")

#lets start styling
ggplot(V, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "orange") +
  geom_nodes(aes(size = close, color = count)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank()+
  theme(legend.position = "bottom")

#this one is just supposed to be pretty
ggplot(V, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = "blue")) +
  geom_nodes(aes(colour = as.factor(count))) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  theme(legend.position = "bottom")


