
# Loading the Dataset --------------------------------------------------------------------------------------------

setwd("C:\\Users\\Nikhil Chalakkal\\OneDrive - Emory University\\Courses\\ISOM 673\\5.0 Final Project\\Project Data")

library(dplyr)
library(igraph)
library(data.table)
library(sna)
library(network)

relations <- read.csv("edges data.csv")
relations

# Processing and checking the network ----------------------------------------------------------------------------

## Divide into Enemy or Friend Relationship 
enemy_relationship <- relations[relations$type == "-",]
enemy_relationship = unique(enemy_relationship)
friend_relationship <- relations[relations$type == "+",]
friend_relationship = unique(friend_relationship)

## Convert into Matrix
colrange = rainbow(length(unique(most_connected)), start = 0, end = 1/6)

### Friend_relationship Matrix
friend_edge = cbind(friend_relationship$source, friend_relationship$target)
friend_network = graph.data.frame(friend_edge, directed = TRUE)
friend_network = simplify(friend_network, remove.loops = TRUE)

E(friend_network)$color = 'green' 
plot.igraph(friend_network, layout = layout.fruchterman.reingold, vertex.label.color = "black", edge.arrow.size = .2)

friend_adjacency = as.matrix(as_adjacency_matrix(friend_network))
friend_adjacency[friend_adjacency == "2"] <- 1
most_connected = colSums(friend_adjacency)
friend_network = set_vertex_attr(friend_network, "most_connected", index = V(friend_network), as.factor(most_connected))
V(friend_network)$color = colrange[V(friend_network)$most_connected]
plot.igraph(friend_network,layout = layout.fruchterman.reingold, vertex.label.color = "black", edge.color = "black",
            edge.width = E(friend_network)$weight, vertex.size = 12, edge.arrow.size = .3, edge.curved = FALSE)

### Enemy_relationship Matrix
enemy_edge = cbind(enemy_relationship$source, enemy_relationship$target)
enemy_network = graph.data.frame(enemy_edge, directed = TRUE)
enemy_network = simplify(enemy_network, remove.loops = TRUE)

E(enemy_network)$color = 'red' 
plot.igraph(enemy_network, layout = layout.fruchterman.reingold, vertex.label.color = "black", edge.arrow.size = .2)

enemy_adjacency = as.matrix(as_adjacency_matrix(enemy_network))
enemy_adjacency[enemy_adjacency == "2"] <- 1
enemy_most_connected =colSums(enemy_adjacency)
enemy_network = set_vertex_attr(enemy_network, "enemy_most_connected", index = V(enemy_network), as.factor(enemy_most_connected))
V(enemy_network)$color = colrange[V(enemy_network)$enemy_most_connected]
plot.igraph(enemy_network,layout = layout.fruchterman.reingold, vertex.label.color = "black", edge.color = "black",
            edge.width = E(enemy_network)$weight, vertex.size = 12, edge.arrow.size = .3, edge.curved = FALSE)

# Friendship network ---------------------------------------------------------------------------------------------
which(duplicated(relations[,c(1,2)]) == TRUE)
which(duplicated(relations) == TRUE)
name  = read.csv('name.csv')
colnames(name) = c('id', 'name')

big_network = igraph::union(enemy_network, friend_network)
E(big_network)$color <- ifelse(is.na(E(big_network)$color_1), E(big_network)$color_2, E(big_network)$color_1)

degree_f = as.data.frame(igraph::degree(friend_network))
degree_f = as.data.frame(cbind(rownames(degree_f),degree_f))

for (i in c(1:nrow(degree_f)))
  degree_f$name[i] = as.character(name$name[name$id == as.numeric(as.character(degree_f$id[i]))])

colnames(degree_f) = c('id','degree')
topfriend = degree_f %>% top_n(5,degree)
topfriend$name = c('Albus Dumbledore', 'Hermione Granger', 'Harry Potter', 'Ron Weasley', 'Ginny Weasley')
big_network_topfriend = big_network
V(big_network_topfriend)$color = 'grey'

for ( i in c(1:nrow(topfriend)))
{
  V(big_network_topfriend)$color[V(big_network_topfriend)$name == topfriend$id[i]] = 'red'
  #V(big_network_topfriend)$name[V(big_network_topfriend)$name == topfriend$id[i]] = topfriend$name[i]
}

name_plot = rep('', length(V(big_network_topfriend)))
name_plot[as.numeric(as.character(topfriend$id[1]))+1] = 'Albus Dumbledore'
name_plot[as.numeric(as.character(topfriend$id[2]))+1] = 'Hermione Granger'
name_plot[as.numeric(as.character(topfriend$id[3]))+1] = 'Harry Potter'
name_plot[as.numeric(as.character(topfriend$id[4]))+1] = 'Ron Weasley'
name_plot[as.numeric(as.character(topfriend$id[5]))+1] = 'Ginny Weasley'
name_plot[42+1] = as.character(name$name[name$id == 42])
name_plot[5+1] = as.character(name$name[name$id == 5])
name_plot[2+1] = as.character(name$name[name$id == 2])

for (i in c(1:length(V(big_network_topfriend))))
  V(big_network_topfriend)$name[i] = name_plot[as.numeric(V(big_network_topfriend)$name[i])+1]

# 42,5,20 






tkplot(big_network_topfriend,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(big_network)$weight, edge.color = E(big_network_topfriend)$color, vertex.size = 18, edge.arrow.size=.3,edge.curved=FALSE, vertex.label=V(big_network_topfriend)$name)







```



# Friends network by group 

```{r}
aff = read.csv('affiliation.csv',header = TRUE)
aff = aff[,c(1,3,6,9)]
aff = aff[aff$ï..ID<65,]
aff[48,2] = 0
aff[58,2] = 0
aff[6,2] = 0 
aff[9,4] = 0
aff[22,4] = 0
aff[40,4] = 0
aff[55,4] = 0 
aff[59,4] = 0



aff$others = ifelse(aff$Student == 0 & aff$Order.of.the.Phoenix.II == 0 & aff$Death.Eater == 0, 1, 0)

for (i in c(1:nrow(aff)))
  aff$status[i] = sum(aff[i,c(2,3,4)])


color_Palette = c('green','sky blue','orange','grey')

for (i in c(1:length(V(big_network)$name))){
  V(big_network)$color[i] =  color_Palette[which(aff[as.numeric(as.character(V(big_network)$name[i]))+1,c(2,3,4,5)]==1)]
}



tkplot(big_network,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(big_network)$weight, edge.color = E(big_network_topfriend)$color, vertex.size = 18, edge.arrow.size=.3,edge.curved=FALSE)


big_network_enemy = igraph::delete.edges(big_network, which(E(big_network)$color == 'green'))

tkplot(big_network_enemy,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(big_network_enemy)$weight, edge.color = E(big_network_enemy)$color, vertex.size = 18, edge.arrow.size=.3,edge.curved=FALSE)


big_network_friend = igraph::delete.edges(big_network, which(E(big_network)$color == 'red'))
tkplot(big_network_friend,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.width=E(big_network_friend)$weight, edge.color = E(big_network_friend)$color, vertex.size = 18, edge.arrow.size=.3,edge.curved=FALSE)
```

# Enemy network 
```{r}

degree_e = as.data.frame(igraph::degree(enemy_network))
degree_e = as.data.frame(cbind(rownames(degree_e),degree_e))

colnames(degree_e) = c('id','degree')
topenemy = degree_e %>% top_n(5,degree)
topenemy$name = c('Albus Dumbledore', 'Hermione Granger', 'Lord Voldemort', 'Harry Potter', 'Peter Pettigrew','Ron Weasley')
enemy_network_topenemy = enemy_network
V(enemy_network_topenemy)$color = 'grey'

for ( i in c(1:nrow(topenemy)))
{
  V(enemy_network_topenemy)$color[V(enemy_network_topenemy)$name == topenemy$id[i]] = 'red'
  V(enemy_network_topenemy)$name[V(enemy_network_topenemy)$name == topenemy$id[i]] = topenemy$name[i]
}



V(enemy_network_topenemy)$name[V(enemy_network_topenemy)$color == 'grey'] = ''



tkplot(enemy_network_topenemy,layout=layout.fruchterman.reingold, vertex.label.color="#222222",edge.color = E(enemy_network_topenemy)$color, vertex.size = 18, edge.arrow.size=.3,edge.curved=FALSE, vertex.label=V(enemy_network_topenemy)$name)





```
# Top betweeness
```{r}

name = read.csv('name.csv')
colnames(name) = c('id','name')

degree_b = as.data.frame(igraph::betweenness(big_network))
degree_b = as.data.frame(cbind(rownames(degree_b),degree_b))

colnames(degree_b) = c('id','degree')
topbetweeness = arrange(degree_b,desc(degree))%>% top_n(10,degree) 


for (i in c(1:nrow(topbetweeness)))
  print(as.character(name$name[name$id == topbetweeness$id[i]]))

```

# Top closeness
```{r}
close = as.data.frame(igraph::closeness(big_network, mode = 'all'))
close = as.data.frame(cbind(rownames(close),close))

colnames(close) = c('id','degree')
topclose= arrange(close,desc(degree))%>% top_n(10,degree) 


for (i in c(1:nrow(topclose)))
  print(as.character(name$name[name$id == topclose$id[i]]))
```
# Eigen centralilty 
```{r}
eigen = as.data.frame(igraph::eigen_centrality(big_network)$vector)
eigen = as.data.frame(cbind(rownames(eigen),eigen))

colnames(eigen) = c('id','degree')
topeigen= arrange(eigen,desc(degree))%>% top_n(10,degree) 


for (i in c(1:nrow(topeigen)))
  print(as.character(name$name[name$id == topeigen$id[i]]))
```

# Check network 
```{r}
colnames(friend_edge) = c('f','t')

colnames(enemy_edge) = c('f','t')
friend_edge = as.data.frame(friend_edge)
enemy_edge = as.data.frame(enemy_edge)
intersect(intersect(intersect(union(enemy_edge$f[enemy_edge$t == 58],enemy_edge$t[enemy_edge$f == 58]),union(enemy_edge$f[enemy_edge$t == 39],enemy_edge$t[enemy_edge$f == 39])),union(enemy_edge$f[enemy_edge$t == 21],enemy_edge$t[enemy_edge$f == 21])),union(enemy_edge$f[enemy_edge$t == 11],enemy_edge$t[enemy_edge$f == 11]))


```

top betweeness: Ron Weasley, Albus Dumbledore

# Clustering based on correlation with membership matrix. 

```{r}

aff_full<- read.csv('affiliation_matrix.csv')

rownames(aff_full) = aff_full$ï..ID
aff_full$sum = apply(aff_full[,-1],1,FUN = sum)
aff_full = aff_full[aff_full$sum != 0, -10]
data = aff_full
aff_full = aff_full[,-1]

aff_color = as.data.frame(aff_full[,c(2,5,8)])
aff_color$others = ifelse(aff_color$Student == 0 & aff_color$Order.of.the.Phoenix.II == 0 & aff_color$Death.Eater == 0, 1, 0)

aff_color['47',1] = 0
aff_color['57',1] = 0
aff_color['5',1] = 0 
aff_color['8',3] = 0
aff_color['21',3] = 0
aff_color['39',3] = 0
aff_color['54',3] = 0 
aff_color['58',3] = 0


for (i in 1:nrow(aff_color))
  aff_color$status[i] = sum(aff_color[i,c(1,2,3)])

color_Palette = c('green','sky blue','orange','grey')





aff_matrix = as.matrix(aff_full) %*% t(as.matrix(aff_full))
concor = list()
concor[[1]] = aff_matrix
for (i in c(2:20))
  concor[[i]] = cor(concor[[i - 1]])

concor[[9]][concor[[9]] < 0] = 0
concor_net = graph.adjacency(as.matrix.network(network(concor[[9]])), "undirected")
```

```{r}
for (i in c(1:length(V(concor_net)$name))){
  V(concor_net)$color[i] =  color_Palette[which(aff_color[V(concor_net)$name[i],c(1,2,3,4)]==1)]
}
V(concor_net)$label[93] = 'Marcus Flint'
V(concor_net)$label[82] = 'Gwenog Jones'
V(concor_net)$label[45] = 'Fred Weasley'





concor_net = simplify(concor_net, remove.multiple = TRUE, remove.loops = TRUE)



which((concor[[9]][2,] == 0)&(aff_color$others[] == 1))


tkplot(concor_net, edge.layout=layout.fruchterman.reingold,  vertex.label.color="azure1", edge.arrow.size=.5, vertex.size = 7, vertex.label = V(concor_net)$label)


```


# Membership matrix plotting 

```{r}

library(scales)
names(data)[1] <- "Name"
colnames(data)

ID <- seq(1, nrow(data), 1)
data <- cbind(ID, data)
head(data, n = 10)
colnames(data)

names <- data[,c(1,2)]
head(names)

membership <- as.matrix(data[,3:ncol(data)]) %*% t(as.matrix(data[,3:ncol(data)]))

adj_matrix <- membership
adj_matrix[adj_matrix > 1] <- 1

graph <- network(adj_matrix, directed = FALSE)
plot(graph)
summary(graph)

edgelist <- as.matrix(graph, matrix.type = "edgelist")
edgelist <- as.data.frame(edgelist)
names(edgelist) <- c("Source", "Target")

network.vertex.names(graph) <- as.character(names[,2])
#plot.network(graph, displaylabels = T)

affiliation <- read.csv("vertex_attribute_affiliations.csv")
names(affiliation)[1] <- "Name"
colnames(affiliation)
table(affiliation$Affiliation)

set.vertex.attribute(graph, "affiliation", as.character(affiliation$Affiliation))

num_nodes <- as.numeric(graph$gal[1])
node_colors <- rep("", num_nodes)

for(i in 1:num_nodes)
{ ifelse(get.vertex.attribute(graph,"affiliation")[i] == "Death Eater", node_colors[i] <- "green",
         ifelse(get.vertex.attribute(graph,"affiliation")[i] == "Students", node_colors[i] <- "sky blue",
                ifelse(get.vertex.attribute(graph,"affiliation")[i] == "Others", node_colors[i] <- "grey",
                       node_colors[i] <- "orange")))
}

plot.network(graph, displaylabels = T, label.pos = 5, vertex.col = node_colors, vertex.size = .2, 
             edge.col = muted('blue'), interactive = FALSE, label.cex = .5)

network.vertex.names(graph)[which.max(evcent(graph))]


```


# Similarity Measures Between Different Groups
```{r}

library(Matrix)
library(gdata)
library(proxy)
library(data.table)
library(igraph)
library(readxl)
HP<- read_excel("C:/Users/cyma9/Dropbox/SocialNetwork/PJ/HP Affiliation Matrix.xlsx")
# Note: "HP" (used below) is the affilation matrix for all 12 Potterverse groups, which we imported from an Excel file (.xlsx)

# ----- COSINE SIMILARITY -----

# Compute cosine similarity values manually on matrix
cosine = function(x){
  x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) 
}

cosine_manual = cosine(t(HP))

# Setting own distance to zero to find ego-alter pairs
diag(cosine_manual) = 0

# Unmatrix from gdata will turn a matrix into a vector with the rows as ij entries
# Useful because preserves row and column names in new vector
pairs = unmatrix(as.matrix(cosine_manual))

# How many are maximally similar?
length(pairs[pairs == 1])

# ----- JACCARD SIMILARITY ------

# Compute Jaccard similarity values manually and with proxy
jaccard <- function(m) {
  A = tcrossprod(m) # numerator intersected values 
  im = which(A > 0, arr.ind=TRUE) # indices for non-zero intersected values
  b = rowSums(m) # b and c in denominator, counts for each row
  Aim = A[im] # only non-zero values of numerator
  
  ## Jacard set formula: #intersected / (#i + #j - #intersected)
  J = sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
  
  return( J )
}

jaccard_manual = jaccard(t(HP))

diag(jaccard_manual) = 0
jaccard_manual[jaccard_manual > 1,] = 1

pairs = (as.matrix(jaccard_manual))

# How many are maximally similar?
length(pairs[pairs == 1])

# ----- BUILD Final Cosine and Jaccard Similarity Matrices -----

# Final Cosine Matrix
final_cosine <- as.data.frame(cosine_manual)

# Remove "ID" column and row
final_cosine = final_cosine[-1,]
final_cosine = final_cosine[,-1]

# Final Jaccard Matrix
final_jaccard <- as.data.frame(pairs)

# Jaccard similarity matrix columns
colnames(final_jaccard)[1] <- "ID"
colnames(final_jaccard)[2] <- "Dumbledore's Army"
colnames(final_jaccard)[3] <- "Death Eater" 
colnames(final_jaccard)[4] <- "Ministry of Magic"
colnames(final_jaccard)[5] <- "Staff"
colnames(final_jaccard)[6] <- "Student"
colnames(final_jaccard)[7] <- "Quidditch Player"
colnames(final_jaccard)[8] <- "Order of the Phoenix I"
colnames(final_jaccard)[9] <- "Order of the Phoenix II"
colnames(final_jaccard)[10] <- "Dead"
colnames(final_jaccard)[11] <- "Alive"
colnames(final_jaccard)[12] <- "Inquisitorial Squad"
colnames(final_jaccard)[13] <- "Auror"

# Jaccard similarity matrix rows
rownames(final_jaccard)[1] <- "ID"
rownames(final_jaccard)[2] <- "Dumbledore's Army"
rownames(final_jaccard)[3] <- "Death Eater" 
rownames(final_jaccard)[4] <- "Ministry of Magic"
rownames(final_jaccard)[5] <- "Staff"
rownames(final_jaccard)[6] <- "Student"
rownames(final_jaccard)[7] <- "Quidditch Player"
rownames(final_jaccard)[8] <- "Order of the Phoenix I"
rownames(final_jaccard)[9] <- "Order of the Phoenix II"
rownames(final_jaccard)[10] <- "Dead"
rownames(final_jaccard)[11] <- "Alive"
rownames(final_jaccard)[12] <- "Inquisitorial Squad"
rownames(final_jaccard)[13] <- "Auror"

# Remove "ID" column and row
final_jaccard = final_jaccard[-1,]
final_jaccard = final_jaccard[,-1]


```
