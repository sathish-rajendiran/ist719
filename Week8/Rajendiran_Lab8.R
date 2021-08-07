#
# Title: "Week 8: Visualizing Social Networks and Advanced Topic Presentations"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab8
#
#

#############################################
##    create re-usuable functions
##
#

# Create a function to install and load R libraries/packages

EnsurePackage <- function(x){
  x <- as.character(x)
  if (!require(x,character.only = TRUE)){
    install.packages(pkgs=x, repos = "http://cran.us.r-project.org")
    require(x, character.only = TRUE)
  }
  cat("\n Necessary libraries/pacakges are ready to go!")
}

# Create a function to to calculate data size

dataVolume <- function(x){
  nrows <- nrow(x) # number of rows
  ncols <- ncol(x) # number of columns
  data.size <- (ncols * 4) * (nrows/100)
  cat("\nTotal Number of Observations: ",nrows)
  cat("\nTotal Number of Variables: ",ncols)
  cat("\nData Size is ",data.size) #3600
}


#############################################
##    Load R Packages/Libraries
##
#

EnsurePackage("igraph")
# EnsurePackage("ggplot2")
# EnsurePackage("maps")
# EnsurePackage("mapproj")
# EnsurePackage("raster")
# EnsurePackage("stringr")
EnsurePackage("plotrix")
# EnsurePackage("ggmap")
# EnsurePackage("tmaptools")

# install.packages("devtools") # I guess you also need this
# devtools::install_github("ropensci/rnaturalearthhires")
# library("rnaturalearth")
# EnsurePackage("rnaturalearth")
# EnsurePackage("rnaturalearthhires")


# EnsurePackage("reshape2")
# EnsurePackage("RColorBrewer")
# EnsurePackage("mice") # missing data 
# EnsurePackage("visdat") # missing data
# EnsurePackage("wordcloud")
# EnsurePackage("dplyr")
# EnsurePackage("tm")

##################################################
##
## Import Data
#
# nodes-421-719network.csv; links-421-719network.csv

##################################################


#import csv
nodes.fname = "/Users/sathishrajendiran/Documents/R/IST719/nodes-421-719network.csv"
node.data <- read.csv(file=nodes.fname
                     ,header = TRUE
                     ,sep = ","
                     ,stringsAsFactors = FALSE)

links.fname = "/Users/sathishrajendiran/Documents/R/IST719/links-421-719network.csv"
link.data <- read.csv(file=links.fname
                      ,header = TRUE
                      ,sep = ","
                      ,stringsAsFactors = FALSE)


View(node.data)
View(link.data)

########################
##
##
#  Data pre-processing


colnames(link.data)
colnames(link.data) <- gsub("\\.","",colnames(link.data))


link.data$X <- gsub(" |-","",link.data$X)
cbind(link.data$X,colnames(link.data)[-1])


node.data$Name <- gsub(" |-","",node.data$Name)
cbind(node.data$Name,link.data$X)

#create a matrix
M <- as.matrix(link.data[ , -1])

rownames(M) <- colnames(M)
dim(M)

# look for NAs and replace with 0s
any(is.na(M))
M[is.na(M)] <- 0
#look for anything greater than 1
M[M >1]

g <- graph_from_adjacency_matrix(M)
g

##################################################################
##
##
#    Graph Object and First Plot

vcount(g)
ecount(g)

plot.igraph(g)

#suppress self loops
g <- simplify(g)


par(mar = c(0,0,0,0))
plot.igraph(g)


plot.igraph(g, edge.arrow.size =0,edge.narrow.width=0)

E(g)$arrow.size <- 0
E(g)$arrow.width <- 0

plot.igraph(g)

V(g)$color <- "gold"
V(g)$frame.color <- "white"
V(g)$label.color <- "black"
V(g)$size <- 5

E(g)$color <- "cadetblue"

plot.igraph(g)

?igraph.plotting()

E(g)$curved <- .4


##########################################################
## Visualizing centrality abd cetrality measurement
##
#  

par(mar = c(3,10,1,1))
barplot(sort(degree(g)), horiz = T, las = 2)

V(g)$degree <- degree(g)
par(mar = c(3,10,1,1))
barplot(sort(degree(g)), horiz = T, las = 2, main = "Centrality Graph")

V(g)$deg.out <- degree(g,mode = "out")
V(g)$deg.in <- degree(g,mode = "in")

barplot(V(g)$deg.out
        , names.arg = V(g)$name
        , horiz = T, las = 2
        , main = "Most Friendly")

barplot(V(g)$deg.in
        , names.arg = V(g)$name
        , horiz = T, las = 2
        , main = "Most Popular")


# g.bak <- g
# g <- as.undirected(g)
# 


V(g)$close <- closeness(g, normalized = T, mode = "all")
V(g)$bet <- betweenness(g, directed = FALSE)

my.pallete <- colorRampPalette(c("steelblue","violet","tomato","red","red")) 

V(g)$color <- rev(my.pallete(200))[round(1 +rescale(V(g)$close,c(1,199)),0)]


plot.igraph(g)


V(g)$size <- 2 + rescale(V(g)$degree,c(0,13))
V(g)$label.cex <- .7 +rescale(V(g)$bet,c(0,1.25))

plot.igraph(g)


###################################################
###
##  Visualizing social network structures
##
#



cbind(V(g)$name,node.data$Name)

V(g)$Class <- node.data$Class
V(g)$Country <- node.data$Country
V(g)$year <- node.data$year

g <- delete_vertices(g,"JoHunter")
plot.igraph(g)

V(g)$shape <- "circle"
V(g)$shape[V(g)$Class =="wednesday"] <- "square"
V(g)$shape[V(g)$Class =="Both"] <- "rectangle"
plot.igraph(g)

V(g)$color <- "gold"
V(g)$color[V(g)$Country =="India"] <- "springgreen4"
V(g)$color[V(g)$Country =="China"] <- "red"
V(g)$color[V(g)$Class =="Both"] <- "purple"
plot.igraph(g)


V(g)$label.color <- "blue"
V(g)$label.color[V(g)$year==1] <- "black"
plot.igraph(g)


fc <- cluster_fast_greedy(as.undirected(g))
print(modularity(fc))  


membership(fc)  
V(g)$cluster <- membership(fc)  
length(fc)
sizes(fc)  
par(mar = c(0,0,0,0))
plot_dendrogram(fc,palette = rainbow(7))  

###################################################################
##
##  Visualizing social network structures
##  use ist719networkobject.rda
  
#import csv
nwobject.fname = "/Users/sathishrajendiran/Documents/R/IST719/ist719networkobject.rda"
load(nwobject.fname)
par(mar = c(0,0,0,0))

l <- layout_in_circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)


l <- layout_with_fr(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_as_star(g, center="LeelaDeshmukh")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)


l <- layout_as_star(g, center="LeelaDeshmukh")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

E(g)$color <- "gray"
E(g)[from("LeelaDeshmukh")]$color <- "red"
l <- layout_as_star(g, center="LeelaDeshmukh")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)



l <- layout_with_kk(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)


V(g)$x <- 0
V(g)$y <- 0
plot.igraph(g)
coord <- cbind(V(g)$x,V(g)$y)

iteration <- c(500,100,20,10,5,3,2,1)
for(i in 1:length(iteration)){
  l <- layout_with_fr(g, coords = coord, dim = 2, niter = iteration[i])
  V(g)$x <- l[,1]
  V(g)$y <- l[,2]
  plot.igraph(g)
  mtext(paste("layout FR:",iteration[i]), side = 3
        , line = 0, cex = 1.5, adj = 0)
}  
  
#more layouts

l <- layout_with_gem(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_with_dh(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)

l <- layout_on_grid(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g)


##########################################################
##
##
#
g.old <- g
  
my.linked.list <- data.frame(person = V(g)$name,event = V(g)$country)
my.linked.list

g <- graph_from_data_frame(my.linked.list,directed = F)  
g  

V(g)$type <- FALSE
V(g)$type[V(g)$name %in% node.data$Name] <- TRUE  
l <- layout_as_bipartite(g,types = V(g)$type)
V(g)$x <- l[,2]
V(g)$y <- l[,1]

par(mar=c(0,0,0,0))
plot.igraph(g)


V(g)$size <- 0
plot.igraph(g)
