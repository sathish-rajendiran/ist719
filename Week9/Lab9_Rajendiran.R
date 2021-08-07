##
# Title: "Week 9 · RGL (3-D Visualization), Animation, and Advanced Topic Presentations"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab9
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
EnsurePackage("rgl")
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

g <- simplify(g)

##################################################
##
## Import Data
#
# ist719networkobject.rda

##################################################


#import csv
nwobject.fname = "/Users/sathishrajendiran/Documents/R/IST719/ist719networkobject.rda"
load(nwobject.fname)

coords <- layout_with_kk(g, dim=3)
rglplot(g,layout= coords)

#add degree, closeness and betweenness
V(g)$degree <- degree(g)
V(g)$close <- closeness(g, normalized = T, mode = "all")
V(g)$bet <- betweenness(g, directed = FALSE)

my.pallete <- colorRampPalette(c("steelblue","violet","tomato","red","red")) 
V(g)$color <- rev(my.pallete(200))[round(1 +rescale(V(g)$close,c(1,199)),0)]



l <- layout_with_kk(g)
V(g)$x <- l[,1]   
V(g)$y <- l[,2] 
V(g)$z <- V(g)$bet

# rglplot(g)


V(g)$label <- ""

V(g)$x <- coords[,1]   
V(g)$y <- coords[,2] 
V(g)$z <- coords[,3]
rglplot(g)

E(g)$color <- "cadetblue"
rglplot(g)
par3d(windowRect = c(100,100,640,640))
rgl.bringtotop()
rgl.bg(coor = "black")
rgl.viewpoint(0,20)


V(g)$color <- "gold"
V(g)$frame.color <- "white"
V(g)$label.color <- "black"
V(g)$size <- 5

# E(g)$color <- "yellow"
E(g)$width <- 2.5
V(g)$label <- ""

rglplot(g)
par3d(windowRect = c(100,100,640,640))
rgl.bringtotop()
rgl.viewpoint(0,20)


V(g)$size <- 2 + rescale(V(g)$degree,c(0,13))
V(g)$label.cex <- .7 +rescale(V(g)$bet,c(0,1.25))


################################
##
#  Animation

#import csv
out.dir <- "/Users/sathishrajendiran/Documents/R/IST719/Out/week9/"

EnsurePackage("stringr")
EnsurePackage("animation")
EnsurePackage("rgl")
EnsurePackage("igraph")
EnsurePackage("plotrix")

rglplot(g)

my.rgl.out <- paste0(out.dir,"network3dvisualization.png")
rgl.snapshot(filename=my.rgl.out) #output to folder



rglplot(g)
par3d(windowRect=c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,0,zoom = .7)

max.loops <- 40
my.angle <- rescale(1:max.loops,c(-90,90))

for(i in 1:max.loops) {
  rgl.viewpoint(theta = my.angle[i], phi = my.angle[i], zoom = .75-i/(max.loops+1.7))
  #theta is spinning around y
  #phi is spinning
}

##ffmpeg
images.out <- paste0(out.dir,"out\\")
rglplot(g)
par3d(windowRect=c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,0,zoom = .7)

max.loops <- 40
my.angle <- rescale(1:max.loops,c(-180,180))

for(i in 1:max.loops) {
  # i <- 1+1
  rgl.viewpoint(theta = my.angle[i], phi = 0, zoom = .75-i/(max.loops+1.7))
  #theta is spinning around y
  #phi is spinning
  snapshot.filename <- paste0(out.dir,"network",str_pad(i,width = 4,side = "left", pad = 0),".png")
  rgl.snapshot(filename=snapshot.filename) #output to folder
}

##############

EnsurePackage("magick")
ani.options("magick") #interval = .1
imgs <- list.files(out.dir, pattern = "*.png")
saveGIF(
  {
  for(img in imgs) {
    im <- magick::image_read(paste0(out.dir,img))
    print(plot(im))
    # print(im)
  }
},movie.name = paste0(out.dir,"classnetwork.gif"))



##########
##
#  Save as Video

ani.options(ffmpeg="/Users/sathishrajendiran/Documents/R/ffmpeg") 
# imgs <- list.files(out.dir, pattern = "*.png")
saveVideo(
  {
    for(img in imgs) {
      im <- magick::image_read(paste0(out.dir,img))
      plot(im)
      # print(im)
    }
  },video.name = paste0(out.dir,"classnetwork.mp4"), other.opts = "-vf format=yuv420p")

################
## RGL in Depth
##

EnsurePackage("rgl")
EnsurePackage("scatterplot3d")

n <- 1000
x <- rnorm(n)
y <- (2*x)^2/10 + rnorm(n, mean = 0, sd=.2)
z <- sqrt(abs(x)) + rnorm(n, mean = 0, sd=.2)

plot3d(x,y,z,pch = 16,type = "h")
plot3d(x,y,z, col= "red", size = 3)
plot3d(x,y,z, col= "gold", size = 1, type = "s")

rgl.bg(color="black")
rgl.viewpoint(0,0,zoom = .7)
plot3d(x,y,z, col= "gold", size = 1, type = "s")


rgl.bg(color="black")
rgl.viewpoint(0,0,zoom = .7)
rgl.clear(type="shapes")
plot3d(x,y,z, col= "gold", size = 1, type = "s")

spheres3d(4*x,4*y,4*z, radius = .1,col="gold")
rgl.light(theta = 0
          ,phi = 0
          ,viewpoint.rel = TRUE
          ,ambient = "#FFFFFF"
          ,diffuse = "#FFFFFF"
          ,specular = "#FFFFFF")

rgl.clear(type="lights")

light3d(diffuse ="gray"
        ,specular = "gray75"
        ,viewpoint.rel=FALSE)

rgl.light(ambient = "#444444"
          ,diffuse = "#0000FF"
          ,specular = "#FF0000")

###########################
##
##
#

rgl.clear()

EnsurePackage("rgl")
open3d()
par3d()$windowRect

lines3d(x=c(-2,2),y=c(0,0), z=c(0,0),col="red", lwd=1)

text3d(2,0,0,"x"
       ,color="red", cex=1,adj=0)

lines3d(x=c(0,0),y=c(-2,2), z=c(0,0),col="green", lwd=1)

text3d(0,2,0,"y"
       ,color="green", cex=1,adj=0)

lines3d(x=c(0,0),y=c(0,0), z=c(-2,2),col="blue", lwd=1)
text3d(0,0,2,"z"
       ,color="blue", cex=1,adj=0)


wire3d(cube3d())

wire3d(scale3d(cube3d(),2,2,2),col="red")

rgl.ids()

rgl.pop(id=6728)


rgl.clear(type="shapes")

shapelist3d(tetrahedron3d(),2,0,0
            , size =.5
            , color="red")

material3d(alpha=.2,shininess=75
           , emision="blue")

shapelist3d(cube3d(),0,2,0
            , size =.5
            , color="red")

material3d(alpha=1,shininess=5
           , emision="cadetblue")

shapelist3d(octahedron3d(),0,0,2.5
            , size =.9
            , color="red")

material3d(alpha=.5,shininess=0
           , emision="black")

shapelist3d(icosahedron3d(),2,0,2.5
            , size =2
            , color="orange")


##########################################
##
## 
#
#

EnsurePackage("rgl")
web.dir <- "/Users/sathishrajendiran/Documents/R/IST719/week9/web"

open3d()
material3d(alpha=1, shininess= 50, emission="black")
rgl.bg(color = "black")

rgl.clear()
material3d(alpha=1, shininess= 50, emission="black")
rgl.bg(color = "black")

rgl.clear(type = "shapes")

n <- 10
x <- 0
y <- 0
z.start <- 0
z.end <- 10

M <- matrix(c(rep(x,n)
              , rep(y,n)
              , seq(from = z.start , to = z.end, length.out = n))
            , nrow = n, byrow = FALSE)

M2 <- cylinder3d(center = M, radius = .2, sides = 50, closed = -2)
shade3d(M2,alpha=1, color="gray45")

z.end <- .25

M <- matrix(c(rep(x,n)
              , rep(y,n)
              , seq(from = z.start , to = z.end, length.out = n))
            , nrow = n, byrow = FALSE)

M2 <- cylinder3d(center = M, radius = 1.5, sides = 50, closed = -2)
shade3d(M2,alpha=1, color="gray45")


material3d(alpha=1.5, shininess = 50, emission="tan")
spheres3d(x,y,10.5,radius = .25, col= "yellow")

material3d(alpha=.5, shininess = 50, emission="black")

shapelist3d(cuboctahedron3d(), x,y,10.5, size=.575, color="gold")

ns <- 100
xs <- rnorm(ns,0,3)
ys <- rnorm(ns,0,3)
zs <- rnorm(ns,9,2)
rs <- rnorm(ns,1,3)

material3d(alpha= 1, shininess=100, emission="black")
particles3d(xs,ys,zs,radius = rs, color = "white")

writeWebGL(dir = web.dir, filename = lab9_index.html)
filename <- writeWebGL(dir = web.dir, width = 500, reuse = TRUE)

#output file for lab9 submission



##################
##
## other topics
##
#

writeOBJ()
readOBJ()

readSTL()
writeSTL()

tmp <- installed.packages()
ip <- as.data.frame(tmp,stringsAsFactors = FALSE)

View(ip)
table(ip$LibPath)

.libPaths()

ip$Package[grep(pattern = "^gg",ip$Package)]

tmp <- sessionInfo()

library(igraph)
detach(igraph)  

memory.size()

gc()
