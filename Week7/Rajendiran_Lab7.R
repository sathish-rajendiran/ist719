#
# Title: "Week 7: A Grammar of Graphics: Maps, Work-inProgress, and Ethics Discussion"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab7
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

EnsurePackage("ggplot2")
EnsurePackage("maps")
EnsurePackage("mapproj")

EnsurePackage("raster")
EnsurePackage("stringr")
EnsurePackage("plotrix")
EnsurePackage("ggmap")
EnsurePackage("tmaptools")

install.packages("devtools") # I guess you also need this
devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearth")
EnsurePackage("rnaturalearth")
EnsurePackage("rnaturalearthhires")


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
# 

##################################################

#import csv
maps.fname = "/Users/sathishrajendiran/Documents/R/IST719/maplecturedata.csv"
mapsfile <- read.csv(file=maps.fname
                      ,header = TRUE
                      ,sep = ","
                      ,stringsAsFactors = FALSE)

dfMaps <- data.frame(mapsfile)

object.size(dfMaps)

dataVolume(dfMaps)

View(dfMaps)


########################
##
# 7.1 Map Basics and Choropleths

plot(dfMaps$x,dfMaps$y)

polygon(dfMaps$x,dfMaps$y,col = "firebrick1", border =NA)

map(database ="world")
map("world", regions = "India")
map("world", regions = "China")
map("world", regions = c("India","Pakistan")
    , fill = TRUE, col = c("blue","green") )


map("world", regions = "Finland")

m <- map("state")
plot(m$x,m$y)

#US States
map("state", fill = TRUE, col = c("orange","red","yellow"))

#US Counties
map("county", region = "New York", fill = TRUE, col = terrain.colors(20))



install.packages("devtools") # I guess you also need this
devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearth")

india <- ne_countries(country = "India")
map(india)

india <- ne_states(country = "India")
map(india)

india



attributes(india)

names(india)

india$name
map(india,namefield = "name",regions = "Puducherry")
map(india,namefield = "name",regions = "Tamil Nadu")
map(india,namefield = "name",regions = "Karnataka")
map(india,namefield = "name",regions = "Gujarat")

map(india,namefield = "name"
         , regions = c("Tamil Nadu","Kerala","Karnataka","Andhra Pradesh","Puducherry")
         , fill = TRUE 
         , col = terrain.colors(7) )


india <- raster::getData("GADM", country="IND", level=1)
map(india)

india$NAME_1

map(india,namefield = "NAME_1", regions ="Gujarat")
map(india,namefield = "NAME_1", regions ="Tamil Nadu")


india <- raster::getData("GADM", country="IND", level=2)
map(india)

india$NAME_2

map(india,namefield = "NAME_2", regions ="Nagappattinam")

map(india,namefield = "NAME_2", regions ="Chennai"
    , fill = TRUE, col = "springgreen4")

map(india,namefield = "NAME_2", regions ="North 24 Parganas"
         , fill = TRUE, col = "springgreen4")


china <- raster::getData("GADM", country="CHN", level=2)
map(china)


############################################################
##
##
#   7.1 Map Basics and Choropleths

shootings.fname = "/Users/sathishrajendiran/Documents/R/IST719/shootings.rda"
load(shootings.fname)

# which state has the most mass-shooting victims in the US

# View(shootings)

shootings$Total.Number.of.Victims

sort(shootings$State)

#replace extra white space before and after State names
shootings$State <- str_trim(shootings$State,"both")
sort(shootings$State)

agg.dat <- aggregate(shootings$Total.Number.of.Victims,
                     list(shootings$State),
                     sum)
colnames(agg.dat) <- c("state","victims")

num.cols <- 10
my.color.vec <- rev(heat.colors(num.cols))

pie(rep(1,num.cols),col=my.color.vec)

my.color.vec[1]
my.color.vec[4]
## steps 1. map to color
agg.dat$index <- round(rescale(x = agg.dat$victims,c(1,num.cols)),0)
agg.dat$color <- my.color.vec[agg.dat$index]

# expand number of colors to 100 - if needed
num.cols <- 100

shootings$State1 <- gsub("[[:upper:]]|\\s+|e", "", shootings$State)

shootings$State1

gsub("[[:upper:]]|\\s+|e", "", "Connecticut")
gsub("[[:upper:]]|\\s+|e", "", "Texas")
gsub("[[:upper:]]|\\s+|e", "", "Minnesota")


agg.dat
m <- map("state")

m$names

##step 2
state.order <- match.map(database = "state", regions = agg.dat$state
                         , exact = FALSE, warn = TRUE)


cbind(m$names,agg.dat$state[state.order])


## step 3
#choropleth map

map("state", col=agg.dat$color[state.order],fill = TRUE
           ,resolution = 0, lty=1, projection = "polyconic", border="tan")


############################################################
##
##
#  7.2 Points and Geocoding

nylibs.fname = "/Users/sathishrajendiran/Documents/R/IST719/newyorklibraries.csv"
libs <- read.csv(file=nylibs.fname
                     ,header = TRUE
                     ,sep = ","
                     ,stringsAsFactors = FALSE)

map("world")

points(0,0,col="red", cex= 3, pch=8)
abline(h=43, col= "blue", lty=3) #lat
abline(v=-76, col= "blue", lty=3) $lan

us.cities
map("state")

my.cols <- rep(rgb(1,.6,.2,.7),length(us.cities$name))
my.cols[us.cities$capital >0] <- rgb(.2,.6,1,.9)


points(us.cities$long,us.cities$lat, col=my.cols
       , pch=16, cex = rescale(us.cities$pop,c(.5,7)))


install.packages("tmaptools")
library("tmaptools")

geocode_OSM("3649 Erie Blvd East, Dewitt, ny"
            , return.first.only = TRUE
            , server = "http://nominatim.openstreetmap.org")

table(libs$CITY)

index <- which(libs$CITY %in% c("SYRACUSE","DEWITT","FAYETTEVILLE"))
addy <- paste(libs$ADDRESS[index], libs$CITY[index],libs$STABR[index]
              , sep = ",")

map("county","new york", fill = TRUE, col="orange")


g.codes <- geocode_OSM(addy, return.first.only=TRUE
                       ,server = "http://nominatim.openstreetmap.org")

points(g.codes$lon, g.codes$lat, col ="blue", cex=1.1, pch = 16)


############################################################
##
##
#  7.3 More Map Stuff!

EnsurePackage("rworldmap")

countries.fname = "/Users/sathishrajendiran/Documents/R/IST719/countries.csv"
countries <- read.csv(file=countries.fname
                 ,quote = "\""
                 ,header = TRUE
                 ,sep = ";"
                 ,stringsAsFactors = FALSE)

View(countries)

range(countries$Life.expectancy)  #  0.0 89.5

# zap <- which(countries$Life.expectancy ==0.0)
rm(zap)

countries <- countries[-zap,]

range(countries$Life.expectancy)  #49.8 89.5

num.cat <- 10

iso3.codes <- tapply(countries$Country..en.
                     ,1:length(countries$Country..en.)
                     , rwmGetISO3)

df <- data.frame(country = iso3.codes
                 ,labels= countries$Country..en.
                 , life = countries$Life.expectancy)
df.map <- joinCountryData2Map(df
                              , joinCode = "ISO3"
                              , nameJoinColumn = "country")

par(mar = c(0,0,1,0))

# quantiles [2]
mapCountryData(df.map
               , nameColumnToPlot = "life"
               , numCats = num.cat
               , catMethod = 
                 c("pretty","fixedwidth","diverging","quantiles")[2]
               , colourPalette = colorRampPalette(
                 c("orangered","palegoldenrod","forestgreen"))(num.cat)
               , oceanCol = "royalblue4"
               , borderCol = "peachpuff4"
               , mapTitle = "Life Expectancy"
               )

# quantiles [3]
mapCountryData(df.map
               , nameColumnToPlot = "life"
               , numCats = num.cat
               , catMethod = 
                 c("pretty","fixedwidth","diverging","quantiles")[3]
               , colourPalette = colorRampPalette(
                 c("orangered","palegoldenrod","forestgreen"))(num.cat)
               , oceanCol = "royalblue4"
               , borderCol = "peachpuff4"
               , mapTitle = "Life Expectancy"
)
# quantiles [4]
mapCountryData(df.map
               , nameColumnToPlot = "life"
               , numCats = num.cat
               , catMethod = 
                 c("pretty","fixedwidth","diverging","quantiles")[4]
               , colourPalette = colorRampPalette(
                 c("orangered","palegoldenrod","forestgreen"))(num.cat)
               , oceanCol = "royalblue4"
               , borderCol = "peachpuff4"
               , mapTitle = "Life Expectancy"
)


###########################################
## ggmaps
##
#

reported.fname <- "/Users/sathishrajendiran/Documents/R/IST719/indiareportedrapes.csv"
india.reported <- read.csv(file=reported.fname
                      ,quote = "\""
                      ,header = TRUE
                      ,sep = ","
                      ,stringsAsFactors = FALSE)

# View(india.reported)

india <- raster::getData("GADM",country = "IND", level = 1)
cbind(unique(india.reported$Area_Name), india$NAME_1 )

india$NAME_1[india$NAME_1 == "Andaman & Nicobar"] <- "Andaman & Nicobar Islands"
india$NAME_1[india$NAME_1 == "NCT of Delhi"] <- "Delhi"
india$NAME_1 <- gsub(" and "," & ",india$NAME_1 )

map <- fortify(india, region = "NAME_1") #create a data frame

class(map)

head(map)

crimes <- aggregate(india.reported$Cases,list(india.reported$Area_Name), sum)
colnames(crimes) <- c("id","ReportedRapes")
crimes[order(crimes$ReportedRapes),]

my.map <- merge(x = map, y= crimes, by ="id")

ggplot() + geom_map(data = my.map, map = my.map) + 
  aes(x = long, y = lat, map_id = id, group = group, fill = ReportedRapes) +
  theme_minimal() + ggtitle("Reported Rapes in India")

###########################################
## ggmaps
##
#

EnsurePackage("stringr")
EnsurePackage("rgdal")
EnsurePackage("raster")
EnsurePackage("TeachingDemos")

shape.dat.dir <- "/Users/sathishrajendiran/Documents/R/IST719/"
  
bikes <- readRDS(paste0(shape.dat.dir,"bikes.rds"))
nypp <- readOGR(paste0(shape.dat.dir,"nyct2010_17a")
                , "nyct2010", stringsAsFactors = FALSE)
syr.neighborhood <- readOGR(paste0(shape.dat.dir,"syracuse-neighborhoods_ny.geojson"))


par(mar = c(.5,.5,.5,.5))


plot(nypp, border = "bisque4", lwd = .5)

zoomInPlot(c(978000,999800), ylim=c(185000,225000))  #not this one

zoomplot(c(978000,999800), ylim=c(185000,225000)) #map of Manhattan


df <- data.frame(lat = bikes$start.station.latitude
                 ,lon = bikes$start.station.longitude)
head(df)
point.tab <- sort(table(paste(df$lat,df$lon)),decreasing = TRUE)

point.tab[1:3]

df2 <- data.frame(lat = as.numeric(word(names(point.tab),1))
                  ,lon = as.numeric(word(names(point.tab),2))
                  )

df2$size <- as.numeric(point.tab)

df2[1:5,]

coordinates(df2) <-  ~lon + lat

crs(df2) <- CRS("+proj=longlat +datum=WGS84")
df2 <- spTransform(df2,crs(nypp))

tmp.size <- .2 + (2*df2$size/max(df2$size))

points(df2$lon,df2$lat,col="red", pch =19, cex=tmp.size)
