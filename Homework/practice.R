

#####
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
EnsurePackage("ggplot2")
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



###################################
#  strptime functions - exercise
#
tmp <- "2018.08.30-16.24.49"
strptime(tmp,"%Y.%m.%d-%H.%M.%S")

tmp <- "10AM and 27 minutes, on June 22, 1999"
strptime(tmp,"%H%p and %M minutes, on %B %d, %Y")

rm(tmp)


# Import Sales data
fname.sales <- '/Users/sathishrajendiran/Documents/R/IST719/art.csv'
art <- read.csv(file=fname.sales
                  ,header = TRUE
                  ,sep = ","
                  ,stringsAsFactors = FALSE)

dim(sales)
dim(art)

# sample(LETTERS[7:(6+C.num.letters)]
       
my.df <- data.frame(sample(LETTERS(0:25,739,rep=TRUE)))
View(my.df)

dim(my.df)


#define conversion string
dates <- c(  "2014, Aug, Fri the 16 at 18:40"
           , "2014, Jun, Sat the 24 at 11:51"
           , "2014, Jun, Sun the 25 at 7:22")
conv.string <- c("%Y, %b, %a the %d at %R")



my.df <- data.frame(dates) #create a dataframe
my.df$conv.date <- as.POSIXct(strptime(my.df$date,conv.string )) #transform date


#min and max dates
min.date <- min(my.df$conv.date)
max.date <- max(my.df$conv.date)

min.date
max.date

#remove temporary objects
rm(my.df,dates,min.date,max.date,conv.string)


##########
#sample df with 739 rows
my.df <- data.frame(replicate(2,sample(0:100,739,rep=TRUE)))  
my.df$mode<- replicate(1,sample(c("T","H","Q"),739,rep=TRUE))
#rename columns
colnames(my.df) <- c("x", "y","mode")
View(my.df)
#convert the mode column to factors
my.df$mode<-as.factor(my.df$mode)

table(my.df$mode)

cols <- c(rgb(212, 41, 6,maxColorValue=255) #
          , rgb(149,79,247,97,maxColorValue=255)
          , rgb(114, 245, 66,maxColorValue=255)
)
plot(my.df$x, my.df$y
     , col=alpha(cols, 0.5)[my.df$mode] #transparency 50%
     , pch=16
     , main="scattor plot"
     , xlab="x values"
     , ylab="y values")

legend("topright", c("H","Q","T")
       , cex=1.0, bty="n"
       , fill=cols
       )

#remove temporary objects
rm(my.df,cols)



plot(rnorm(100), col = "#FF7733", pch = 16, cex = 3)

plot(rnorm(100), col = rgb(1, .5, .2), pch = 16, cex = 3)

plot(rnorm(100), col = "blue", pch = 16, cex = 3)
plot(rnorm(100), col = "lavenderblush3", pch = 16, cex = 3)
plot(rnorm(100), col = rgb(.3, .7, 1), pch = 16, cex = 3)


plot(my.df$x, my.df$y
     , col=c(rgb(139,0,0,127.5,maxColorValue=255)
             ,rgb(160,32,240,127.5,maxColorValue=255)
             ,rgb(144,238,144,127.5,maxColorValue=255)
     )[my.df$mode]
     ,pch=16)

legend("topright", c("H","Q","T")
       , cex=1.0, bty="n"
       , fill=c(rgb(139,0,0,127.5,maxColorValue=255)
                ,rgb(160,32,240,127.5
                     ,maxColorValue=255)
                ,rgb(144,238,144,127.5
                     ,maxColorValue=255)))





#sample df with 739 rows
my.df <- data.frame(replicate(2,sample(0:100,739,rep=TRUE)))  
my.df$mode<- replicate(1,sample(c("T","H","Q"),739,rep=TRUE))
#rename columns
colnames(my.df) <- c("x", "y","mode")
View(my.df)
#convert the mode column to factors
my.df$mode<-as.factor(my.df$mode)

table(my.df$mode)

#define rgb color code for light green, dark red and purple
cols <- c(rgb(212, 41, 6,maxColorValue=255) 
          , rgb(149,79,247,97,maxColorValue=255)
          , rgb(114, 245, 66,maxColorValue=255)
)
plot(my.df$x, my.df$y
     , col=alpha(cols, 0.5)[my.df$mode] #transparency 50%
     , pch=16
     , main="scattor plot"
     , xlab="x values"
     , ylab="y values")

legend("topright", c("H","Q","T")
       , cex=1.0, bty="n"
       , fill=cols
)

#remove temporary objects
rm(my.df,cols)
