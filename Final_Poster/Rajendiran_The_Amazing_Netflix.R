#
# Title: xThe Amazing Netflix"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Final Poster
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

(5 * 4) * (10000/100)

#############################################
##    Load R Packages/Libraries
##
#

EnsurePackage("ggplot2")
EnsurePackage("reshape2")
EnsurePackage("RColorBrewer")
EnsurePackage("mice") # missing data 
EnsurePackage("visdat") # missing data
EnsurePackage("stringr")
EnsurePackage("tidyr")
EnsurePackage("wordcloud")
EnsurePackage("dplyr")
EnsurePackage("tm")

##################################################
##
## Import Data
#
# 

#  Data: Netflix Titles
#  Source : Kaggle
#  Desription: Various titles(shows & movies) added to Netflix 
# - There are about 7787 observations across 12 variables
#   "show_id"      "type"         "title"        "director"     "cast" 
#   "country"      "date_added"   "release_year" "rating"       "duration"     
#   "listed_in"    "description" 
# - Titles are broadly categorized into Movie and TV Show
# - There are about 14 sub categories these titles are further rated against as
#   "G"  "NC-17"  "NR"  "PG"  "PG-13"  "R"  "TV-14"  "TV-G" 
#   "TV-MA","TV-PG" "TV-Y"  " TV-Y7"  "TV-Y7-FV"   "TV-Y7-FV"
# -  Movies/Shows that were originally released from  1925 and 2021

##################################################

#import csv
netflix.fname = "/Users/sathishrajendiran/Documents/R/IST719/netflix.csv"
netflix <- read.csv(file=netflix.fname
                ,header = TRUE
                ,sep = ","
                ,stringsAsFactors = FALSE)

#column names
# colnames(netflix)
# "show_id"      "type"         "title"        "director"     "cast"         "country"      "date_added"   "release_year" "rating"       "duration"     "listed_in"    "description" 

#structure of the data set
# str(netflix)

# summary(netflix)
# class(netflix)

##################################################
#
# Data Exploration
#

# Print data size
dataVolume(netflix)

# View the data
# View(netflix)



#assign it to Dataframe
netflixDF <- data.frame(netflix)


#############################################################
##
## Genre & Country
##
#


#Split Countries into differnt rows
netflixDF <- separate_rows(netflixDF, country,  sep = ",",convert = TRUE)
# View(netflixDF)

#Split Genre into differnt rows
netflixDF <- separate_rows(netflixDF, listed_in,  sep = ",",convert = TRUE)
# View(netflixDF)

# unique(netflixDF$country)
# unique(netflixDF$listed_in)

#trim leading and trailing whitespaces
netflixDF$country <- str_trim(netflixDF$country,"both")
netflixDF$listed_in <- str_trim(netflixDF$listed_in,"both")

#replace blanks with NAs
netflixDF[netflixDF==""] <- NA

# View(netflixDF)



######################################################
#
# Missing data analysis
#
# # missing data 
# md.pattern(netflixDF, plot=FALSE)

# missing data visualization
vis_miss(netflixDF)

#find out missing columns
clnames <- colnames(netflixDF)[colSums(is.na(netflixDF)) > 0]
# clnames
# "director"   "cast"       "country"    "date_added" "rating"  

# since more than 30% director column has missing values, lets remove it for now
netflixDF <- netflixDF[,!grepl("director",colnames(netflixDF))]


# since almost 10% cast column has missing values, lets remove it for now
netflixDF <- netflixDF[,!grepl("cast",colnames(netflixDF))]


# include a column to have dummy count as 1 for each row as it signifies a title for each row
netflixDF$count <- 1

# # re - run missing data visualization
vis_miss(netflixDF)
# Now, Country has ~4.86% of NAs, date_added and rating has almost .1% and less NAs

#lets View the data
# View(netflixDF)

# lets find out number of rows with NAs
na_netflixDF <- netflixDF[rowSums(is.na(netflixDF)) > 0,]
# dim(na_netflixDF) # 522- rows
# View(na_netflixDF)

# index <- which(is.na(netflixDF))
# index

#before count 
# nrow(netflixDF) #7787
# Remove rows with NAs
netflixDF <- na.omit(netflixDF)
#after count
# nrow(netflixDF) # 7265
# 7787-7265 = 522

# missing data visualization
vis_miss(netflixDF)  # No more missing values

# Print data size
dataVolume(netflixDF)


#########################
## Histogram
##
##
#

# ggplot parameters
hcolor <- c("orange")
hfill <- c("steelblue")
htitle <- c("Histogram - Data availability")
theme <- theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())
# Plot histogram
gghist <- ggplot(data=melt(netflixDF),mapping = aes(x= value)) 
gghist <- gghist + geom_histogram(bins = 20,color=hcolor,fill=hfill,na.rm = TRUE)
gghist <- gghist + facet_wrap(~variable,scales = "free_x") + ggtitle(htitle)
gghist + labs(caption ="Data Source: Kaggle Data") + theme



######################################################
#
# Data Visualization - Single Dimension Reports
#
#

# display.brewer.all()
# display.brewer.pal(8,"Set3")
# display.brewer.pal(4,"RdYlBu")

num.colors <- 13
xyz <- colorRampPalette(c("blue","red","green"))
my.cols <- xyz(num.colors) 

par(mfrow = c(2,2))

###################################################
##  bar plot | Titles by Release Year
##
#

# table(netflixDF[netflixDF$release_year > 2010,]$release_year)

barplot(table(netflixDF[netflixDF$release_year >= 2010 & netflixDF$release_year < 2021 ,]$release_year)
        , ylab = "Number of Releases"
        , col = my.cols
        , xlab = "Year"
        , border = "white"
        , horiz = F
        , main="Titles by Release Year"
)


# table(netflixDF$rating)



###################################################
##  line chart | Titles by Rating
##
#

shows.AllReleased <- aggregate(netflixDF$count,list(netflixDF$rating),sum)
names(shows.AllReleased) <- c("Rating","Releases")
shows.Released.more <- shows.Released[shows.Released$Releases > 100 & shows.Released$Year >2015,]

#plot

plot(shows.AllReleased$Releases
     , type ="b"
     , xaxt ="n"
     , lwd = 2
     , pch = 16
     , col = c("blue")
     , lty = 3
     # , las =2
     , ylim = c(0,8000)
     , xlab = "Rating"
     , ylab = "Number of Titles"
     , main="Titles by Rating"
     , frame = FALSE
)
axis(1,at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
     ,labels=c("G", "NC-17", "NR","PG","PG-13","R"
               ,"TV-14","TV-G","TV-MA","TV-PG","TV-Y"," TV-Y7"
               ,"TV-Y7-FV","TV-Y7-FV")
     ,las = 2)

###################################################
##  Pie chart | Titles by Show Type
##
#
pie(table(netflixDF$type)
        , col = c("orange","darkblue")
        , border = "white"
        , main="Titles by Show Type"
)


# table(netflixDF$type)

# View(netflixDF)


###################################################
##  bar plot | Top 10 Titles by Rating
##
#

barplot(sort(table(netflixDF$listed_in)[1:10], decreasing = FALSE)
        , xlab = "Number of Shows"
        , col = brewer.pal(11,name = "Paired")
        # , xlab = "Year"
        , border = "white"
        , las=1
        , horiz = T
        , main="Top 10 Genre"
        , las=2
        )
# table(netflixDF$listed_in)[1:10]


#########################################################################
##  Multi dimensional Reports
##
#

# par(mfrow = c(2,2))

##############################
##  Stacked bar chart with years 2010 and above
##
##
#

shows.Type <- aggregate(netflixDF$count,list(netflixDF$type,netflixDF$release_year),sum)
names(shows.Type) <- c("Show","Year","Releases")
shows.Type.more <- shows.Type[shows.Type$Year >=2010 & shows.Type$Year < 2021,]
shows.Type.more
x_axis_labels <- min(shows.Type.more$Year):max(shows.Type.more$Year)

gg <- ggplot(shows.Type.more, aes(fill=Show, y=Releases, x=Year)) + geom_bar(position="stack", stat="identity")
gg <- gg + scale_color_brewer(palette = 4) + ggtitle("Shows available by Released Year and by Type") 
gg <- gg + labs(caption ="Data Source: Kaggle Data") + theme 
gg <- gg + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)
gg + theme_classic()

##############################
##  bar chart  with years 2010 and above
##
##
#

shows.Released <- aggregate(netflixDF$count,list(netflixDF$release_year,netflixDF$rating),sum)
names(shows.Released) <- c("Year","Rating","Releases")
shows.Released.more <- shows.Released[ shows.Released$Year >=2010 & shows.Type$Year < 2021,]

x_axis_labels <- min(shows.Released.more$Year):max(shows.Released.more$Year)

gg1 <- ggplot(shows.Released.more, aes(fill=Rating, y=Releases, x= Year)) + geom_bar(position="dodge", stat="identity")
gg1 <- gg1 + scale_color_brewer(palette = 9) + ggtitle("Shows available by Released Year and Rating") 
gg1 <- gg1 + labs(caption ="Data Source: Kaggle Data") 
gg1 <- gg1 + theme()
gg1 <- gg1 + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)

gg1 + theme_classic()

##############################################
##
## wordcloud | based on Description
##
#

# netflixDF$description[5:10]


tags <- str_extract_all(netflixDF$description,"\\S+", simplify = FALSE)
# tags

tags <- tags[lengths(tags) >0]
tags <- unlist(tags)
tags <- tolower(tags)
tags <- gsub("#|[[:punct:]]","",tags)
tags.tab <- sort(table(tags),decreasing = TRUE)
# tags.tab[1:10]

zap <- which(tags.tab <3)
tags.tab <- tags.tab[-zap]

# tags.tab[1:10]
# boxplot(as.numeric(tags.tab))
# plot(as.numeric(tags.tab))

descDF <- data.frame(words = names(tags.tab), count = as.numeric(tags.tab)
                     , stringsAsFactors = FALSE)


#stop words
stopwords.fname = "/Users/sathishrajendiran/Documents/R/IST719/stopwords.csv"
stopwords <- read.csv(file=stopwords.fname
                      ,header = FALSE
                      ,sep = ","
                      ,stringsAsFactors = FALSE)

stopwordsDF <- data.frame(stopwords)
# str(stopwordsDF)
colnames(stopwordsDF) <-c("words")

# str(df)
# str(stopwordsDF)

#remove stop words
descDF <- anti_join(descDF, stopwordsDF, by="words")

# Remove rows with NAs
descDF[descDF==""] <- NA
descDF <- na.omit(descDF)

# missing data visualization
# vis_miss(descDF)

#
# barplot(descDF[1:10,]$count, las = 2, names.arg = descDF[1:10,]$words,
#         col = "lightblue", main ="Most frequent words",
#         ylab = "Word frequencies")

par(bg="white")
myPal <- colorRampPalette(c("orange","blue","red","green","black"))

index <- which(descDF$count > 400)

my.counts <- (descDF$count[index])^(1/2)
wordcloud(descDF$words[index],my.counts,scale = c(4,.5)
          , min.freq = 100, max.words = Inf
          , random.color = FALSE
          , random.order = FALSE
          , ordered.colors = TRUE
          , rot.per = 0
          , colors = myPal(length(descDF$words[index])))
# View(descDF)


