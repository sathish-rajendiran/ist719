#
# Title: "Week 4: Homework"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Week 4 Homework
#

#########################################################################
#
# Part 1: Modifying Plots with Illustrator
# Visualizing this: Chapter 4
#
#########################################################################


#########################################################################
#
#  Reproduce Bar chart: Figure 4-5
#
#

# getting started with data

#import csv
# fname <- file.choose()
"/Users/sathishrajendiran/Documents/R/IST719/hot-dog-contest-winners.csv"
fname.hotdogs <- 'http://datasets.flowingdata.com/hot-dog-contest-winners.csv'
hotdogs <- read.csv(file=fname.hotdogs
                    ,header = TRUE
                    ,sep = ","
                    ,stringsAsFactors = FALSE)

colnames(hotdogs)
# [1] "Year"       "Winner"     "Dogs.eaten" "Country"    "New.record"
head(hotdogs)
View(hotdogs)

str(hotdogs)
dim(hotdogs) #31 rows and 5 columns

#reproduce Bar chart: Figure 4-11

barplot(hotdogs$Dogs.eaten
        , col= c("lightblue")
        , main = "Sathish's Hot Dog Eating Contest results, 1980-2010"
        , ylab = "Hot dogs and buns (HDB) eaten"
        , xlab = "Year"
        , ylim = c(0,70)
        , names.arg = hotdogs$Year
        , border = NA
        , horiz = F
        , las = 1
)
# defin colors function to conditionally format to indicate New records
fill_colors <- c()
for (i in 1:length(hotdogs$New.record)) {
  if(hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors,"#29B754")
  } else {
    fill_colors <- c(fill_colors,"#cccccc")
  }
}

barplot(hotdogs$Dogs.eaten
        , col= fill_colors
        # , main = "Sathish's Hot Dog Eating Contest results, 1980-2010"
        , ylab = "Hot dogs and buns (HDB) eaten"
        , xlab = "Year"
        , ylim = c(0,70)
        , names.arg = hotdogs$Year
        , border = NA
        , horiz = F
        , las = 1
)

#########################################################################
#
#  Reproduce Stacked bar chart: Figure 4-21
#
#


#import csv
fname.places <- 'http://datasets.flowingdata.com/hot-dog-places.csv'
hot_dog_places <- read.csv(file=fname.places
                           ,header = TRUE
                           ,sep = ","
                           ,stringsAsFactors = FALSE)
#view data
colnames(hot_dog_places)
# [1] "X2000" "X2001" "X2002" "X2003" "X2004" "X2005" "X2006" "X2007" "X2008" "X2009" "X2010"
head(hot_dog_places)
View(hot_dog_places)

#rename columns
names(hot_dog_places) <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")

hot_dog_matrix <- as.matrix(hot_dog_places) #convert to matrix

barplot(hot_dog_matrix
        # , space = 0.25
        , ylim = c(0,160)
        , col = c('#286622','#28B622','#7DF731')
        , border="white"
        , horiz = F
        , las = 1
        , ylab = ""
        , xlab = ""
        , main = ""
)

#########################################################################
#
#  Reproduce Scatterplot: Figure 4-25
#
#

#import csv
fname.subc <- 'http://datasets.flowingdata.com/flowingdata_subscribers.csv'
subscribers <- read.csv(file=fname.subc
                        ,header = TRUE
                        ,sep = ","
                        ,stringsAsFactors = FALSE)
#view data
colnames(subscribers)
# [1] "Date"        "Subscribers" "Reach"       "Item.Views"  "Hits" 
head(subscribers)
View(subscribers)
dim(subscribers)
str(subscribers)
colnames(subscribers)
# plot - scatter plot
plot(subscribers$Subscribers
     , type = "p"
     , ylim = c(0,30000)
     , lwd = 1 
     , pch = 19
     , col = "#8A332C"
     , cex =1
     , lend = 2
     , bty = "n"
     , xlab = "Day" # x axis label
     , ylab = "subscribers"  # y axis label
)

#########################################################################
#
#  Reproduce Time series: Figure 4-40
#
#
# 

#import csv
fname.time <- 'http://datasets.flowingdata.com/world-population.csv'
population <- read.csv(file=fname.time
                       ,header = TRUE
                       ,sep = ","
                       ,stringsAsFactors = FALSE)
#view data
colnames(population)
# [1] "Year"       "Population"
head(population)
View(population)

# options(scipen=999)
# plot - scatter plot
plot(population$Year,population$Population
     , type = "l"
     , lwd = 4
     , ylim = c(0,8000000000)
     , col = "#8A332C"
     , xlab = "" # x axis label
     , ylab = ""  # y axis label
)

#########################################################################
#
#  Reproduce Step chart: Figure 4-43
#
#
# 


#import csv
fname.step <- 'http://datasets.flowingdata.com/us-postage.csv'
postage <- read.csv(file=fname.step
                    ,header = TRUE
                    ,sep = ","
                    ,stringsAsFactors = FALSE)
#view data
colnames(postage)
# [1] "Year"  "Price"
head(postage)
View(postage)

postage <- subset(postage,postage$Year <"2010") #limiting to 2009

plot(postage$Year,postage$Price
     , type = "s"
     , lwd = 1
     , xlab = "" # x axis label
     , ylab = ""  # y axis label
     , main = ""
     , axes = ""
)


#########################################################################
#
#  Reproduce LOESS Curve: Figure 4-47
#
#
# 


#import csv
fname.curve <- 'http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv'
unemployment <- read.csv(file=fname.curve
                    ,header = TRUE
                    ,sep = ","
                    ,stringsAsFactors = FALSE)
#view data
colnames(unemployment)
# [1] "Series.id" "Year"      "Period"    "Value" 
head(unemployment)
View(unemployment)


#Plain Scatter Plot

scatter.smooth(  x=1:length(unemployment$Value)
               , y=unemployment$Value
               , ylim = c(0,11)
               , degree = 2
               , col ="#FFE4F9"
               , pch =16
               , span = .5
               , lpars = list(col = "#BD407D", lwd = 5, lty = 1)
               )

#########################################################################
#
#  Reproduce Scatterplot Matrix: Figure 6-4
#
#
# 

#import csv
fname.crime <- 'http://datasets.flowingdata.com/crimeRatesByState2005.csv'
crime <- read.csv(file=fname.crime
                         ,header = TRUE
                         ,sep = ","
                         ,stringsAsFactors = FALSE)
#view data
colnames(crime)
# [1] "state"               "murder"              "forcible_rape"      
# [4] "robbery"             "aggravated_assault"  "burglary"           
# [7] "larceny_theft"       "motor_vehicle_theft" "population"  
head(crime)
View(crime)

crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime$state != "United States",]

plot(crime2$murder,crime2$burglary
     , type = "p"
     , lwd = 1
     , xlim = c(0,10)
     , ylim = c(0,1200)
)

plot(crime2[,2:9])


pairs(crime2[,2:9]
      , panel = panel.smooth
      , col = "#B5BDE0"
      , span = .5
      , pch= 16
      )

?panel.smooth



#########################################################################
#
#  Reproduce Bubble Chart : Figure 6-15
#
#
# 

#import csv
fname.tcrime <- 'http://datasets.flowingdata.com/crimeRatesByState2005.tsv'
crime <- read.csv(file=fname.tcrime
                  ,header = TRUE
                  ,sep = "\t"
                  ,stringsAsFactors = FALSE)
#view data
colnames(crime)
# [1] "state"               "murder"              "forcible_rape"      
# [4] "robbery"             "aggravated_assault"  "burglary"           
# [7] "larceny_theft"       "motor_vehicle_theft" "population"  
head(crime)
View(crime)

radius <- sqrt(crime$population/pi)
symbols( crime$murder
        , crime$burglary
        , xlim = c(0,10)
        , bty ="n"
        , fg ="white"
        , bg = "red"
        , circles = radius
        , inches = 0.5
        )
text(crime$murder,crime$burglary,crime$state,cex = 0.5)


# ?symbols

#########################################################################
#
#  Reproduce Histogram : Figure 6-24
#
#
# 

#import csv
fname.birth <- 'http://datasets.flowingdata.com/birth-rate.csv'
birth <- read.csv(file=fname.birth
                  ,header = TRUE
                  ,sep = ","
                  ,stringsAsFactors = FALSE)
#view data
colnames(birth)
head(birth)
View(birth)

stem(birth$X2008)

hist(birth$X2008,breaks = 5)

hist(birth$X2008
     , breaks = 20
     , xlim = c(0,60)
     , ylim = c(0,60)
     , xlab = NULL
     , ylab = NULL
     , col = "#9842B3"
     , border="white"
     , main = NULL
     )

#########################################################################
#
#  Reproduce Density Plot : Figure 6-32
#
#
# 

birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)

density.default(x = birth2008)
# d2008$x
# d2008$y

d2008frame <- data.frame(d2008$x,d2008$y)

plot(d2008, type = "n"        
     , bty = "n"
     , xlab = ""
     , ylab = ""
     , main = ""
     # , span = 10
     )
polygon(d2008
        , col = "#821122"
        , border = "#cccccc"

)


#########################################################################
#
#  Reproduce MultiPlots : Art sales
#
#
# 

fname.art = "/Users/sathishrajendiran/Documents/R/IST719/art.csv"
art <- read.csv(file=fname.art
                ,header = TRUE
                ,sep = ","
                ,stringsAsFactors = FALSE)
#view data
colnames(art)
# [1] "date"       "year"       "rep"        "store"      "paper"      "paper.type" "unit.price" "units.sold"
# [9] "total.sale"

head(art)
str(art)
dim(art) #[1] 10000     9

# View(art)

# unique(art$year)
# unique(art$paper.type)
# max(art$total.sale)

library("lattice")
breaks <- seq(0,100,by=20)

par(mfrow = c(2,3))
hist(art[art$paper.type == "pad",]$total.sale
       , breaks = breaks , col = "#9842B3", border = "#9842B3"
       , xlab = "" , ylab = "", main = "",ylim = c(0,1500))

hist(art[art$paper.type == "roll",]$total.sale
       , breaks = breaks , col = "#9842B3", border = "#9842B3"
       , xlab = "" , ylab = "", main = "",ylim = c(0,1500))

hist(art[art$paper.type == "pads",]$total.sale
       , breaks = breaks , col = "#9842B3", border = "#9842B3"
       , xlab = "" , ylab = "", main = "",ylim = c(0,1500))

hist(art[art$paper.type == "journal",]$total.sale
       , breaks = breaks , col = "#9842B3", border = "#9842B3"
       , xlab = "" , ylab = "", main = "",ylim = c(0,1500))

hist(art[art$paper.type == "sheet",]$total.sale
       , breaks = breaks , col = "#9842B3", border = "#9842B3"
       , xlab = "" , ylab = "", main = "",ylim = c(0,1500))

hist(art[art$paper.type == "block",]$total.sale
       , breaks = breaks , col = "#9842B3", border = "#9842B3"
       , xlab = "" , ylab = "", main = "",ylim = c(0,1500))


#########################################################################
#
#  Reproduce MultiPlots : Birth rates
#
#
# 

#import csv
fname.rate <- 'http://datasets.flowingdata.com/birth-rate-yearly.csv'
birth.rate <- read.csv(file=fname.rate
                       ,header = TRUE
                       ,sep = ","
                       ,stringsAsFactors = FALSE)

install.packages("lattice")
library("lattice")

#view data
# colnames(birth.rate)
# head(birth.rate)
# View(birth.rate)

birth.rate$year<- as.character(birth.rate$year)

history <- histogram(~ rate | year
                , data=birth.rate
                , layout=c(10,5)
                , col = "#9842B3")

update(history
       , index.cond=list(c(41:50, 31:40, 21:30, 11:20, 1:10)))

