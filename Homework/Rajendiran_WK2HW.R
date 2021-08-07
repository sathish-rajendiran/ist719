#
# Title: "Week 2: Homework"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Week 2 Homework
#

##########################################################
#
# Part 1: Using the "Visualize This" book
#
##########################################################

#Bar chart: Figure 4-11
# getting started with data

#import csv
# fname <- file.choose()
"/Users/sathishrajendiran/Documents/R/IST719/hot-dog-contest-winners.csv"
fname <- 'http://datasets.flowingdata.com/hot-dog-contest-winners.csv'
hotdogs <- read.csv(file=fname
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
        , names.arg = hotdogs$Year
        , border = NA
        , horiz = F
        , las = 1
      )
# defin colors function to conditionally format if the country is United States
fill_colors <- c()
for (i in 1:length(hotdogs$Country)) {
  if(hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors,"#821122")
  } else {
      fill_colors <- c(fill_colors,"#cccccc")
    }
}

barplot(hotdogs$Dogs.eaten
        , col= fill_colors
        , main = "Sathish's Hot Dog Eating Contest results, 1980-2010"
        , ylab = "Hot dogs and buns (HDB) eaten"
        , xlab = "Year"
        , names.arg = hotdogs$Year
        , border = NA
        , horiz = F
        , las = 1
)

#reproduce Stacked bar chart: Figure 4-22

#import csv
fname <- 'http://datasets.flowingdata.com/hot-dog-places.csv'
hot_dog_places <- read.csv(file=fname
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
        , border = NA
        , space = 0.25
        , ylim = c(0,200)
        , horiz = F
        , las = 1
        , ylab = "Hot dogs and buns (HDB) eaten"
        , xlab = "Year"
        , main = "Hot Dog Eating Contest results, 1980-2010"
        )

#reproduce Scatterplot: Figure 4-28

#import csv
fname <- 'http://datasets.flowingdata.com/flowingdata_subscribers.csv'
subscribers <- read.csv(file=fname
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
     , col = "black"
     , cex =1
     , lend = 2
     , bty = "n"
     , xlab = "Day" # x axis label
     , ylab = "subscribers"  # y axis label
)

# plot - scatter plot
plot(subscribers$Subscribers
     , type = "h"
     , ylim = c(0,30000)
     , cex =1
     , lend = 2
     , xlab = "Day" # x axis label
     , ylab = "Subscribers"  # y axis label
)
points(subscribers$Subscribers
       , lwd = 1 
       , pch = 19
       , col = "black"
       )

# Time series: Figure 4-34

#import csv
fname <- 'http://datasets.flowingdata.com/world-population.csv'
population <- read.csv(file=fname
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
     , lwd = 2
     , ylim = c(0,7000000000)
     , xlab = "Year" # x axis label
     , ylab = "Population"  # y axis label
)

# Step chart: Figure 4-43

#import csv
fname <- 'http://datasets.flowingdata.com/us-postage.csv'
postage <- read.csv(file=fname
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
     , xlab = "Year" # x axis label
     , ylab = "Postage Rate (Dollars)"  # y axis label
     , main = "US Postage Rates for Letters, First Ounce, 1991-2009"
)

plot(postage$Year,postage$Price
     , type = "l"
     , lwd = 1
     , xlab = "Year" # x axis label
     , ylab = "Postage Rate (Dollars)"  # y axis label
     , main = "US Postage Rates for Letters, First Ounce, 1991-2009"
)

##########################################################
#
# Part 2: Simple Distributions
#
##########################################################

#import csv
# fname <- file.choose()
#
fname = "/Users/sathishrajendiran/Documents/R/IST719/art.csv"
art <- read.csv(file=fname
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
View(art)

boxplot(art$total.sale)

# What is the distribution of total.sales for the whole dataset? 
# Provide two different plots that show two different ways of showing distribution. 
# Title your plot(s): Distribution of total.sales

# install.packages("vioplot")
# library(vioplot)


sales.PaperType <- aggregate(art$total.sale,list(art$paper.type),sum)
sales.Year <- aggregate(art$total.sale,list(art$year),sum)

# sales.PaperType
# sales.Year
names(sales.PaperType) <- c("PaperType","TotalSales")
names(sales.Year) <- c("Year","TotalSales")

barplot(sales.Year$TotalSales
        , col= c("orange","blue","green","pink")
        , main = "Distribution of total.sales"
        , ylab = "Sales"
        , xlab = "Year"
        , names.arg = sales.Year$Year
        , border = "white"
        , horiz = F
        , las = 1
        , density = 20
        , angle = c(45,90,180,10)
)


#pie chart
pie(sales.PaperType$TotalSales
    , main="Distribution of total.sales"
    , col = c("orange","blue","green","pink","tan","steelblue")
    , labels = c("block","journal","pad","pads","roll","sheet")
)


barplot(sales.PaperType$TotalSales
        , col= c("orange","blue","green","pink","tan","steelblue")
        , main = "Distribution of total.sales"
        , ylab = "Sales"
        , xlab = "Paper Type"
        , names.arg = sales.PaperType$PaperType
        , border = "white"
        , horiz = F
)



sales.Paper <- aggregate(art$total.sale,list(art$paper),sum)
names(sales.Paper) <- c("Paper","TotalSales")


# 
art.drawing <- art[art$paper == "drawing" , ]
art.water <- art[art$paper == "watercolor" , ]


boxplot(art$total.sale ~ art$paper
        , col=c("pink","blue")
        , xlab = "Paper"
        , ylab = "Total Sales in $"
         , main="distribution of the totals sales by  paper"
        )



par(mfrow=c(2,2))
d <- density(art$total.sale)
plot(d,
     main = "Distribution of total.sales")
polygon(d
        , col="pink"
        , main = "Distribution of total.sales")

boxplot(art$total.sale ~ art$paper
        , col=c("pink","orange")
        , xlab = "Paper"
        , ylab = "Total Sales in $"
        , main="Distribution of total.sales"
)


# What is the distribution of the totals sales for drawing paper?
hist(art.drawing$total.sale
     , xlab = "Sales"
     , main = "distribution of the totals sales for drawing paper"
     , col="orange")

# What is the distribution of the totals sales for watercolor paper?
hist(art.water$total.sale
     , xlab = "Sales"
     , main = "distribution of the totals sales for watercolor paper"
     , col="pink")

##########################################################
#
# Part 3: Grouping and Multidimension Plots
#
##########################################################


options(scipen=999) 
barplot(sales.Paper$TotalSales
        , col= c("orange","steelblue")
        , main = "Distribution of total.sales"
        , ylab = "Sales"
        , xlab = "Paper Type"
        , names.arg = sales.Paper$Paper
        , border = "white"
        , horiz = F
)

# Does the art company sell more units of drawing paper or watercolor paper?
# Indicate which plot answers this question.
sales.UnitsSold <- aggregate(art$units.sold,list(art$paper),sum)
names(sales.UnitsSold) <- c("Paper","UnitsSold")

barplot(sales.UnitsSold$UnitsSold
        , col= c("pink","lightblue")
        , ylab = "# of Units sold"
        , xlab = "Paper"
        , names.arg = sales.UnitsSold$Paper
        , border = "white"
        , horiz = F
        , main = "Units Sold by Paper"
)

#pie chart
pie(sales.UnitsSold$UnitsSold
    , col = c("pink","lightblue")
    , labels = c("drawing","watercolor")
    , main="Units Sold by Paper"
)


# Does the art company bring in more money (income) selling drawing paper or watercolor paper? 
#   Indicate which plot answers this question
options(scipen=999) 
barplot(sales.Paper$TotalSales
        , col= c("orange","steelblue")
        , ylab = "Sales"
        , xlab = "Paper Type"
        , names.arg = sales.Paper$Paper
        , border = "white"
        , horiz = T
        , main = "Income by Paper"
)

#pie chart
pie(sales.Paper$TotalSales
    , col = c("orange","steelblue")
    , labels = c("drawing","watercolor")
    , main="Income by Paper"
)


par(mfrow=c(2,2))
#Is there a relationship between the unit price of art goods and their units sold? 
#If so, what kind of relationship is it? Indicate which plot answers this question.

plot(art$unit.price~art$units.sold
     , data=art
     , type ="p"
     , col="steelblue"
     , xlab="Units Sold"
     , ylim= c(0,30)
     , ylab="Unit Price in US Dollars"
     , main="Relationship between Units Sold and Unit Price"
)

barplot(sales.UnitsSold$UnitsSold
        , col= c("pink","lightblue")
        , ylab = "# of Units sold"
        , xlab = "Paper"
        , names.arg = sales.UnitsSold$Paper
        , border = "white"
        , horiz = F
        , main = "Units Sold by Paper"
)

barplot(sales.Paper$TotalSales
        , col= c("orange","steelblue")
        , ylab = "Sales"
        , xlab = "Paper Type"
        , names.arg = sales.Paper$Paper
        , border = "white"
        , horiz = T
        , main = "Income by Paper"
)

#pie chart
pie(sales.Paper$TotalSales
    , col = c("orange","steelblue")
    , labels = c("drawing","watercolor")
    , main="Income by Paper"
)


d <- density(mtcars$mpg) 
plot(d) 
polygon(d, col="blue", border="black") 

d <- density(mtcars$mpg) 
hist(mtcars$mpg) 
polygon(d, col="blue", border="blue") 

d <- density(mpg) 
plot(d) 
polygon(d, color="blue") 

save (df, file = "art.rda")
dir ()

d <- density(mtcars$mpg)
plot(d)
polygon(d, col="blue", border="black")

d <- density(mtcars$mpg)
polygon(d, border="blue")
hist(d)

d <- density(mtcars$mpg)
plot(d)
polygon(d, col="orange", border="blue")


barplot(sales.UnitsSold$UnitsSold
        , col= c("pink","lightblue")
        , ylab = "# of Units sold"
        , xlab = "Paper"
        , names.arg = sales.UnitsSold$Paper
        , border = "white"
        , horiz = F
        , main = "Units Sold by Paper"
)

str(aggregate(art$units.sold,list(art$paper),sum))
summary(aggregate(art$units.sold,list(art$paper),sum))
class(aggregate(art$units.sold,list(art$paper),sum))

sales.UnitsSold <- aggregate(art$units.sold,list(art$paper),sum)
names(sales.UnitsSold) <- c("Paper","UnitsSold")

barplot(sales.UnitsSold$UnitsSold
        , col= c("pink","lightblue")
        , ylab = "# of Units sold"
        , xlab = "Paper"
        , names.arg = sales.UnitsSold$Paper
        , border = "white"
        , horiz = F
        , main = "Units Sold by Paper"
)
