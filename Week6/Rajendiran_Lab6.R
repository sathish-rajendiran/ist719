#
# Title: "Week 6: Async - A Grammer of Graphics: ggplot2"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab6
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
# EnsurePackage("reshape2")
# EnsurePackage("RColorBrewer")
# EnsurePackage("mice") # missing data 
# EnsurePackage("visdat") # missing data
# EnsurePackage("stringr")
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
sales.fname = "/Users/sathishrajendiran/Documents/R/IST719/sales.csv"
sales.raw <- read.csv(file=sales.fname
                    ,header = TRUE
                    ,sep = ","
                    ,stringsAsFactors = FALSE)

sales <- data.frame(sales.raw)

#column names
colnames(sales)
# [1] "sale.date"    "sales.rep"    "rep.sex"      "rep.region"   "rep.feedback" "wine"         "type"        
# [8] "cost"         "unit.price"   "units.sold"   "income"       "expenses"     "year" 

#structure of the data set
str(sales)

summary(sales)
class(sales)


######################################################
#
# Data Visualization - Single Dimension Reports
#
#

p <- ggplot(sales)
p

class(p) # [1] "gg"     "ggplot"

attributes(p)
# $names
# [1] "data"        "layers"      "scales"      "mapping"     "theme"       "coordinates"
# [7] "facet"       "plot_env"    "labels"     
# 
# $class
# [1] "gg"     "ggplot"

p$layers #list()

p$scales
# <ggproto object: Class ScalesList, gg>
#   add: function
# clone: function
# find: function
# get_scales: function
# has_scale: function
# input: function
# n: function
# non_position_scales: function
# scales: NULL
# super:  <ggproto object: Class ScalesList, gg>

summary(p)

# data: sale.date, sales.rep, rep.sex, rep.region, rep.feedback, wine, type, cost,
# unit.price, units.sold, income, expenses, year [10000x13]
# faceting: <ggproto object: Class FacetNull, Facet, gg>
#   compute_layout: function
# draw_back: function
# draw_front: function
# draw_labels: function
# draw_panels: function
# finish_data: function
# init_scales: function
# map_data: function
# params: list
# setup_data: function
# setup_params: function
# shrink: TRUE
# train_scales: function
# vars: function
# super:  <ggproto object: Class FacetNull, Facet, gg>

View(p$data)

# data: sale.date, sales.rep, rep.sex, rep.region, rep.feedback, wine, type, cost,
# unit.price, units.sold, income, expenses, year

ggplot(sales) + aes(x=expenses)

range(sales$expenses)
# > range(sales$expenses)
# [1]  2.8120 14.2905

plot(sales$expenses)

ggplot(sales) + aes(x=expenses)
ggplot(sales, aes(x=expenses))  # same as above

ggplot(sales) + aes(x=expenses,y=income)
ggplot(sales) + aes(x=expenses,y=income) + geom_point() #scatter plot

#same as above but lines separated
ggplot(sales) + 
  aes(x=expenses,y=income) + 
  geom_point() #scatter plot

p <- ggplot(sales) # assign to variable

p + aes(x=expenses,y=income) + 
  geom_point() #scatter plot



# assign to variable
p <- ggplot(sales) + aes(x=expenses,y=income)
p + geom_point() #scatter plot


p <- ggplot(sales) + aes(x=expenses,y=income)
p + geom_point(color ="blue") #scatter plot

# options to save
# save(p,file = )

###################################################
##  scatter plot
##  differnt coloring options
#

# option 1
ggplot(sales) + 
  aes(x=expenses,y=income) +   
  geom_point() #scatter plot

# option 2
ggplot(sales) + 
  aes(x=expenses,y=income, color = type) +   #apply color based on data
  geom_point() #scatter plot

# option 3
ggplot(sales) + 
  aes(x=expenses,y=income, color = unit.price > 14) +  #mapping aesthetic takes default color + legend
  geom_point() #scatter plot

# option 4
ggplot(sales) + 
  aes(x=expenses,y=income) +  
  geom_point(color = ifelse(sales$unit.price > 14, "yellow","blue")) # setting aesthetic in geometry point

# option 5
ggplot(sales) + 
  aes(x=expenses,y=income, color = unit.price) +   #apply color based on data | continuos/integer values being aesthetic
  geom_point() #scatter plot

# option 6

ggplot(sales) + 
  aes(x=expenses,y=income
      , color = rep.region
      , shape = type
      , alpha = unit.price
      , size = units.sold
      ) +   
  geom_point() #scatter plot



p1 <- ggplot(sales)
p2 <- ggplot(sales) + aes( x= expenses, y = income, shape = re.region)

summary(p1)
summary(p2)

attributes(p1)
attributes(p2)

p1$labels
p2$labels


####
## ggplot that sets the colors of the points
#

ggplot(sales) + 
  aes(x=expenses,y=income, color = unit.price > 14) +  #mapping aesthetic takes default color + legend
  geom_point() #scatter plot

unique(sales$wine)

ggplot(sales) + 
  aes(x=expenses,y=income) +  
  geom_point(color = ifelse(sales$wine=="Riesling","red"
                            ,ifelse(sales$wine=="Pinot Gris","blue",
                                    ifelse(sales$wine=="Chardonnay","orange"    
                                           ,"pink"))))  

####
## ggplot that maps the color of points to some variable
#

ggplot(sales) + 
  aes(x=expenses,y=income, color = sales.rep) +  #mapping aesthetic takes default color + legend
  geom_point() #scatter plot

ggplot(sales) + 
  aes(x=expenses,y=income, color = wine) +  #mapping aesthetic takes default color + legend
  geom_point() #scatter plot

###################################################
##  scatter plot
##  geometry
#

ggplot(sales) + 
  aes(x=expenses,y=income) +   
  geom_point() + geom_rug()


income.pred <- predict(lm(sales$income~sales$expenses))

ggplot(sales) + 
  aes(x=expenses,y=income) +   
  geom_point() + geom_line(aes(y=income.pred),color = "red", lwd = 3)

# ggplot(sales) + 
#   aes(x=expenses,y=income) +  geom_point() + 
#   aes(y=income.pred)+ geom_line(color = "red", lwd = 3)

ggplot(sales) +
  aes(x=expenses,y=income) +  geom_point() +
  aes(y=income.pred)+ geom_line(color = "red", lwd = 3) + geom_rug()


ggplot(sales) + 
  aes(x=expenses,y=income) +  geom_point(color ="pink") +
  geom_rug() +
  geom_line(aes(y = income.pred)) +
  geom_line(aes(y = income.pred +150)) +
  geom_vline(xintercept = 10, color = "blue") +
  geom_hline(yintercept = 500, color ="orange") +
  geom_abline(intercept = 50, slope = 100, color ="red", lty = 3, lwd = 2)
  

ggplot(sales) + 
  aes(x=expenses,y=income) +  geom_point() +
  geom_smooth(method = "loess") 

ggplot(sales) + 
  aes(x=expenses,y=income) +  geom_point() +
  geom_smooth()

#####
##  desnity plots
##
#

ggplot(sales) + 
  aes(x=expenses,y=income) +  geom_bin2d()

ggplot(sales) + 
  aes(x=expenses,y=income) +  geom_bin2d(bins = 50)


price <- ifelse(sales$unit.price > 14, "expensive", "moderate")
price(sales$unit.price < 9) <- "cheap"


ggplot(sales) +
  aes(x = expenses, y = income, color = price) +
  geom_bin2d(bins = 50)


#######################################################
##
## Other geoms
##
#


ggplot(sales) + 
  aes(x=income) +  geom_blank()

ggplot(sales) + 
  aes(x=income) +  geom_histogram()

ggplot(sales) + 
  aes(x=income) +  geom_histogram(binwidth = 10)

#basic plot
hist(sales$income)

ggplot(sales) +   aes(x=income) +  
  geom_histogram(binwidth = 10, fill ="orange")


ggplot(sales) +   aes(x=income) +  
  geom_histogram(binwidth = 10, fill ="orange") +
  geom_vline(aes(xintercept = mean(income))
             , color = "blue", linetype ="dashed", size = 1)

ggplot(sales) +   aes(x=income) +  
  geom_histogram(binwidth = 10, fill ="orange") +
  geom_vline(aes(xintercept = mean(income))
             , col = "blue", lty ="dashed", size = 1)


ggplot(sales) +   aes(x=income) +  
  geom_histogram(binwidth = 10, fill ="orange", alpha =.9) +
  aes(y = ..density..) +
  geom_density(alpha = .3, fill = "blue", color = "blue")



ggplot(sales) +
  aes(x = "sathish", y = income) +
  geom_boxplot()


ggplot(sales) +
  aes(x = rep.region, y = income) +
  geom_boxplot()


df  <- aggregate(sales$units.sold, list(year = sales$year), sum)
df2 <- aggregate(sales$units.sold
                 , list(year = sales$year, region = sales$rep.region), sum)

# line plot

ggplot(df) + aes(x = year, y = x) +
  geom_line() + ylim(c(0,40000))

ggplot(df) + aes(x = year, y = x) +
  geom_step() + ylim(c(0,40000))


ggplot(df) + aes(x = year, y = x) +
  geom_ribbon(aes(ymin = x - 1000, ymax = x + 1000, fill = "yellow")) +
  geom_line() + ylim(c(0,40000))


df2
ggplot(df2) + aes(x = year, y = x, color = region) +
  geom_line() + ylim(c(0,10000))



# bar plot  stacked vs dodged
ggplot(df2) +
  aes( y=x, x= year,fill=region) +
  geom_bar(stat="identity", position = "stack")

ggplot(df2) +
  aes( y=x, x= year,fill=region) +
  geom_bar(stat="identity",position=position_dodge(),size=.3) +
  ylab("units sold")

# bar plot  geom_smooth with scatter plot
ggplot(df2, aes(x=year, y=x)) + 
  geom_point(aes(col=region, size= .4)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  ylim(c(4000, 8000)) 
 
#####################################
## geom_bar
##
#



df  <- aggregate(sales$units.sold, list(region = sales$rep.region), sum)
colnames(df)[2] <- "sales"

ggplot(sales) +
  aes(x=rep.region) + geom_bar()

#
ggplot(sales) +
  aes(x=rep.region) + geom_bar(fill = "orange")

#
ggplot(sales) +
  aes(x=rep.region) + geom_bar(fill = "orange") +
  ggtitle("number of sales by region")


#
ggplot(sales) +
  aes(x=rep.region, fill=type) + geom_bar() 


#
ggplot(sales) +
  aes(x=rep.region, fill=type) + geom_bar(position = "dodge") 

#
ggplot(sales) +
  aes(x=rep.region, fill=type) + geom_bar(position = "fill") 


#
ggplot(sales) +
  aes(x=rep.region, fill=type) + geom_bar(position = "fill") 


#
ggplot(df) +
  aes(x=region, y=sales, fill=region) + geom_bar(stat =  "identity") 


# pie chart
ggplot(df) +
  aes(x="", y=sales, fill=region) + 
  geom_bar(width = 1,stat =  "identity") +
  coord_polar("y", start = 45)

#######################
##
##
#

hist(sales$income)

p <- ggplot(sales) + aes(x=income)
p+geom_histogram() + stat_bin(binwidth = 20)

p+stat_density()

ggplot(sales) + aes(y=income) + geom_boxplot() + stat_boxplot()


ggplot(sales) + aes(y=income) +  stat_boxplot()


ggplot(sales) + aes(x=expenses, y=income) + stat_bin2d()


ggplot(sales) + aes(x=expenses, y=income) + stat_bin2d() +
  stat_density_2d(col="red")

ggplot(sales) + aes(x=rep.region) + geom_bar()

ggplot(sales) + aes(x=rep.region) + stat_count()


ggplot(df) + aes(x = region, y = sales) +
  geom_bar(stat ="identity")

ggplot(df) + aes(x = region, y = sales) + geom_bar() +   stat_identity()

ggplot(sales) + aes(x = income) +
  geom_histogram(aes(fill = ..count..)) +
  aes(y=..density..) + 
  geom_density(fill="yellow", alpha = .1)


####################################################
##
##
#

memory.size() #[1] Inf

object.size(sales) #1000168 bytes

p <- ggplot(sales) + aes(x=income)
p+geom_histogram() + stat_bin(binwidth = 20)

object.size(p) # 1004552 bytes
memory.size() #[1] Inf  'memory.size()' is Windows-specific 


gc()


memory.size()

dev.off()
