#
# Title: "Week 3: Async"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Week 3 Async
#

install.packages("RColorBrewer")
library("RColorBrewer")

fname.sales <- '/Users/sathishrajendiran/Documents/R/IST719/sales.csv'
sales <- read.csv(file=fname.sales
                  ,header = TRUE
                  ,sep = ","
                  ,stringsAsFactors = FALSE)

colnames(sales)
display.brewer.all()
display.brewer.pal(8,"Set3")
display.brewer.pal(4,"RdYlBu")

rand.data <- replicate(8,rnorm(35,35,sd=1.5))

boxplot(rand.data,col = brewer.pal(8,"Set3"))

num.colors <- 8
xyz <- colorRampPalette(c("blue","red","green"))

my.cols <- xyz(num.colors) 
boxplot(rand.data,col = my.cols)


plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = "orange")        

col.vec <- rep("orange",nrow(sales))
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec) 

col.vec <- rep(rgb(30,144,255,maxColorValue = 255),nrow(sales))
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec) 

hist(sales$unit.price)
col.vec[sales$unit.price >14] <- rgb(255,64,64
                                     ,maxColorValue = 255)

#####################################################################
#
#  Exercise 3.2.3 Color Apps and Color in R
#
#####################################################################

col.vec <- rep(rgb(30,144,255
                   ,maxColorValue = 255)
                   ,nrow(sales))

col.vec[sales$unit.price >14] <- rgb(255,64,64
                                     ,maxColorValue = 255)
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec)


col.vec[sales$type =="red"] <- rgb(255,64,64,maxColorValue = 255)
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec)

#blue vector
col.vec <- rep(rgb(30,144,255
                   ,maxColorValue = 255),nrow(sales)) # all blue
#red vector
col.vec <- rgb(255,64,64
               ,maxColorValue = 255)

#####################################################################
#
#  Exercise 3.2.4 Color Apps and Color in R
#
#####################################################################
x <- rnorm(117)
y <- rnorm(117)

col.vec[y > 0]  <- rgb(30,144,255,maxColorValue = 255)
col.vec[y <= 0] <- rgb(255,64,64,maxColorValue = 255)
plot(x,y
     , pch = 16
     , cex = 1
     , col = col.vec)

#####################################################################
#
#  Exercise 3.2.5 Overplotting and Transparency
#
#####################################################################

col.vec <- rep(rgb(.8,.15,.15),nrow(sales))
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec) 

# set trasparence to shade the points | alpha=.3
col.vec <- rep(rgb(.8,.15,.15, alpha=.3),nrow(sales))
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec) 

# set trasparence to shade the points | alpha=.2
col.vec <- rep(rgb(.8,.15,.15, alpha=.2),nrow(sales))
plot(sales$expenses,sales$income
     , pch = 16
     , cex = 1
     , col = col.vec) 


# set trasparence to shade the points | alpha=.3 & cex = 0.3 to resize the point 
col.vec <- rep(rgb(.8,.15,.15, alpha=.3),nrow(sales))
plot(sales$expenses,sales$income
     , pch = 16
     , cex = .3
     , col = col.vec) 

# apply colorRampPalette with background color as black
smoothScatter(sales$expenses,sales$income
              ,colramp = colorRampPalette(c("black","cyan","pink","red")))

# apply colorRampPalette with background color as white
smoothScatter(sales$expenses,sales$income
              ,colramp = colorRampPalette(c("white","cyan","pink","red")))


install.packages("aplpack")
library("aplpack")

bagplot(sales$expenses,sales$income
        , show.whiskers = F
        , col.loophull = "#aaccff"
        , col.looppoints = "#3355ff"
        , col.baghull = "7799ff"
        , col.bagpoints = "000088"
        , transparency = T)


my.alpha <- 100
col.vec <- rep(rgb(30,144,255,maxColorValue = 255,alpha = my.alpha)
               , nrow(sales))

col.vec[sales$unit.price > 10] <- rgb(64,255,64
                                      , maxColorValue = 255
                                      , alpha = my.alpha)
col.vec[sales$unit.price > 14] <- rgb(255,64,64
                                      , maxColorValue = 255
                                      , alpha = my.alpha)
plot(sales$expenses,sales$income
     # , pch = 16
     # , cex = .3
     , col = col.vec) 


#######################################################
#
# 3.3 Adobe Illustrator
#
#######################################################


n <- 1000
x <- rnorm(n)
y <- x^2 * rnorm(n,mean = 1, sd = .25)

plot(c(x,-1.5,1.5,0)
     , c(y,14,14,0))

A <- sample(c("here","there","nowhere","everywhere")
            , size = n,replace = T)

B <- sample(c("now","later")
            , size = n, replace = T)

barplot(table(B,A),beside = T) # beside
barplot(table(B,A)) # stacked
pie(table(A))
