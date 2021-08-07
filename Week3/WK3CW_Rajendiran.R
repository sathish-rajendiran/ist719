#
# Title: "Week 3: CW"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Week 3 CW
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

boxplot(rand.data,col = brewer.pal(8,"PuRd")
        , main ="Random Data Ditribution")

num.colors <- 8
FUN <- colorRampPalette(c("darkorange4","#006400","8B7355"))
my.cols <- FUN(num.colors)
my.cols

plot(1:8,rep(1,8),axes = FALSE,pch=16,cex=4
     ,col=my.cols
     ,xlab = NA
     ,ylab = NA
     ,xlim=c(0,8))

boxplot(rand.data,col = my.cols
        , main ="Random Data Ditribution")

f <- colorRampPalette(c("red","blue"))

f(3) #  "#FF0000" "#7F007F" "#0000FF"
f(6) # "#FF0000" "#CC0033" "#990066" "#650099" "#3200CC" "#0000FF"
f(9) # "#FF0000" "#DF001F" "#BF003F" "#9F005F" "#7F007F" "#5F009F" "#3F00BF" "#1F00DF" "#0000FF"


my.cols <- f(3)
my.cols

plot(1:8,rep(1,8),axes = FALSE,pch=16,cex=4
     ,col=my.cols
     ,xlab = NA
     ,ylab = NA
     ,xlim=c(0,8))


my.col.vec <- colorRampPalette(c("red","white","blue"))(11)
my.col.vec
plot(1:11,rep(1,11),axes = FALSE,pch=16,cex=4
     ,col=my.col.vec
     ,xlab = NA
     ,ylab = NA
     ,xlim=c(0,11))



iris
str(iris)
View(iris)




plot(iris$Petal.Length,iris$Petal.Width
     , data=my.iris
     , pch = 16
     , cex = 1
     , col = iris$Species
     , main="Iris data"
)

unique(iris$Species)

col.iris <- rep("red",nrow(iris))
col.iris[iris$Species=="setosa"] <- "red"
col.iris[iris$Species=="versicolor"] <- "green"
col.iris[iris$Species=="virginica"] <- "blue"

plot(iris$Petal.Length,iris$Petal.Width
     , data=my.iris
     , pch = 19
     , cex = 1
     , col = col.iris
     , main="Iris data"
)
legend("topleft"
       , title = "Species"
       , legend=c("setosa","versicolor", "virginica")
       , fill = c("red", "green","blue")
       , cex = 0.8
       , bty = "n"
       )


install.packages("ggplot2")
library(ggplot2)
data("diamonds")

str(diamonds)

unique(diamonds$cut)

plot(diamonds$carat,diamonds$price
     , data = diamonds
     , pch =16
    
)

col.diamonds <- rep("red",nrow(diamonds))
col.diamonds[diamonds$cut == "Fair"] <- "orange"
col.diamonds[diamonds$cut == "Good"] <- "blue"
col.diamonds[diamonds$cut == "Very Good"] <- "yellow"
col.diamonds[diamonds$cut == "Premium"] <- "green"
col.diamonds[diamonds$cut == "Ideal"] <- "red"

plot( diamonds$carat,diamonds$price
     , data=diamonds
     , pch = 16
     , cex = 1
     , col = col.diamonds
     , main="Diamonds data"
)

legend("bottomright"
       , title = "Cut"
       , legend=c("Ideal","Premium", "Very Good","Good","Fair")
       , fill = c("red", "green","yellow","blue","orange")
       , cex = 0.8
       , bty = "n"
)


?mtcars
dim(mtcars)
