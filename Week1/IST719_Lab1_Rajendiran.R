#
# Title: "Week 1: Async"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab1
#
# getting started with charts

# pie chart
pie(c(7,6,7.2,12))

x <- c(7,6,7.2,12)

pie(x)

pie(x,main="Sathish's Pie")

pie(x,main="Sathish's Pie", col = c("red","orange","tan","yellow"))

pie(x
    , main="Sathish's Pie"
    , col = c("red","orange","tan","yellow")
    , labels = c("a","b","c","d")
    , main="Sathish's Pie"
    )

# plot chart
plot(c(1,3,6,4))

y1 <- c(1,3,6,4)
plot(y1,pch=8) # * points
plot(y1,pch=16, col = c("red","orange","tan","yellow")) # circle points

plot(y1
     , pch=12
     , col = c("red","orange","tan","yellow")
     , cex=3
     , main="Sathish's Plot") # circle points, increase size and title

# working with variables
x <- rnorm(n=10) # assign randomly generated numbers
x

plot(x)
plot(x,type="l") #line plot
plot(x,type="h") #histogram plot

plot(x,type="h",lwd=5) #histogram plot & adjust thickness of the line
plot(x,type="h",lwd=5,lend=2) #line end type 2
plot(x,type="h",lwd=5,lend=2,col="orange")

plot(x
     , type= "h"
     , lwd= 5
     , lend= 2
     , col= "orange"
     , main= "change in net worth")

plot(x
     , type= "h"
     , lwd= 5
     , lend= 2
     , col= "orange"
     , main= "change in net worth"
     , xlab = "time in years"
     , ylab = "in millions")

plot(x
     , type= "h"
     , lwd= 5
     , lend= 2
     , col= "orange"
     , main= "change in net worth"
     , xlab = "time in years" # x axis label
     , ylab = "in millions"  # y axis label
     , bty = "n")  #no border

par(bg="gray")

plot(x
     , type = "h"
     , lwd = 20 
     , col = c("steelblue","tan")
     , lend = 2
     , bty = "n"
     , main = "change in net worth"
     , xlab = "time in years" # x axis label
     , ylab = "in millions"  # y axis label
     )

#cleanup par variables
my.par <- par()
par(my.par)

#barplot
n <- 27
my.letters <- sample(letters[1:3],size=n,replace = T)
my.letters[2]

letters[7:9]
letters[c(8,3,1)]
my.letters

tab <- table(my.letters)
tab

barplot(tab)  #barplot
barplot(tab,col="brown")  #barplot
barplot(tab
        , col= c("orange","tan","yellow")
        , main = "employees distribution"
        , ylab = "employees"
        , xlab = "department"
        , names.arg = c("Sales","Ops","IT")
        , border = "white"
        , horiz = T
        , las = 1
        )

barplot(tab
        , col= c("orange","tan","yellow")
        , main = "employees distribution"
        , ylab = "employees"
        , xlab = "department"
        , names.arg = c("Sales","Ops","IT")
        , border = "white"
        , horiz = T
        , las = 1
        , density = 20
        , angle = c(45,90,12)
        )

x <- rnorm(n=1000,mean=10,sd=1)
x

?c()

#histogram
hist(x
     ,main = "what is the distribution of x?"
     )

#box plot
boxplot(x
        , horizontal = T)

x <- rlnorm(n=1000,meanlog = 1,sdlog = 1)
hist(x)

par(mfrow=c(2,1))
boxplot(x
        , horizontal = T)
hist(x
     ,main = "what is the distribution of x?"
     )

hist(x)
hist(log10(x))

letters[7:9]

my.letters <- sample(letters[7:9], size = 50, replace =T)
barplot(table(my.letters))

