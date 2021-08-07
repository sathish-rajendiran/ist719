#
# Title: "Week 1: Class Work"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab1
# Date: 01/19/2021
#
# getting started with charts

# ?c()
# ?pch

# Exercise 1.
# y1 <- c(3, 1, 5, 2, 3, 8, 4, 7, 6, 9)
# plot(y1)
# Incorporate as many of the below parameters/features covered in this past week’s asynch material in a 
# single plot of y1,, show your work

# Exercise 1
y1 <- c(3, 1, 5, 2, 3, 8, 4, 7, 6, 9)

plot(y1
     , lwd = 20 
     , col = c("steelblue")
     , pch = "x"
     , cex =3
     , lend = 2
     , bty = "n"
     , main = "Lab1-Exercise1"
     , xlab = "time in years" # x axis label
     , ylab = "in millions"  # y axis label
)

# Exercise 2.
# mtcars
# hist(mtcars$hp)
# Using the R provided dataframe mtcars, create a histogram (hist function) of the 
# hp variable and incorporate as many of the below parameters/features covered in this past week’s 
# asynch material in your plot,,, show your work
x <- mtcars
hist(x$hp)

mtcars

hist(x$hp
     , main = "distribution of hp"
     , col = c("steelblue","orange")
     , xlab = "hp"
     , ylab = "number of cars"
     , bty = "n"
     , lend = 2
     , lwd =1
     )

#logrthmic 
hist(log10(x$hp))


# Exercise 3
max.temp <- c(22, 27, 26, 24, 23, 26, 28) 
barplot(max.temp
        , col =c("steelblue","red","orange","tan","yellow","green")
        , border = "white"
        , names.arg = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
        , main = "Temperature by Day"
        , xlab = "Day of Week"
        , ylab = "Day"
        , horiz = TRUE
        , las = 2
        , density = 80
        , angle = c(45,90,12)
        )













