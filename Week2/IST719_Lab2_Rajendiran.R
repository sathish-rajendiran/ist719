#
# Title: "Week 2: Async"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab2, Data Exploration & Distribution
#

# getting started with data

#import csv
fname <- file.choose()
tips <- read.csv(file=fname
                ,header = TRUE
                ,stringsAsFactors = FALSE)

colnames(tips)
fix(tips)
View(tips)
str(tips)

tips[1,] # first row
tips[,1] # first column
tips[3,3] # 3rd row 3rd coulumn value
tips[1:3,] # first 3 rows

length(tips[1:3,2])

dim(tips)
dim(tips)[1]

tips$time
tips[,"time"]

plot(tips$total_bill)
plot(sort(tips$total_bill))

boxplot(tips$total_bill)
hist(tips$total_bill)

d <- density(tips$total_bill)
plot(d)

plot(d
     , col = c("blue")
     , main="Tips Plot") 

polygon(d
        , col="orange"
        , main="Tips Total")

# install.packages("vioplot")
library(vioplot)
vioplot(tips$total_bill)

par(mfrow=c(2,2))
boxplot(tips$total_bill)
hist(tips$total_bill)
d <- density(tips$total_bill)
plot(d)
polygon(d, col="orange")
vioplot(tips$total_bill)

tips$sex
unique(tips$sex)

tips.M <- tips[tips$sex =="Male",] #subset only Male
View(tips.M)

tips.F <- tips[tips$sex =="Female",] #subset only Female
View(tips.F)

par(mfrow=c(2,1),mar=c(2,3,1,2))
boxplot(tips.M$tip,horizontal = T,ylim=c(1,10))
boxplot(tips.F$tip,horizontal = T,ylim=c(1,10))

#working with json data
fname <- file.choose()
install.packages("jsonlite")
library("jsonlite")

raw.tweet <- fromJSON(fname,flatten = FALSE)
str(raw.tweet)     
View(raw.tweet)
names(raw.tweet)

raw.tweet$text
raw.tweet$user$followers_count

raw.tweet[["user"]]
raw.tweet[["user"]]$followers_count
raw.tweet[["user"]][["followers_count"]]

#working with json/tweets data
fname <- file.choose()
con <- file(fname,open="r")
tweets <- stream_in(con)
close(con)

dim(tweets)

tweets$text[1:3]
tweets$user$followers_count

boxplot(tweets$user$followers_count, horizontal = TRUE) #highly skewed data
boxplot(log10(tweets$user$followers_count), horizontal = TRUE) #apply log10 to standardize

task.time <- c(rnorm(n=30,mean=30,sd=2.25)
               , rnorm(n=30,mean = 25,sd=1.5))
hist(task.time)

status <- c(rep("AMA",30),rep("PRO",30))
df <- data.frame(time=task.time,status=status)
df
View(df)
df.grouped <- aggregate(df$time,list(df$status),mean)  #aggregate values based on average | returns a dataframe
df.grouped
colnames(df.grouped) <- c("stat","time") #rename columns
df.grouped

barplot(df.grouped$time,names.arg = df.grouped$stat,col=c("orange","steelblue"))

M.grouped <- tapply(df$time,list(df$status),mean) #aggregate values based on average | returns an array
M.grouped

class(df.grouped)
class(M.grouped)

tapply(df$time,list(df$status),range) #min and max values

range(task.time)

summary(task.time)

aggregate(df$time,list(df$status),summary)

table(df$status)
table(df$time)  #table on continuous data is meaningless

df$sex <- sample(c("M","F"),60,replace=T)

aggregate(df$time,list(df$status,df$sex),mean)

M <- tapply(df$time,list(df$status,df$sex),mean)
M <- tapply(df$time,list(df$sex,df$status),mean)
barplot(M,beside = TRUE)


##########################################################
#
# reshaping data with tidyr
#
##########################################################
# gather() makes "wide" data longer
# spread() makes "long" data wider
# separate() splits a single column into multiple columns
# unite() combines multiple columns into a single column

install.packages("tidyr")
library("tidyr")

n <- 5
year <- 2001:(2000+n)
q1 <- runif(n=n, min = 100, max = 120)
q2 <- runif(n=n, min = 103, max = 130)
q3 <- runif(n=n, min = 105, max = 140)
q4 <- runif(n=n, min = 108, max = 150)

df.wide <- data.frame(year,q1,q2,q3,q4)

gather(df.wide,qt,sales,q1:q4)

df.wide %>% gather(gt,sales,q1:q4)  #tidyverse

df.long <- df.wide %>% gather(qt,sales,q1:q4)

df.long
o <- order(df.long$year,df.long$qt)
df.long <- df.long  [0,]

gather(df.wide,qt,sales,2:5)


df <- data.frame(cat=rep(c("tap","reg","zed","vum"),3)
                 , group = rep(letters[7:9],4)
                 , x = 1:12)
df

spread(df,cat,x)

##########################################################
#
# using rect function to build a custom plot
#
##########################################################


install.packages("plotrix")
library("plotrix")

# n <- 70 #intital run
n <- 7000
age.min <- 1
age.max <- 90
age.range <- c(age.min,age.max)

m <- round(rescale(rbeta(n,5,2.5),age.range),0)
f <- round(rescale(rbeta(n,5,2.0),age.range),0)

x <- age.min:age.max
f.y <- m.y <- rep(0,length(x))

# hist(m)
# hist(f)

m.tab <- table(m)
m.y[as.numeric((names(m.tab)))] <- as.numeric(m.tab)

f.tab <- table(f)
f.y[as.numeric((names(f.tab)))] <- as.numeric(f.tab)

age.freqs <- data.frame(ages=x,males=m.y,females=f.y)

max.x <- round(1.2 * max(age.freqs[,2:3]),0)

plot(c(-max.x,max.x)
     , c(0,100)
     , type ="n"
     , bty ="n"
     , ylab ="age"
     , xaxt ="n"
     , xlab ="freq"
     , main ="sample age distribution"
     )

grid()
last.y <- 0
for (i in 1:90) {
    rect(xleft = 0,ybottom = last.y, xright = age.freqs$males[i] 
       ,ytop = age.freqs$ages[i], col="lightblue", border = NA)
  
    rect(xleft = 0,ybottom = last.y, xright = -age.freqs$females[i] 
       ,ytop = age.freqs$ages[i], col="lightpink", border = NA)
    last.y <- age.freqs$ages[i]
}
