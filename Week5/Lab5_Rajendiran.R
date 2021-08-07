#
# Title: "Week 5: Async"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Week 5 Lab
#

# Import Sales data
fname.tweets <- '/Users/sathishrajendiran/Documents/R/IST719/climatetweets_useforlecture_25k.csv'
tweets <- read.csv(file=fname.tweets
                  ,header = TRUE
                  ,quote = "\""
                  ,stringsAsFactors = FALSE)

colnames(tweets)
#  "X"                             "id_str"                       
#  "created_at"                    "source"                       
#  "lang"                          "place"                        
#  "geo"                           "text"                         
#  "is_retweet"                    "is_reply"                     
#  "hashtags"                      "media"                        
#  "user_screen_name"              "user_time_zone"               
#  "user_followers_count"          "user_location"                
#  "user_statuses_count"           "user_friends_count"           
#  "user_profile_background_color" "user_created_at"              
#  "user_is_verified"              "user_utc_offset"              
# "if_rt_user_screen_name"        "if_rt_user_followers_count"   
# "if_reply_user_screen_name" 
head(tweets)
# View(tweets)

str(tweets)
dim(tweets) #[1] 25000    25

my.media <-  tweets$media
table(my.media)

my.media[my.media == ""] <- "text only"
table(my.media)

my.media <- gsub("\\|photo","",my.media)
round(table(my.media)/sum(table(my.media)),4) *100

## Lab 5 deliverable 1 
pie(round(table(my.media)/sum(table(my.media)),4) *100)  

tweets$created_at[1:3]
# "Mon Aug 15 13:05:42 +0000 2016" 
# "Thu Jul 14 14:36:38 +0000 2016"

conversion.string <- "%a %b %d %H:%M:%S +0000 %Y"
tmp <- strptime(tweets$created_at[1:3],conversion.string)  
# tmp  

class(tmp)  #"POSIXlt" "POSIXt" 
  
tmp <- strptime(tweets$created_at,conversion.string) 
any(is.na(tmp)) # looks for any NAs False --> No NAs


rm(tmp) # remove the temporary variable

tweets$date <- strptime(tweets$created_at,conversion.string) 

#working with date time conversion
tmp <- "10AM and 27 minutes, on June 22, 1999"
strptime(tmp,"%H%p and %M minutes, on %B %d, %Y")

rm(tmp) # remove the temporary variable
#############
min(tweets$date)
max(tweets$date)
range(tweets$date)
summary(tweets$date)

difftime(min(tweets$date),max(tweets$date))

difftime(min(tweets$date),max(tweets$date), units = "min")
difftime(min(tweets$date),max(tweets$date), units = "weeks")
difftime(min(tweets$date),max(tweets$date), units = "days")

install.packages("lubridate")
library("lubridate")

wday(tweets$date[1:3], label = TRUE, abbr = TRUE)
barplot(table(wday(tweets$date, label = TRUE, abbr = TRUE)))


tmp <- tweets$user_utc_offset
# tmp

tweets$date[7:10] + tmp[7:10]

known.times <- tweets$date + tweets$user_utc_offset
any(is.na(known.times))  

index <- which(is.na(known.times))
known.times <- known.times[-index]
known.times  

barplot(table(hour(known.times))) 

rm(tmp) # remove the temporary variable

###################################
#  strptime functions - exercise
#
tmp <- "2018.08.30-16.24.49"
strptime(tmp,"%Y.%m.%d-%H.%M.%S")

tmp <- "10AM and 27 minutes, on June 22, 1999"
strptime(tmp,"%H%p and %M minutes, on %B %d, %Y")

rm(tmp) # remove the temporary variable
###################################
start.date <- as.POSIXct("2016-06-24 23:59:59")
end.date <- as.POSIXct("2016-06-26 00:00:00")
index <- which((tweets$date > start.date) & (tweets$date < end.date))

tweets.25th <- tweets$date[index]
format.Date(tweets.25th,"%Y%m%d%H%M")


tmp.date <- as.POSIXct(strptime(format.Date(tweets.25th,"%Y%m%d%H%M")
                                ,"%Y%m%d%H%M")  )
# tmp.date

plot(table(tmp.date))  

length(table(tmp.date))


tmp.tab <- table(tmp.date)
plot(as.POSIXct(names(tmp.tab)),as.numeric(tmp.tab),type = "h")
class(names(tmp.tab))

x <- seq.POSIXt(from = start.date+1, to = end.date-1, by="min")
length(x)

y <- rep(0,length(x))

y[match(names(tmp.tab), as.character(x))] <- as.numeric(tmp.tab)
plot(x,y, type = "p", pch = 16, cex=.4)

plot(x,y, type = "l")

# ?seq.POSIXt()
seq(ISOdate(1990,1,1), ISOdate(2000,1,1), by = "quarter")

start.date <- ISOdate(2021,1,1)
end.date <- ISOdate(2021,3,31)

seq(start.date, end.date, "day")

####################################################
#
#  Hashtags transformation - wordcloud
#

tweets$text[5:10]

install.packages("stringr")
library("stringr")

tags <- str_extract_all(tweets$text,"#\\S+", simplify = FALSE)
tags

tags <- tags[lengths(tags) >0]
tags <- unlist(tags)
tags <- tolower(tags)
tags <- gsub("#|[[:punct:]]","",tags)
tags.tab <- sort(table(tags),decreasing = TRUE)
tags.tab[1:10]

zap <- which(tags.tab <3)
tags.tab <- tags.tab[-zap]

tags.tab[1:10]
boxplot(as.numeric(tags.tab))
plot(as.numeric(tags.tab))

df <- data.frame(words = names(tags.tab), count = as.numeric(tags.tab)
                 , stringsAsFactors = FALSE)

par(mfrow=c(3,3))
plot(df$count,main="raw")

y <- df$count/max(df$count)
plot(y,main="0 -1")

plot(df$count^2, main="^2")
plot(df$count^(1/2), main="^(1/2)")
plot(df$count^(1/5), main="^(1/5)")
plot(log10(df$count), main="log10")
plot(log(df$count), main="log")

log10(c(1,10,100,1000,1237,10000))


##############################################
#
# hashtag - wordcloud
#

install.packages("wordcloud")
library("wordcloud")

myPal <- colorRampPalette(c("green","red","white"))

# gc()  # garbage collection

# df

index <- which(df$count > 9)
par(mar=c(0,0,0,0),bg="black")

my.counts <- (df$count[index])^(1/2)
wordcloud(df$words[index],my.counts,scale = c(4,.4)
          , min.freq = 1, max.words = Inf
          , random.color = FALSE
          , random.order = FALSE
          , ordered.colors = TRUE
          , rot.per = 0
          , colors = myPal(length(df$words[index])))


# gc()

######################################################
#
#  Alluvial Plot and Treemap Plots
#


# Import Sales data
fname.sales <- '/Users/sathishrajendiran/Documents/R/IST719/sales.csv'
sales <- read.csv(file=fname.sales
                   ,header = TRUE
                   ,stringsAsFactors = FALSE)
# View(sales)
install.packages("alluvial")
library("alluvial")
dat <- as.data.frame(Titanic, stringsAsFactors = FALSE)

alluvial(dat[,1:4], freq = dat$Freq)

alluv.df <- aggregate(sales$units.sold
                      ,list(sales$rep.region,sales$type)
                      , sum)

colnames(alluv.df) <- c("region","type","units.sold")

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold)

my.col <- rep("gold",nrow(alluv.df))
my.col[alluv.df$type=="red"] <- "red"
alluvial(alluv.df[,1:2]
         , freq = alluv.df$units.sold
         , col = my.col)

#alternative to my.col
alluvial(alluv.df[,1:2]
         , freq = alluv.df$units.sold
         , col = ifelse(alluv.df$type=="red","red","gold")
         )

options(stringsAsFactors = FALSE)

alluv.df <- aggregate(sales$units.sold
                      ,list(sales$rep.region
                            ,sales$type
                            ,sales$wine)
                      , sum)

colnames(alluv.df) <- c("region","type","wine","units.sold")

par(bg="#EDEDEB")
alluvial(alluv.df[,1:3]
         , freq = alluv.df$units.sold
         , col = ifelse(alluv.df$type=="red","red","orange1")
         , alpha=0.7
         , cex = .6
         , gap.width = .3
         , border="white"
         # ,xw=0.0
)

# ?alluvial()

######################################################
#
#  Treemap Plot
#


install.packages("RColorBrewer")
library("RColorBrewer")
install.packages("treemap")
library("treemap")

colnames(sales)
treemap(sales, index = c("rep.region")
        , vSize = "income"
        , fontsize.labels = 12
        , palette ="Greens")

treemap(sales, index = c("rep.region")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "dens"
        , fontsize.labels = 12
        , palette = "Greens")

treemap(sales, index = c("rep.region")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "dens"
        , fontsize.labels = 12
        , palette = "OrRd")

treemap(sales, index = c("rep.region")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "value"
        , fontsize.labels = 18
        , palette = "OrRd")
# par(mfrow=c(2,2))
treemap(sales, index = c("rep.region","sales.rep","type")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "dens"
        , fontsize.labels = 8
        , border.lwds=c(.1,.1)
        , border.col= "white"
        , align.labels=list(c("center", "center"), 
                            c("right", "bottom"))
        , bg.labels=c("transparent")
        , palette = brewer.pal(8,"Set1")
        )

######################################################
#
#  River Plot
#

install.packages("riverplot")
library("riverplot")

river <- riverplot.example()

par(mfrow=c(2,1))

plot(river, sort=90, lty=1)

class(river)     
# "list"      "riverplot"

x <- river

x$edges
x$nodes

x$edges$Value
     
x$edges$Value[1] <- 15
x$edges$Value[2] <- 45   
x$edges$Value[5] <- 5  
     
plot(x) 


df <- aggregate(sales$income
                , list(type = sales$type, wine = sales$wine)
                ,sum)

df <- df[order(df$type,df$x),]   
node.name <- c("wine",unique(df$type),df$wine)     
node.pos <- c(1,2,2,3,3,3,3,3,3,3)
node.col <- rep("gray",length(node.name))
node.col <- c("deepskyblue","red","yellow"
              ,"brown","orange1","khaki"
              ,"gold","pink","green","steelblue")     

node <- data.frame(ID=node.name
                   , x = node.pos
                   , col=node.col
                   ,stringsAsFactors = FALSE)     

parent.nodes <- c("wine","wine",df$type)
child.nodes <- c("red","white",df$wine)
value <- c(sum(df$x[df$type=="red"]), sum(df$x[df$type=="white"]),df$x)
edges <- data.frame(N1=parent.nodes,N2=child.nodes,Value=value)

r <- makeRiver(node,edges)
plot(r)


######################################################
#
#  Plot to Word
#


dat <- tapply(sales$units.sold,list(sales$type,sales$rep.region),sum)
barplot(dat,beside = TRUE,col = c("brown","gold")
        , main = "units sold by region and type")     
     
     
     