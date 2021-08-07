

fname = "/Users/sathishrajendiran/Documents/R/IST719/art.csv"
art <- read.csv(file=fname
                ,header = TRUE
                ,sep = ","
                ,stringsAsFactors = FALSE)
#view data

str(aggregate(art$units.sold,list(art$paper),sum))
summary(aggregate(art$units.sold,list(art$paper),sum))
class(aggregate(art$units.sold,list(art$paper),sum))

tapply(vector, index, function)
str(tapply(art$units.sold,art$paper,sum))
summary(tapply(art$units.sold,art$paper,sum))     
class(tapply(art$units.sold,art$paper,sum))

table(art$units.sold,art$paper)
str(table(art$units.sold,art$paper))
summary(table(art$units.sold,art$paper))   
class(table(art$units.sold,art$paper))



fname.sales <- '/Users/sathishrajendiran/Documents/R/IST719/sales.csv'
sales <- read.csv(file=fname.sales
                  ,header = TRUE
                  ,sep = ","
                  ,stringsAsFactors = FALSE)

colnames(sales)
# filter for year 2012
sales.2012 <- sales[sales$year=="2012",]
# sales.2012
#aggregate by region and wine for income
sales.agg <- aggregate(sales.2012$income,list(sales.2012$rep.region,sales.2012$wine),sum)
salesDF1 <- data.frame(sales.agg)
# salesDF1
colnames(salesDF1) <- c("Region","Wine","Income") # rename columns
#list wine  responsible for the highest income in 2012 for each region
sqldf("select Wine,Region, max(Income) Income from salesDF1 group by Region") 


#filter only for year 2010 and white wine
sales.2010 <- sales[sales$year=="2010" & sales$type=="white",]
# sales.2010
#aggregate by region and rep for units sold
sales.agg2 <- aggregate(sales.2010$units.sold,list(sales.2010$sales.rep,sales.2010$rep.region),sum)
salesDF2 <- data.frame(sales.agg2)


#RColorBrewer loaded already
library(RColorBrewer)
display.brewer.all()

#generate a boxplot with a random dataset
rand.data<-replicate(8,rnorm(35,35,sd=1.5))
boxplot(rand.data, col = brewer.pal(8, "Set1"))

#generate a boxplot with color
num.colors <- 8
my.FUN <- colorRampPalette(c("chartreuse4","azure2","darkorchid1"))
my.cols <- my.FUN(num.colors)
boxplot(rand.data, col=my.cols)
boxplot(rand.data, col = c("#62F24B", "#FF9A4F"))

col.1 <- rgb(255, 154, 79, maxColorValue = 255)
col.2 <- rgb(98,242,75, maxColorValue = 255)
boxplot(rand.data, col=c(col.1, col.2, "#5789E6"))

#a more beautiful scatterplot
plot(sales$receipt,sales$expenses)

colnames(sales)
my.col <-rep(col.1, dim(sales)[1])
my.col[sales$rep.sex ==1] <- col.2
plot(sales$receipt, sales$expenses, col=my.col)

my.col<-rep(col.1, dim(sales)[1])
my.col[sales$type =="red"] <- col.2
plot(sales$receipt, sales$expenses, col=my.col)


col.1 <-rgb(255,154,79, alpha=50, maxColorValue = 255)
col.2 <- rgb(98,242,75, alpha=255, maxColorValue = 255)
my.col <- rep(col.1, dim(sales)[1])
my.col[sales$unit.price>14] <- col.2
plot(sales$receipt, sales$expenses, col=my.col, pch=16)

#generate a list that displays the sum of sales of each type of wine
?aggregate
colnames(sales)
agg.data<- aggregate(sales$units.sold, by=list(type = sales$type, wine=sales$wine), FUN = sum)
barplot(agg.data$x, names.arg = agg.data$wine)

#generate a beautiful barplot the displays how many of each type of wine sold
par(mar=c(5,10,4,2))
barplot(agg.data$x, names.arg = agg.data$wine, las=2, horiz=T)
wine.colors<- c(rgb(255,240,150, maxColorValue = 255), rgb(160,30,65, maxColorValue = 255))
pie(c(10,10), col=wine.colors)

bar.colors <- rep("blue", nrow(agg.data))
bar.colors[agg.data$type =="red"] <- wine.colors[1]
bar.colors[agg.data$type == "white"] <- wine.colors[1]
bar.colors[agg.data$type == "white"] <- wine.colors[2]

barplot(agg.data$x, names.arg = agg.data$wine,las=2, horiz=T, col=bar.colors)

#########
#working with images in plots
#######
#load png

agg.data.receipts <- aggregate(sales$receipt, by=list(type=sales$type,wine= sales$wine), FUN = sum)

options(scipen = 9)
ima<- readPNG("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Data and Images/bottles.png")
r1 <- readPNG("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Data and Images/R1.png")
w1 <- readPNG("/Users/cpkoywk/Library/Mobile Documents/com~apple~CloudDocs/IST719/Data and Images/W1.png")

pch <- rep("W", 7)
pch [agg.data$type == "red"] <- "R"
plot(agg.data$x, agg.data.receipts$x)

colnames(agg.data)[3] <- "units"
agg.data$receipt <- agg.data.receipts$x
agg.data

par(mar=c(5.1,4.1,4.1,2.1))
plot(agg.data$units, agg.data$receipt, pch=pch, col=bar.colors, bty ="n", xlab="units sold", ylab = "receipts", xlim = c(0, 1.25 * max(agg.data$units)), ylim=c(0,1.25*max(agg.data$receipt)), main="units sold by receipts", adj=0)

lim <-par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
rect(lim$usr[1], lim$usr[3] , lim$usr[2], lim$usr[4], col = rgb(1,1,1,.85), border="white")

r1.x1 <- agg.data$units[agg.data$type == "red"]
r1.x2 <- r1.x1 +3000
r1.y1 <- agg.data$receipt[agg.data$type =="red"]
r1.y2 <- r1.y1 +65000

rasterImage(r1, r1.x1, r1.y1, r1.x2, r1.y2)

w1.x1 <- agg.data$units[agg.data$type == "white"]
w1.x2 <- w1.x1 +3000
w1.y1 <- agg.data$receipt[agg.data$type =="white"]
w1.y2 <- w1.y1 +65000

rasterImage(w1, w1.x1, w1.y1, w1.x2, w1.y2)

text(agg.data$units + 2000, agg.data$receipt, labels= agg.data$wine, adj = 0, cex=1.2)
# salesDF2
colnames(salesDF2) <- c("SalesRep","Region","UnitsSold") # rename columns
#ales representatives who sold the most units of white wine in each region in 2010
sqldf("select SalesRep,Region, max(UnitsSold) as MostUnitsSold from salesDF2 group by Region")
