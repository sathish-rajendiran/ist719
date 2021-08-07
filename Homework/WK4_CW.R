# Import Sales data
fname.sales <- '/Users/sathishrajendiran/Documents/R/IST719/sales.csv'
sales <- read.csv(file=fname.sales
                  ,header = TRUE
                  ,sep = ","
                  ,stringsAsFactors = FALSE)

colnames(sales)
# [1] "sale.date"    "sales.rep"    "rep.sex"      "rep.region"   "rep.feedback" "wine"         "type"        
# [8] "cost"         "unit.price"   "units.sold"   "income"       "expenses"     "year"  
head(sales)
# View(sales)

############################
# mfrow
#

par(mfrow=c(3,2))

#Box Plot
p1 <- boxplot(art$total.sale,
        data = art, main="Total Sales distribution", 
        ylab="",
        xlab = "Sales",
        border="orange", 
        col="steelblue",
        freq=FALSE,
        las=1, 
        breaks=5,
        horizontal = TRUE,na.rm=TRUE)

p1
#bar plot
sales.Year <- aggregate(art$total.sale,list(art$year),sum)
names(sales.Year) <- c("Year","TotalSales")
p2 <- barplot(sales.Year$TotalSales
        , col= c("orange","blue","green","pink")
        , main = "Total Sales by Year"
        , ylab = "Sales"
        , xlab = "Year"
        , names.arg = sales.Year$Year
        , border = "white"
        , horiz = F
        , family="HersheyGothicEnglish"
)

#pie chart
sales.Region <- aggregate(art$units.sold,list(art$store),sum)
names(sales.Region) <- c("Store","UnitsSold")
sales.Region
p3 <- pie(sales.Region$UnitsSold
    , col = c("pink","lightblue","blue","orange")
    , labels = c("Davenport","Dublin","Portland","Syracuse")
    , main="Units Sold by Store"
)

#pie chart
sales.UnitsSold <- aggregate(art$units.sold,list(art$paper),sum)
names(sales.UnitsSold) <- c("Paper","UnitsSold")

p4 <- pie(sales.UnitsSold$UnitsSold
    , col = c("pink","lightblue")
    , labels = c("drawing","watercolor")
    , main="Units Sold by Paper"
)

# line chart
sales.PaperType <- aggregate(art$total.sale,list(art$paper.type),sum)
names(sales.PaperType) <- c("PaperType","TotalSales")
sales.PaperType
plot(sales.PaperType$TotalSales
     , type ="b"
     , xaxt ="n"
     , lwd = 2
     , col ="red"
     , lty = 3
     , ylim = c(0,50000)
     , xlab = "Paper Type"
     , ylab = "Total Sales"
     , main="Total Sales by Paper Type"
     , frame = FALSE
     , family="HersheyGothicEnglish"
)
axis(1,at=c(1,2,3,4,5,6),labels=c("block", "journal", "pad","pads","roll","sheet"))

p5 <- barplot(sales.PaperType$TotalSales,sales.PaperType$TotalSales
        
        , col= c("orange","blue","green","pink","tan","steelblue")
        , main = "Total Sales by Paper Type"
        , xlab = "Sales"
        , ylab = "Paper Type"
        , border = "white"
        , names.arg = sales.PaperType$PaperType
        , horiz = F
        , family="HersheyGothicEnglish"
)

############################
# Layout function
#

M <- matrix(
   c(1,1,1,3
    ,1,1,1,4
    ,2,2,2,5
    ,2,2,2,5)
  ,nrow = 4,byrow = T
)
layout(M)
layout.show(5)

par(mar=c(2,2,2,2,))
plot(sales.PaperType$TotalSales
     , type ="b"
     , xaxt ="n"
     , lwd = 2
     , col ="red"
     , lty = 3
     , ylim = c(0,50000)
     , xlab = "Paper Type"
     , ylab = "Total Sales"
     , main="Total Sales by Paper Type"
     , frame = FALSE
     , family="HersheyGothicEnglish"
)
axis(1,at=c(1,2,3,4,5,6),labels=c("block", "journal", "pad","pads","roll","sheet"))


barplot(sales.PaperType$TotalSales,sales.PaperType$TotalSales
        
        , col= c("orange","blue","green","pink","tan","steelblue")
        , main = "Total Sales by Paper Type"
        , xlab = "Sales"
        , ylab = "Paper Type"
        , border = "white"
        , names.arg = sales.PaperType$PaperType
        , horiz = F
        , font=1
        , family="HersheyGothicEnglish"
)

pie(sales.UnitsSold$UnitsSold
    , col = c("pink","lightblue")
    , labels = c("drawing","watercolor")
    , main="Units Sold by Paper"
    , font=2
    , family="sens"
)

pie(sales.Region$UnitsSold
    , col = c("pink","lightblue","blue","orange")
    , labels = c("Davenport","Dublin","Portland","Syracuse")
    , main="Units Sold by Store"
    , font=3
    , family="sens"
)

barplot(sales.Year$TotalSales
        , col= c("orange","blue","green","pink")
        , main = "Total Sales by Year"
        , ylab = "Sales"
        , xlab = "Year"
        , names.arg = sales.Year$Year
        , border = "white"
        , horiz = F
        , font=4
        , family="HersheyGothicEnglish"
)
