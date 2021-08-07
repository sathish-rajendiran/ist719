#
# Title: "Week 2: Class Work"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Week 2 Class work
#

fname.sales <- '/Users/sathishrajendiran/Documents/R/IST719/sales.csv'
sales <- read.csv(file=fname.sales
                    ,header = TRUE
                    ,sep = ","
                    ,stringsAsFactors = FALSE)

colnames(sales)
# [1] "sale.date"    "sales.rep"    "rep.sex"      "rep.region"   "rep.feedback" "wine"         "type"        
# [8] "cost"         "unit.price"   "units.sold"   "income"       "expenses"     "year"  
head(sales)
View(sales)

str(sales)
dim(sales) #[1] 10000    13

sales.Region <- aggregate(sales$units.sold,list(sales$rep.region,sales$type),sum)

# sales.Region
names(sales.Region) <- c("Region","Type","Units")
sales.Region
options(scipen=999) 
barplot( sales.Region$Units
        , col=c("green","blue", "orange", "red","lightblue")
        , beside = T
        , xlab ="type"
        , ylab ="Units"
        , main="Units by Region"
        , legend.text = c(unique(sales.Region$Region))
        , args.legend=list(x="topright",bty="s"))


sales.units <- tapply(sales$units.sold,list(sales$rep.region,sales$type),sum)
sales.units

barplot( sales.units
         , col= c("palegreen","blue", "orange", "brown","lightblue")
         , beside = T
         , xlab ="Type"
         , main="Units by Region"
         , legend.text = rownames(sales.units)
         , args.legend=list(x="topright",bty="s")
         , names.arg = c("Red","White")
)

wine_agg <- tapply(sales$units.sold,list(color = sales$type, region = sales$rep.region), sum)

barplot(wine_agg,
        
        beside = T, 
        
        col = c('red','antiquewhite'),
        
        main="Comparison of Wine sales")

legend("topright",
       
       legend=c('red', 'white'), 
       
       fill=c('red', 'antiquewhite'),
       
       inset=.02)


wine_agg2 <- tapply(sales$units.sold, list(region = sales$rep.region, color = sales$type), sum)

barplot(wine_agg2, 
        
        beside = T, 
        
        col = c('lightgreen','blue','orange','red','lightblue'),
        
        main="units by region")

legend("topright",
       
       legend=c('Central', 'East', 'North', 'South', 'West'),
       
       fill=c('lightgreen','blue','orange','red','lightblue'),
       
       inset=.02)

