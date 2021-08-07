##
# Title: "Week 10 · Viz-a-thon"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Viz-a-thon
# 03/23/2021
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
EnsurePackage("reshape2")
EnsurePackage("RColorBrewer")
EnsurePackage("hrbrthemes")
EnsurePackage("mice") # missing data 
EnsurePackage("visdat") # missing data
EnsurePackage("stringr")
EnsurePackage("tidyr")
EnsurePackage("dplyr")
EnsurePackage("lubridate")

######################################################################################
##
##
##
#

# Import Sales data
fname.buoydata <- '/Users/sathishrajendiran/Documents/R/IST719/vizathon/buoydata.csv'
buoydata <- read.csv(file=fname.buoydata
                  ,header = TRUE
                  ,sep = ","
                  ,stringsAsFactors = FALSE)

colnames(buoydata)
# [1] "DATE_TIME" "DEPTH_m"   "T_DEGC"    "SC_us_cm"  "pH"        "Dox_mg_L"  "Tn_Ntu"    "Chl_ug_L"  "PRECIP_in" "AWND_mph"  "WDF5_deg" 
# [12] "WSF5_mph" 
head(buoydata)

summary(buoydata)

str(buoydata)

# View(buoydata)

#review data volume
dataVolume(buoydata)

#assign it to Dataframe
buoydataDF <- data.frame(buoydata)


#replace blanks with NAs
buoydataDF[buoydataDF==""] <- NA

######################################################
#
# Missing data analysis
#
# # missing data 
md.pattern(buoydataDF, plot=FALSE)

# missing data visualization
vis_miss(buoydataDF)

#find out missing columns
clnames.NA <- colnames(buoydataDF)[colSums(is.na(buoydataDF)) > 0]
# [1] "pH"       "Dox_mg_L" "Tn_Ntu"   "Chl_ug_L"

#replace NAs with mean value
buoydataDF$pH[is.na(buoydataDF$pH)] <- mean(buoydataDF$pH,na.rm = TRUE)
buoydataDF$Dox_mg_L[is.na(buoydataDF$Dox_mg_L)] <- mean(buoydataDF$Dox_mg_L,na.rm = TRUE)
buoydataDF$Tn_Ntu[is.na(buoydataDF$Tn_Ntu)] <- mean(buoydataDF$Tn_Ntu,na.rm = TRUE)
buoydataDF$Chl_ug_L[is.na(buoydataDF$Chl_ug_L)] <- mean(buoydataDF$Chl_ug_L,na.rm = TRUE)

# missing data visualization
vis_miss(buoydataDF)

#review data volume
dataVolume(buoydata)

#find out missing columns
clnames.NA <- colnames(buoydataDF)[colSums(is.na(buoydataDF)) > 0]

# ggplot Histogram
hcolor <- c("orange")
hfill <- c("steelblue")
htitle <- c("Histogram - Data from Onondaga Lake Water Quality Monitoring Buoy")
theme <- theme(plot.title = element_text(hjust = 0.5),axis.title = element_text())
# Plot histogram
gghist <- ggplot(data=melt(buoydataDF),mapping = aes(x= value)) 
gghist <- gghist + geom_histogram(bins = 20,color=hcolor,fill=hfill,na.rm = TRUE)
gghist <- gghist + facet_wrap(~variable,scales = "free_x") + ggtitle(htitle)
gghist + labs(caption ="Data Source:  Onondage County Department of Water Environment Protection and Upstate Freshwater Institiute") + theme


#add date month hour year columns
buoydataDF$DATE_TIME[1:5]

buoydataDF$Date.Time <- as.POSIXct(buoydataDF$DATE_TIME,"%m/%d/%y %H:%M",tz="")
buoydataDF$Date <- date(buoydataDF$Date.Time)

buoydataDF$Month <- month(buoydataDF$Date.Time)
buoydataDF$Year <- year(buoydataDF$Date.Time)
buoydataDF$Month.Name <- months(buoydataDF$Date.Time, abbreviate=FALSE)
buoydataDF$Weekday.Name <- weekdays(buoydataDF$Date.Time, abbreviate=FALSE)
buoydataDF$Week.month <- weeks(buoydataDF$Date.Time)
buoydataDF$Hour <- hour(buoydataDF$Date.Time)
buoydataDF$Min <- minute(buoydataDF$Date.Time)

buoydataDF$Day <- day(buoydataDF$Date)
buoydataDF$WeekNbr.Month <- ceiling(buoydataDF$Day/7)

#convert month name to factor
buoydataDF$Month.Name <- factor(buoydataDF$Month.Name,levels = month.name)
buoydataDF$Weekday.Name <- factor(buoydataDF$Weekday.Name
                                  , levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
buoydataDF$Date[1:5]

# lets remove 
buoydataDF <- buoydataDF[,!grepl("DATE_TIME",colnames(buoydataDF))]

# View(buoydataDF)

head(buoydataDF)

min(buoydataDF$Date)
max(buoydataDF$Date)




####################################################################
###
##
#  Visualization

EnsurePackage("ggridges")
browseVignettes("ggridges")

colnames(buoydataDF)

# g2 <- ggplot(buoydataDF, aes(x = pH))
# g2 + 
#   

###############
TempSeries.Plot <- ggplot( buoydataDF, aes(x = T_DEGC, y = Month.Name)) +
  geom_density_ridges_gradient( aes(fill = ..x..), scale = 3, color = "gray", alpha =.5) +
  scale_fill_gradientn(
    colours = c("blue", "green", "red"),
    name = "Temperature in Degrees C" )+
  labs(title = "Temperature in Degrees C") 

TempSeries.Plot + theme_classic()


##############

EnsurePackage("ggpubr")
# Basic density plot with mean line and marginal rug
ggdensity(buoydataDF, x = "pH", 
               fill = "#0073C2FF", color = "#0073C2FF",
               add = "mean", rug = FALSE)


pH.plot <- ggplot(buoydataDF, aes(x = pH))
pH.plot + geom_histogram(aes(y = ..density..), 
                   colour="white", fill="#00AFBB",position = "identity") +
  geom_density(alpha = 0.5, fill = "#E7B800") +
  geom_vline(aes(xintercept = mean(pH)), 
             linetype = "dashed", size = 0.6, color = "red")+ theme_classic()


###############
x <- tapply(buoydataDF$PRECIP_in, list(buoydataDF$Year, buoydataDF$Month.Name),sum)
x
barplot(x, main="Amount of Precipitation by Year and by Month", beside = TRUE,  ylim = c(0,300),
        col= brewer.pal(3, "BuPu"),legend.text = rownames((x)),args.legend=list(x="topright",bty="s"))



q1 <- buoydataDF %>% group_by(T_DEGC, Chl_ug_L) %>% summarise(pH.mean = mean(pH))
plot5 <- ggplot(q1) + aes(x=T_DEGC, y=Chl_ug_L) + geom_jitter(aes(color=pH.mean), size = 0.5) +
  scale_colour_gradient(low="blue",high="green") +
  xlab("Temperature in Degrees C") + ylab("Amount of Algae") + 
  ggtitle("Variation in Algae and pH by Temperature") + 
  theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot5+ theme_classic()

############################
##
#


buoydataDF$pH_z <- round((buoydataDF$pH - mean(buoydataDF$pH))/sd(buoydataDF$pH), 2)  # compute normalized pH
buoydataDF$pH_type <- ifelse(buoydataDF$pH_z < 0, "below", "above")  # above / below avg flag
buoydataDF <- buoydataDF[order(buoydataDF$pH_z), ]  # sort

# Diverging Barcharts
ggplot(buoydataDF, aes(x=Month.Name, y=pH_z, label=pH_z)) + 
  geom_bar(stat='identity', aes(fill=pH_type), width=.5)  +
  scale_fill_manual(name="pH", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title= "Diverging Bars") + 
  coord_flip()+ theme_classic()


g <- ggplot(buoydataDF, aes(pH))
g + geom_density(aes(fill=factor(Year)), alpha=0.8) + 
  labs(title="Density plot - pH across years", 
       x="pH Value",
       fill="Year") + theme_classic()

# g.violin <- ggplot(buoydataDF, aes(Month.Name,pH))
# g.violin + geom_violin() +
#   labs(title="Density plot - pH across years",
#        x="pH Value",
#        fill="Year") +  theme_classic()


# buoydataDF.2016 <- buoydataDF[buoydataDF$Year=="2016",]
# pH.2016 <- ggplot(buoydataDF.2016, aes(Month.Name, pH))
# pH.2016 + geom_boxplot(varwidth=T, fill="plum") + 
#   labs(title="Box plot - pH distribution in 2016", 
#        x="Month Name",
#        y="pH")
# 
# buoydataDF.2017 <- buoydataDF[buoydataDF$Year=="2017",]
# pH.2017 <- ggplot(buoydataDF.2017, aes(Month.Name, pH))
# pH.2017 + geom_boxplot(varwidth=T, fill="plum") + 
#   labs(title="Box plot - pH distribution in 2017", 
#        x="Month Name",
#        y="pH")

# buoydataDF.2018 <- buoydataDF[buoydataDF$Year=="2018",]
# pH.2018 <- ggplot(buoydataDF.2018, aes(Month.Name, pH))
# pH.2018 + geom_boxplot(varwidth=T, fill="plum") + 
#   labs(title="Box plot - pH distribution in 2018", 
#        x="Month Name",
#        y="pH")


p<-ggplot(buoydataDF, aes(x=Year, y=pH, fill=Year)) +
  geom_boxplot()
p


scale_fill_manual(values=c("#B0D9F3", "#549CCA", "#29526D")) +

pH.Yearly <- ggplot(buoydataDF, aes(Month.Name, pH))
 pH.Yearly + geom_boxplot(aes(fill=factor(Year))) +
   theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
   labs(title="Box plot - pH distribution",
        x="Month Name",
        y="pH") +  theme_classic()

 
 pH.Yearly <- ggplot(buoydataDF, aes(Year, pH))
 pH.Yearly + geom_boxplot(aes(fill=factor(Year))) +
   theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
   labs(title="Box plot - pH distribution",
        x="Month Name",
        y="pH") +  theme_classic()



ggplot(buoydataDF, aes(WeekNbr.Month,Weekday.Name,  fill = pH)) + 
  geom_tile(colour = "white") + 
  facet_grid(Year~Month.Name) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       fill="pH")+  theme_classic() 


unique(buoydataDF$WeekNbr.Month)
summary(buoydataDF$pH_z)





# gc()
