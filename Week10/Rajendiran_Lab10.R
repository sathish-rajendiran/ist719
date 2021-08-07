##
# Title: "Week 10 · Shiny: Making a Simple Interactive Dashboard in R"
# Author: "Sathish Kumar Rajendiran"
# Purpose: Lab10
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
EnsurePackage("shiny")
# EnsurePackage("igraph")
# EnsurePackage("rgl")
# EnsurePackage("ggplot2")
# EnsurePackage("maps")
# EnsurePackage("mapproj")
# EnsurePackage("raster")
# EnsurePackage("stringr")
# EnsurePackage("plotrix")
# EnsurePackage("ggmap")
# EnsurePackage("tmaptools")

# install.packages("devtools") # I guess you also need this
# devtools::install_github("ropensci/rnaturalearthhires")
# library("rnaturalearth")
# EnsurePackage("rnaturalearth")
# EnsurePackage("rnaturalearthhires")

# EnsurePackage("reshape2")
# EnsurePackage("RColorBrewer")
# EnsurePackage("mice") # missing data 
# EnsurePackage("visdat") # missing data
# EnsurePackage("wordcloud")
# EnsurePackage("dplyr")
# EnsurePackage("tm")


######################################################################################
##
##
##
#

server <- function(input,output){
  output$myPie <- renderPlot({
    pie(c(8,12,3), main = "hello world")
  })
}

ui <- fluidPage(
  mainPanel(plotOutput("myPie"))
)

shinyApp(ui,server)




