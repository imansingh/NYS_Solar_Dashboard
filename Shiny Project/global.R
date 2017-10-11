library(dplyr)
library(data.table)
library(ggplot2)
library(shiny)
library(DT)
library(shinydashboard)

# import solar data and convert fields
solar <- fread(file = "./solar.csv")
solar$`Reporting Period` <- as.Date(solar$`Reporting Period`)
solar$`Date Application Received` <- as.Date(solar$`Date Application Received`)
solar$`Date Completed` <- as.Date(solar$`Date Completed`)
summary(solar)

# create variable with colnames as choice
choice <- colnames(solar)[-1]

# ggplot layers
mapG = ggplot(data = nyCounties, aes(x = long, y = lat)) 

blankTheme = theme(axis.line=element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          legend.position="none",
                          panel.background=element_blank(),
                          panel.border=element_blank(),
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          plot.background=element_blank())
counties_filled = mapG + blankTheme + geom_polygon(aes(group = group, fill=group))
counties_no_fill = mapG + blankTheme + geom_polygon(aes(group = group), color = 'black',fill=NA)
all_points = counties_no_fill + geom_point(data = solar, aes(x = Longitude, y = Latitude))
densityMap = no_fill + stat_density_2d(aes(fill=..level..),geom ='polygon', alpha=.4)

