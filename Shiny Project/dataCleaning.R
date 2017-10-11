library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

solar = Solar_Electric_Programs_Reported_by_NYSERDA_Beginning_2000

# adding missing latitude and longitude for row 29826
solar[29826,30] = "Three Mile Bay, NY 13693\r(44.0814, -76.1983)"

# converting 'Reporting Period' to date object
solar$`Reporting Period` = as.Date(solar$`Reporting Period`, "%m/%d/%y")

# converting 'Date Appliation Received' to date object
solar$`Date Application Received` = as.Date(solar$`Date Application Received`, "%m/%d/%y")

# converting 'Date Completed' to date object
solar$`Date Completed` = as.Date(solar$`Date Completed`, "%m/%d/%y")

#removing '$' from $Incentives column name
colnames(solar)[23] = 'Incentive'

#removing '$' from values in Incentives column, and changing them to numeric
solar$'Incentive' <- as.numeric(gsub('\\$|,', '', solar$'Incentive'))

#removing '$' from values in Project Cost column, and changing them to numeric
solar$'Project Cost' <- as.numeric(gsub('\\$|,', '', solar$'Project Cost'))

#separating 'location 1' field into Location [city, state zip] and Coordinates (latitude, longitude)
solar = separate(solar, 'Location 1', into = c('Location', 'Coordinates'), sep="\\(")

#removing '$' from values in Project Cost column, and changing them to numeric
solar$'Project Cost' <- as.numeric(gsub('\\$|,', '', solar$'Project Cost'))

#removing closing parentheses from Coordinates column
solar$Coordinates <- gsub('\\)', '', solar$Coordinates)

#replacing 'comma space' with colon in Coordinates column
solar$Coordinates <- gsub(', ', ':', solar$Coordinates)

#separating 'Coordinates' field into latitude and longitude - keep original in case I want to ever use GoogleVis
solar = separate(solar, 'Coordinates', into = c('Latitude', 'Longitude'), sep=":", remove = FALSE)

#change Latitude and Longitude columns to numeric
solar$Latitude = as.numeric(solar$Latitude)
solar$Longitude = as.numeric(solar$Longitude)

#rename

#copying database in case something goes wrong 
solar1 = solar

solar = solar1

#
counties = map_data("county")
nyCounties = filter(counties, region == 'new york')


