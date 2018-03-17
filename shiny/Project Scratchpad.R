ggplot()

ggplot(solar, aes(Date.Completed, Views)) + geom_line() +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")

, ticks = c(.01, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

solar1 = solar
solar1 = mutate(solar1, Year.Applied=format(Date.Application.Received, '%Y'))

counties_filled = mapG + blankTheme + geom_polygon(data=test, aes(group = group, fill=Project.Cost))
counties_filled

solar_counties <- solar %>% 
  group_by(County)
summarize(solar_counties, mean(Incentive))

test = merge(nyCounties, solar_counties, by.x = 'subregion', by.y = 'County')
?merge

solar_counties$County = tolower(solar_counties$County)
counties_no_fill + geom_polygon(data=test, aes(fill=Project.Cost))


#############

gvisGeoChart(solar, locationvar = 'Coordinates')

M1 <- gvisMap(solar, "Coordinates",
              options=list(
                mapType='hybrid', useMapTypeControl=TRUE,
                width=800,height=400))

plot(M1) 


M1 <- gvisMap(Andrew, "LatLong", "Tip",
              options=list(showTip = TRUE, mapType='hybrid', useMapTypeControl=TRUE,
                           width=800,height=400))

plot(M1) 

M2 = gvisGeoChart(solar, 'Location', 'Project Cost',
                  options=list(region="US-NY", displayMode="regions", 
                               resolution="provinces",
                               width="auto", height="auto"))

plot(M2)
counties = map_data("county")

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
filled = blankTheme + geom_polygon(aes(group = group, fill=group))
no_fill = blankTheme + geom_polygon(aes(group = group), color = 'black',fill=NA)
counties_no_fill + geom_point(data = solar, aes(x = Longitude, y = Latitude), size = .1, alpha = .01)


densityMap = no_fill + stat_density_2d(aes(fill=..level..),geom ='polygon', alpha=.4)
ggplot(data = solar, aes(x = `Project Cost`, y = Incentive)) + geom_point(aes(color=`Date Application Received`))
processingTime = solar$`Date Completed`-solar$`Date Application Received`
no_fill
filled
all_points
densityMap
max(solar$`Date Application Received`)
min(solar$`Date Application Received`)
max(solar$`Date Completed`)
min(solar$`Date Completed`)
dateRange = c("2017-07-09", "2017-08-16")
dateRange = as.Date(dateRange)
filteredSolar1 = solar %>% filter(`Date Application Received` >= dateRange[1] & `Date Application Received` <= dateRange[2])
write.csv(solar, file="solar.csv")
filteredSolar1 = solar %>% filter(`Date Completed` == 'NA' & `Date Application Received` <= dateRange[2])
write.csv(solar_all_variables, file="solar_all_variables.csv")

numVec = c(1,2)
if (1 %in% numVec){print(numVec)}
if (1 %in% numVec){
  filteredSolar = filter(solar, `Project Status`== 'Complete')}

filteredSolar1 = filter(if (1 %in% numVec){solar$`Project Status`=='Completed'})

solar1 = solar
summary(solar1)
rsconnect::setAccountInfo(name='imansingh',
                          token='CAC419058A6622729D12A8557E5C0460',
                          secret='JFAwk+i9s2I96egDz2v1bVd/+6S+b/dRqpfrxNtD')
rsconnect::deployApp('/Users/imansingh/Documents/NYC Data Science Academy/Shiny Project')
solar$Sector <- as.factor(solar$Sector)
solar$`Program Type` <- as.factor(solar$`Program Type`)
solar$City <- as.factor(solar$City)
solar$County <- as.factor(solar$County)
solar$`Zip Code` <- as.factor(solar$`Zip Code`)
solar$`Electric Utility` <- as.factor(solar$`Electric Utility`)
solar$`Purchase Type` <- as.factor(solar$`Purchase Type`)
solar$`Project Status` <- as.factor(solar$`Project Status`)
solar$Contractor <- as.factor(solar$Contractor)
solar$`Primary Inverter Manufacturer` <- as.factor(solar$`Primary Inverter Manufacturer`)
solar$`Primary PV Module Manufacturer` <- as.factor(solar$`Primary PV Module Manufacturer`)
levels(solar$Sector)
levels(solar$`Program Type`)

solar2 = read.csv('./solar_all_variables.csv')
getwd()
summary(solar2)

processingTime
solar3 = group_by(solar, Contractor)
contractorDays = summarise(group_by(solar,Contractor), testMean = mean(Days.To.Complete))
filled = mapG + blankTheme + geom_polygon(aes(group = group, fill=solar4$testMean))
filled
solar1 = solar
nyCounties1 = nyCounties

summary(solar)

[13] ""             ""                       
[15] ""                        "Contractor"                           
[17] ""         ""              
[19] "Primary Inverter Model Number"         "P"       
[21] ""              "PV Module Model Number"               
[23] ""                          "Incentive"                            
[25] ""                 ""       
[27] "Remote Net Metering"                   "Affordable Solar"                     
[29] "Community Distributed Generation"      "Green Jobs Green New York Participant"
[31] "Location"                              "Coordinates"                          
[33] "Latitude"                              "Longitude"  )
names(solar)
solar$Sector <- as.factor(solar$Sector)
solar$`Program Type` <- as.factor(solar$`Program Type`)
solar$City <- as.factor(solar$City)
solar$County <- as.factor(solar$County)
solar$`Zip Code` <- as.factor(solar$`Zip Code`)
solar$`Electric Utility` <- as.factor(solar$`Electric Utility`)
solar$`Purchase Type` <- as.factor(solar$`Purchase Type`)
solar$`Project Status` <- as.factor(solar$`Project Status`)
solar$Contractor <- as.factor(solar$Contractor)
solar$`Primary Inverter Manufacturer` <- as.factor(solar$`Primary Inverter Manufacturer`)
solar$`Primary PV Module Manufacturer` <- as.factor(solar$`Primary PV Module Manufacturer`)

choice

#copying database in case something goes wrong 
solar1 = solar

solar = solar1

#
counties = map_data("county")
nyCounties = filter(counties, region == 'new york')

solar$Reporting.Period <- as.Date(solar$Reporting.Period)
solar$Date.Application.Received <- as.Date(solar$Date.Application.Received)
solar$Date.Completed <- as.Date(solar$Date.Completed)
summary(solar)
solar = select(solar, -1, -2)

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot() + 
  stat_summary(fun.data = give.n, geom = "text")
bar_plot_variables = c("Sector", "Program.Type", "Electric.Utility", "Purchase.Type", "Project.Status", "Remote.Net.Metering", "Affordable.Solar", "Community.Distributed.Generation", "Green.Jobs.Green.New.York.Participant", "County", "Contractor")                

summary(solar)
