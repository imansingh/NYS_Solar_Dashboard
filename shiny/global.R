library(dplyr)
library(ggplot2)
library(scales)
library(shiny)
library(DT)
library(maps)
library(shinydashboard)
library(data.table)
library(mgcv)

# import solar data and convert fields
# setwd("~/Docuunments/GitHub/NYS_Solar_Dashboard/shiny")
solar <- fread(file = "solar.csv")

solar$Reporting.Period <- as.Date(solar$Reporting.Period)
solar$Date.Application.Received <- as.Date(solar$Date.Application.Received)
solar$Date.Completed <- as.Date(solar$Date.Completed)
solar = solar %>%
  mutate(Days.To.Complete = Date.Completed - Date.Application.Received)


# categorizing the different types of variables, for plotting purposes:

location = c("City", "County", "Zip.Code") 

categorical_large = 
  c("Solicitation" , "Contractor", "Primary.Inverter.Manufacturer", 
    "Primary.Inverter.Model.Number", "Primary.PV.Module.Manufacturer", 
    "PV.Module.Model.Number")

categorical_medium = 
  c("Electric.Utility", "Purchase.Type", "Program.Type")

categorial_small = 
  c("Green.Jobs.Green.NY",  "Sector", "Project.Status")

quantitative =
  c("Total.Inverter.Quantity", "Total.PV.Module.Quantity", "Project.Cost",
    "Incentive", "Total.Nameplate.kW.DC", "Expected.KWh.Annual.Production",
    "Net.Cost", "Incentive.Per.Nameplate.kW", "Incentive.Per.Annual.KW", 
    "Total.Cost.Per.Nameplate.kW", "Total.Cost.Per.Annual.KW", 
    "Net.Cost.Per.Nameplate.kW", "Net.Cost.Per.Annual.KW")

time = 
  c("Date.Application.Received", "Date.Completed", 
    "Days.To.Complete")

boolean = 
  c("Remote.Net.Metering", "Affordable.Solar", 
    "Community.Distributed.Generation")

choice <- colnames(solar)[-1]
choice_map_scatter = c("Sector", "Program.Type", "Electric.Utility", "Purchase.Type", "Date.Application.Received", "Date.Completed", "Days.To.Complete", "Project.Status", "Total.PV.Module.Quantity", "Project.Cost", "Incentive", "Net.Cost",
               "Total.Nameplate.kW.DC", "Expected.KWh.Annual.Production", "Remote.Net.Metering", "Affordable.Solar", "Community.Distributed.Generation", "Green.Jobs.Green.New.York.Participant")                 
choice_hist_box = c("Sector", "Program.Type", "Electric.Utility", "Purchase.Type", "Date.Application.Received", "Date.Completed", "Days.To.Complete", "Project.Status", "Total.PV.Module.Quantity", "Project.Cost", "Incentive", "Net.Cost",
                "Total.Nameplate.kW.DC", "Expected.KWh.Annual.Production", "Remote.Net.Metering", "Affordable.Solar", "Community.Distributed.Generation", "Green.Jobs.Green.New.York.Participant", "County", "Contractor")                
bar_plot_variables = c("Sector", "Program.Type", "Electric.Utility", "Purchase.Type", "Project.Status", "Remote.Net.Metering", "Affordable.Solar", "Community.Distributed.Generation", "Green.Jobs.Green.New.York.Participant", "County", "Contractor")

# ggplot map layers
ny_counties = filter(map_data('county'), region == 'new york')
ny_map = ggplot(data = ny_counties, aes(x = long, y = lat)) + coord_quickmap()
blank_theme = theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="bottom",
                   # legend.title = element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())
# counties_filled = mapG + blankTheme + geom_polygon(aes(group = group, fill=group))
ny_counties_map_unfilled = 
  ny_map + 
  blank_theme + 
  geom_polygon(aes(group = group), 
               color = 'black',
               fill=NA)
all_points = ny_counties_map_unfilled + geom_point(data = solar, aes(x = Longitude, y = Latitude))
densityMap = ny_counties_map_unfilled + stat_density_2d(aes(fill=..level..),geom ='polygon', alpha=.4)

