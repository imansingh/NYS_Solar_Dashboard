library(dplyr)
library(tidyr)
library(data.table)

solar = fread('solar_raw.csv')

#rename all the variables with spaces
solar = solar %>% 
  rename(
    Reporting.Period = `Reporting Period`, 
    Project.Number = `Project Number`, 
    Zip.Code = `Zip Code`, 
    Program.Type = `Program Type`, 
    Electric.Utility = `Electric Utility`,
    Purchase.Type = `Purchase Type`, 
    Date.Application.Received = `Date Application Received`,
    Date.Completed = `Date Completed`, 
    Project.Status = `Project Status`,                      
    Total.Inverter.Quantity = `Total Inverter Quantity`,
    Primary.Inverter.Model.Number = `Primary Inverter Model Number`,
    Total.PV.Module.Quantity = `Total PV Module Quantity`,
    PV.Module.Model.Number = `PV Module Model Number`,
    Project.Cost = `Project Cost`, 
    Total.Nameplate.kW.DC = `Total Nameplate kW DC`,               
    Expected.KWh.Annual.Production = `Expected KWh Annual Production`,
    Primary.Inverter.Manufacturer = `Primary Inverter Manufacturer`,
    Remote.Net.Metering = `Remote Net Metering`,
    Affordable.Solar = `Affordable Solar`,
    Community.Distributed.Generation = `Community Distributed Generation`,
    Primary.PV.Module.Manufacturer = `Primary PV Module Manufacturer`,
    Green.Jobs.Green.NY = `Green Jobs Green New York Participant`
  )

#removing '$' from $Incentive column name
solar = rename(solar, Incentive = `$Incentive`)

# adding missing latitude and longitude for row 29826
solar[23177,30] = "Three Mile Bay, NY 13693\r(44.0814, -76.1983)"

# removing parentheses from two of the Program.Type values
solar$Program.Type = gsub('\\(', '- ', solar$Program.Type)
solar$Program.Type = gsub('\\)', '', solar$Program.Type)

#removing '$' from values in Incentives and Project.Cost columns, and changing them to numeric
solar$Incentive = as.numeric(gsub('\\$|,', '', solar$Incentive))
solar$Project.Cost = as.numeric(gsub('\\$|,', '', solar$Project.Cost))

#separating 'location 1' field into Location [city, state zip] and Coordinates (latitude, longitude)
solar = separate(solar, 'Location 1', into = c('Location', 'Coordinates'), sep="\\(")

#removing closing parentheses from Coordinates column
solar$Coordinates <- gsub('\\)', '', solar$Coordinates)

#replacing 'comma space' with colon in Coordinates column
solar$Coordinates <- gsub(', ', ':', solar$Coordinates)

#separating 'Coordinates' field into latitude and longitude - keep original in case I want to ever use GoogleVis
solar = separate(solar, 'Coordinates', into = c('Latitude', 'Longitude'), sep=":", remove = FALSE)

#change Latitude and Longitude columns to numeric
solar$Latitude = as.numeric(solar$Latitude)
solar$Longitude = as.numeric(solar$Longitude)

#remove columns that are not needed
solar = solar %>%
  select(-State, -Location)

# convert empty string to NA
#function to convert empty string to NA
empty_to_na = function(string){
  if(is.null(string)){
    string = NA
  } else if(string == ''){
    string = NA
  }
  return(string)
}

# check for empty strings 
sapply(solar, function(x) sum(x == ''))

solar = solar %>%
  mutate(Purchase.Type = ifelse(Purchase.Type == '', NA, Purchase.Type)) %>%
  mutate(Contractor = ifelse(Contractor == '', NA, Contractor)) %>%
  mutate(Primary.Inverter.Manufacturer = 
           ifelse(Primary.Inverter.Manufacturer == '', 
                  NA, Primary.Inverter.Manufacturer)) %>%
  mutate(Primary.Inverter.Model.Number = 
           ifelse(Primary.Inverter.Model.Number == '',
                  NA, Primary.Inverter.Model.Number)) %>%
  mutate(Primary.PV.Module.Manufacturer = 
           ifelse(Primary.PV.Module.Manufacturer == '', 
                  NA, Primary.PV.Module.Manufacturer)) %>%
  mutate(PV.Module.Model.Number = 
           ifelse(PV.Module.Model.Number == '',
                  NA, PV.Module.Model.Number)) %>%
  mutate(Remote.Net.Metering = 
           ifelse(Remote.Net.Metering == '', 
                  NA, Remote.Net.Metering)) %>%
  mutate(Green.Jobs.Green.NY = 
           ifelse(Green.Jobs.Green.NY == '', 
                  NA, Green.Jobs.Green.NY))


# converting 'Reporting Period' to date object
solar$Reporting.Period = as.Date(solar$Reporting.Period, "%m/%d/%Y")

# converting 'Date Appliation Received' to date object
solar$Date.Application.Received = as.Date(solar$Date.Application.Received, "%m/%d/%Y")

# converting 'Date Completed' to date object
solar$Date.Completed = as.Date(solar$Date.Completed, "%m/%d/%Y")


#add columns derived from data
solar = solar %>%
  mutate(
    Days.To.Complete = as.numeric(Date.Completed) - 
      as.numeric(Date.Application.Received),
    Net.Cost = Project.Cost - Incentive,
    # Year.Completed = format(Date.Completed, '%Y'), 
    # Year.Applied=format(Date.Application.Received, '%Y'),
    Incentive.Per.Nameplate.kW = Incentive / Total.Nameplate.kW.DC,
    Incentive.Per.Annual.KW = Incentive / Expected.KWh.Annual.Production,
    Total.Cost.Per.Nameplate.kW = Project.Cost / Total.Nameplate.kW.DC,
    Total.Cost.Per.Annual.KW = Project.Cost / Expected.KWh.Annual.Production,
    Net.Cost.Per.Nameplate.kW = Net.Cost / Total.Nameplate.kW.DC,
    Net.Cost.Per.Annual.KW = Net.Cost / Expected.KWh.Annual.Production)


write.csv(x = solar, file = 'solar.csv')

