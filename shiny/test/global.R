
library(shiny)
library(shinydashboard)
library(data.table)

# import solar data and convert fields
solar <- fread(file = "solar.csv")
solar$Reporting.Period <- as.Date(solar$Reporting.Period)
solar$Date.Application.Received <- as.Date(solar$Date.Application.Received)
solar$Date.Completed <- as.Date(solar$Date.Completed)
solar = solar %>%
   mutate(Days.To.Complete = Date.Completed - Date.Application.Received)

print(max(as.numeric(solar$Days.To.Complete), na.rm = TRUE))
print(min(as.numeric(solar$Days.To.Complete), na.rm = TRUE))
#hist(solar$Days.To.Complete, na.rm = TRUE)
