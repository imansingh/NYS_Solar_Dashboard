shinyServer(function(input, output){
  
#  filter dataset based on input choices ranges selected
  get_solar_filtered <- reactive({
    
    solar_filtered = solar
    
    # Days to complete, date applied, date completed
    solar_filtered = solar_filtered %>% 
      filter(Days.To.Complete >= input$days_to_complete[1] | is.na(Days.To.Complete)) %>%
      filter(Days.To.Complete <= input$days_to_complete[2] | is.na(Days.To.Complete)) %>%
      filter(Date.Application.Received >= input$date_applied[1]) %>%
      filter(Date.Application.Received <= input$date_applied[2]) %>%
      filter(Date.Completed >= input$date_completed[1] | is.na(Date.Completed)) %>%
      filter(Date.Completed <= input$date_completed[2] | is.na(Date.Completed))

    # Project Status
    if (length(input$project_status) != 2) {
      if (1 %in% input$project_status){
        solar_filtered = solar_filtered %>% 
          filter(Project.Status == 'Complete')
        } else if (2 %in% input$project_status){
          solar_filtered = solar_filtered %>%
            filter(Project.Status == 'Pipeline')
        } else solar_filtered = NULL
    }
    
    # County
    if(!(is.null(input$counties))){
      mtx = sapply(input$counties,
                  function(str)
                    solar_filtered$County == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = solar_filtered
    
    # City
    if(!(is.null(input$cities))){
      mtx = sapply(input$cities,
                   function(str)
                     solar_filtered$City == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = solar_filtered
    
    # Zip Code
    if(!(is.null(input$zip_codes))){
      mtx = sapply(input$zip_codes,
                   function(str)
                     solar_filtered$Zip.Code == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = solar_filtered
    
    # Sector
    if(!(is.null(input$sector))){
      mtx = sapply(input$sector,
                   function(str)
                     solar_filtered$Sector == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = NULL
    
    # Program Type
    if(!(is.null(input$program_type))){
      mtx = sapply(input$program_type,
                   function(str)
                     solar_filtered$Program.Type == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = NULL

    # Solicitation
    if(!(is.null(input$solicitation))){
      mtx = sapply(input$solicitation,
                   function(str)
                           solar_filtered$Solicitation == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = solar_filtered
    
    # Purchase Type
    if(!(is.null(input$purchase_type))){
      mtx = sapply(input$purchase_type,
                   function(str)
                           solar_filtered$Purchase.Type == str)
      if('None Listed' %in% input$purchase_type){
        mtx = cbind(mtx,
                       is.na(solar_filtered$Purchase.Type))
      }
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0, ]
    } else solar_filtered = NULL
    
    # Affordable Solar
    if(!(is.null(input$affordable_solar))){
      mtx = sapply(input$affordable_solar,
                   function(str)
                     solar_filtered$Affordable.Solar == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = NULL
    
    # Green Jobs Green NY
    if(!(is.null(input$green_jobs))){
      mtx = sapply(input$green_jobs,
                   function(str)
                           solar_filtered$Green.Jobs.Green.NY == str)
      if('None Listed' %in% input$green_jobs){
        mtx = cbind(mtx,
                       is.na(solar_filtered$Green.Jobs.Green.NY))
      }
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else solar_filtered = NULL
    
    # Contractor
    if((!is.null(input$contractor)) & input$contractor_missing){
      mtx = sapply(input$contractor,
                   function(str)
                     solar_filtered$Contractor == str)
      mtx = cbind(mtx, is.na(solar_filtered$Contractor))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!is.null(input$contractor)){
      mtx = sapply(input$contractor,
                   function(str)
                     solar_filtered$Contractor == str)
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!input$contractor_missing){
      mtx = matrix(!is.na(solar_filtered$Contractor))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    }
    else solar_filtered = solar_filtered
    
    # Electric Utility
    if(!(is.null(input$utility))){
      mtx = sapply(input$utility,
                   function(str)
                     solar_filtered$Electric.Utility == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = solar_filtered
    
    # Remote Net Metering
    if(!(is.null(input$remote_net_metering))){
      mtx = sapply(input$remote_net_metering,
                   function(str)
                     solar_filtered$Remote.Net.Metering == str)
      if('None Listed' %in% input$remote_net_metering){
        mtx = cbind(mtx,
                    is.na(solar_filtered$Remote.Net.Metering))
      }
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else solar_filtered = NULL
    
    # Community Distributed Generation
    if(!(is.null(input$community_distributed_generation))){
      mtx = sapply(input$community_distributed_generation,
                   function(str)
                     solar_filtered$Community.Distributed.Generation == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = NULL
    
    # Primary Inverter Manufacturer
    if((!is.null(input$inverter_manufacturer)) & 
       input$inverter_manufacturer_missing){
      mtx = sapply(input$inverter_manufacturer,
                   function(str)
                     solar_filtered$Primary.Inverter.Manufacturer == str)
      mtx = cbind(mtx, is.na(solar_filtered$Primary.Inverter.Manufacturer))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!is.null(input$inverter_manufacturer)){
      mtx = sapply(input$inverter_manufacturer,
                   function(str)
                     solar_filtered$Primary.Inverter.Manufacturer == str)
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!input$inverter_manufacturer_missing){
      mtx = matrix(!is.na(solar_filtered$Primary.Inverter.Manufacturer))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    }
    else solar_filtered = solar_filtered

    # Primary Inverter Model
    if((!is.null(input$inverter_model)) & 
       input$inverter_model_missing){
      mtx = sapply(input$inverter_model,
                   function(str)
                     solar_filtered$Primary.Inverter.Model.Number == str)
      mtx = cbind(mtx, is.na(solar_filtered$Primary.Inverter.Model.Number))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!is.null(input$inverter_model)){
      mtx = sapply(input$inverter_model,
                   function(str)
                     solar_filtered$Primary.Inverter.Model.Number == str)
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!input$inverter_model_missing){
      mtx = matrix(!is.na(solar_filtered$Primary.Inverter.Model.Number))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    }
    else solar_filtered = solar_filtered
    
    # Total Inverter Quantity
    if(input$inverter_quantity_missing){
      solar_filtered = solar_filtered %>%
        filter(Total.Inverter.Quantity >= input$inverter_quantity_min |
                 is.na(Total.Inverter.Quantity)) %>%
        filter(Total.Inverter.Quantity <= input$inverter_quantity_max |
                 is.na(Total.Inverter.Quantity))
    } else {
      solar_filtered = solar_filtered %>%
        filter(Total.Inverter.Quantity >= input$inverter_quantity_min) %>%
        filter(Total.Inverter.Quantity <= input$inverter_quantity_max)
    }
    
    # Primary PV Module Manufacturer
    if((!is.null(input$pv_manufacturer)) & 
       input$pv_manufacturer_missing){
      mtx = sapply(input$pv_manufacturer,
                   function(str)
                     solar_filtered$Primary.PV.Module.Manufacturer == str)
      mtx = cbind(mtx, is.na(solar_filtered$Primary.PV.Module.Manufacturer))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!is.null(input$pv_manufacturer)){
      mtx = sapply(input$pv_manufacturer,
                   function(str)
                     solar_filtered$Primary.PV.Module.Manufacturer == str)
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!input$pv_manufacturer_missing){
      mtx = matrix(!is.na(solar_filtered$Primary.PV.Module.Manufacturer))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    }
    else solar_filtered = solar_filtered
    
    # Primary PV Module Model
    if((!is.null(input$pv_model)) & 
       input$pv_model_missing){
      mtx = sapply(input$pv_model,
                   function(str)
                     solar_filtered$PV.Module.Model.Number == str)
      mtx = cbind(mtx, is.na(solar_filtered$PV.Module.Model.Number))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!is.null(input$pv_model)){
      mtx = sapply(input$pv_model,
                   function(str)
                     solar_filtered$PV.Module.Model.Number == str)
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    } else if(!input$pv_model_missing){
      mtx = matrix(!is.na(solar_filtered$PV.Module.Model.Number))
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0,]
    }
    else solar_filtered = solar_filtered
    
    # Total PV Module Quantity
    if(input$pv_quantity_missing){
      solar_filtered = solar_filtered %>%
        filter(Total.PV.Module.Quantity >= input$pv_quantity_min |
                 is.na(Total.PV.Module.Quantity)) %>%
        filter(Total.PV.Module.Quantity <= input$pv_quantity_max |
                 is.na(Total.PV.Module.Quantity))
    } else {
      solar_filtered = solar_filtered %>%
        filter(Total.PV.Module.Quantity >= input$pv_quantity_min) %>%
        filter(Total.PV.Module.Quantity <= input$pv_quantity_max)
    }
    
    # PV Module Wattage
    solar_filtered = solar_filtered %>%
      filter(Total.Nameplate.kW.DC >= input$pv_wattage_min) %>%
      filter(Total.Nameplate.kW.DC <= input$pv_wattage_max)
    
    # Expected Annual Production
    solar_filtered = solar_filtered %>%
      filter(Expected.KWh.Annual.Production >= input$annual_kwh_min) %>%
      filter(Expected.KWh.Annual.Production <= input$annual_kwh_max)
    
      
      
      
      
      
    
    
  })
    
    
    # %>%
  #     
  #   print(nrow(get_filtered_solar()))
  # })

  
  # #filter dataset based on completed and/or pipeline projects
  # get_filtered_solar <- reactive({
  #   if (length(input$project_status) == 2) {
  #     filter(solar, Project.Status == 'Complete' | Project.Status == 'Pipeline')
  #     } else if (1 %in% input$project_status){
  #         filter(solar, Project.Status == 'Complete')
  #       } else if (2 %in% input$project_status){
  #           filter(solar, Project.Status == 'Pipeline')}
  #   
  # })

    # show map using ggplot2, with dot size, transparency and 'highlight variable' selected by user
    output$map <- renderPlot({
      counties_no_fill + geom_point(data = filteredSolar1(), aes_string(x = "Longitude", y = "Latitude", color = input$selected), size = input$dotSize, alpha = input$transparency) + coord_quickmap()
      })
    
    #show histogram or bar chart using ggplot2, with plot and 'highlight' variables selected by user
    output$histogram <- renderPlot({
      if (input$selectedH %in% bar_plot_variables){
        ggplot(data = filteredSolar1(), aes_string(x = input$selectedH)) + geom_bar(aes_string(fill=input$selected))
      } else {
        ggplot(data = filteredSolar1(), aes_string(x = input$selectedH)) + geom_histogram(aes_string(fill=input$selected))
      }
    })
    # show scatterplot using ggplot2, with x, y, and 'highlight' variables selected by user
    output$scatterplot <- renderPlot({
      if (input$regression == 1){
      ggplot(data = filteredSolar1(), aes_string(x = input$selectedX , y = input$selectedY)) + geom_point(aes_string(color=input$selected)) + geom_smooth(color = 'red')
      } else {
      ggplot(data = filteredSolar1(), aes_string(x = input$selectedX , y = input$selectedY)) + geom_point(aes_string(color=input$selected))
      }
    })
    #show boxplot using ggplot2, with plot and 'highlight' variables selected by user
    output$boxplot <- renderPlot({
        ggplot(data = filteredSolar1(), aes_string(x = input$selectedB, y = input$selected)) + geom_boxplot()
    }) 
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(get_solar_filtered()[-1], 
                  rownames=FALSE,
                  options = list(scrollX = TRUE, 
                                 scrollY = TRUE, 
                                 autoWidth = TRUE)) # %>%
        # formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    # show statistics using infoBox
  #output$maxBox <- renderInfoBox({
   #     max_value <- max(solar[,input$selected])
        #max_state <- 
        #    solar$state.name[solar[,input$selected] == max_value]
        #infoBox(max_state, max_value, icon = icon("hand-o-up"))
   # })
    #output$minBox <- renderInfoBox({
     #   min_value <- min(state_stat[,input$selected])
      #  min_state <- 
       #     state_stat$state.name[state_stat[,input$selected] == min_value]
        #infoBox(min_state, min_value, icon = icon("hand-o-down"))
    #})
    #output$avgBox <- renderInfoBox(
     #   infoBox(paste("AVG.", input$selected),
      #          mean(state_stat[,input$selected]), 
       #         icon = icon("calculator"), fill = TRUE))
})
