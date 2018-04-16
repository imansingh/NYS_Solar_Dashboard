shinyServer(function(input, output){
  
  #  filter dataset based on input choices ranges selected
  get_solar_filtered <- reactive({
    
    solar_filtered = solar
    
    # Days to complete, date applied, date completed
    solar_filtered = solar_filtered %>% 
      filter(Days.To.Complete >= input$days_to_complete[1] | 
               is.na(Days.To.Complete)) %>%
      filter(Days.To.Complete <= input$days_to_complete[2] | 
               is.na(Days.To.Complete)) %>%
      filter(Date.Application.Received >= input$date_applied[1]) %>%
      filter(Date.Application.Received <= input$date_applied[2]) %>%
      filter(Date.Completed >= input$date_completed[1] | 
               is.na(Date.Completed)) %>%
      filter(Date.Completed <= input$date_completed[2] | 
               is.na(Date.Completed))

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
                   function(str) solar_filtered$Purchase.Type == str)
      if('None Listed' %in% input$purchase_type){
        mtx = cbind(mtx,
                       is.na(solar$Purchase.Type))
      }
      solar_filtered = solar_filtered[rowSums(mtx, na.rm = TRUE) > 0, ]
    } else solar_filtered = NULL

    # Affordable Solar
    if(!(is.null(input$affordable_solar))){
      mtx = sapply(input$affordable_solar,
                   function(str) solar_filtered$Affordable.Solar == str)
      solar_filtered = solar_filtered[rowSums(mtx) > 0,]
    } else solar_filtered = NULL

    # Green Jobs Green NY
    if(!(is.null(input$green_jobs))){
      mtx = sapply(input$green_jobs,
                   function(str) solar_filtered$Green.Jobs.Green.NY == str)
      if('None Listed' %in% input$green_jobs){
        mtx = cbind(mtx, is.na(solar_filtered$Green.Jobs.Green.NY))
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
        mtx = cbind(mtx, is.na(solar_filtered$Remote.Net.Metering))
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
      mtx = cbind(mtx, 
                  is.na(solar_filtered$Primary.Inverter.Manufacturer))
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
      mtx = cbind(mtx, 
                  is.na(solar_filtered$Primary.Inverter.Model.Number))
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
      mtx = cbind(mtx, 
                  is.na(solar_filtered$Primary.PV.Module.Manufacturer))
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
    
    # Project Cost
    if(input$project_cost_missing){
      solar_filtered = solar_filtered %>%
        filter(Project.Cost >= input$project_cost_min |
                 is.na(Project.Cost)) %>%
        filter(Project.Cost <= input$project_cost_max |
                 is.na(Project.Cost))
    } else {
      solar_filtered = solar_filtered %>%
        filter(Project.Cost >= input$project_cost_min) %>%
        filter(Project.Cost <= input$project_cost_max)
    }

    # Cost per kW, Cost per annual kWh
    solar_filtered = solar_filtered %>%
      filter(Total.Cost.Per.Nameplate.kW >= input$cost_per_kw[1] |
               is.na(Total.Cost.Per.Nameplate.kW)) %>%
      filter(Total.Cost.Per.Nameplate.kW <= input$cost_per_kw[2] |
               is.na(Total.Cost.Per.Nameplate.kW)) %>%
      filter(Total.Cost.Per.Annual.KW >= input$cost_per_annual_kwh[1] |
               is.na(Total.Cost.Per.Annual.KW)) %>%
      filter(Total.Cost.Per.Annual.KW <= input$cost_per_annual_kwh[2] |
               is.na(Total.Cost.Per.Annual.KW))

    # Incentive
    if(input$incentive_missing){
      solar_filtered = solar_filtered %>%
        filter(Incentive >= input$incentive_min |
                 is.na(Incentive)) %>%
        filter(Incentive <= input$incentive_max |
                 is.na(Incentive))
    } else {
      solar_filtered = solar_filtered %>%
        filter(Incentive >= input$incentive_min) %>%
        filter(Incentive <= input$incentive_max)
    }

    # Incentive per kW, Incentive per annual kWh
    solar_filtered = solar_filtered %>%
      filter(Incentive.Per.Nameplate.kW >= input$incentive_per_kw[1] |
               is.na(Incentive.Per.Nameplate.kW)) %>%
      filter(Incentive.Per.Nameplate.kW <= input$incentive_per_kw[2] + .5 |
               is.na(Incentive.Per.Nameplate.kW))  %>%
      filter(Incentive.Per.Annual.KW >= input$incentive_per_annual_kwh[1] |
               is.na(Incentive.Per.Annual.KW)) %>%
      filter(Incentive.Per.Annual.KW <= input$incentive_per_annual_kwh[2] |
               is.na(Incentive.Per.Annual.KW))

    # Net Cost
      solar_filtered = solar_filtered %>%
        filter(Net.Cost >= input$net_cost_min |
                 is.na(Net.Cost)) %>%
        filter(Net.Cost <= input$net_cost_max |
                 is.na(Net.Cost))

    # Net Cost per kW, Net Cost per annual kWh
    solar_filtered = solar_filtered %>%
      filter(Net.Cost.Per.Nameplate.kW >= input$net_cost_per_kw[1] |
               is.na(Net.Cost.Per.Nameplate.kW)) %>%
      filter(Net.Cost.Per.Nameplate.kW <= input$net_cost_per_kw[2] |
               is.na(Net.Cost.Per.Nameplate.kW)) %>%
      filter(Net.Cost.Per.Annual.KW >= input$net_cost_per_annual_kwh[1] |
               is.na(Net.Cost.Per.Annual.KW)) %>%
      filter(Net.Cost.Per.Annual.KW <= input$net_cost_per_annual_kwh[2] |
               is.na(Net.Cost.Per.Annual.KW))
    
    })
    
  output$cost_per_kw_slider = renderUI({
    sliderInput(
      'cost_per_kw',
      'Cost Per Installed Wattage ($ / kW)',
      min = round(min(solar$Total.Cost.Per.Nameplate.kW[
        solar$Project.Cost >= input$project_cost_min &
          solar$Project.Cost <= input$project_cost_max],
        na.rm = TRUE) - .1, 2),
      max = round(max(solar$Total.Cost.Per.Nameplate.kW[
        solar$Project.Cost >= input$project_cost_min &
          solar$Project.Cost <= input$project_cost_max],
        na.rm = TRUE) + .1, 2),
      value=c(min(solar$Total.Cost.Per.Nameplate.kW,
                  na.rm = TRUE),
              max(solar$Total.Cost.Per.Nameplate.kW,
                  na.rm = TRUE)))
  })

  output$cost_per_annual_kwh_slider = renderUI({
    sliderInput(
      'cost_per_annual_kwh',
      'Cost Per Annual Production ($ / kWh)',
      min = round(min(solar$Total.Cost.Per.Annual.KW[
        solar$Project.Cost >= input$project_cost_min &
          solar$Project.Cost <= input$project_cost_max],
        na.rm = TRUE) - .01, 2),
      max = round(max(solar$Total.Cost.Per.Annual.KW[
        solar$Project.Cost >= input$project_cost_min &
          solar$Project.Cost <= input$project_cost_max],
        na.rm = TRUE) + 1, 2),
      value=c(min(solar$Total.Cost.Per.Annual.KW,
                  na.rm = TRUE),
              max(solar$Total.Cost.Per.Annual.KW,
                  na.rm = TRUE) + 1))
  })

  
    # map with dot size, transparency and 'highlight variable' selected by user
    output$map <- renderPlot({
      map_base = 
        ny_counties_map_unfilled +
        guides(color = guide_legend(override.aes = list(size = 10)),
               shape = guide_legend(override.aes = list(size = 10))) +
        # geom_polygon(data = get_solar_filtered(),
        #              aes_string()) +
        scale_fill_gradient(low = 'blue',
                            # mid = 'yellow',
                            high = 'red',
                            trans = "log10",
                            labels = 'comma')

        
         
      
      if(input$map_color_highlight != 'None' & 
         input$map_shape_highlight != 'None')  {
        map_base = map_base +
          geom_point(data = get_solar_filtered(), 
                     aes_string(x = 'Longitude', 
                                y = 'Latitude', 
                                color = input$map_color_highlight,
                                shape = input$map_shape_highlight),
                     size = input$map_dotsize, 
                     alpha = input$map_transparency)
      } else if (input$map_color_highlight != 'None'){
        map_base = map_base +
          geom_point(data = get_solar_filtered(), 
                     aes_string(x = 'Longitude', 
                                y = 'Latitude', 
                                color = input$map_color_highlight),
                     size = input$map_dotsize, 
                     alpha = input$map_transparency)
      } else if (input$map_color_highlight != 'None'){
        map_base = map_base +
          geom_point(data = get_solar_filtered(), 
                     aes_string(x = 'Longitude', 
                                y = 'Latitude', 
                                color = input$map_shape_highlight),
                     size = input$map_dotsize, 
                     alpha = input$map_transparency)
      } else {
        map_base = map_base +
          geom_point(data = get_solar_filtered(), 
                     aes_string(x = 'Longitude', 
                                y = 'Latitude'),
                     size = input$map_dotsize, 
                     alpha = input$map_transparency)
      }
      
      map_base

      
      # if(input$map_color_highlight != 'None'){
      #   map_base + geom_point(data = get_solar_filtered(),
      #                         aes_string(x = 'Longitude', 
      #                                    y = 'Latitude',
      #                                    color = input$map_color_highlight))
      # } else map_base
      
      
      # if(input$map_highlight)
      
      
      # if(input$map_highlight %in% categorial_small){
      #   counties_no_fill + 
      #     geom_point(data = get_solar_filtered(), 
      #                aes_string(x = "Longitude", 
      #                           y = "Latitude", 
      #                           color = input$map_highlight), 
      #                size = input$map_dotsize, 
      #                alpha = input$map_transparency) + 
      #     guides(colour = guide_legend(override.aes = list(size=10))) +
      #     scale_fill_brewer(palette = 'YlOrRd') +
      #     coord_quickmap()
      # }
      
      })
    
    #show histogram or bar chart using ggplot2, with plot and 'highlight' variables selected by user
    output$histogram <- renderPlot({
      if(input$histogram_highlight != 'None'){
        if (input$histogram_selected %in% 
            c(categorial_small, categorical_medium)){
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$histogram_selected)) + 
            geom_bar(aes_string(# color = 'black',
                                fill=input$histogram_highlight))
                                # position = position_dodge()))
        } else {
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$histogram_selected)) + 
            geom_histogram(aes_string(fill=input$histogram_highlight))
        }
      } else {
        if (input$histogram_selected %in% 
            c(categorial_small, categorical_medium)){
          ggplot(data = get_solar_filtered()) + 
                 # aes_string(x = input$histogram_selected)) + 
            geom_bar(aes_string(x = input$histogram_selected))
        } else {
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$histogram_selected)) + 
            geom_histogram()
        }
      }
      
      
    })
    # show scatterplot using ggplot2, with x, y, and 'highlight' variables selected by user
    output$scatterplot <- renderPlot({
      if(input$scatter_color_highlight != 'None' & 
         input$scatter_shape_highlight != 'None'){
        if (input$regression == 1){
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(aes_string(color = input$scatter_color_highlight,
                                  shape = input$scatter_shape_highlight),
                       size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) + 
            geom_smooth(color = 'red', na.rm = TRUE) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        } else {
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(aes_string(color = input$scatter_color_highlight,
                                  shape = input$scatter_shape_highlight),
                       size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        }
      } else if(input$scatter_color_highlight != 'None'){
        if (input$regression == 1){
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(aes_string(color = input$scatter_color_highlight),
                       size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) + 
            geom_smooth(color = 'red', na.rm = TRUE) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        } else {
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(aes_string(color=input$scatter_color_highlight),
                       size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        }
      } else if(input$scatter_shape_highlight != 'None'){
        if (input$regression == 1){
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(aes_string(shape = input$scatter_shape_highlight),
                       size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) + 
            geom_smooth(color = 'red', na.rm = TRUE) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        } else {
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(aes_string(shape = input$scatter_shape_highlight),
                       size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        }
      } else {
        if (input$regression == 1){
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) + 
            geom_smooth(color = 'red', na.rm = TRUE) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        } else {
          ggplot(data = get_solar_filtered(), 
                 aes_string(x = input$scatter_x , 
                            y = input$scatter_y)) + 
            geom_point(size = input$scatter_dotsize, 
                       alpha = input$scatter_transparency) +
            guides(color = guide_legend(override.aes = list(size = 10)),
                   shape = guide_legend(override.aes = list(size = 10)))
        }
      } 
      
    })
    #show boxplot using ggplot2, with plot and 'highlight' variables selected by user
    output$boxplot <- renderPlot({
      
      ggplot(data = get_solar_filtered(), 
             aes_string(x = input$boxplot_x, 
                        y = input$boxplot_y)) +
        geom_boxplot()
    }) 
    
    # show data using DataTable
    output$filtered_table <- DT::renderDataTable({
      req(input$cost_per_kw)
      datatable(get_solar_filtered()[-1],
                rownames=FALSE,
                options = list(scrollX = TRUE,
                               scrollY = TRUE,
                               autoWidth = TRUE)) # %>%
        # formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    output$full_table <- DT::renderDataTable({
      datatable(solar[-1],
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
