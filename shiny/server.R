shinyServer(function(input, output){
  
  #filter dataset based on date ranges selected
  filteredSolar <- reactive({
    solar %>% filter(`Date.Application.Received` >= input$dateApplied[1] & `Date.Application.Received` <= input$dateApplied[2]) %>%
      filter((`Date.Completed` >= input$dateCompleted[1] & `Date.Completed` <= input$dateCompleted[2]) | is.na(solar$`Date.Completed`))
  })

  
  #filter dataset based on completed and/or pipeline projects
  filteredSolar1 <- reactive({
    if (length(input$completedPipeline) == 2) {
      filter(filteredSolar(), `Project.Status`== 'Complete' | `Project.Status` == 'Pipeline')
      } else if (1 %in% input$completedPipeline){
          filter(filteredSolar(), `Project.Status`== 'Complete')
        } else if (2 %in% input$completedPipeline){
            filter(filteredSolar(), `Project.Status`== 'Pipeline')}
    
  })

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
        datatable(filteredSolar1()[-1], rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
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
