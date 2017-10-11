# in ui.R
#  hr(),
#  fluidRow(column(4, verbatimTextOutput("value")))

#   output$value <- renderPrint({ input$dateRange })

shinyServer(function(input, output){
    
  filteredSolar <- reactive({
    solar %>% filter(`Date.Application.Received` >= input$dateApplied[1] & `Date.Application.Received` <= input$dateApplied[2]) %>%
      filter((`Date.Completed` >= input$dateCompleted[1] & `Date.Completed` <= input$dateCompleted[2]) | is.na(solar$`Date.Completed`))
  })
  filteredSolar1 <- reactive({
    if (length(input$completedPipeline) == 2) {
      filter(filteredSolar(), `Project.Status`== 'Complete' | `Project.Status` == 'Pipeline')
      } else if (1 %in% input$completedPipeline){
          filter(filteredSolar(), `Project.Status`== 'Complete')
        } else if (2 %in% input$completedPipeline){
            filter(filteredSolar(), `Project.Status`== 'Pipeline')}
    
  })

    # show map using ggplot2
    output$map <- renderPlot({
      counties_no_fill + geom_point(data = filteredSolar1(), aes_string(x = "Longitude", y = "Latitude", color = input$selected))
      })
    
    # show histogram using googleVis
    output$hist <- renderPlot({
      ggplot(data = filteredSolar1(), aes(x = `Project Cost`, y = Incentive)) + geom_point(aes(color=Date.Application.Received))
    })
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(filteredSolar1(), rownames=FALSE) %>% 
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
