shinyUI(dashboardPage(
    dashboardHeader(title = "NYS Solar Data"),
    dashboardSidebar(
        
        sidebarUserPanel("NY-Sun Initiative",
                         image = "NYSun-stacked.jpg"),
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Histogram/Bar Chart", tabName = "histogram", icon = icon("bar-chart")),
            menuItem("Scatterplot", tabName = "scatterplot", icon = icon("line-chart")),
            menuItem("Boxplot", tabName = "boxplot", icon = icon("television")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        ),
        selectizeInput("selected", "Select Variable to Highlight", choice_map_scatter),
        dateRangeInput("dateApplied", "Select Range for 'Date.Application.Received'", start = "2000-12-21", end = "2017-08-31", min = "2000-12-21", max = "2017-08-31", format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to "),
        dateRangeInput("dateCompleted", "Select Range for 'Date.Completed'", start = "2000-12-21", end = "2017-08-31", min = "2000-12-21", max = "2017-08-31", format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to "),
        checkboxGroupInput("completedPipeline", "Display Completed Projects, Projects in Pipeline, or Both?", 
                           choices = list("Completed" = 1, "Pipeline" = 2), selected = c(1,2))
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "map", h2("Map of Projects Meeting Your Criteria"),
                    fluidRow(
                      column(width = 6, 
                             box(plotOutput("map"), width = NULL)),
                      column(width = 1),
                      column(width = 4,
                            box(sliderInput("dotSize", "Select Dot Size", min = .01, max = 1, value = .5), width = NULL),
                            box(sliderInput("transparency", "Select Transparency", min = .01, max = 1, value = .5), width = NULL)))),
            tabItem(tabName = "histogram", h2("Histogram or Bar Chart of Projects Meeting Your Criteria"),
                    fluidRow(box(plotOutput("histogram"), width = NULL)),
                    fluidRow(box(selectizeInput("selectedH", "Select Variable to Plot", choice_hist_box)))),
            tabItem(tabName = "scatterplot", h2("Scatterplot of Projects Meeting Your Criteria"),
                    fluidRow(box(plotOutput("scatterplot"), width = 12)),
                    fluidRow(box(selectizeInput("selectedX", "Select X Variable", choice_map_scatter, selected = 10), width = 4),
                             box(selectizeInput("selectedY", "Select Y Variable", choice_map_scatter, selected = 7), width = 4),
                             box(radioButtons("regression", "Add Regression Line?", choices = list("Yes" = 1, "No" = 2), selected = 2), width = 4))),
            tabItem(tabName = "boxplot", h2("Boxplot of Projects Meeting Your Criteria"),
                    fluidRow(box(plotOutput("boxplot"), width = 12)),
                    fluidRow(box(selectizeInput("selectedB", "Select Variable to Plot", choice)))),
            tabItem(tabName = "data", h2("Title For Data Content"),
                    fluidRow(box(DT::dataTableOutput("table"), width = 12)))
        )
    )
))

#fluidRow(infoBoxOutput("maxBox"),
#        infoBoxOutput("minBox"),
#       infoBoxOutput("avgBox")),