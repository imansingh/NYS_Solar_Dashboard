shinyUI(dashboardPage(
    dashboardHeader(title = "NYS Solar Data"),
    dashboardSidebar(
        
        sidebarUserPanel("NY-Sun Initiative",
                         image = "NYSun-stacked.jpg"),
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Graph", tabName = "graph", icon = icon("bar-chart")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        ),
        selectizeInput("selected",
                       "Select Item to Display",
                       choice),
        dateRangeInput("dateApplied", "Select Range for 'Date Application Received'", start = "2000-12-21", end = "2017-08-31", min = "2000-12-21", max = "2017-08-31", format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to "),
        dateRangeInput("dateCompleted", "Select Range for 'Date Completed'", start = "2000-12-21", end = "2017-08-31", min = "2000-12-21", max = "2017-08-31", format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to ")
        
    ),
    dashboardBody(
        #tags$head(
        #    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        #),
        tabItems(
            tabItem(tabName = "map", h2("Title For Map Content"),
                    #fluidRow(infoBoxOutput("maxBox"),
                     #        infoBoxOutput("minBox"),
                      #       infoBoxOutput("avgBox")),
                    fluidRow(box(plotOutput("map"), width = 6, height = 500))),
            tabItem(tabName = "graph", h2("Title for Graph Content"),
                    fluidRow(box(plotOutput("hist"), height = 300))),
            tabItem(tabName = "data", h2("Title For Data Content"),
                    fluidRow(box(DT::dataTableOutput("table"), width = 12)))
        )
    )
))