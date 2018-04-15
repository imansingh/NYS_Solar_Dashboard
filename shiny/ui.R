shinyUI(dashboardPage(
  dashboardHeader(title = "NY-Sun Solar Installations"),
  dashboardSidebar(
    sidebarUserPanel('Iman Singh',
                     image = 'imansingh_headshot.jpg'),
    h4(HTML('&emsp;'), 'NY-Sun Incentive Program'),
    p(HTML('&emsp;'), 'Solar Projects Funded, 2000-2017'),
    sidebarMenu(
      # menuItem("App Info", 
      #          tabName = 'info', 
      #          icon = icon('info-circle')),
      menuItem('Project Criteria', 
               tabName = 'criteria', 
               icon = icon('filter')),
      menuItem('Visualize Selected Projects',
               tabName = 'eda',
               icon = icon('eye'),
               menuSubItem("Map", 
                           tabName = "map", 
                           icon = icon("map")),
               menuSubItem("Histogram/Bar Chart", 
                        tabName = "histogram", 
                        icon = icon("bar-chart")),
               menuSubItem("Scatterplot", 
                        tabName = "scatterplot", 
                        icon = 
                          icon("line-chart")),
               menuSubItem("Boxplot", 
                        tabName = "boxplot", 
                        icon = icon("television"))),
      menuItem("Full Dataset",
               tabName = "data",
               icon = icon("database"))),
    br(),
    br(),
    br(),
    h4(HTML('&emsp;'), 'About Iman:'),
    sidebarMenu(
      menuItem('Blog', icon = icon('wordpress'), 
               href = 'https://nycdatascience.com/blog/author/imansingh/'),
      menuItem('LinkedIn', icon = icon('linkedin-square'), 
               href = 'https://www.linkedin.com/in/imansingh/'),
      menuItem('GitHub', icon = icon('github'), 
               href = 'https://github.com/imansingh/Scraping-Project'))
    ),
  dashboardBody(
    tabItems(
      # tabItem(tabName = 'info', 
      #         p('info on how to use the app')),
      tabItem(
        tabName = 'criteria',
        h2('Filter Which Projects to Visualize and Analyze'),
        fluidRow(style = 'padding:15px',
                 h3('Enter project criteria using the four input tabs'),
                 h4('The table below will update based on your choices')),
        fluidRow(tabBox(
          id = 'criteriaInput',
          width = 12,
          selected = 'Cost and Incentive',
          tabPanel(
            'Date and Location',
            h4('Only include projects with these date and location attributes:'),
            fluidRow(
              box(width = 3,
                  checkboxGroupInput(
                    "project_status",
                    "Project Status:",
                    choices = list("Complete" = 1, "Pipeline" = 2),
                    selected = c(1,2))),
              box(width = 3,
                  sliderInput(
                    "days_to_complete",
                    label = 
                      HTML('# Days, Application to Completion: 
                                           <br>
                                           <h6>(negative = 
                                           completed before application)</h6>'),
                    min = min(as.numeric(solar$Days.To.Complete),
                              na.rm = TRUE),
                    max = max(as.numeric(solar$Days.To.Complete),
                              na.rm = TRUE),
                    value=c(min(as.numeric(solar$Days.To.Complete),
                                na.rm = TRUE),
                            max(as.numeric(solar$Days.To.Complete),
                                na.rm = TRUE)))),
              box(width = 3,
                  dateRangeInput("date_applied",
                                 "Date Application Received:",
                                 start = min(solar$Date.Application.Received),
                                 end = max(solar$Date.Application.Received),
                                 min = min(solar$Date.Application.Received),
                                 max = max(solar$Date.Application.Received),
                                 format = "yyyy-mm-dd",
                                 startview = "month",
                                 weekstart = 0,
                                 language = "en",
                                 separator = " to ")),
              box(width = 3,
                  dateRangeInput("date_completed",
                                 "Date Project Completed:",
                                 start = min(solar$Date.Completed, na.rm = TRUE),
                                 end = max(solar$Date.Completed, na.rm = TRUE),
                                 min = min(solar$Date.Completed, na.rm = TRUE),
                                 max = max(solar$Date.Completed, na.rm = TRUE),
                                 format = "yyyy-mm-dd",
                                 startview = "month",
                                 weekstart = 0,
                                 language = "en",
                                 separator = " to "))
              ),
            fluidRow(
              box(width = 4,
                  selectizeInput(
                    'counties',
                    'Counties:',
                    choices = sort(unique(solar$County)),
                    multiple = TRUE,
                    options = list(placeholder =
                                     '(choose one or more)'))),
              box(width = 4,
                  # radioButtons(
                  #   'cities_in_county',
                  #   'Only Cities in Selected Counties?',
                  #   choices = list('yes' = 1,
                  #                  'no' = 2),
                  #   selected = 2),
                  selectizeInput(
                    'cities',
                    'Cities:',
                    choices = sort(unique(solar$City)),
                    multiple = TRUE,
                    options = list(placeholder =
                                     '(choose one or more)',
                                   maxOptions = 2000))),
              box(width = 4,
                  # radioButtons(
                  #   'zip_in_county',
                  #   'Only Zipcodes in Selected Counties?',
                  #   choices = list('yes' = 1,
                  #                  'no' = 2),
                  #   selected = 2),
                  selectizeInput(
                    'zip_codes',
                    'Zip Codes:',
                    choices = formatC(sort(unique(solar$Zip.Code)), 
                                      width = 5, flag = 0),
                    multiple = TRUE,
                    options = list(placeholder =
                                     '(choose one or more)')))
              )
            ),
          tabPanel('Customer and Application',
                   h4('Only include projects with these customer and 
                              application attributes:'),
                   fluidRow(
                     box(width = 4,
                         checkboxGroupInput(
                           'sector',
                           'Sector:',
                           choices = sort(unique(solar$Sector)),
                           selected = unique(solar$Sector))),
                     box(width = 4,
                         checkboxGroupInput(
                           'program_type',
                           'Program Type:',
                           choices = sort(unique(solar$Program.Type)),
                           selected = unique(solar$Program.Type))),
                     box(width = 4,
                         selectizeInput(
                           'solicitation',
                           'Solicitation:',
                           choices = sort(unique(solar$Solicitation)),
                           multiple = TRUE,
                           options =
                             list(placeholder = '(choose one or more)')))
                   ),
                   fluidRow(
                     box(width = 4,
                         checkboxGroupInput(
                           'purchase_type',
                           'Purchase Type:',
                           choices = c('Lease', 
                                       'Purchase',
                                       'Power Purchase Agreement',
                                       'None Listed'),
                           selected = c('Lease', 
                                        'Purchase', 
                                        'Power Purchase Agreement',
                                        'None Listed'))),
                     box(width = 3,
                         checkboxGroupInput(
                           'affordable_solar',
                           'Affordable Solar:',
                           choices = list('Yes',
                                          'No'),
                           selected = c('Yes',
                                        'No'))),
                     box(width = 3,
                         checkboxGroupInput(
                           'green_jobs',
                           'Green Jobs - Green NY:',
                           choices = list('Residential',
                                          'Non-Residential',
                                          'None Listed'),
                           selected = c('Residential',
                                        'Non-Residential',
                                        'None Listed')))
                   )
            ),
          tabPanel(
            'Contractor, Utility and Hardware',
            h4('Only include projects with these contractor, utility
                       and hardware attributes:'),
            fluidRow(
              box(width = 3,
                  selectizeInput(
                    'contractor',
                    'Contractor:',
                    choices = sort(unique(
                      solar$Contractor[solar$Contractor != 'NA'])),
                    multiple = TRUE,
                    options =
                      list(placeholder = '(choose one or more)')),
                  checkboxInput('contractor_missing', 
                                'Include projects with 
                                        no contractor listed',
                                value = TRUE)),
              box(width = 3,
                  selectizeInput(
                    'utility',
                    'Electric Utility:',
                    choices = unique(solar$Electric.Utility),
                    multiple = TRUE,
                    options =
                      list(placeholder = '(choose one or more)'))),
              box(width = 3,
                  checkboxGroupInput(
                    'remote_net_metering',
                    'Remote Net Metering:',
                    choices = list('Yes',
                                   'No',
                                   'None Listed'),
                    selected = c('Yes',
                                 'No',
                                 'None Listed'))),
              box(width = 3,
                  checkboxGroupInput(
                    'community_distributed_generation',
                    'Community Distributed Generation:',
                    choices = list('Yes',
                                   'No'),
                    selected = c('Yes',
                                 'No')))
              ),
            fluidRow(
              box(width = 3,
                  selectizeInput(
                    'inverter_manufacturer',
                    'Inverter Manufacturer:',
                    choices = sort(unique(
                      solar$Primary.Inverter.Manufacturer[
                        solar$Primary.Inverter.Manufacturer != 'NA'])),
                    multiple = TRUE,
                    options =
                      list(placeholder = '(choose one or more)')),
                  checkboxInput('inverter_manufacturer_missing', 
                                'Include projects with 
                                      no inverter manufacturer listed',
                                value = TRUE)),
              box(width = 3,
                  selectizeInput(
                    'inverter_model',
                    'Inverter Model:',
                    choices = sort(unique(
                      solar$Primary.Inverter.Model.Number[
                        solar$Primary.Inverter.Model.Number != 'NA'])),
                    multiple = TRUE,
                    options =
                      list(placeholder = '(choose one or more)',
                           maxOptions = 1500)),
                  checkboxInput('inverter_model_missing', 
                                'Include projects with 
                                      no inverter model listed',
                                value = TRUE)),
              box(width = 6,
                  HTML('<strong>Total Inverter Quantity: </strong>
                               <br>
                               <h6>(range: 1 to 800)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'inverter_quantity_min', 
                                label = '',
                                min = min(solar$Total.Inverter.Quantity,
                                          na.rm = TRUE),
                                max = max(solar$Total.Inverter.Quantity,
                                          na.rm = TRUE),
                                step = 1,
                                value = min(solar$Total.Inverter.Quantity,
                                            na.rm = TRUE)),
                              HTML('<br> <p style="margin-top: 0px;"> 
                                           <center> to </center> </p>'),
                              numericInput(
                                'inverter_quantity_max',
                                label = '',
                                min = min(solar$Total.Inverter.Quantity,
                                          na.rm = TRUE),
                                max = max(solar$Total.Inverter.Quantity,
                                          na.rm = TRUE),
                                step = 1,
                                value = max(solar$Total.Inverter.Quantity,
                                            na.rm = TRUE))),
                  checkboxInput('inverter_quantity_missing',
                                'Include projects with no
                                        inverter quantity listed',
                                value = TRUE))
              ),
            fluidRow(
              box(width = 3,
                  selectizeInput(
                    'pv_manufacturer',
                    'PV Module Manufacturer:',
                    choices = 
                      sort(unique(solar$Primary.PV.Module.Manufacturer)),
                    multiple = TRUE,
                    options =
                      list(placeholder = '(choose one or more)')),
                  checkboxInput('pv_manufacturer_missing', 
                                'Include projects with 
                                        no PV module manufacturer listed',
                                value = TRUE)),
              box(width = 3,
                  selectizeInput(
                    'pv_model',
                    'PV Module Model:',
                    choices = sort(unique(solar$PV.Module.Model.Number)),
                    multiple = TRUE,
                    options =
                      list(placeholder = '(choose one or more)',
                           maxOptions = 2500)),
                  checkboxInput('pv_model_missing', 
                                'Include projects with 
                                        no PV module model listed',
                                value = TRUE)),
              box(width = 6,
                  HTML('<strong>Total PV Module Quantity: </strong>
                               <br>
                               <h6>(range: 1 to 14,800)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'pv_quantity_min', 
                                label = '',
                                min = min(solar$Total.PV.Module.Quantity,
                                          na.rm = TRUE),
                                max = max(solar$Total.PV.Module.Quantity,
                                          na.rm = TRUE),
                                step = 1,
                                value = min(solar$Total.PV.Module.Quantity,
                                            na.rm = TRUE)),
                              HTML('<br> <p style="margin-top: 0px;"> 
                                           <center> to </center> </p>'),
                              numericInput(
                                'pv_quantity_max',
                                label = '',
                                min = min(solar$Total.PV.Module.Quantity,
                                          na.rm = TRUE),
                                max = max(solar$Total.PV.Module.Quantity,
                                          na.rm = TRUE),
                                step = 1,
                                value = max(solar$Total.PV.Module.Quantity,
                                            na.rm = TRUE))),
                  checkboxInput('pv_quantity_missing',
                                'Include projects with no
                                        PV module quantity listed',
                                value = TRUE))
              ),
            fluidRow(
              box(width = 6,
                  HTML('<strong>PV Module Wattage (kW): </strong>
                               <br>
                               <h6>(range: 0.14 to 5799.42)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'pv_wattage_min', 
                                label = '',
                                min = min(solar$Total.Nameplate.kW.DC),
                                max = max(solar$Total.Nameplate.kW.DC),
                                step = 1,
                                value = min(solar$Total.Nameplate.kW.DC)),
                              HTML('<br> <p style="margin-top: 0px;"> 
                                           <center> to </center> </p>'),
                              numericInput(
                                'pv_wattage_max',
                                label = '',
                                min = min(solar$Total.Nameplate.kW.DC),
                                max = max(solar$Total.Nameplate.kW.DC),
                                step = 1,
                                value = max(solar$Total.Nameplate.kW.DC)))),
              box(width = 6,
                  HTML('<strong>Expected Annual Production (kWh): </strong>
                               <br>
                               <h6>(range: 164 to 6,807,591)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'annual_kwh_min', 
                                label = '',
                                min = min(solar$Expected.KWh.Annual.Production),
                                max = max(solar$Expected.KWh.Annual.Production),
                                step = 1,
                                value = min(solar$Expected.KWh.Annual.Production)),
                              HTML('<br> <p style="margin-top: 0px;"> 
                                           <center> to </center> </p>'),
                              numericInput(
                                'annual_kwh_max',
                                label = '',
                                min = min(solar$Expected.KWh.Annual.Production),
                                max = max(solar$Expected.KWh.Annual.Production),
                                step = 1,
                                value = max(solar$Expected.KWh.Annual.Production))))
              )
            ),
          tabPanel(
            'Cost and Incentive',
            h4('Only include projects with these cost and incentive 
                       attributes:'),
            fluidRow(
              box(width = 6,
                  HTML('<strong>Project Cost ($): </strong>
                               <br>
                               <h6>(range: $150 to $22,671,836)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'project_cost_min',
                                label = '',
                                min = min(solar$Project.Cost,
                                          na.rm = TRUE),
                                max = max(solar$Project.Cost,
                                          na.rm = TRUE),
                                step = 1,
                                value = min(solar$Project.Cost,
                                            na.rm = TRUE)),
                              HTML('<br> <p style="margin-top: 0px;">
                                           <center> to </center> </p>'),
                              numericInput(
                                'project_cost_max',
                                label = '',
                                min = min(solar$Project.Cost,
                                          na.rm = TRUE),
                                max = max(solar$Project.Cost,
                                          na.rm = TRUE),
                                step = 1,
                                value = max(solar$Project.Cost,
                                            na.rm = TRUE))),
                  checkboxInput('project_cost_missing',
                                'Include projects with no
                                        project cost listed',
                                value = TRUE)),
              box(width = 3,
                  uiOutput('cost_per_kw_slider')),
              box(width = 3,
                  uiOutput('cost_per_annual_kwh_slider'))
              ),
            fluidRow(
              box(width = 6,
                  HTML('<strong>Incentive ($): </strong>
                                <br>
                                <h6>(range: $0 to $3,290,860)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'incentive_min',
                                label = '',
                                min = min(solar$Incentive ,
                                          na.rm = TRUE),
                                max = max(solar$Incentive,
                                          na.rm = TRUE),
                                step = 1,
                                value = min(solar$Incentive,
                                            na.rm = TRUE)),
                              HTML('<br> <p style="margin-top: 0px;">
                                            <center> to </center> </p>'),
                              numericInput(
                                'incentive_max',
                                label = '',
                                min = min(solar$Incentive,
                                          na.rm = TRUE),
                                max = max(solar$Incentive,
                                          na.rm = TRUE),
                                step = 1,
                                value = max(solar$Incentive,
                                            na.rm = TRUE))),
                  checkboxInput('incentive_missing',
                                'Include projects with no
                                         incentive listed',
                                value = TRUE)),
              box(width = 3,
                  sliderInput(
                    'incentive_per_kw',
                    'Incentive Per Installed Wattage ($ / kW)',
                    min = round(min(solar$Incentive.Per.Nameplate.kW,
                                    na.rm = TRUE), 2),
                    max = round(max(solar$Incentive.Per.Nameplate.kW,
                                    na.rm = TRUE), 2),
                    value=c(min(solar$Incentive.Per.Nameplate.kW,
                                na.rm = TRUE),
                            max(solar$Incentive.Per.Nameplate.kW,
                                na.rm = TRUE)))),
              box(width = 3,
                  sliderInput(
                    'incentive_per_annual_kwh',
                    'Incentive Per Annual Production ($ / kWh)',
                    min = round(min(solar$Incentive.Per.Annual.KW,
                                    na.rm = TRUE), 2),
                    max = round(max(solar$Incentive.Per.Annual.KW,
                                    na.rm = TRUE) + .1, 2),
                    value=c(min(solar$Incentive.Per.Annual.KW,
                                na.rm = TRUE),
                            max(solar$Incentive.Per.Annual.KW,
                                na.rm = TRUE))))
              ),
            fluidRow(
              box(width = 6,
                  HTML('<strong>Net Cost ($): </strong>
                               <br>
                               <h6>(range: $-48,482 to $21,826,743.04)</h6>'),
                  splitLayout(cellWidths = c('40%', '20%', '40%'),
                              numericInput(
                                'net_cost_min',
                                label = '',
                                min = min(solar$Net.Cost,
                                          na.rm = TRUE),
                                max = max(solar$Net.Cost,
                                          na.rm = TRUE),
                                step = 1,
                                value = min(solar$Net.Cost,
                                            na.rm = TRUE)),
                              HTML('<br> <p style="margin-top: 0px;">
                                           <center> to </center> </p>'),
                              numericInput(
                                'net_cost_max',
                                label = '',
                                min = min(solar$Net.Cost,
                                          na.rm = TRUE),
                                max = max(solar$Net.Cost,
                                          na.rm = TRUE),
                                step = 1,
                                value = max(solar$Net.Cost,
                                            na.rm = TRUE)))),
              box(width = 3,
                  sliderInput(
                    'net_cost_per_kw',
                    'Net Cost Per Installed Wattage ($ / kW)',
                    min = round(min(solar$Net.Cost.Per.Nameplate.kW,
                                    na.rm = TRUE), 2),
                    max = round(max(solar$Net.Cost.Per.Nameplate.kW,
                                    na.rm = TRUE),2),
                    value=c(min(solar$Net.Cost.Per.Nameplate.kW,
                                na.rm = TRUE),
                            max(solar$Net.Cost.Per.Nameplate.kW,
                                na.rm = TRUE)))),
              box(width = 3,
                  sliderInput(
                    'net_cost_per_annual_kwh',
                    'Net Cost Per Annual Production ($ / kWh)',
                    min = round(min(solar$Net.Cost.Per.Annual.KW,
                                    na.rm = TRUE) - .1, 2),
                    max = round(max(solar$Net.Cost.Per.Annual.KW,
                                    na.rm = TRUE) + .1, 2),
                    value=c(min(solar$Net.Cost.Per.Annual.KW,
                                na.rm = TRUE),
                            max(solar$Net.Cost.Per.Annual.KW,
                                na.rm = TRUE))))
              )
            )
          )),
        fluidRow(box(DT::dataTableOutput("filtered_table"), width = 12))
        ),
      tabItem(tabName = "map", 
              fluidRow(
                column(width = 9,
                       plotOutput("map", 
                                  height = '800px')),
                column(width = 3,
                       box(
                         h4("Select Highight Variables"),
                         selectizeInput("map_color_highlight", 
                                        "Select Color Variable", 
                                        c('None',
                                          categorial_small,
                                          categorical_medium,
                                          boolean,
                                          quantitative,
                                          time)),
                          selectizeInput("map_shape_highlight", 
                                         "Select Shape Variable", 
                                         c('None',
                                           categorial_small,
                                           categorical_medium,
                                           boolean)),
                         width = NULL),
                       box(
                         h4("Customize Visualization"),
                          sliderInput("map_dotsize", 
                                     "Select Dot Size", 
                                     min = .1, 
                                     max = 3, 
                                     value = 1, 
                                     ticks = FALSE), 
                          sliderInput("map_transparency",
                                      "Select Transparency", 
                                      min = .01, 
                                      max = 1, 
                                      value = .5, 
                                      ticks = FALSE),
                          width = NULL))
                )
              ),
      tabItem(tabName = "histogram", 
              h2("Histogram or Bar Chart of Projects Meeting Your Criteria"),
              fluidRow(box(plotOutput("histogram"), width = NULL)),
              fluidRow(
                box(width = 6,
                    selectizeInput("histogram_selected", 
                                   "Select Variable to Plot", 
                                   c(categorial_small,
                                     categorical_medium,
                                     categorical_large,
                                     boolean,
                                     quantitative,
                                     time))),
                box(width = 6,
                    selectizeInput('histogram_highlight',
                                   'Select Variable to Highlight - Color Fill',
                                   c("None",
                                     categorial_small,
                                     categorical_medium,
                                     categorical_large,
                                     boolean))))),
                # box(width = 4,
                #     radioButtons('stack_or_dodge',
                #                  'Stack or "Dodge" Higlighted Variable?',
                #                  choices = list("Stack" = 1, 
                #                                 "Dodge" = 2), 
                #                  selected = 2)))),
      tabItem(tabName = "scatterplot", 
              h2("Scatterplot of Projects Meeting Your Criteria"),
              fluidRow(box(plotOutput("scatterplot"), width = 12)),
              fluidRow(box(selectizeInput("selectedX", 
                                          "Select X Variable", 
                                          c(quantitative,
                                            time), 
                                          selected = 10), 
                           width = 4),
                       box(selectizeInput("selectedY", 
                                          "Select Y Variable", 
                                          c(quantitative,
                                            time), 
                                          selected = 7), 
                           width = 4),
                       box(radioButtons("regression", 
                                        "Add Regression Line?", 
                                        choices = list("Yes" = 1, "No" = 2), 
                                        selected = 2), 
                           width = 4)),
              fluidRow(box(
                h4("Select Highight Variables"),
                column(width = 6,
                       selectizeInput("scatter_color_highlight", 
                                      "Select Color Variable", 
                                      c('None',
                                        categorial_small,
                                        categorical_medium,
                                        boolean,
                                        quantitative,
                                        time))),
                column(width = 6,
                       selectizeInput("scatter_shape_highlight", 
                                      "Select Shape Variable", 
                                      c('None',
                                        categorial_small,
                                        categorical_medium,
                                        boolean))),
                width = 12))),
      tabItem(tabName = "boxplot", 
              h2("Boxplot of Projects Meeting Your Criteria"),
              fluidRow(box(plotOutput("boxplot"), width = 12)),
              fluidRow(box(selectizeInput("selectedB", "Select Variable to Plot", choice)))),
      tabItem(tabName = "data", 
              h2('Solar Installations Funded by NY-Sun Incentive Program, 2000-2017'),
              fluidRow(box(DT::dataTableOutput("full_table"), width = 12)))
              )
    )
))

#fluidRow(infoBoxOutput("maxBox"),
#        infoBoxOutput("minBox"),
#       infoBoxOutput("avgBox")),
