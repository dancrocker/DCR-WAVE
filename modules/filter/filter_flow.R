##############################################################################################################################
#     Title: filter_flow.R
#     Type: Module for DCR Shiny App
#     Description: Filter and Export Flow data (HOBO and USGS)
#     Written by: Nick Zinck/Dan Crocker, Spring 2017
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

FILTER_FLOW_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    br(),
    wellPanel(
      em('Select and filter data to view, export, and use in the "Plots" and "Statistics" tabs. To view and use the full (unfiltered) data, check the box below.'),
      checkboxInput(ns("full_data"), "Use Full Dataset")
    ),
    tabsetPanel(
      tabPanel("Select / Filter Data",
               # Create a div() for area to be reset by Refresh Button
               div(id = ns('form'),
                   wellPanel(
                     fluidRow(h3("Main Selections", align = "center")),
                     fluidRow(
                       column(3,
                              div(actionButton(ns("refresh"), "Reset Selections / Filters"), align = "center"),
                              br(),
                              wellPanel(
                                h3(textOutput(ns("text_full_data")), align = "center"),
                                h4(textOutput(ns("text_site_null")), align = "center"),
                                h4(textOutput(ns("text_param_null")), align = "center"),
                                h4(textOutput(ns("text_date_null")), align = "center"),
                                h4(textOutput(ns("text_no_storm")), align = "center"),
                                h4(textOutput(ns("text_no_month")), align = "center"),
                                h5(textOutput(ns("text_num_text")), align = "center"),
                                strong(textOutput(ns("text_num")), align = "center")
                              ), # end Well Panel
                              wellPanel(
                                SITE_MAP_UI(ns("site_map"))
                              ) # end Well Panel
                       ), # end Column
                       column(5,
                              uiOutput(ns("site_ui"))
                       ), # end Column
                       column(4,
                              # Parameter Input - Using Module Parameter Select
                              # Select 1 Parameter
                              uiOutput(ns("param_ui")),
                              uiOutput(ns("valrange_ui")),
                              br(),
                              # Date Input - Using Module Date Select
                              DATE_SELECT_UI(ns("date")),
                              br(),
                              wellPanel(
                                # Month Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                                CHECKBOX_SELECT_ALL_UI(ns("month"))
                              )
                       ) # end column
                     ) # end fluidrow
                   ), # end well panel
                   # Advanced Filters
                   wellPanel(
                     fluidRow(h3("Advanced Filters", align = "center")),
                     fluidRow(
                       column(4,
                              # Flag Selection
                              wellPanel(
                                # Flag Input - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
                                SELECT_SELECT_ALL_UI(ns("flag"))
                              ),
                              # storm Sample Selection
                              wellPanel(
                                strong("Storm Samples:"), # Bold Text
                                checkboxInput(ns("storm"),
                                              label =  "Include Storm Samples",
                                              value = TRUE),
                                checkboxInput(ns("nonstorm"),
                                              label =  "Include Non-Storm Samples",
                                              value = TRUE)

                              ) # end Well Panel
                       ), # end column
                       column(4,
                              # Meteoro/Hydro Filter 1
                              wellPanel(
                                strong("Meteoro/Hydro Filter 1"), # Bold Text
                                br(), br(),
                                radioButtons(ns("met_option_1"), label = NULL,
                                             choices = c("off", "on", "group"),
                                             inline = TRUE),
                                selectInput(ns("met_param_1"), label = NULL,
                                            choices = c("Wind Speed",
                                                        "Wind Direction",
                                                        "Precipitation - 24 hrs",
                                                        "Precipitation - 48 hrs",
                                                        "Temperature",
                                                        "Cloud Cover",
                                                        "Flow - Quabbin Aquaduct",
                                                        "Flow - East Branch Swift",
                                                        "Flow - West Branch Swift",
                                                        "Flow - Quinapoxet",
                                                        "Flow - Stillwater"),
                                            selected = "Wind Speed"),
                                sliderInput(ns("met_value_1"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                              ) # end Well Panel
                       ),
                       column(4,
                              # Meteoro/Hydro Filter 2
                              wellPanel(
                                strong("Meteoro/Hydro Filter 2"), # Bold Text
                                br(), br(),
                                radioButtons(ns("met_option_2"), label = NULL,
                                             choices = c("off", "on", "group"),
                                             inline = TRUE),
                                selectInput(ns("met_param_2"), label = NULL,
                                            choices = c("Wind Speed",
                                                        "Wind Direction",
                                                        "Precipitation - 24 hrs",
                                                        "Precipitation - 48 hrs",
                                                        "Temperature",
                                                        "Cloud Cover",
                                                        "Flow - Quabbin Aquaduct",
                                                        "Flow - East Branch Swift",
                                                        "Flow - West Branch Swift",
                                                        "Flow - Quinapoxet",
                                                        "Flow - Stillwater"),
                                            selected = "Precipitation - 24 hrs"),
                                sliderInput(ns("met_value_2"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                              ) # end Well Panel
                       ) # end column
                     ) # end fluidrow
                   ) # end well panel
               )
      ),
      tabPanel("View Data in Table",
               fluidRow(br(), downloadButton(ns("download_data"), "Download table as csv"), align = "center"),
               dataTableOutput(ns("table"))
      ) # end tabPanel
    ) # end tabSetpanel
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# This module does not take any reactive expressions. Changes will have to be made to accmodate reactive expressions
# dfs is a list of dataframes

FILTER_FLOW <- function(input, output, session, df, df_site, df_wq, df_flags = NULL, df_flag_index = NULL, type = "wq"){

  ########################################################
  # Main Selection

  ns <- session$ns # see General Note 1
  # Change case of DATE column for Date Select, pull into long format for filtering
  # df <- df_wach_flow

  ### Site Selection
  site_choices <- reactive({
    df_site %>% .$LocationLabel %>% sort() %>% paste()
  })

  output$site_ui <- renderUI({
    checkboxGroupInput(ns("site"),
                      label = "Choose Location(s):",
                      choices = site_choices(),
                      width = "100%",
                      inline = FALSE)
  })


  ### Parameter and Range Selection
  parameter_choices <- reactive({
      # req(Df1)
      Df1() %>% .$Parameter %>% unique() %>% sort() %>% paste()
    })

  minrange <- reactive({
      # req(Df1)
      min(Df1()$Result)
    })
  maxrange <- reactive({
    # req(Df1)
      max(Df1()$Result)
    })

  output$param_ui <- renderUI({
    selectInput(ns("param"),
                label = "Choose Parameter(s):",
                choices = parameter_choices(),
                multiple = TRUE,
                selected = "")
  })

  output$valrange_ui <- renderUI({
    sliderInput(ns("valrange"),
                label = "Restrict value range of selection:",
                min = minrange(),
                max = maxrange(),
                value = c(minrange(),maxrange()),
                round = TRUE)
  })

  # observe({
  #   Df1()
  #   # Control the value, min, max, and step.
  #   # Step size is 2 when input value is even; 1 when value is odd.
  #   updateSliderInput(session,
  #                     "valrange",
  #                     min = minrange(),
  #                     max = maxrange(),
  #                     value = c(minrange():maxrange()),
  #                     round = TRUE)
  # })

  # Reactive Dataframe - first filter of the dataframe for Site
  Df1 <- reactive({
    # A Site must be selected in order for Df1 (or anything that uses Df1()) to be executed
    req(input$site)

    df %>% filter(LocationLabel %in% input$site)

  })

  ### Parameter and Date Range

  # Parameter Selection using Param_Select Module
  # Param <- callModule(PARAM_SELECT, "param", Df = Df1)

  # Date Range and Year Using Date_Select Module
  Date_Year <- callModule(DATE_SELECT, "date", Df = Df1)

  # Month Selection
  Month <- callModule(CHECKBOX_SELECT_ALL, "month",
                      label = "Months:",
                      Choices = reactive({month.name}),
                      Selected = reactive({month.name}),
                      colwidth = 3,
                      inline = TRUE)

  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df2 <- reactive({
    # Wait for all neccesary Inputs to Proceed
    req(input$param, input$valrange, Month(),
        (isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years())) # See General Note _

    Df1() %>%
      # filter by parameter, parameter value range, and by date range
      filter(Parameter %in% input$param,
             # Filter by Result Range
             Result >= input$valrange[1], Result <= input$valrange[2],
             # Filter by either Date Range or By Years (Include both)
             (Date >= Date_Year$Lower() & Date <= Date_Year$Upper()) | year(Date) %in% Date_Year$Years(),
             # Filter by Month
             as.character(month(Date, label = TRUE, abbr = FALSE)) %in% Month())

  })

  ##################################################
  # Advanced Filter

  ### Flag Selection

  # # server - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
  Flag <- callModule(SELECT_SELECT_ALL, "flag",
                     label = "Select flag(s) to EXCLUDE from the data:",
                     Choices = reactive({sort(unique(df_flags$label[df_flags$Flag_ID %in% df_flag_index$FlagCode & df_flags$Flag_ID != 114]))}),
                     colwidth = 3)

  # Subset the Sample Flag Index by the flags selected to exclude - this results in a vector of IDs to filter out
  flagged_ids <- reactive({
    df_flag_index %>%
      filter(FlagCode %in% as.numeric(substr(Flag(),1, 3))) %>%
      .$SampleID
  })

  ### Storm Sample Selection

  # Filter df_flag_index so that only flag 114 (Storm Sample Flag) are included
  storm_ids <- reactive({
    df_flag_index %>%
      filter(FlagCode == 114) %>%
      .$SampleID
  })

  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result

  Df3 <- reactive({

    # Assign a temporary dataframe and filter NAs
    df_temp <- Df2() %>% filter(!is.na(Result))

    # filter out Selected Flags
    if(isTruthy(Flag()) & isTruthy(df_flag_index)){
      df_temp <- df_temp %>% filter(!(ID %in% flagged_ids()))
    }

    # filter out Storm Samples if unchecked
    if(input$storm != TRUE & isTruthy(df_flag_index)){ # Box is unchecked apply filter to remove storm samples
      df_temp <- df_temp %>% filter(!(ID %in% storm_ids()))
    }

    # filter out Non Storm Samples if unchecked
    if(input$nonstorm != TRUE & isTruthy(df_flag_index)){ # Box is unchecked apply filter to remove non-storm samples
      df_temp <- df_temp %>% filter(ID %in% storm_ids())
    }

    # filter out Depth for Profile Data
    if(isTruthy(input$depth)){
      df_temp <- df_temp %>%
        filter(Depth_m >= input$depth[1],
               Depth_m <= input$depth[2])
    }

    df_temp

  })


  ########################################################
  # Create Final Dataframes for use Table, Export, Plots, and Statistics

  # If Full dataframe is used or if selection/filters are used
  # Reactive Dataframe - Long Format (Regular format)
  Df4 <- reactive({
    if(input$full_data){
      df
    } else{
      Df3()
    }
  })

  # Reactive Dataframe - Wide Format (for Correlation ScatterPlot and Correlation Matrix)
  Df4_Wide <- reactive({
    # require Dataframe to be more than zero observations - prevent from crashing
    req(Df4() %>% summarise(n()) %>% unlist() != 0)
    Df4() %>%
      # Need to get rid of Units column to properly Spread the data due to discrepencies in Units
      select(-Units) %>%
      # Should verify no duplicate records and then remove this dinstinct code line
      distinct(LocationLabel, Date, Parameter, .keep_all = TRUE) %>%
      # Spread parameters to each have their own row (wide format)
      spread("Parameter", "Result")
  })

  # Reactive Dataframe - Adding Columns for Year, Season, and Month for grouping purposesin some modules
  Df4_Stat <- reactive({
    Df4() %>%
      mutate(Year = factor(lubridate::year(Date)),
             Season = getSeason(Date),
             Month = month.abb[lubridate::month(Date)])
  })

  #####################################################
  # CSV output and Table

  # render Datatable
  output$table <- renderDataTable(Df4(), selection = 'none')

  # Downloadable csv of selected dataset
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Df4(), file)
    }
  )

  ######################################################
  # Texts

  # Text - Number of Samples - Words
  output$text_num_text <- renderText({
    req(Df4()) # See General Note 1
    "Number of Samples in Selected Data:"
  })

  # Text - Number of Samples - Number
  output$text_num <- renderText({
    req(Df4()) # See General Note 1
    Df4() %>% summarise(n()) %>% paste()
  })

  # Text - Select Month
  output$text_full_data <- renderText({
    req(input$full_data)
    "Full Data Selected"
  })

  # Text - Select Site
  output$text_site_null <- renderText({
    req(!isTruthy(input$site), !(input$full_data))
    "Select Site(s)"
  })

  # Text - Select Param
  output$text_param_null <- renderText({
    req(!isTruthy(input$param), !(input$full_data))
    "Select Parameter"
  })

  # Text - Select Param
  output$text_date_null <- renderText({
    req(!isTruthy(Date_Year$Lower()) | !isTruthy(Date_Year$Upper()), !isTruthy(Date_Year$Years()), !(input$full_data))
    "Select Date Range or Years"
  })

  # Text - Select Month
  output$text_no_month <- renderText({
    req(!isTruthy(Month()), !(input$full_data))
    "Select Months"
  })

  # Text - Select Storm Sample Types when none are selected
  output$text_no_storm <- renderText({
    req(!(input$storm), !(input$nonstorm), !(input$full_data))
    "- Please Select Storm Sample Types"
  })

  #####################################################
  # Other

  ### Site Map
  # Selected Sites to Highlight Red
  Site_List <- reactive({
    if(input$full_data){
      df_site$LocationLabel
    } else {
      input$site
    }
  })

  # Site Map Generation from Site_Map Module
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site_List)


  ### Refresh Button
  observeEvent(input$refresh, {
    shinyjs::reset("form")
  })

  #####################################################
  # Return from Module a list of reactive dataframes.

  return(list(Long = Df4,
              Wide = Df4_Wide,
              Stat = Df4_Stat))

} # end Server Function

