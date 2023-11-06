


##############################################################################################################################.
#     Title: Export-WQ.R
#     Type: Module for DCR Shiny App
#     Description: Filter and Export Water Quality Data
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################.

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

#####################.
# User Interface ####
#####################.

FILTER_WQ_UI <- function(id) {

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
                              PARAM_SELECT_UI(ns("param")),
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
                                selectInput(ns("flag"), label = "Select Flag(s) of interest:",
                                            choices = df_flags$label[df_flags$Flag_ID != 114], 
                                            multiple = TRUE),
                                switchInput(
                                  inputId = ns("invert_selection"),
                                  label = "Filter data to show only data matching flag(s) selected",
                                  labelWidth = "100%",
                                  width = "100%",
                                  value = FALSE
                                ),
                                h5(textOutput(ns("text_invert_flag_filter")), align = "center")
                                # SELECT_SELECT_ALL_UI(ns("flag"))
                              )
                        ), # end column
                        column(4,
                              # storm Sample Selection
                              uiOutput(ns("storm_ui")),
                              # Depth Selection
                              uiOutput(ns("depth_ui"))
                       ), # end column
                       column(4,
                              # Meteoro/Hydro Filter 1
                              wellPanel(
                                strong("Weather Filter"), # Bold Text
                                br(), br(),
                                radioButtons(ns("met_option_1"), label = NULL,
                                             choices = c("off", "on"),
                                             inline = TRUE),
                                selectInput(ns("met_param_1"), label = NULL,
                                            choices = c(#"Air Temperature (C)",
                                                        "Precipitation (in) - 24 hrs",
                                                        "Precipitation (in) - 48 hrs",
                                                        "Precipitation (in) - 7 day",
                                                        "Precipitation (in) - 14 day",
                                                        "Precipitation (in) - 30 day"),
                                            selected = "Precipitation (in) - 24 hrs"),
                                uiOutput(ns("weather_filter"))
                              ) # end Well Panel
                       )
                       # column(4,
                       #        # Meteoro/Hydro Filter 2
                       #        wellPanel(
                       #          strong("Meteoro/Hydro Filter 2"), # Bold Text
                       #          br(), br(),
                       #          radioButtons(ns("met_option_2"), label = NULL,
                       #                       choices = c("off", "on", "group"),
                       #                       inline = TRUE),
                       #          selectInput(ns("met_param_2"), label = NULL,
                       #                      choices = c("Wind Speed",
                       #                                  "Wind Direction",
                       #                                  "Precipitation - 24 hrs",
                       #                                  "Precipitation - 48 hrs",
                       #                                  "Temperature",
                       #                                  "Cloud Cover",
                       #                                  "Flow - Quabbin Aquaduct",
                       #                                  "Flow - East Branch Swift",
                       #                                  "Flow - West Branch Swift",
                       #                                  "Flow - Quinapoxet",
                       #                                  "Flow - Stillwater"),
                       #                      selected = "Precipitation - 24 hrs"),
                       #          sliderInput(ns("met_value_2"), "Value Range:", min = 0, max = 12, value = c(0,12), step = 0.5)
                              # ) # end Well Panel
                       # ) # end column
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


#######################.
# Server Function ####
#######################.

# This module does not take any reactive expressions. Changes will have to be made to accomodate reactive expressions
# dfs is a list of dataframes

FILTER_WQ <- function(input, output, session, df, df_site, df_flags = NULL, df_precip, df_flag_index = NULL, type){

  # Types include: "wq_trib", "wq_res" ,"wq_depth", and "profile". More can be added

  # Main Selection ####

  ns <- session$ns # see General Note 1


  ### Site Selection ####

  # Display sites w/o depths OR sites w/ Depths
  output$site_ui <- renderUI({
    if(type == "wq_trib" | type == "profile"){
      SITE_CHECKBOX_UI(ns("site"))
    } else if(type == "wq_depth"){
      STATION_LEVEL_CHECKBOX_UI(ns("site"))
    }
  })

  # Site Selection using Site Select Module
  Site <- if(type == "wq_depth") {
    callModule(STATION_LEVEL_CHECKBOX, "site", df = df)
  } else { 
    callModule(SITE_CHECKBOX, "site", df = df)
  }
  
  # Reactive Dataframe - first filter of the dataframe for Site
  Df1 <- reactive({
    # A Site must be selected in order for Df1 (or anything that uses Df1()) to be executed
    req(Site())
    df %>% filter(LocationLabel %in% Site())
  })


  ### Parameter and Date Range ####

  # Parameter Selection using Param_Select Module
  Param <- callModule(PARAM_SELECT, "param", Df = Df1)

  # Date Range and Year Using Date_Select Module
  Date_Year <- callModule(DATE_SELECT, "date", Df = Df1)

  # Month Selection ####
  Month <- callModule(CHECKBOX_SELECT_ALL, "month",
                      label = "Months:",
                      Choices = reactive({month.name}),
                      Selected = reactive({month.name}),
                      colwidth = 3,
                      inline = TRUE)


  # Reactive Dataframe - filter for param, value range, date, and remove rows with NA for Result
  Df2 <- reactive({
    # Wait for all neccesary Inputs to Proceed
    req(Param$Type(), Param$Range_Min(), Param$Range_Min(), Month(),
        (isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years())) # See General Note _

    Df1() %>%
      # filter by parameter, parameter value range, and by date range
      filter(Parameter %in% Param$Type(),
             # Filter by Result Range
             Result >= Param$Range_Min(), Result <= Param$Range_Max(),
             # Filter by either Date Range or By Years (Include both)
             (Date >= Date_Year$Lower() & Date <= Date_Year$Upper()) | year(Date) %in% Date_Year$Years(),
             # Filter by Month
             as.character(month(Date, label = TRUE, abbr = FALSE)) %in% Month())

  })

  # Advanced Filters ####

  ### Flag Selection ####

  # Choices
  # flag_choices <- df_flags$label[df_flags$Flag_ID != 114]

  # server - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
  # Flag <- callModule(SELECT_SELECT_ALL, "flag",
  #                    label = "Select flag(s) to EXCLUDE from the data:",
  #                    Choices = reactive({df_flags$label}),
  #                    colwidth = 3)

  #1 Select input for flags to choose from (input$flag)
  #2 Toggle - off - defaults to exclude selected flags, toggle on to only show data with flags selected
  #3 Update text to indicate that the Filter is set to [exclude | only return] Data with flag(s): x,y,z 
  #4 The Flag data df that gets passed into the module should be as paired down as possible. 
  #5 filter the flag index from both dfs according to the toggle setting in #2
  #6 Recombine the filtered data dfs into 1 df
  #7 Apply the filter in the filter pipeline
  #9 Show output text that indicates how many records were excluded/included from the data using the flag filter
  
  #Fixes needed for Quabbin:
  # 1. Modify rds file generation script: wq_quab_Flag_sample needs the flag code column
  # 2. The trib data needs to have a column indicating which table the data is coming from
  # 3. tblMDHEMLWQData does not seem to exist??? Where is this table?
  # 4. Quab flag index dataset only has 1 value: "df_quab_flags_all" - this in not helpful in splitting data for modules
  
  ### Storm Sample Selection ####
  if(type == "wq_trib") {
  storm_ids <- reactive({
    Df2() %>%
      filter(!is.na(StormSampleN),
                    StormSampleN != "") %>%
      .$UniqueID
  })
  }

  # ### Depth Filter (Profile) ####

  # UI
  output$depth_ui <- renderUI({
    if(type == "profile") {
      max_depth <- max(df$Depth_m)
      tagList(
        wellPanel(
          sliderInput(ns("depth"),"Depth Range", min = 0, max = max_depth, value = c(0, max_depth))
        )
      )
    } else {
      wellPanel(
        em("Depth filters not applicable to data selected.")
      )
    }
  })
  
  ### UI for Storm sample filter
  # UI
  output$storm_ui <- renderUI({
    if(type == "wq_trib") {
      tagList(
        wellPanel(
          strong("Storm Samples:"), # Bold Text
          checkboxInput(ns("nonstorm"),
                        label =  "Include Non-Storm Samples",
                        value = TRUE),
          checkboxInput(ns("storm"),
                        label =  "Include Storm Samples",
                        value = FALSE)
        ) # end Well Panel
      )
    } else {
      wellPanel(
      em("Storm sample filter not applicable to data selected.")
      )
    }
  })
  
  #* Weather filter ----
  #* Set slider range and label according to which choice is selected
  met_range_min <- reactive({
    ifelse(input$met_param_1 == "Air Temperature (C)", -5, 0)
    })
 
  met_range_max <- reactive({
    ifelse(input$met_param_1 == "Air Temperature (C)", 40, 20) 
  })
              
  output$weather_filter <- renderUI({
    sliderInput(ns("met_value_1"), 
                "Value Range:", min = met_range_min(), max = met_range_max(), value = c(0, 20), step = 0.5)
  })
  

  ### Reactive List of (non-reactive) Dataframes - filter for selected site, param, value range, date, and remove rows with NA for Result

  Df3 <- reactive({
    req(Df2())
    # Assign a temporary dataframe and filter NAs
    df_temp <- Df2() %>% filter(!is.na(Result))

    # filter out Selected Flags
    if(isTruthy(input$flag) & isTruthy(df_flag_index)) {
      
      flagged_recs <- df_flag_index %>% 
        filter(FlagCode %in% as.numeric(substr(input$flag, 1, 3))) %>% 
        mutate("Flag_Index" = glue("{DataTableName}_{SampleID}"))
      
      x <- df_temp %>% 
        mutate("Flag_Index" = glue("{TableName}_{ID}"))
      
      if(input$invert_selection) {
        # keeps only records that are flagged
        x <- x %>% 
          filter(Flag_Index %in% flagged_recs$Flag_Index)
        df_temp <- x %>% 
          select(-c(Flag_Index))
      } else {
        # Excludes all records that are flagged
        x <- x %>% 
          filter(!Flag_Index %in% flagged_recs$Flag_Index)
        df_temp <- x %>% 
          select(-c(Flag_Index))
      }
      
      df_temp
    }

    # Storm Sample filters only if data has storm samples

      # filter out Storm Samples if unchecked
      if(input$storm != TRUE & isTruthy(storm_ids())){
        df_temp <- df_temp %>% filter(!(UniqueID %in% storm_ids()))
      }

      # filter out Non Storm Samples if unchecked
      if(input$nonstorm != TRUE & isTruthy(storm_ids())){
        df_temp <- df_temp %>% filter(UniqueID %in% storm_ids())
      }
    
    # filter out Depth for Profile Data
    if(isTruthy(input$depth)){
      df_temp <- df_temp %>%
        filter(Depth_m >= input$depth[1],
               Depth_m <= input$depth[2])
    }
    # Filter results if the weather filter is on
    # For now the air temp filter goes to the default, which is all dates
    if(input$met_option_1  == "on") {
      weather_dates <- switch(input$met_param_1,
                              # input$met_param_1 == "Air Temperature (C)" ~ df_precip$DATE[between(df_precip$DailyPrcpAve, input$met_value_1[1], input$met_value_1[2])],
        "Precipitation (in) - 24 hrs" = df_precip$DATE[between(df_precip$DailyPrcpAve, input$met_value_1[1], input$met_value_1[2])],
        "Precipitation (in) - 48 hrs" = df_precip$DATE[between(df_precip$`2dPRCP`, input$met_value_1[1], input$met_value_1[2])],
        "Precipitation (in) - 7 day" = df_precip$DATE[between(df_precip$`7dPRCP`, input$met_value_1[1], input$met_value_1[2])],
        "Precipitation (in) - 14 day" = df_precip$DATE[between(df_precip$`14dPRCP`, input$met_value_1[1], input$met_value_1[2])],
        "Precipitation (in) - 30 day" = df_precip$DATE[between(df_precip$`30dPRCP`, input$met_value_1[1], input$met_value_1[2])],  
        df_precip$DATE
      )
      
      df_temp <- df_temp %>%
        filter(Date %in% na.omit(weather_dates))
    }
    
    df_temp
    
  })
  
  # Create Final Dataframes for use Table, Export, Plots, and Statistics ####

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

  # Reactive Dataframe - Adding Columns for Year, Season, and Month for grouping purposes in some modules
  Df4_Stat <- reactive({
    Df4() %>%
      mutate(Year = factor(lubridate::year(Date)),
             Season = getSeason(Date),
             Month = month.abb[lubridate::month(Date)])
  })


  # CSV output and Table ####

  # render Datatable
  output$table <- renderDataTable({
    datatable(Df4()) %>% 
    formatDate(columns = "DateTimeET", method = 'toLocaleString')
  })

  # Downloadable csv of selected dataset
  output$download_data <- downloadHandler(
    filename = function() {
      paste("DCRExportedWQData", ".csv", sep = "")
    },
    content = function(file) {
        df_csv <- Df4()
        df_csv$DateTimeET <- format(df_csv$DateTimeET, usetz=TRUE)
      write_csv(df_csv, file)
    }
  )

  # Texts ####

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
    req(!isTruthy(Site()), !(input$full_data))
    "Select Site(s)"
  })

  # Text - Select Param
  output$text_param_null <- renderText({
    req(!isTruthy(Param$Type()), !(input$full_data))
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
    req(type == "wq_trib", isTruthy(Param$Type()), isTruthy(Site()), !isTruthy(input$storm), !isTruthy(input$nonstorm), !isTruthy(input$full_data))
    "- Please Select Storm Sample Types"
  })

  output$text_invert_flag_filter <- renderText({
    
    req(input$flag, Df3())
    if(input$invert_selection) {
      glue("The flag filter is set to only INCLUDE data with selected flag(s):{paste(input$flag, collapse = ', ')}")
    } else {
      glue("The flag filter is set to EXCLUDE data with selected flag(s):{paste(input$flag, collapse = ', ')}")
    }
  })

    
  # OTHER ####

  ### Site Map ####
  # Selected Sites to Highlight Red
  Site_List <- reactive({
    if(input$full_data){
      df_site$LocationLabel
    } else {
      Site()
    }
  })

  # Site Map Generation from Site_Map Module
  callModule(SITE_MAP, "site_map", df_site = df_site, Site_List = Site_List)


  ### Refresh Button ####
  observeEvent(input$refresh, {
    shinyjs::reset("form")
  })

  # Return from Module a list of reactive dataframes. ####

  return(list(Long = Df4,
              Wide = Df4_Wide,
              Stat = Df4_Stat))

} # end Server Function

