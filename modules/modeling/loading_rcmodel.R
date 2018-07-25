#__________________________________________________________________________________________________________
#     Title: LOADING_RCMODEL.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Customizable chart of daily datasets (HOBO and USGS Locations)
#     Written by: Dan Crocker, Spring 2018
#__________________________________________________________________________________________________________
# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:

#__________________________________________________________________________________________________________
# User Interface
#__________________________________________________________________________________________________________

LOADING_RCMODEL_UI <- function(id) {

  ns <- NS(id) # see General Note 1
  tagList(
    tabsetPanel(
      tabPanel("MODEL SETUP/INPUT:", ### INPUT UI ####
          div(id = ns("form2"), ### Set div() for Refresh Button ####
                fluidRow(
                  br(),
                   column(1,
                          div(actionButton(ns("refresh"), "REFRESH"), align = "center")
                   ),
                   column(11,
                          (h3("MODEL INPUT OPTIONS", align = "center"))
                   )
                 ),
                 fluidRow(
                   column(6,
                      wellPanel(h4("1. REQUIRED OPTIONS:"), ### Required options ####
                          br(),
                          uiOutput(ns("site_ui")),
                          uiOutput(ns("param_ui")),
                          uiOutput(ns("model_years_ui")),
                          uiOutput(ns("pred_years_ui")),
                          strong("Model Output Directory:"),# Bold Text
                          br(),
                          textInput(ns("dir"),label =  "Type a directory to save model results\n (change all backslashes to forward slashes)",
                                    value = "C:/WQDatabase/WAVE_WIT_Apps/Outputs")
                      ) # End Well Panel  
                   ), # end Column  
                   column(6,
                        wellPanel(
                          h4("2. OPTIONAL FILTERS:"), ### Optional Filters ####
                          br(),
                          checkboxInput(ns("rm_storms"),
                                        label =  "Remove Storm Samples from model data",
                                        value = FALSE),
                          checkboxInput(ns("rm_below_detect"),
                                        label =  "Remove Below detection results from model data",
                                        value = FALSE),
                          SELECT_SELECT_ALL_UI(ns("flag"))
                        ),# end Well Panel
                        wellPanel(h4("3. PREPARE MODEL INPUT DATA:", align = "left"), ### Prep Data Button ####
                          br(),
                          uiOutput(ns("prep_data.UI"))
                          # placeholder for messages
                        ), # end Well Panel
                        wellPanel(h4("4. RUN RCMODEL:", align = "left"), ### Run Model Button ####
                          br(),
                          uiOutput(ns("run_model.UI"))
                          # Placeholder for messages
                        ) # end Well Panel
                 ) # End Column
               ),
              fluidRow(
                column(12,
                  tabsetPanel( ### Model Data Tabs ####
                    tabPanel("Basic Model Data",
                             dataTableOutput(ns("rawdata"))
                    ),
                    tabPanel("Full Model Data",
                             dataTableOutput(ns("df_full"))
                    ), # End Tab Panel
                    tabPanel("Prediction Data",
                             dataTableOutput(ns("df_predict"))
                    ) # End Tab Panel
                  ) # End Tabset Panel
                ) # End Column
              )# End fluid row
          )  ### End Div ####
      ),
      tabPanel("MODEL OUTPUT:", ### Model Output Data Tab ####
               fluidRow(column(12,
                               h3("RC MODEL OUTPUT RESULTS", align = "center"),
                               br(),
                               wellPanel(
                                 div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput(ns("results_site_ui"))),
                                 div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput(ns("results_param_ui")))
                               )
               )
               ),
               fluidRow(column(12,
                               tabsetPanel(
                                 tabPanel("Monthly Loads",
                                          dataTableOutput(ns("results_month"))
                                 ),
                                 tabPanel("Annual Loads",
                                          dataTableOutput(ns("results_year"))
                                 ) # End Tab Panel
                               ) # End Tabset Panel
               )
               )
      ),# End Tab Panel
      tabPanel("MODEL REPORT:", ### Model Report ####
               fluidRow(column(12,
                          h3("RC MODEL OUTPUT REPORT", align = "center"),
                          br(),
                          wellPanel(
                            div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput(ns("report_site_ui"))),
                            div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput(ns("report_param_ui")))
                          )
                      )
               ),
               fluidRow(column(12,
                        htmlOutput(ns("report_html"))
                        )
               )
      )# End Tab Panel
    ) # End Tabset Panel
  ) # End tagList
} ### end UI ####

#__________________________________________________________________________________________________________
# Server Function ####
#__________________________________________________________________________________________________________
LOADING_RCMODEL <- function(input, output, session, df_wq, df_flow, df_precip, 
                            df_locs, gam_models, df_flags, df_flag_index, type = "wq"){
  
  ns <- session$ns # see General Note 1
  
  substrRight <- function(x, n){
    substr(x, nchar(x) -n + 1, nchar(x))
  }
  
  ### REACTIVE OBJECTS ####
  
  ### Location choices ####
  site_choices <- reactive({
    site_choices <- df_locs$LocationLabel[df_locs$Site %in% gam_models$Location] %>%  factor() %>% levels()
  })
  
  ### Locations Selected ####
  Site <- reactive({
    Site <- input$site
  })
  
 Param <- reactive({
    Param <- input$param
  }) 
  
  ### Parameter Choices ####
  ### Parameter Selection using Param_Select Module

  parameter_choices <- reactive({
    parameter_choices <- gam_models$Parameter[gam_models$LocationLabel %in% input$site] %>% factor() %>% levels()
  })
  
  ### 2. Reactive list - Here we need to get from a list of modeling parameters (like TN, to a list of actual parameters)
  # Test if input$params contains Total Nitrogen or InorgN... if so then replace with the component parts, add the others, and run unique()
  # This is only to 
  paramfilter <- reactive({
    req(input$param)
    paramfilter <- input$param
    
    if("Total Nitrogen" %in% input$param){
      paramfilter <- c(paramfilter, "Nitrate","Nitrite","Total Kjeldahl Nitrogen", "Ammonia")
      paramfilter <- paramfilter[!paramfilter %in% "Total Nitrogen"]
    }
    if("InorgN" %in% input$param){
      paramfilter <- c(paramfilter, "Nitrate","Nitrite", "Ammonia", -"InorgN")
      paramfilter <- paramfilter[!paramfilter %in% "InorgN"]
    }
  })
  
  ### Min and Max ranges for model fitting ####  Depends on Site selection
  min_model_range <- reactive({
    req(Df0)
    min_model_range <- min(as.numeric(year(Df0()$Date)))
  })
  max_model_range <- reactive({
    req(Df0)
    max_model_range <- max(as.numeric(year(Df0()$Date)))
  })

  ### Min and Max ranges for prediction ####  Depends on Site selection  
  # min_pred_range <- reactive({
  #   # req(Df_flow0())
  #   min_pred_range <- min(as.numeric(year(df_flow$Date)))
  # })
  # max_pred_range <- reactive({
  #   # req(Df_flow0())
  #   max_pred_range <- max(as.numeric(year(df_flow$Date)))
  # })
  min_pred_range <- min(as.numeric(year(df_flow$Date)))
  max_pred_range <- max(as.numeric(year(df_flow$Date)))
### REACTIVE DATAFRAMES ####
  ### Filter df_wq ####
    ### 1. Reactive Dataframe - Filter df_wq based on the Site(s) chosen
    Df0 <- reactive({
      req(input$site)
      df_wq %>% filter(LocationLabel %in% Site())
    })
    ### Filter Df0 by the reactive parameter list
    Df1 <- reactive({
      req(paramfilter())
      Df0() %>% filter(Parameter %in% paramfilter())
    })  
    
  ### Filter flow df ####
    ### 1. Reactive Dataframe - Filter df_wq based on the site(s) chosen
    # Df_flow0 <- reactive({
    #   req(input$site)
    #   Df_flow0 <- df_flow %>% filter(LocationLabel %in% input$site)
    # })
### UI OUTPUTS ####
  ### Site UI ####  
  output$site_ui <- renderUI({
    checkboxGroupInput(ns("site"),
                       label = "Choose Location(s):",
                       choices = site_choices(),
                       width = "100%",
                       inline = FALSE)
  })
  ### Param UI #### 
  output$param_ui <- renderUI({
    selectInput(ns("param"),
                label = "Choose Parameter(s):",
                choices = parameter_choices(),
                multiple = TRUE,
                selected = "")
  })
  ### Model Years UI ####
  output$model_years_ui <- renderUI({
    sliderInput(ns("model_years"),
                label = "Years of data to use for fitting model:",
                min = min_model_range(),
                max = max_model_range(),
                # value = c(min_model_range(),max_model_range()),
                value = c(2012,2017),
                round = TRUE,
                sep = "")
  })
  ### Pred Years UI ####
  output$pred_years_ui <- renderUI({
    sliderInput(ns("pred_years"),
                label = "Years for model prediction output:",
                min = min_pred_range,
                max = max_pred_range,
                # value = c(min_pred_range,max_pred_range),
                value = c(2012,2017),
                round = TRUE,
                sep = "")
  })
    ### Results_Site UI ####  
    output$results_site_ui <- renderUI({
      selectInput(ns("results_site"),
                  label = "Choose Location:",
                  choices = Site(),
                  multiple = FALSE,
                  selected = "")
    })
    ### Results_Param UI #### 
    output$results_param_ui <- renderUI({
      selectInput(ns("results_param"),
                  label = "Choose Parameter:",
                  choices = Param(),
                  multiple = FALSE,
                  selected = "")
    })
    
    ### Report_Site UI ####  
    output$report_site_ui <- renderUI({
      selectInput(ns("report_site"),
                  label = "Choose Location:",
                  choices = Site(),
                  multiple = FALSE,
                  selected = "")
    })
    ### Report_Param UI #### 
    output$report_param_ui <- renderUI({
      selectInput(ns("report_param"),
                  label = "Choose Parameter:",
                  choices = Param(),
                  multiple = FALSE,
                  selected = "")
    })
    
    ### Report UI #### 

    getReport <- function() {
      rep_site <- substrRight(input$report_site, 4)
      return(includeHTML(paste0(input$dir,"/", rep_site, "/", input$report_param, "/", "RC_Model_Report_", rep_site,"_", input$report_param, ".html")))
    }
    output$report_html <- renderUI({
        req(input$report_site)
        req(input$report_param)
        getReport()
      })
    
  ### FLAG MODULE ####
  ## server - Using the custom Module SELECT_SELECT_ALL, see script of dev manual
  Flag <- callModule(SELECT_SELECT_ALL, "flag",
                     label = "Select flag(s) to EXCLUDE samples from the modeling data:",
                     Choices = reactive({sort(unique(df_flags$label[df_flags$Flag_ID %in% df_flag_index$FlagCode & df_flags$Flag_ID != 114]))}),
                     colwidth = 3)
  
  # Subset the Sample Flag Index by the flags selected to exclude - this results in a vector of IDs to filter out
  flags <- reactive({
    as.numeric(substr(Flag(),1, 3))
  })
  
### Prep Model Data ####

  # Prep Data Action Button
  output$prep_data.UI <- renderUI({
    req(input$site)
    req(input$param)
    actionButton(inputId = ns("prep_data"),
                 label = "Prepare Model Input Data",
                 width = '100%')
  })
  
  # observeEvent(input$prep_data, {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'Model input data is being prepared')
  # })
  
  # Run the function to process the data and return 2 dataframes and path as list
  dfs <- eventReactive(input$prep_data,{
    source("functions/Prep_rcmodelData.R", local = T) # Hopefully this will overwrite functions as source changes...needs more testing
    PREPRCMODELDATA(df_wq = df_wq,
                    df_flow2 = df_flow,
                    df_precip = df_precip,
                    df_flag_index = df_flag_index,
                    df_locs = df_locs,
                    locs =  input$site,
                    model_pars = input$param,
                    rm_flags = flags(),
                    minyear_wq =  input$model_years[1],
                    maxyear_wq = input$model_years[2],
                    minyear_q = input$pred_years[1],
                    maxyear_q = input$pred_years[2],
                    rm_storms = input$rm_storms,
                    rm_bdl = input$rm_below_detect
    )
  })
  
  # Extract each dataframe

  rawdata <- reactive({dfs()[[1]]})
  df_flow3 <- reactive({dfs()[[2]]})
  df_full <- reactive({dfs()[[3]]})
### Text Outputs ####
 

  ### Table Outputs ####
  
  # Data for simple model
  output$rawdata <- renderDataTable({
    req(try(dfs()))
    rawdata()
  })
  
  # Data for more complex model
  output$df_full <- renderDataTable({
    req(try(dfs()))
    df_full()
  })
  # Prediction data
  output$df_predict <- renderDataTable({
    req(try(dfs()))
    df_flow3()
  })  
  
  output$results_month <- renderDataTable({
    req(input$results_site)
    req(input$results_param)
    results_site <- substrRight(input$results_site, 4)
    readRDS(paste0(input$dir,"/", results_site, "/", input$results_param, "/", "rc_month.rds"))
  })
 
   output$results_year <- renderDataTable({
     req(input$results_site)
     req(input$results_param)
     results_site <- substrRight(input$results_site, 4) 
     readRDS(paste0(input$dir,"/", results_site, "/", input$results_param, "/", "rc_year.rds"))
  })
  
### Run RCMODEL ####
  # Run Model Button - Will only be shown when a data is prepared successfully
  output$run_model.UI <- renderUI({
    req(try(dfs()))
    actionButton(inputId = ns("run_model"),
                 label = "Run Model",
                 width = '100%')
  })
  
  # observeEvent(input$run_model, {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'Model run in progress')
  #   
  # })
  
  ### Run the function to process the data and return 2 dataframes and path as list
   observeEvent(input$refresh, {
     shinyjs::reset("form2")
   })  
   
    observeEvent(input$run_model,{
    showModal(modalDialog(
      title = paste0("Model run(s) started at ", now()),
      paste0("Model run(s) have begun for location(s) (",Site(),") and parameter(s) (", Param(), ").\n A new message will appear upon completion."),
      fade = TRUE,
      footer = NULL
    ))
    ### Make sure output Directory is valid, if so then proceed with model loop, if not then send message to user
    if (file.exists(input$dir)){
        source("functions/RUNRCMODEL.R", local = T) # Hopefully this will overwrite functions as source changes...needs more testing
      # Create the output directory
      dir.create(input$dir, recursive = TRUE, showWarnings = FALSE )  
      ### Run Model for Locations Selected
        for(i in seq_along(Site())){
          RUNRCMODEL(rawdata = rawdata(),
                 gam_models = gam_models,
                 flow = df_flow3(),
                 df_full = df_full(),
                 loc = Site()[i],
                 pars = input$param,
                 dir = input$dir)
        }
    } else {
      stop("Output directory is not valid. Enter a valid directory to save model output")
    }
    showModal(modalDialog(
      title = paste0("Model run(s) finished at ", now()),
      paste0("Model report(s) can be viewd on the Model Report tab and output data is on the Model Output tab.\n All model outputs are available at ", 
             input$dir),
      easyClose = TRUE,
      footer = "Click anywhere or press esc. to continue"
      )
    )

  })
  
} # End Server function


