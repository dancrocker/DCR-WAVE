##############################################################################################################################
#     Title: plot_flow.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Customizable chart of daily datasets (HOBO and USGS Locations)
#     Written by: Dan Crocker, Spring 2018
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
# App location: app.r (Hydro//Met page) -->
#  PLOT_FLOW_UI --> (Df = Df_Flow_Wach$Long, df2 = df_trib_wach, df_prcip = df_wach_prcp_daily)
# df is the filtered or full version of Df_Flow (long format)
#Df = df_wach_flow
##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_FLOW_UI <- function(id) {

  ns <- NS(id) # see General Note 1
  # Df <- df


  tagList(
    tabsetPanel(
      tabPanel("Custom Hydrographs",
               fluidRow(
                 column(7, # Plot Output
                    uiOutput(ns("plot_ui")),
                    fluidRow(br(),
                             column(4,
                                    downloadButton(ns('save_plot'), "Save Plot")
                             ),
                             column(4,
                                    numericInput(ns("plot_save_width"), "Plot Width (inches):", 7,
                                                 min = 3, max = 17, step = 0.25),
                                    numericInput(ns("plot_save_height"), "Plot Height (inches):", 5,
                                                 min = 3, max = 17, step = 0.25)
                             ),
                             column(4,
                                    radioButtons(ns("plot_save_type"), "File Type:",
                                                 choices= c("pdf","jpg","png"),
                                                 selected = "png")
                             ) # End Col
                    ) # End fr
                 ), # End Column
                  # column(1), # Spacer Column
                 column(5, # Main selections
                  wellPanel(em('Plots use data from the "Select / Filter Data" tab'),
                    # Select 1 Station
                    uiOutput(ns("site_ui")),
                    # Select 1 Parameter
                    uiOutput(ns("param1_ui")),
                    br(),
                    # Date Input - Using Module Date Select
                    DATE_SELECT_UI(ns("date"))
                  ), # End Well Panel
                  wellPanel(em('Optional Plot Additions:'),
                    checkboxInput(ns("showUSGS"), "Check here to show instantaneous discharge data\n(Note: for USGS Stations - Max 30 Days)"),
                    numericInput(ns("USGSdays"), "Days of USGS Instantaneous data:", min = 0, max = 30, value = 7, step = 1),
                    br(),
                    checkboxInput(ns("showPrecip"), "Check here to show daily precipitation (Watershed Average)"),
                    br(),
                    uiOutput(ns("param2_ui"))
                  ) # End Well Panel
                 ) # End Column
               ) # End fluid row
      ) # End Tab Panel Sub
    ) # End Tabset Panel
  ) # end taglist
} # end UI

##############################################################################################################################
# Server Function
##############################################################################################################################

PLOT_FLOW <- function(input, output, session, Df, df2, df_site, df_precip) {

  ns <- session$ns # see General Note 1

###################################################

  # Location choices - come from Df (the filtered Dataframe) This list only changes when the filter changes (Select 1)
  Site <- reactive({Df()$LocationLabel %>% factor() %>% levels()})

  # Parameter Choices -
    #Param is always based on the filtered dataset Df - whatever parameters are there (select 1)
    Param1 <- reactive({Df()$Parameter %>% factor() %>% levels()})
    # Param2 comes from df2 (which are descrete trib samples - needs to get filtered by site chosen and date range available
    Param2 <- reactive({sort(unique(df2$Parameter[df2$LocationLabel == input$site & df2$Date >= mindate() & df2$Date <= maxdate()]))})

  # values <- reactiveValues()
  # Date range values to use for filtering Param2
  # values$mindate <- reactiveValues(min(Df()$Date))
  # values$maxdate <- reactiveValues(max(Df()$Date))
    mindate <- reactive(min(Df()$Date, na.rm=TRUE))
    maxdate <- reactive(max(Df()$Date, na.rm=TRUE))

   # Site UI
    output$site_ui <- renderUI({
      if(Df() %>% summarise(n()) %>% unlist() != 0){
        selectInput(ns("site"), "Location (Select 1):",
                    choices = c(Site(), "None"),
                    selected = "None",
                    multiple = FALSE)
      }
    })

  # Parameter UI - Axis Choice for Primary Axis
  output$param1_ui <- renderUI({
    if(Df() %>% summarise(n()) %>% unlist() != 0){
      selectInput(ns("param1"), "Parameter 1 - Primary Y-Axis (Daily):",
                  choices = c(Param1(),"None"),
                  multiple = FALSE)
    }
  })
  # Parameter2 UI
  output$param2_ui <- renderUI({
    req(input$site, input$param1)
      selectInput(ns("param2"), "Parameter 2 - Secondary Y-Axis (Discrete Samples):",
                  choices = c(Param2(),"None"),
                  selected = "None",
                  multiple = FALSE)
  })

  # # Reactive Dataframe - Filter Df based on the site chosen
  Df0 <- reactive({
    req(input$site)
    Df() %>% filter(LocationLabel == input$site)})
  # # Reactive Dataframe - Filter Df based on parameter 1 chosen
  Df1 <- reactive({
    req(input$param1)
    Df0() %>% filter(Parameter == input$param1)})

  Date_Year <- callModule(DATE_SELECT, "date", Df = Df)
  # Reactive Dataframe - filter for date, and remove rows with NA for Result

  Df2 <- reactive({
    # Wait for all neccesary Inputs to Proceed
    req(input$param1,
        (isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years())) # See General Note _

    Df1() %>%
      # Filter by either Date Range or By Years (Include both)
      filter(Date >= Date_Year$Lower() & Date <= Date_Year$Upper() | year(Date) %in% Date_Year$Years())
  })
  # Reactive Dataframe - filter
  Df3 <- reactive({
    req(input$param1, input$param2,
        (isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years())) # See General Note _
    if(input$param2 != "None"){
      Df3 <- df2 %>% filter(Date >= Date_Year$Lower() & Date <= Date_Year$Upper() | year(Date) %in% Date_Year$Years(),
             Parameter == input$param2)
    } else {
      NULL
    }
  })



  output$plot_ui <- renderUI({
    req(input$param1, input$site,(isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years()))
      plotOutput(ns("flowPlot"), width = "100%", height = 600)
  })

p  <- reactive({
    p <- HYDRO_G(Df = Df2(),
                 df2 = Df3(),
                 df_site = df_site,
                 df_precip = df_precip,
                 loc = input$site ,
                 Y1Par = input$param1,
                 Y2Par = input$param2 ,
                 USGS_days = input$USGSdays ,
                 USGS_IV = input$showUSGS,
                 PlotPrecip = input$showPrecip)

    p
  })

  output$flowPlot <- renderPlot({p()})

  # Plot Print
  output$save_plot <- downloadHandler(
    filename <- function() {
      paste0("Daily_", input$param1, "_at_", input$site,"_", Sys.Date(), ".", input$plot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p(), device = input$plot_save_type, width = input$plot_save_width, height = input$plot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )

} # End Server function


