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
      tabPanel("Plot flow with additional parameters",
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
                 
                 # First Location is selected  
                 # Then we need to choose either daily or instantaneous output (type = daily/iv) 
                 # If daily is selected then we use the filtered dataset Df_Flow_Wach$Long as Df()
                 # If iv is selected then we use the filtered dataset Df_Flow_Wach$Inst as Df_inst()
                 
                 
                 column(5, # Main selections
                  wellPanel(em('Plots use data from the "Select / Filter Data" tab'),
                    # Select Station
                    uiOutput(ns("site_ui")),
                    # Select type - daily or instantaneous daya
                    uiOutput(ns("type_ui")),
                    # Select y1 Parameter
                    uiOutput(ns("param1_ui")),
                    # Date Input - Using Module Date Select
                    uiOutput(ns("date_ui")),
                    # Select y2 parameter:
                    uiOutput(ns("y2_ui")),
                    uiOutput(ns("param2_ui")),
                    uiOutput(ns("plot_button_ui")) # Action button to render plot
                    # uiOutput(ns("param_wq_ui"))
                    # DATE_SELECT_UI(ns("date"))
                  ) # End Well Panel
                  # wellPanel(em('Optional Plot Additions:'),
                  #   checkboxInput(ns("showUSGS"), "Check here to show instantaneous discharge data\n(Note: for USGS Stations - Max 30 Days)"),
                  #   numericInput(ns("USGSdays"), "Days of USGS Instantaneous data:", min = 0, max = 90, value = 30, step = 1),
                  #   br(),
                  #   checkboxInput(ns("showPrecip"), "Check here to show daily precipitation (Watershed Average)"),
                  #   br(),
                  #   uiOutput(ns("param2_ui"))
                  # ) # End Well Panel
                 ) # End Column
               ) # End fluid row
      ) # End Tab Panel Sub
    ) # End Tabset Panel
  ) # end taglist
} # end UI


### Server Function ####

PLOT_FLOW <- function(input, output, session, Df, Df_inst, df_wq, df_site, df_precip) {

  ns <- session$ns # see General Note 1

### Function to pull right side characters ####  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

### List of sites --- come from the daily data df (which should be the same exact site list as the continuous #### 
Site <- reactive({Df()$LocationLabel %>% factor() %>% levels()})

### Site UI ####
output$site_ui <- renderUI({
  if(Df() %>% summarise(n()) %>% unlist() != 0){
    selectInput(ns("site"), "Location (Select 1):",
                choices = Site(),
                multiple = FALSE)
  }
})  

### Choices for Type UI ####
  type_choices <- reactive({
    if(is.null(Df_inst())){
      "Daily"
    } else {
      c("Daily", "Instantaneous")
    }
  })
  
### Type UI #### 
  output$type_ui <- renderUI({
    radioButtons(ns("type"), "Plot Daily or Instantaneous values:",
                 choices = type_choices(),
                 selected = "Daily",
                 inline = TRUE
    )
  })
  
### Data Type: Daily or Instantaneous ####
type <- reactive({input$type})

### Parameter 1 Choices ####
  Param1 <- reactive({
    req(input$site)
    req(input$type)
    switch(type(),
      "Daily" = Df()$Parameter %>% factor() %>% levels(),
      "Instantaneous" = Df_inst()$Parameter %>% factor() %>% levels()
    )
  })
  
### Parameter 1 UI  ####
  output$param1_ui <- renderUI({
    if(Df0() %>% summarise(n()) %>% unlist() != 0){
      selectInput(ns("param1"), "Parameter 1 - Primary Y-Axis (Continuous Sensor Data):",
                  choices = Param1(),
                  multiple = FALSE)
    }
  })
  
### DATES ####
  ### Min Date ####  
    min_date <- reactive({
      switch(type(),
             "Daily" = min(Df1()$Date, na.rm=TRUE),
             "Instantaneous" = as_date(min(Df_inst()$DateTime, na.rm=TRUE))
      )
    })
    
  ### Max Date ####    
    max_date <- reactive({
      switch(type(),
             "Daily" = max(Df1()$Date, na.rm=TRUE),
             "Instantaneous" = as_date(max(Df_inst()$DateTime, na.rm=TRUE))
      )
    })
  
### Date UI ####
  output$date_ui <- renderUI({
    req(input$site, input$param1)
    req(try(min_date()),try(max_date()))
    dateRangeInput(ns("date"), "Select a date range to plot",
                   start = min_date(), 
                   min = min_date(),
                   end = max_date(),
                   max = max_date(),
                   startview = "month",
                   separator = " - ") 
  })  
  
### Select Y2 Axis option ####  
  y2 <- reactive({
    input$y2
  })
 
### y2 UI ####
  output$y2_ui <- renderUI({
    req(input$site)
    
    radioButtons(ns("y2"), "Select data to plot on Secondary (Y2) Axis",
                 choices = list("Continuous Sensor parameters" = "sensor", 
                                "Daily Data" = "daily",
                                "Discrete grab sample parameters" = "wq",
                                "Daily Precipitation" = "precip",
                                "None" = "None"),
                 selected = "None",
                 inline = FALSE
    )
  })    


  
  
### Parameter 2 Choices - Depends on y2 #### 
  
### Daily Parameters ####
Param_daily <- reactive({
  req(input$site, input$type, input$param1)
  Df() %>% filter(!Parameter %in% input$param1) %>% 
    .$Parameter %>% factor() %>% levels()
})
  
### Sensor Parameters ####  
Param_sensor <- reactive({
  req(input$site, input$type, input$param1)
    Df_inst() %>% filter(!Parameter %in% input$param1) %>% 
      .$Parameter %>% factor() %>% levels()
  })
  
### WQ Parameters ####      
Param_wq <- reactive({
  req(input$site, input$type, input$param1)
    sort(unique(df_wq$Parameter[df_wq$LocationLabel == input$site & df_wq$Date >= input$date[1] & df_wq$Date <= input$date[2]]))
    })

### Parameter2 UI ####
  output$param2_ui <- renderUI({
    req(input$param1)
    req(input$y2 != "None")
    switch(input$y2,
           "None" =  NULL,
           "precip" = NULL,
           "wq" =     selectInput(ns("param2"), "Parameter 2 - Secondary Y-Axis (Discrete Water Quality Measurements):",
                                  choices = Param_wq(),
                                  multiple = FALSE),
           "sensor" = selectInput(ns("param2"), "Parameter 2 - Secondary Y-Axis (Continuous Sensor Data):",
                                    choices = Param_sensor(),
                                    multiple = FALSE),
           "daily" = selectInput(ns("param2"), "Parameter 2 - Secondary Y-Axis (Daily Data):",
                                  choices = Param_daily(),
                                  multiple = FALSE)
    )
  })

  ### y2 Parameter ####  
  y2param <- reactive({
    switch(input$y2,
           "None" =  "None" ,
           "precip" = "Precipitation",
           "daily" = input$param2,
           "wq" = input$param2,
           "sensor" = input$param2)
  })  
  
  
### Reactive Dataframes ####
  
  ### Filter daily data by site
  Df0 <- reactive({
    req(input$site)
    Df() %>% filter(LocationLabel == input$site)
    })
  
  ### Filter daily data based on parameter 1 and 2 chosen
  Df1 <- reactive({
    req(input$param1)
    Df0() %>% filter(Parameter %in% c(input$param1,input$param2))
    })

  ### Filter Daily data by Date
  Df2 <- reactive({
    req(input$param1)
    Df1() %>% filter(Date >= input$date[1] & Date <= input$date[2])
  })
  
  ### Reactive Dataframe - wq ####
  Df_wq <- reactive({
    req(input$param1, input$y2 == "wq")
      Df_wq <- df_wq %>%
        mutate("DateTime" = SampleDateTime) %>%
        filter(LocationLabel == input$site,
               Date >=  input$date[1] & Date <= input$date[2],
               Parameter == y2param())
      Df_wq
  })
  
  ### Reactive Dataframe - Instantaneous ####
  Df_inst0 <- reactive({
    # print(head(Df_inst()))
    Df_inst() %>% 
      ## Further filter df_inst2 by site, parameter, and dates
       filter(Location == input$site, 
              Parameter %in% c(input$param1, input$param2), 
              as_date(DateTime) >= input$date[1] & as_date(DateTime) <= input$date[2])
    })
  
# ### Plot Data for Y2 ####
# df_plot_y2 <- reactive({
#   if(type() == "Daily"){
#     df_plot_y2 <- Df2() %>% 
#       mutate(DateTime = force_tz(ymd_hms(paste(as.character(Date),"12:00:00", sep = " ")),tzone = "America/New_York"))
#     } else {
#     df_plot_y2 <- df_inst()
#   }
# })

dfy1 <-  reactive({
  dfy1 <- switch(type(),
         "Daily" = Df2() %>%
           filter(Parameter == input$param1) %>% 
           mutate(DateTime = force_tz(ymd_hms(paste(as.character(Date),"12:00:00", sep = " ")),tzone = "America/New_York")),
         "Instantaneous" = Df_inst0() %>% filter(Parameter == input$param1) 
  )
})
           
  
### Set y2 df based on if Daily or Instantaneous data is selected
dfy2 <- reactive({
    dfy2 <- switch(input$y2,
              "None" = NULL ,
              "precip" = precip(),
              "daily" = Df2() %>% 
                filter(Parameter == input$param2) %>% 
                mutate(DateTime = force_tz(ymd_hms(paste(as.character(Date),"12:00:00", sep = " ")),tzone = "America/New_York")),
              "wq" = Df_wq(),
              "sensor" = Df_inst0() %>% 
                filter(Parameter == input$param2)
    )
})

precip <- reactive({
      precip <- df_precip %>% 
                  select(DATE, DailyPrcpAve) %>%
                  filter(DATE >= input$date[1], DATE <= input$date[2]) %>%
                  mutate(DateTime = force_tz(ymd_hms(paste(as.character(DATE),"12:00:00", sep = " ")),tzone = "America/New_York"), 
                  Parameter = "Precipitation") %>% 
                  dplyr::rename("Result" = DailyPrcpAve)

})

### Plot Button ####
output$plot_button_ui <- renderUI({
  actionButton(inputId = ns("plot_button"), label = 'MAKE PLOT')
})

### Plot Button Click --> Show the plot panel
observeEvent(input$plot_button, {
  output$plot_ui <- renderUI({
    req(input$param1, input$site, input$type)
    plotOutput(ns("flowPlot"), width = "100%", height = 600)
  })
})

### Plot function ####
p <- eventReactive(input$plot_button,{
  print(head(dfy1()))
  print(input$y2)
  print(y2param())
  print(input$param2)
  print(input$site)
  print(head(precip()))
  # print(head(Df_wq()))
  print(head(dfy2()))
        p <- HYDRO_PLOT (df = dfy1(), # df for Y1 Data
                   daily_precip = precip(), # df for precip 
                   dfwq = Df_wq(), # df for discrete wq data
                   loc = input$site, # Site choice
                   df_site = df_site, # Site df
                   type = input$type, # Y1 plot data - Daily or instantaneous 
                   xmin = input$date[1], # Min date
                   xmax = input$date[2], # Max date
                   y1par = input$param1, # Parameter on Y1
                   y2par = y2param(), # Parameter on Y2 
                   df2 = dfy2(), # df for y2 data 
                   y2 = y2() # y2 data option
        )
    p
  })

observeEvent(input$plot_button, {
  output$flowPlot <- renderPlot({p()})
})

  # Plot Print
  output$save_plot <- downloadHandler(
    filename <- function() {
      paste0("Daily_", input$param1, "_at_", input$site,"_", Sys.Date(), ".", input$plot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p(), device = input$plot_save_type, width = input$plot_save_width, height = input$plot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )

} ### End Server function ####


