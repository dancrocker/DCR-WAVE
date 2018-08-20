###_________________________________________________________________
#     Title: precip_historical.R
#     Type: Module for DCR Shiny App
#     Description: Displays historical precipitation conditions
#     Written by: Dan Crocker, Summer 2018
###_________________________________________________________________

### UI #### 

PRECIP_HISTORICAL_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  tagList(
    h3("Historical Precipitation", align = "center"),
    tabsetPanel(
      tabPanel("Bar Plots",
        br(),
          fluidRow(
            column(4,
                   wellPanel(em('Plot Options'),
                             radioButtons(inputId = ns("slider_type"),
                                                label = "Plot precipitation totals by: ",
                                                selected = "month",
                                                choiceNames = c("Day", "Month","Year"),
                                                choiceValues = c("day", "month", "year")
                             ),
                             uiOutput(ns("slider_ui")),
                             # verbatimTextOutput(ns("range")),
                             checkboxInput(ns("plot_type"), "Check here to make interactive plot)"),
                             downloadButton(ns('save_plot'), "Save Plot")
                   ) # End Well Panel
            ), # End Column
            column(8,
                   uiOutput(ns("plot_ui")) # The plot
            ) # End Column
        ) # End FR
      ) # End TabPanel
      # tabPanel("Annual (Running)",
      # tabPanel("Box Plots")
      # tabPanel("Matrix")
    ) # End Tabset Panel
  ) # End TagList  
} # end UI

###_________________________________________________________________
# SERVER ####
###_________________________________________________________________

PRECIP_HISTORICAL <- function(input, output, session, df) {
  ns <- session$ns 
# df is df_wach_prcp_daily

  source("functions/wach_precip_stats.R") # Data
  source("modules/plots/plot_precip.R") # Plots
  
  dfs <- PRECIP_STATS(df, vyear = NULL) ### Running this again makes an independent dataset for historical plots w/o affecting current stats
  # dfs[[1]] <- PrcpMonthYear
  # dfs[[2]] <- PrcpMonthMean
  # dfs[[3]] <- YTD_J_Day
  # dfs[[4]] <- jday_sum_thisyear
  # dfs[[5]] <- jday_sum_vyear

  ### TIME SLIDER UI ####
  output$slider_ui <- renderUI({
    switch (input$slider_type,
            day = sliderInput(ns("date_range"), "Pick a date range:", min = min(df$DATE, na.rm = T), max = max(df$DATE, na.rm = T) - 1,
                              step = 1,
                              value = c(Sys.Date() - 90, Sys.Date() -1),
                              ticks = T,
                              timeFormat = "%Y-%m-%d"),
            month = sliderInput(ns("date_range"), "Pick a date range:", min = min(df$DATE, na.rm = T), max = floor_date(max(df$DATE, na.rm = T), unit = "month"),
                                step = 31,
                                value = c(Sys.Date() - 500, Sys.Date() -1),
                                ticks = T,
                                timeFormat = "%b-%Y"),
            year = sliderInput(ns("date_range"), "Pick a date range:", min = min(df$DATE, na.rm = T), max = floor_date(max(df$DATE, na.rm = T), unit = "year") - 365,
                                 step = 365,
                                 value = c(as_date("1985-01-01"), floor_date(max(df$DATE, na.rm = T), unit = "year")),
                                 ticks = T,
                                 timeFormat = "%Y") 
    )
  })
  
  # output$range <- renderPrint({ input$date_range })
  
  ### Make Plot Object based on Slider Type ####
  p  <- reactive({
    switch(input$slider_type,
           day = PRECIP_DAILY_BAR(df = df, date_min = input$date_range[1], date_max = input$date_range[2], type = input$plot_type),
           month =  PRECIP_MONTH_BAR2(df = dfs[[1]], date_min = input$date_range[1], date_max = input$date_range[2], type = input$plot_type),
           year =  PRECIP_YEAR_BAR(df = dfs[[3]], date_min = input$date_range[1], date_max = input$date_range[2], type = input$plot_type)
    )
  }) 

  ### PLOT UI ####
  
  output$plot_static <- renderPlot({p()})
  output$plot_inter <- renderPlotly({p()})
  
  output$plot_ui <- renderUI({
    if(input$plot_type == FALSE){
      plotOutput(ns("plot_static"))
    }else{
      plotlyOutput(ns("plot_inter"))
    }
  })
  
  # # Plot Print
  # output$save_plot <- downloadHandler(
  #   filename <- function() {
  #     paste0("Daily_", input$param1, "_at_", input$site,"_", Sys.Date(), ".", input$plot_save_type)},
  #   content = function(file) {
  #     ggplot2::ggsave(file, plot = p(), device = input$plot_save_type, width = input$plot_save_width, height = input$plot_save_height, dpi = 300)
  #     contentType = 'image/png'
  #   }
  # )
  
  } # End Server function


