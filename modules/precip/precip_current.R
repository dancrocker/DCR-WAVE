###_________________________________________________________________
#     Title: precip_current.R
#     Type: Module for DCR Shiny App
#     Description: Displays current precipitation conditions
#     Written by: Dan Crocker, Summer 2018
###_________________________________________________________________

### UI #### 

PRECIP_CURRENT_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  tagList(
    wellPanel(em('Precipitation Overview'),
      br(),
      fluidRow(
        column(12, 
          br(),
          textOutput(ns("intro")),
          br()
        )
      ),
      fluidRow(
        column(7,
          tableOutput(ns("t_gauges")),
          br(),
          em("Current Precipitation Statistics"),
          br(),
          tableOutput(ns("t_precip_sum"))
        ), # End Column
        column(5,
               em("Map of Watershed Precipitation Stations"),
               br(),
               PRECIP_MAP_UI(ns("precip_map"))
        )
      ) # End FR
    ), # End Well Panel 
    wellPanel(
      fluidRow(
        column(6, 
          uiOutput(ns("plot1_ui")),     
          # plotOutput(ns("plot_bar_current")),
          checkboxInput(ns("plot1_type"), "Check here to make interactive plot)"),
          downloadButton(ns('save_plot1'), "Save Plot")
        ), # End Column
        column(6,
          uiOutput(ns("plot2_ui")),  
          # plotOutput(ns("plot_line_current")),
          checkboxInput(ns("plot2_type"), "Check here to make interactive plot)"),
          downloadButton(ns('save_plot2'), "Save Plot")
        ) # End Column
      ) # End FR
    ) # End Well Panel 
  ) # End TagList  
} # end UI

###_________________________________________________________________
# SERVER ####
###_________________________________________________________________

PRECIP_CURRENT <- function(input, output, session, df, df_site) {
  # df <- df_wach_prcp_daily
  callModule(PRECIP_MAP, "precip_map", df_site = df_site)
  source("functions/wach_precip_stats.R") # Function args (df)
  source("modules/plots/plot_precip.R") # Function args (df)
    dfs <- PRECIP_STATS(df, vyear = NULL)
    # dfs[[1]] <- PrcpMonthYear
    # dfs[[2]] <- PrcpMonthMean
    # dfs[[3]] <- YTD_J_Day
    # dfs[[4]] <- jday_sum_thisyear
    # dfs[[5]] <- jday_sum_vyear
    # dfs[[6]] <- t_gauge_summary
    # dfs[[7]] <- t_prcp_summary
    
  ns <- session$ns 

  output$intro <- renderText({paste0("Daily Wachusett Watershed precipitation has been compiled starting January 1, 1985 
    through ", format(max(df$DATE), '%B %d, %Y'), " (Day of Year # ", format(max(df$DATE), '%j'),"). The sources of precipitation 
    data have fluctuated through time and the daily watershed precipitation is a straight average of 1-4 gauges depending on what data is 
    available at a particular time. The table and map show the precipitation gauge locations that 
    have been utilized over the years to estimate watershed average precipitation.")})

df_gauges <- dfs[[6]]
df_gauges$`Start Date` <- format(df_gauges$`Start Date`,'%Y-%m-%d')
df_gauges$`End Date` <- format(df_gauges$`End Date`,'%Y-%m-%d')

output$t_gauges <-  renderTable(df_gauges, striped = T) 
output$t_precip_sum <-  renderTable(dfs[[7]], striped = T)

# Plot 1 - monthly bar chart
p1  <- reactive({
  p1 <- PRECIP_MONTH_BAR(df = dfs[[1]], vyear = year(Sys.Date()), type = input$plot1_type)
  p1
})

  output$plot1_static <- renderPlot({p1()})
  output$plot1_inter <- renderPlotly({p1()})
  
  output$plot1_ui <- renderUI({
    if(input$plot1_type == FALSE){
      plotOutput(ns("plot1_static"))
    }else{
      plotlyOutput(ns("plot1_inter"))
    }
  })

# Plot 2 - Cummulative line chart
p2  <- reactive({
  p2 <- PRECIP_LINE(df = df, vyear = year(Sys.Date()), type = input$plot2_type)
  p2
})

  output$plot2_static <- renderPlot({p2()})
  output$plot2_inter <- renderPlotly({p2()})
  
  output$plot2_ui <- renderUI({
    if(input$plot2_type == FALSE){
      plotOutput(ns("plot2_static"))
    }else{
      plotlyOutput(ns("plot2_inter"))
    }
  })

  # # Plot1 Print
  # output$save_plot1 <- downloadHandler(
  #   filename <- function() {
  #     paste0("Daily_", input$param1, "_at_", input$site,"_", Sys.Date(), ".", input$plot_save_type)},
  #   content = function(file) {
  #     ggplot2::ggsave(file, plot = p(), device = input$plot_save_type, width = input$plot_save_width, height = input$plot_save_height, dpi = 300)
  #     contentType = 'image/png'
  #   }
  # )
  # # Plot2 Print
  # output$save_plot2 <- downloadHandler(
  #   filename <- function() {
  #     paste0("Daily_", input$param1, "_at_", input$site,"_", Sys.Date(), ".", input$plot_save_type)},
  #   content = function(file) {
  #     ggplot2::ggsave(file, plot = p(), device = input$plot_save_type, width = input$plot_save_width, height = input$plot_save_height, dpi = 300)
  #     contentType = 'image/png'
  #   }
  # )

  # 
  # output$plot_ui <- renderUI({
  #   req(input$param1, input$site,(isTruthy(Date_Year$Lower()) & isTruthy(Date_Year$Upper())) | isTruthy(Date_Year$Years()))
  #   plotOutput(ns("flowPlot"), width = "100%", height = 600)
  # })
  # 
  # p  <- reactive({
  #   p <- HYDRO_G(Df = Df2(),
  #                df2 = Df3(),
  #                df_site = df_site,
  #                df_precip = df_precip,
  #                loc = input$site ,
  #                Y1Par = input$param1,
  #                Y2Par = input$param2 ,
  #                USGS_days = input$USGSdays ,
  #                USGS_IV = input$showUSGS,
  #                PlotPrecip = input$showPrecip)
  #   
  #   p
  # })
  # 
  # output$flowPlot <- renderPlot({p()})
  
 
} # End Server function


