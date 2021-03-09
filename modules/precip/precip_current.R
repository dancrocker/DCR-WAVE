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
          uiOutput(ns("year1_ui")),
          uiOutput(ns("plot1_ui")),     
          # plotOutput(ns("plot_bar_current")),
          checkboxInput(ns("plot1_type"), "Check here to make interactive plot"),
          downloadButton(ns('save_plot1'), "Save Plot"),
          br(),
          br(),
          h5(textOutput(ns("table1_title"))),
          uiOutput(ns("table1"))
        ), # End Column
        column(6,
          uiOutput(ns("year2_ui")),
          uiOutput(ns("plot2_ui")),  
          # plotOutput(ns("plot_line_current")),
          checkboxInput(ns("plot2_type"), "Check here to make interactive plot"),
          downloadButton(ns('save_plot2'), "Save Plot")
        ) # End Column
      ) # End FR
    ) # End Well Panel 
  ) # End TagList  
} # end UI

###_________________________________________________________________
# SERVER ####
###_________________________________________________________________

PRECIP_CURRENT <- function(input, output, session, df, df_site, wshed) {
  # df <- df_wach_prcp_daily
  
  df_site <- df_site %>% filter(Watershed == wshed)
  
  callModule(PRECIP_MAP, "precip_map", df_site = df_site)
  source("functions/quab_precip_stats.R") # Function args (df)
  source("functions/wach_precip_stats.R") # Function args (df)
  source("modules/plots/plot_precip.R") # Function args (df)
  
  if(wshed == "Wachusett"){  
  dfs <- PRECIP_STATS_WACH(df, vyear = NULL)
  } else {
  dfs <- PRECIP_STATS_QUAB(df, vyear = NULL)  
  }
    # dfs[[1]] <- PrcpMonthYear
    # dfs[[2]] <- PrcpMonthMean
    # dfs[[3]] <- YTD_J_Day
    # dfs[[4]] <- jday_sum_thisyear
    # dfs[[5]] <- jday_sum_vyear
    # dfs[[6]] <- t_gauge_summary
    # dfs[[7]] <- t_prcp_summary
    
  ns <- session$ns 

intro_text <- switch(wshed,
      "Wachusett" = paste0("Daily Wachusett Watershed precipitation has been compiled starting January 1, 1985 
    through ", format(max(df$DATE), '%B %d, %Y'), " (Day of Year # ", format(max(df$DATE), '%j'),"). The sources of precipitation 
    data have fluctuated through time and the daily watershed precipitation is a straight average of 1-4 gauges depending on what data is 
    available at a particular time. The table and map show the precipitation gauge locations that 
    have been utilized over the years to estimate watershed average precipitation."),
      "Quabbin" = paste0("Intro Text")
  )
 
 output$intro <- renderText({intro_text})

  
df_gauges <- dfs[[6]]
df_gauges$`Start Date` <- format(df_gauges$`Start Date`,'%Y-%m-%d')
df_gauges$`End Date` <- format(df_gauges$`End Date`,'%Y-%m-%d')

output$t_gauges <-  renderTable(df_gauges, striped = T) 
output$t_precip_sum <-  renderTable(dfs[[7]], striped = T)



# Year UIs
output$year1_ui <- renderUI({
  sliderInput(ns("vyear1"), "Switch to another year:", min = 1985, max = max(year(df$DATE)), 
              value = max(year(df$DATE)), step = 1, sep = "", ticks = F)

})  

output$year2_ui <- renderUI({
  sliderInput(ns("vyear2"), "Switch to another year:", min = 1985, max = max(year(df$DATE)), 
              value = max(year(df$DATE)), step = 1, sep = "", ticks = F)
  
})  




# Plot 1 - monthly bar chart [[1]] and table [[2]]
p1out  <- reactive({
  p1out <- PRECIP_MONTH_BAR(df1 = dfs[[1]], df2 = dfs[[2]], vyear = input$vyear1, type = input$plot1_type)
  p1out
})


p1 <- reactive({
  p1out()[[1]]
  })
t1 <- reactive({
  p1out()[[2]]
  })

output$table1_title <- renderText({
  paste0(wshed, " Monthly Precipitation totals for ", input$vyear1)
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

  output$table1_ui <- renderUI({
    tableOutput(ns("table1"))
  })
  
  output$table1 <- renderTable(t1(), bordered = T, rownames = T, striped = T)

  # Plot 2 - Cummulative line chart
p2  <- reactive({
  p2 <- PRECIP_LINE(df = df, vyear = input$vyear2, type = input$plot2_type, wshed = wshed)
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

  # Plot1 Print
  output$save_plot1 <- downloadHandler(
    filename <- function() {
      paste0("MonthlyPrecip_", input$vyear1, ".png")},
    content = function(file) {
      ggplot2::ggsave(file, plot = p1(), device = "png", units = "in", width = 6, height = 4, dpi = 300)
      contentType = 'image/png'
    }
  )
  # Plot2 Print
  output$save_plot2 <- downloadHandler(
    filename <- function() {
      paste0("CummulativePrecip_", input$vyear2, ".png")},
    content = function(file) {
      ggplot2::ggsave(file, plot = p2(), device = "png", units = "in", width = 6, height = 4, dpi = 300)
      contentType = 'image/png'
    }
  )

} # End Server function


