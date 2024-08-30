##############################################################################################################################
#     Title: plot_gw_percentile.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Recent Groundwater plot 
#     Written by: Travis Drury 
##############################################################################################################################

# Notes:


##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_GW_PERCENTILE_UI <- function(id, well_sites) {

  ns <- NS(id)
  
  tagList(
    fluidRow(
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("plot_ui")),
        radioButtons(ns("site_select"), "Well Selection",
                     choices = paste0(well_sites %>% arrange(Site) %>% filter (LocationCategory=="Primary") %>% select(LocationLabel) %>% unlist() %>% unname())),
        sliderInput(ns("months"), "Months of Data",
                    min = 1, max = 12,
                    value = 6, step = 1),
      ),
      mainPanel(
        plotOutput(ns("plot"))
        # plotlyOutput(ns("plot_inter"), width = "100%", height=600) #Interactive plotly option
      )
      ),
    ), # end fluidRow
    fluidRow(
      tabPanel("Save Plot", br(), br(),
               PLOT_SAVE_UI(ns("save"))
      )
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_GW_PERCENTILE <- function(input, output, session, Df) {

  # Turn off warnings about summarise groups
  options(dplyr.summarise.inform = FALSE)
  
  ns <- session$ns # see General Note 1

##################### Main Reactive Expressions

  
  
# Two Dataframes (Primary and Secondary Axis)

  Df1 <- reactive({
    req(input$site_select)
    req(input$months)
    Df %>% filter(LocationLabel == input$site_select,
                    Parameter == "Depth Below Ground Surface",
                    Date >= floor_date(Sys.Date(),'month') %m-% months(input$months-1))})


  percentiles <- reactive({
    Df %>% filter(LocationLabel == input$site_select,
                                 Parameter == "Depth Below Ground Surface") %>%
                          mutate(Month = month(Date)) %>%
    group_by(Month) %>%
    summarise(percentile0 = 0,
              percentile5 = quantile(Result, 0.05, na.rm=TRUE),
              percentile10 = quantile(Result, 0.1, na.rm=TRUE),
              percentile25 = quantile(Result, 0.25, na.rm=TRUE),
              percentile75 = quantile(Result, 0.75, na.rm=TRUE),
              percentile90 = quantile(Result, 0.90, na.rm=TRUE),
              percentile95 = quantile(Result, 0.95, na.rm=TRUE),
              percentile100 = max(Result, na.rm=TRUE)
    )}) 
    
    
    percentiles_long <- reactive({ percentiles() %>%
      mutate(
             `0-5%` = percentile5-percentile0,
             `5-10%` = percentile10-percentile5,
             `10-25%` = percentile25-percentile10,
             `25-75%` = percentile75-percentile25,
             `75-90%` = percentile90-percentile75,
             `90-95%` = percentile95 - percentile90,
             `95-100%` = percentile100 - percentile95) %>% 
        dplyr::select(1, 10:16) %>% 
        pivot_longer(cols=(2:8), names_to="Percentile", values_to="PercentileValue")
      
    })
      
    percentiles_plot <- reactive({Df1() %>% 
      dplyr::select("Date") %>% 
      mutate(Month=month(Date)) %>% 
      right_join(percentiles_long(), by="Month",relationship="many-to-many") %>%
      filter(!is.na(Date)) %>%
      distinct()%>%
      mutate(Date=as.Date(paste0(year(Date),"-",month(Date),"-15")),
             Percentile = factor(Percentile, levels=c("95-100%","90-95%","75-90%","25-75%","10-25%","5-10%","0-5%")))
          })
    

  median_data <- reactive({
    Df %>% 
    mutate(Month = month(Date)) %>%
    filter(LocationLabel == input$site_select,
           Parameter == "Depth Below Ground Surface") %>%
    group_by(Month) %>%
    summarise(Median = median(Result, na.rm=TRUE)) })
    
  median_plot <- reactive ({Df1() %>% 
      dplyr::select("Date") %>% 
      mutate(Month=month(Date)) %>% 
      inner_join(median_data(), by="Month") %>%
      distinct()%>%
      mutate(Date=as.Date(paste0(year(Date),"-",month(Date),"-15")))
  })
  

  # Plot Output
  output$plot <- renderPlot({
    P()
  })
  
  Plot_Name <- reactive({
    paste0(input$site_select," Water Levels with Historical Percentiles ",Sys.Date())
  })
  
  ### Interactive plotly option
  # output$plot_inter <- renderPlotly({
  #   ggplotly(P(), tooltip = c("y", "x"))
  # })
  
  
  # Plot
  P <- reactive({
    
    p <- ggplot(data=Df1(),aes(x=Date))+
      geom_bar(data=percentiles_plot(), aes(x=Date, y=PercentileValue, fill=Percentile), position="stack", stat="identity", width=28, alpha=0.6, just=0.5)+
      geom_point(data=median_plot(), aes(y=Median, x=Date,color="Median"),shape=17,size=2,alpha=0.5)+
      scale_color_manual(values=c("black"))+
      geom_point(aes(y=Result,shape="Measurement"), color="black",fill="red",size=5)+
      scale_shape_manual(values=c(23))+
      scale_fill_manual(values=c("NA","#DD3D2D","#FDB366","#D9F0D3","#98CAE1","#4A7BB7","NA"),
                        labels=c("","5-10%","10-25%","25-75%","75-90%","90-95%",""))+
      # scale_y_reverse()+
      ylab("Depth Below Ground Surface (ft)")+
      ggtitle(paste0(input$site_select," Water Levels with Historical Percentiles"))+
      theme_bw()+
      scale_x_date(breaks = seq.Date(from=(floor_date(Sys.Date(),'month') %m-% months(input$months-1) + days(15)),to=(floor_date(Sys.Date(),"month") + days(15)), by="1 month"),
                   date_labels = "%b-%Y",
                   minor_breaks = NULL,
                   limit = c(floor_date(Sys.Date(),'month') %m-% months(input$months-1),
                             ceiling_date(Sys.Date(),"month")),
                   expand=c(0,0))+
      guides(fill = guide_legend(reverse=TRUE))+
      theme(legend.title=element_blank(),
            panel.grid.major.x = element_blank())+
      coord_cartesian(ylim=c(max(percentiles()$percentile95,Df1()$Result),
                             min(percentiles()$percentile5,Df1()$Result)))
    
    p
    })
    
  
  # Save Tab and Download Function
  callModule(PLOT_SAVE, "save",
             P = P,
             Plot_Name = Plot_Name)
  
} # end Server Function


