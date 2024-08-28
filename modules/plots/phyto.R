##############################################################################################################################.
#     Title: Plot-Phyto.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time series plots for phytoplankton data
#     Written by: Dan Crocker, Fall 2017
#     Edits: JTL updated document outline, minor changes to defaults, Jan 2020
##############################################################################################################################.

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#
#App location: Reservoir --> Biological --> Phytoplankton --> Wachusett
# 4 Tab Panels Phytoplankton, Taxa, Historical, Filter-Export
# Panel Outlines:
# Phytoplankton
# 3 fluid rows
# Row 1 has Data filter - Location Dropdown, Year dropdown, 2 slider bars 0-35 meters, button to create chart
# Row 2 is a big plot
# Row 3 has plot options

##############################################################################################################################.
# User Interface ####
##############################################################################################################################.

PHYTO_UI <- function(id,df) {

  ns <- NS(id) # see General Note 1
  df <- df %>%
    mutate(Year = year(Date))
  
## Phytoplankton Overview Plot ####
  tagList(
    tabsetPanel(
      tabPanel("Phytoplankton Overview Plot",
               # Data filter for plot
               fluidRow(column(5, # fr2 - Site and Year Input
                               selectInput(ns("site"), "Station(s):",
                                           choices = df %>% .$Station %>% levels() %>% paste(),
                                           multiple = TRUE,
                                           width = '200',
                                           selected = c("202","206", "BN3417", "CI3409")),
                               selectInput(ns("year"), "Year:",
                                           choices = df %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                           width = '200',
                                           selected = 1)
               ), # End Column
               column(1), # Spacer Column
               column(5, # Depth Range sliders
                      sliderInput(ns("Depth1"), "Epilimnion Depth Range (meters):",
                                  min = 0, max = 10, value = c(0,10), step = 0.5),
                      sliderInput(ns("Depth2"), "Epi-Metalimnion Depth Range (meters):",
                                  min = 6, max = 40, value = c(10,40), step = 0.5)
               ) # End Column
               ), # End fluid row
               fluidRow(br()),
               fluidRow(
                 column(12, # fr3 - Plot
                        plotOutput(ns("PhytoPlot"), width = "100%", height = 600)
                 )
               ), # End fr
               fluidRow(br(),
                        column(2,
                               downloadButton(ns('save_plot'), "Save Plot")
                        ),
                        column(2,
                               numericInput(ns("plot_save_width"), "Plot Width (inches):", 7,
                                            min = 3, max = 17, step = 0.25),
                               numericInput(ns("plot_save_height"), "Plot Height (inches):", 5,
                                            min = 3, max = 17, step = 0.25)
                        ),
                        column(2,
                               radioButtons(ns("plot_save_type"), "File Type:",
                                            choices= c("pdf","jpg","png"),
                                            selected = "png")
                        ) # End Col
                        # column(2,
                        # checkboxGroupInput(ns("plot_save_grid"), "Gridline Override:",
                        #                     choices= c("major gridlines", "minor gridlines"))
                        # ) # end column
               ) # End fr
      ), # End Tab Panel Sub

## Taxa Plots ####
      tabPanel("Taxa Plots",
               # Function Args
               fluidRow(column(4, # Sites
                               selectInput(ns("taxasite"), "Station(s):",
                                           choices = df %>% .$Station %>% levels() %>% paste(),
                                           multiple = TRUE,
                                           width = '200',
                                           selected = c("202","206", "BN3417", "CI3409")),
                               selectInput(ns("taxayear"), "Year:", # Year
                                           choices = df %>% .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                           width = '200',
                                           selected = 1)
               ), # End Col
               column(4,
                      numericInput(ns("taxaplot_save_width"), "Plot Width (inches):", 7,
                                   min = 3, max = 17, step = 0.25),

                      numericInput(ns("taxaplot_save_height"), "Plot Height (inches):", 5,
                                   min = 3, max = 17, step = 0.25)
               ), # End Col
               column(4,
                      radioButtons(ns("taxaplot_save_type"), "File Type:",
                                   choices= c("pdf","jpg","png"),
                                   selected = "png")
               ) # End Col
               ), # End fr
               fluidRow(br()),
               fluidRow(column(9, plotOutput(ns("taxaplot1"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot1'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot2"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot2'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot3"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot3'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot4"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot4'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot5"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot5'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot6"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot6'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot7"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot7'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot8"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot8'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot9"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot9'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot10"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot10'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot11"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot11'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot12"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot12'), "Save Plot"))),
               fluidRow(column(9, plotOutput(ns("taxaplot13"), width = "100%", height = 400)),
                        column(3, br(),downloadButton(ns('save_plot13'), "Save Plot"))),
               fluidRow(column(4, "Other (Select Taxa):",
                               selectInput(ns("taxa14"), "Taxa:",
                                           choices = df %>%  .$Taxa %>%  unique() %>% sort(decreasing = FALSE),
                                           width = '200',
                                           selected = NULL)
               ), # End Col
               column(4,br(),
                      selectInput(ns("taxa14year"), "Year:",
                                  choices = df %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                  width = '200',
                                  selected = 1)
               ),
               column(3, br(),br(),downloadButton(ns('save_plot14'), "Save Plot"))
               ), # End fr
               fluidRow(column(9, plotOutput(ns("taxaplot14"), width = "100%", height = 400)))
      ), # End Tab Panel Taxa Plots

## Historical Comparison Plots ####
      tabPanel("Historical Comparison Plots",
               fluidRow(column(3, # Sites
                               selectInput(ns("histtaxa"), "Taxa:",
                                           choices = df %>%  .$Taxa %>%  unique() %>% sort(decreasing = FALSE),
                                           width = '200',
                                           selected = 1),
                               selectInput(ns("histlocs"), "Station(s):",
                                           choices = df %>% .$Station %>% levels() %>% paste(),
                                           multiple = TRUE,
                                           width = '200',
                                           selected = c("202","206", "BN3417", "CI3409")),
                               selectInput(ns("histyear"), " Comparison Year:",
                                           choices = df %>%  .$Year %>%  unique() %>% sort(decreasing = TRUE),
                                           width = '200',
                                           selected = 1),
                               radioButtons(ns("stat"), "Stat for Comparison Year:",
                                            choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                            selected = "ave_val"),
                               sliderInput(ns("depth"), "Depth Range (m):",
                                           min = 0, max = ceiling(max(df$Depth_m, na.rm = TRUE)), value = c(0,ceiling(max(df$Depth_m, na.rm = TRUE))), step = 1, sep = "")
               ), # End Col
               column(5,
                      sliderInput(ns("yg1"), "Year Grouping 1 (Min-Max):",
                                  min = min(df$Year), max = max(df$Year), value = c(2012,max(df$Year)), step = 1, sep = ""),
                      sliderInput(ns("yg2"), "Year Grouping 2 (Min-Max):",
                                  min = min(df$Year), max = max(df$Year), value = c(2007,max(df$Year)), step = 1, sep = ""),
                      sliderInput(ns("yg3"), "Year Grouping 3 (Min-Max):",
                                  min = min(df$Year), max = max(df$Year), value = c(min(df$Year),max(df$Year)), step = 1, sep = "")
               ), # End Col
               column(3,
                      radioButtons(ns("stat1"), "Stat for Year Group 1:",
                                   choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                   selected = "ave_val"),
                      radioButtons(ns("stat2"), "Stat for Year Group 2:",
                                   choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                   selected = "ave_val"),
                      radioButtons(ns("stat3"), "Stat for Year Group 3:",
                                   choices= c("Minimum" = "min_val", "Average" = "ave_val", "Maximum" = "max_val"),
                                   selected = "ave_val")
               ) #End Col
               ), # End fr
               fluidRow(column(9, plotOutput(ns("histplot"), width = "100%", height = 600))),
               fluidRow(column(4,
                               numericInput(ns("histplot_save_width"), "Plot Width (inches):", 7,
                                            min = 3, max = 17, step = 0.25),

                               numericInput(ns("histplot_save_height"), "Plot Height (inches):", 5,
                                            min = 3, max = 17, step = 0.25)
                        ), # End Col
                        column(4,
                               radioButtons(ns("histplot_save_type"), "File Type:",
                                            choices= c("pdf","jpg","png"),
                                            selected = "png")
                        ),
                        column(3,
                               downloadButton(ns('save_histplot'), "Save Plot")
                        ) # End Col
               ) # End fr
      ), # End Tab Panel Sub

## Filter/Export Data ####
      tabPanel("Filter/Export Data",
               fluidRow(column(4, # Sites
                               selectInput(ns("filtersite"), "Station(s):",
                                           choices = df %>% .$Station %>% levels() %>% paste(),
                                           multiple = TRUE,
                                           width = '200',
                                           selected = c("202","206", "BN3417", "CI3409"))
                      ) # End Col
               ) # End fr
      ) # End Tab Panel
    ) # End Tabset Panel
  ) # end taglist
} # end UI

##############################################################################################################################.
# Server Function ####
##############################################################################################################################.

PHYTO <- function(input, output, session, df) {

  # p  <- reactive({
  #
  #
  #     # Save Options
  #
  #     # Size dependent? Change size for saving?
  #     p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))
  #
  #     # Gridlines for saving options
  #     if("major gridlines" %in% input$plot_save_grid){
  #       p <- p + theme(panel.grid.major = element_line())
  #     }
  #     if("minor gridlines" %in% input$plot_save_grid){
  #       p <- p + theme(panel.grid.minor = element_line())
  #       p
  #       }
  #     })
  
  ### Overview Plot ####
  
    p <- p  <- reactive({
    p <- phytoplot(df = df,
                   locs = input$site,
                   vyear = input$year,
                   epi_min = input$Depth1[1],
                   epi_max = input$Depth1[2],
                   em_min = input$Depth2[1],
                   em_max = input$Depth2[2])
    p
  })

  output$PhytoPlot <- renderPlot({p()})

  # Plot Print
  output$save_plot <- downloadHandler(
    filename = function() {
      paste0("Phytoplankton-", input$year,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$plot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p(), device = input$plot_save_type, width = input$plot_save_width, height = input$plot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  ### Taxa Plots ####
  # Total_Diatoms 128 dodgerblue
  p1 <- reactive({
    p1 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Total Diatoms", color = "dodgerblue")
    p1
  })
  output$taxaplot1 <- renderPlot({p1()})
  
  # Asterionella 430 lightskyblue
  p2 <- reactive({
    p2 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Asterionella", color = "lightskyblue")
    p2
  })
  output$taxaplot2 <- renderPlot({p2()})
  # Cyclotella 441 lightsteelblue3
  p3 <- reactive({
    p3 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Cyclotella", color = "lightsteelblue3")
    p3
  })
  output$taxaplot3 <- renderPlot({p3()})
  # Total_Chlorophytes 472 mediumseagreen
  p4 <- reactive({
    p4 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Total Chlorophytes", color = "mediumseagreen")
    p4
  })
  output$taxaplot4 <- renderPlot({p4()})
  # Total_Chrysophytes 144 gold2
  p5 <- reactive({
    p5 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Total Chrysophytes", color = "gold2")
    p5
  })
  output$taxaplot5 <- renderPlot({p5()})

  # Chrysosphaerella 78 darkgoldenrod3
  p6 <- reactive({
    p6 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Chrysosphaerella", color = "darkgoldenrod3")
    p6
  })
  output$taxaplot6 <- renderPlot({p6()})
  # Dinobryon 145 gold3
  p7 <- reactive({
    p7 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Dinobryon", color = "gold3")
    p7
  })
  output$taxaplot7 <- renderPlot({p7()})
  # Synura 149 goldenrod2
  p8 <- reactive({
    p8 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Synura", color = "goldenrod2")
    p8
  })
  output$taxaplot8 <- renderPlot({p8()})
  # Uroglenopsis 573 sandybrown
  p9 <- reactive({
    p9 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Uroglenopsis", color = "sandybrown")
    p9
  })
  output$taxaplot9 <- renderPlot({p9()})
  # Total_Cyanophytes 475 mediumturquoise
  p10 <- reactive({
    p10 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Total Cyanophytes", color = "mediumturquoise")
    p10
  })
  output$taxaplot10 <- renderPlot({p10()})
  # Dolichospermum 518 palegreen4
  p11 <- reactive({
    p11 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Dolichospermum", color = "palegreen4")
    p11
  })
  output$taxaplot11 <- renderPlot({p11()})
  # Microcystis 523 paleturquoise4
  p12 <-  reactive({
    p12 <-  taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Microcystis", color = "paleturquoise4")
    p12
  })
  output$taxaplot12 <-  renderPlot({p12()})
  # Grand_Total_Algae 24 black
  p13 <- reactive({
    p13 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxayear, taxa = "Grand Total", color = "black")
    p13
  })
  output$taxaplot13 <- renderPlot({p13()})
  # Other (Select dropdown) 604 slategrey
  p14 <- reactive({
    p14 <- taxaplot(df = df, locs = input$taxasite, vyear = input$taxa14year, taxa = input$taxa14, color = "slategrey")
    p14
  })
  output$taxaplot14 <- renderPlot({p14()})
  # Save Taxa Plot
  output$save_plot1 <- downloadHandler(
    filename = function() {
      paste0("Total_Diatoms", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p1(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot2 <- downloadHandler(
    filename = function() {
      paste0("Asterionella-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p2(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot3 <- downloadHandler(
    filename = function() {
      paste0("Cyclotella-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p3(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot4 <- downloadHandler(
    filename = function() {
      paste0("Total_Chlorophytes-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p4(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot5 <- downloadHandler(
    filename = function() {
      paste0("Total_Chrysophytes-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p5(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot6 <- downloadHandler(
    filename = function() {
      paste0("Chrysosphaerella-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p6(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot7 <- downloadHandler(
    filename = function() {
      paste0("Dinobryon-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p7(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot8 <- downloadHandler(
    filename = function() {
      paste0("Synura-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p8(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot9 <- downloadHandler(
    filename = function() {
      paste0("Uroglenopsis-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p9(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot10 <- downloadHandler(
    filename = function() {
      paste0("Total_Cyanophytes-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p10(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot11 <- downloadHandler(
    filename = function() {
      paste0("Dolichospermum-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p11(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot12 <- downloadHandler(
    filename = function() {
      paste0("Microcystis-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p12(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot13 <- downloadHandler(
    filename = function() {
      paste0("Grand_Total-", input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p13(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  output$save_plot14 <- downloadHandler(
    filename = function() {
      paste0(input$taxa14, input$taxayear,"_", format(Sys.time(), "%Y-%m-%d"), ".", input$taxaplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = p14(), device = input$taxaplot_save_type,
                      width = input$taxaplot_save_width, height = input$taxaplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
  ### Historical Plot ####

  phist <- reactive({
    phist <- historicplot(df = df,
                      taxa = input$histtaxa,
                      locs = input$histlocs,
                      vyear= input$histyear,
                      yg1min = input$yg1[1],
                      yg1max = input$yg1[2],
                      yg2min = input$yg2[1],
                      yg2max = input$yg2[2],
                      yg3min = input$yg3[1],
                      yg3max = input$yg3[2],
                      stat = input$stat,
                      stat1 = input$stat1,
                      stat2 = input$stat2,
                      stat3 = input$stat3,
                      depthmin = input$depth[1],
                      depthmax = input$depth[2])
    phist
  })
  output$histplot <- renderPlot({phist()})
  output$save_histplot <- downloadHandler(
    filename = function() {
      paste0(input$histtaxa,"_HistoricalComparison_", format(Sys.time(), "%Y-%m-%d"), ".", input$histplot_save_type)},
    content = function(file) {
      ggplot2::ggsave(file, plot = phist(), device = input$histplot_save_type,
                      width = input$histplot_save_width, height = input$histplot_save_height, dpi = 300)
      contentType = 'image/png'
    }
  )
} # End Phyto Server function
