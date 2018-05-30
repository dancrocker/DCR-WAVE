##############################################################################################################################
#     Title: plot_profline_custom_R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1_ req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1_ Make the Metero/Hydro Filters work
#   2_ Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)
#   3_ 1 to None (in shape and color )

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_PROFLINE_CUSTOM_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(

    uiOutput(ns("plot_ui")),
    # Plot Options
    fluidRow(br(), br()),
    fluidRow(
      column(11,
             tabsetPanel(
               tabPanel("Display Options", br(), br(),
                        column(2,
                               radioButtons(ns("plot_display_theme"), "Theme:",
                                            choices= c("Gray",
                                                       "Black and White",
                                                       "Line Draw",
                                                       "Light",
                                                       "Dark",
                                                       "Minimal",
                                                       "Classic"))
                        ), # end column
                        column(2,
                               checkboxInput(ns("plot_display_int"), "Interactive Plot:")
                        ), # end column
                        column(2,
                               sliderInput(ns("plot_display_lsize"), "Line Thickness:",
                                           min = 0.5, max = 4, value = 1.5, step = 0.5)
                        ) # end column
               ), # end Tab Panel
               tabPanel("Trends and Lines", br(), br(),
                        column(2,
                               strong("Secchi Depth:"),
                               checkboxInput(ns("plot_line_sec"),"Show Line"),
                               radioButtons(ns("plot_line_sec_type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot_line_sec_size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ) # end column
               ),
               tabPanel("Title and Axis Labels", br(), br(),
                        column(3,
                               radioButtons(ns("plot_title"), "Title Options:",
                                            choices= c("None", "Auto", "Custom"),
                                            selected = "Auto"),
                               textInput(ns("plot_title_text"), "")
                        ), # end column
                        column(3,
                               radioButtons(ns("plot_xlab"), "X Label Options:",
                                            choices= c("None", "Auto", "Custom"),
                                            selected = "Auto"),
                               textInput(ns("plot_xlab_text"), "")
                        ), # end column
                        column(3,
                               radioButtons(ns("plot_ylab"), "Y Label Options:",
                                            choices= c("None", "Auto", "Custom"),
                                            selected = "Auto"),
                               textInput(ns("plot_ylab_text"), "")
                        ) # end column
               ), # end Tab Panel
               tabPanel("Grouping (Color/Shape)", br(), br(),
                        column(2,
                               radioButtons(ns("plot_color"), label = "Group with Colors:",
                                            choices = c("Parameter", "Site"),
                                            selected = "Parameter")

                        ), # end column
                        # new column
                        column(2,
                               radioButtons(ns("plot_linetype"), label = "Group with Linetypes:",
                                            choices = c("Parameter", "Site"),
                                            selected = "Site")
                        ) # end column
               ), # tabPanel
               tabPanel("Save Plot", br(), br(),
                        column(2,
                               downloadButton(ns('save_plot'), "Save Plot")
                        ),
                        column(2,
                               radioButtons(ns("plot_save_size"), "Plot Size:",
                                            choices= c("small",
                                                       "medium",
                                                       "large"))
                        ),
                        column(2,
                               radioButtons(ns("plot_save_type"), "File Type:",
                                            choices= c("pdf",
                                                       "jpg",
                                                       "png"))
                        ),
                        column(2,
                               checkboxGroupInput(ns("plot_save_grid"), "Gridline Override:",
                                                  choices= c("major gridlines",
                                                             "minor gridlines"))
                        ) # end column
               ) # tab panel
             ) # end tabSet Panel
      ), # end Column
      # extend the page with a right column of blank rows (hack at keeping the page the same height no matter the tab open)
      column(1, br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
             br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
      )
    ) # Fluid Row
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

PLOT_PROFLINE_CUSTOM <- function(input, output, session, Df) {


### Text For Plot

  # Site Text
  text_site <- reactive({
    Df()$Site %>% factor() %>% levels() %>% paste()
  })

  # Param Text
  text_param <- reactive({
    Df()$Parameter %>% factor() %>% levels() %>% paste()
  })

  # Units Text
  text_units <- reactive({
    Df()$Units %>% factor() %>% levels() %>% paste()
  })

  # Date Text - Start
  text_date_start <- reactive({
    Df()$Date %>% min(na.rm = TRUE) %>% paste()
  })

  # Date Text - End
  text_date_end <- reactive({
    Df()$Date %>% max(na.rm = TRUE) %>% paste()
  })



### PLOT

  # Plot Creation

  p <- reactive({

    # Features in which all plot options have in common
    p <- ggplot(Df(),aes(x=Result,y=Depth_m)) +
      scale_y_reverse()


# Display Tab

    # Theme based on selection
    if(input$plot_display_theme == "Gray"){
      p <- p + theme_gray()
    }
    if(input$plot_display_theme == "Black and White"){
      p <- p + theme_bw()
    }
    if(input$plot_display_theme == "Line Draw"){
      p <- p + theme_linedraw()
    }
    if(input$plot_display_theme == "Light"){
      p <- p + theme_light()
    }
    if(input$plot_display_theme == "Dark"){
      p <- p + theme_dark()
    }
    if(input$plot_display_theme == "Minimal"){
      p <- p + theme_minimal()
    }
    if(input$plot_display_theme == "Classic"){
      p <- p + theme_classic()
    }

# Grouping and Faceting

    # No Parameter Coloring/linetype --> facet by param
    if(input$plot_color != "Parameter" & input$plot_linetype != "Parameter"){
      p <- p + geom_path(aes_string(color = input$plot_color, linetype = input$plot_linetype), size = input$plot_display_lsize) +
        facet_grid(Parameter~Date)
      # No Site Coloring/linetype --> facet by site
    } else if(input$plot_color != "Site" & input$plot_linetype != "Site"){
      p <- p + geom_path(aes_string(color = input$plot_color, linetype = input$plot_linetype), size = input$plot_display_lsize) +
        facet_grid(Site~Date)
      # Param and Site both colored or linetyped
    } else {
      p <- p + geom_path(aes_string(color = input$plot_color, linetype = input$plot_linetype), size = input$plot_display_lsize) +
        facet_grid(.~Date)
    }

# Add Lines

    # Show Secchi
    if(input$plot_line_sec == TRUE){
      p <- p + geom_hline(yintercept = 2,
                          linetype = input$plot_line_sec_type,
                          size = input$plot_line_sec_size)
    }


# Title and Axis Lables

    # Title
    if(input$plot_title == "None"){
      p <- p + ggtitle("")
    }
    if(input$plot_title == "Auto"){
      p <- p + ggtitle(paste(text_param(), "at",
                             text_site(),
                             "from", text_date_start(), "to", text_date_end(), sep= " "))
    }
    if(input$plot_title == "Custom"){
      p <- p + ggtitle(input$plot_title_text)
    }

    # X Axis
    if(input$plot_xlab == "None"){
      p <- p + xlab("")
    }
    if(input$plot_xlab == "Auto"){
      p <- p + xlab("Date")
    }
    if(input$plot_xlab == "Custom"){
      p <- p + xlab(input$plot_xlab_text)
    }

    # Y Axis
    if(input$plot_ylab == "None"){
      p <- p + ylab("")
    }
    if(input$plot_ylab == "Auto"){
      p <- p + ylab("Depth (m)")
    }
    if(input$plot_ylab == "Custom"){
      p <- p + ylab(input$plot_ylab_text)
    }

# Save Options

    # Size dependent? Change size for saving?
    p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))

    # Gridlines for saving options
    if("major gridlines" %in% input$plot_save_grid){
      p <- p + theme(panel_grid_major = element_line())
    }
    if("minor gridlines" %in% input$plot_save_grid){
      p <- p + theme(panel_grid_minor = element_line())
    }

    p

  })


  # Plot Visualization - Non Interactive

  output$plot <- renderPlot(
    p()
  )


  # Plot Interactive

  output$plot_int <- renderPlotly({
    ggplotly(p() + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in")))
  })

  # Plot Output

  output$plot_ui <- renderUI({
    ns <- session$ns # see General Note 1

    if(input$plot_display_int == FALSE){
      plotOutput(ns("plot"), width = "100%", height = 500)
    } else {
      plotlyOutput(ns("plot_int"), width = "100%", height = 500)
    }
  })

  # Plot Print

  output$save_plot <- downloadHandler(
    filename = function (){paste(text_param(),' Site ', text_site(),' from ', text_date_start(),' to ', text_date_end(), '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},
    contentType = 'image/png'
  )


} # end Server Function

