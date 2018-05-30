##############################################################################################################################
#     Title: Plot-Regress.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_CORR_DEPTH_WQ_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(

    plotlyOutput(ns("plot"), width = "100%", height = 600),
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
                               checkboxGroupInput(ns("plot_display_log"), "Log-Scale :",
                                                  choices= c("X Axis",
                                                             "Y Axis"))
                        ), # end column
                        column(2,
                               sliderInput(ns("plot_display_opacity"), "Opacity:", min = 0, max = 1, value = 1, step = 0.1),
                               sliderInput(ns("plot_display_jitter"), "Jitter:", min = 0, max = 1, value = 0, step = 0.1),
                               sliderInput(ns("plot_display_psize"), "Point Size:", min = 0.5, max = 4, value = 1.5, step = 0.5)
                        ) # end column
               ), # end Tab Panel
               tabPanel("Trends and Lines", br(), br(),
                        column(2,
                               radioButtons(ns("plot_line_trend"), "Add Trendline:",
                                            choices= c("None",
                                                       "Linear" = "lm",
                                                       "Curve" = "loess")),
                               checkboxInput(ns("plot_line_trend_ribbon"), "Show Conf. Ribbon"),
                               sliderInput(ns("plot_line_trend_size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ), # end column
                        column(1),
                        column(2,
                               strong("Non-Detection Level:"),
                               checkboxInput(ns("plot_line_nd"),"Show Line"),
                               radioButtons(ns("plot_line_nd_type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot_line_nd_size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ), # end column
                        column(2,
                               strong("Reporting Limit:"),
                               checkboxInput(ns("plot_line_rl"), "Show Line"),
                               radioButtons(ns("plot_line_rl_type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot_line_rl_size"), "Line Thickness:",
                                           min = 0.5, max = 3, value = 1, step = 0.5)
                        ), # end column
                        column(2,
                               strong("Performace Standard:"),
                               checkboxInput(ns("plot_line_ps"), "Show Line"),
                               radioButtons(ns("plot_line_ps_type"), "Line Type",
                                            choices = c("solid", "dash", "dotted")),
                               sliderInput(ns("plot_line_ps_size"), "Line Thickness:",
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
                        column(3,
                               radioButtons(ns("plot_color"), label = "Group with Colors:",
                                            choices = c("None" = 1,
                                                        "Station" = "Station",
                                                        "Sampling Level" = "Sampling_Level",
                                                        "met/hydro filter 1 (select group)" = "met1",
                                                        "met/hydro filter 2 (select group)" = "met2",
                                                        "Flagged data" = "FlagCode"),
                                            selected = "Site")
                        ), # end column
                        # new column
                        column(3,
                               radioButtons(ns("plot_shape"), label = "Group with Shapes:",
                                            choices = c("None" = 1,
                                                        "Station" = "Station",
                                                        "Sampling Level" = "Sampling_Level",
                                                        "met/hydro filter 1 (select group)" = "met1",
                                                        "met/hydro filter 2 (select group)" = "met2",
                                                        "Flagged data" = "FlagCode"),
                                            selected = 1)
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

PLOT_CORR_DEPTH_WQ <- function(input, output, session, Df) {

### Text For Plot

  # Station Text
  text_station <- reactive({
    Df()$Station %>% factor() %>% levels() %>% paste()
  })

  # Depth Text
  text_depth <- reactive({
    Df()$Depth_m %>% factor() %>% levels() %>% paste()
  })

  # X Param Text
  x_text_param <- reactive({
    Df()$x_Parameter %>% factor() %>% levels() %>% paste()
  })

  # X Units Text
  x_text_units <- reactive({
    Df()$x_Units %>% factor() %>% levels() %>% paste()
  })

  # Y Param Text
  y_text_param <- reactive({
    Df()$y_Parameter %>% factor() %>% levels() %>% paste()
  })

  # Y Units Text
  y_text_units <- reactive({
    Df()$y_Units %>% factor() %>% levels() %>% paste()
  })

  # Date Text - Start
  text_date_start <- reactive({
    Df()$Date %>% min(na.rm = TRUE) %>% paste()
  })

  # Date Text - End
  text_date_end <- reactive({
    Df()$Date %>% max(na.rm = TRUE) %>% paste()
  })


### Other Pre Plot

  # Jitter Scheme Factor Calculation

  jitter_x <- reactive({
    input$plot_display_jitter*IQR(Df()$x_Result)*0.06
  })

  jitter_y <- reactive({
    input$plot_display_jitter*IQR(Df()$y_Result)*0.06
  })


### PLOT

  # Plot Creation

  p <- reactive({

    # Features in which all plot options have in common
    p <- ggplot(Df(), aes(x = x_Result, y = y_Result))


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

    # Log Scale
    if("X Axis" %in% input$plot_display_log){
      p <- p + scale_x_log10()
    }
    if("Y Axis" %in% input$plot_display_log){
      p <- p + scale_y_log10()
    }

# Grouping and Trendline

    # Group by both Color and Shape when both selected
    if(input$plot_color != 1 & input$plot_shape != 1){
      p <- p + geom_point(aes_string(color = input$plot_color, shape = input$plot_shape),
                          size = input$plot_display_psize,
                          alpha = input$plot_display_opacity,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$plot_line_trend != "None"){
        p <- p + geom_smooth(method = input$plot_line_trend,
                             size = input$plot_line_trend_size,
                             se = input$plot_line_trend_ribbon,
                             aes_string(color = input$plot_color, linetype = input$plot_shape))
      }
    }
    # Group by only Color when only color grouping is selected
    else if (input$plot_color != 1){
      p <- p + geom_point(aes_string(color = input$plot_color),
                          size = input$plot_display_psize,
                          alpha = input$plot_display_opacity,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$plot_line_trend != "None"){
        p <- p + geom_smooth(method = input$plot_line_trend,
                             size = input$plot_line_trend_size,
                             se = input$plot_line_trend_ribbon,
                             aes_string(color = input$plot_color))
      }
    }
    # Group by only Shape when only shape grouping is selected
    else if (input$plot_shape != 1){
      p <- p + geom_point(aes_string(shape = input$plot_shape),
                          size = input$plot_display_psize,
                          alpha = input$plot_display_opacity,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$plot_line_trend != "None"){
        p <- p + geom_smooth(method = input$plot_line_trend,
                             size = input$plot_line_trend_size,
                             se = input$plot_line_trend_ribbon,
                             aes_string(linetype = input$plot_shape))
      }
    }
    # No Grouping Selected
    else {
      p <- p + geom_point(size = input$plot_display_psize,
                          alpha = input$plot_display_opacity,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$plot_line_trend != "None"){
        p <- p + geom_smooth(method = input$plot_line_trend,
                             size = input$plot_line_trend_size,
                             se = input$plot_line_trend_ribbon)
      }
    }

    # Facet for Sites if no grouping for site is selected and number of sites is greater than 1
    if(input$plot_color != "Station" & input$plot_shape != "Station" & length(c(input$station)) > 1){
      if(input$plot_color != "SamplingLevel" & input$plot_shape != "SamplingLevel" & length(c(input$level)) > 1){
        p <- p + facet_grid(Station~SamplingLevel)
      } else {
        p <- p + facet_grid(Station~.)
      }
    } else {
      if(input$plot_color != "SamplingLevel" & input$plot_shape != "SamplingLevel" & length(c(input$level)) > 1){
        p <- p + facet_grid(.~SamplingLevel)
      }
    }

# Add Lines

    # Show Non-Detect Level
    if(input$plot_line_nd == TRUE){
      p <- p + geom_hline(yintercept = 2,
                          linetype = input$plot_line_nd_type,
                          size = input$plot_line_nd_size)
    }

    # Show Reprting Limit
    if(input$plot_line_rl == TRUE){
      p <- p + geom_hline(yintercept = 3,
                          linetype = input$plot_line_rl_type,
                          size = input$plot_line_rl_size)
    }

    # Performance Standard
    if(input$plot_line_ps == TRUE){
      p <- p + geom_hline(yintercept = 4,
                          linetype = input$plot_line_ps_type,
                          size = input$plot_line_ps_size)
    }


# Title and Axis Lables

    # Title
    if(input$plot_title == "None"){
      p <- p + ggtitle("")
    }
    if(input$plot_title == "Auto"){
      p <- p + ggtitle(paste(y_text_param(), "vs", x_text_param(), "at", text_station(), text_depth(),
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
      p <- p + xlab(paste(x_text_param(), " (", x_text_units(),")", sep= ""))
    }
    if(input$plot_xlab == "Custom"){
      p <- p + xlab(input$plot_xlab_text)
    }

    # Y Axis
    if(input$plot_ylab == "None"){
      p <- p + ylab("")
    }
    if(input$plot_ylab == "Auto"){
      p <- p + ylab(paste(y_text_param(), " (", y_text_units(),")", sep= ""))
    }
    if(input$plot_ylab == "Custom"){
      p <- p + ylab(input$plot_ylab_text)
    }

# Save Options

    # Size dependent? Change size for saving?
    p <- p + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.5), "in"))

    # Gridlines for saving options
    if("major gridlines" %in% input$plot_save_grid){
      p <- p + theme(panel.grid.major = element_line())
    }
    if("minor gridlines" %in% input$plot_save_grid){
      p <- p + theme(panel.grid.minor = element_line())
    }

    p

  })


# Plot Visualization - convert plot to interactive plot and create an plot output object

  output$plot <- renderPlotly({
    ggplotly(p())
  })


  # Plot Print

  output$save_plot <- downloadHandler(
    filename = function (){paste(text_param(),' Site ', text_station(), text_depth(), ' from ', text_date_start(),' to ', text_date_end(), '.png', sep='')},
    content = function(file) {ggsave(file, plot = p(), device = "png")},
    contentType = 'image/png'
  )


} # end Server Function

