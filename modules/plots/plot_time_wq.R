##############################################################################################################################
#     Title: plot_time_wq.R
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
#   3. 1 to None (in shape and color )

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_TIME_WQ_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      uiOutput(ns("plot_ui"))
    ),
    # Plot Options
    fluidRow(br(), br()),
    tabsetPanel(
      ### Main Options
      tabPanel("Main Options", br(), br(),
               column(4,
                      wellPanel(
                        h4("Axes", align = "center"),
                        wellPanel(
                          fluidRow(
                            column(6,
                                   uiOutput(ns("param1_ui"))
                            ),
                            column(6,
                                   uiOutput(ns("param2_ui"))
                            )
                          )
                        ),
                        uiOutput(ns("axis_ui"))
                      )
               ), # end column
               column(4,
                      wellPanel(
                        h4("Colors and Shapes", align = "center"),
                        wellPanel(
                          fluidRow(
                            column(6,
                                   uiOutput(ns("group_color_ui"))
                            ), # end column
                            # new column
                            column(6,
                                   uiOutput(ns("group_shape_ui"))
                            ) # end column
                          )
                        ),
                        wellPanel(
                          fluidRow(
                            uiOutput(ns("point_color1_ui")),
                            uiOutput(ns("point_color2_ui")),
                            sliderInput(ns("point_size"), "Point Size:",
                                        min = 0.5, max = 4, value = 1.5, step = 0.5)
                          )

                        )
                      )
               ), # end column
               column(4,
                      wellPanel(
                        h4("Trend Lines", align = "center"),
                        wellPanel(
                          checkboxInput(ns("trend_show"), "Show Trendline(s)"),
                          radioButtons(ns("trend_type"), "Trendline Type",
                                       choices= c("Linear" = "lm",
                                                  "Loess" = "loess",
                                                  "Generalized Additive" = "gam"),
                                       inline = TRUE),
                          strong("Confidence Ribbon"),
                          checkboxInput(ns("trend_ribbon"), "Show Conf. Ribbon"),
                          radioButtons(ns("trend_conf"), NULL,
                                       choices= c(0.90,0.95,0.99),
                                       inline = TRUE),
                          uiOutput(ns("trend_line_ui")),
                          sliderInput(ns("trend_size"), "Line Thickness:",
                                      min = 0, max = 3, value = 1, step = 0.25),
                          sliderInput(ns("trend_alpha"), "Line Opacity:",
                                      min = 0, max = 1, value = .1, step = 0.1)

                        )
                      )
               ) # end column
      ), # end Tab Panel
      ### More Display Options 1
      tabPanel("Theme and H-lines", br(), br(),
               PLOT_THEME_AND_HLINE_UI(ns("theme_hline"))
      ),
      ### More Display Options 2
      tabPanel("Texts and V-lines", br(), br(),
               PLOT_TEXT_AND_VLINES_TIME_UI(ns("text_vline"))
      ),
      ### Titles and Axis Labels
      tabPanel("Title and Labels", br(), br(),
               PLOT_TITLE_AND_LABELS_UI(ns("title_label"))
      ), # end Tab Panel
      ### Save Plot
      tabPanel("Save Plot", br(), br(),
               PLOT_SAVE_UI(ns("save"))
      ) # tab panel
    ) # end tabSet Panel
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_TIME_WQ <- function(input, output, session, Df) {

  ns <- session$ns # see General Note 1

##################### Main Reactive Expressions

  # Find Choices for Param
  Param <- reactive({Df()$Parameter %>% factor() %>% levels()})

  # Two Dataframes (Primary and Secondary Axis)

  Df1 <- reactive({
    req(input$param1)
    Df() %>% filter(Parameter %in% input$param1)})

  Df2 <- reactive({
    req(input$param1) # param1 not param2 in req() becuase param2 always has "None" option
    if(input$param2 != "None"){
      Df() %>% filter(Parameter %in% input$param2)
    }else{
      NULL
    }
  })

  # Site List  - Primary Axis Only Plots
  Site <- reactive({
    if(input$param2 == "None"){
      Df1()$LocationLabel %>% factor() %>% levels()
    } else{
      unique(Df1()$LocationLabel %>% factor() %>% levels(), Df2()$LocationLabel %>% factor() %>% levels())
    }
  })


############# Rendered User Interface

  # Parameter Axis Choice for Primary Axis
  output$param1_ui <- renderUI({
    if(Df() %>% summarise(n()) %>% unlist() != 0){
      radioButtons(ns("param1"), "Parameter 1 - Primary Y-Axis",
                   choices = Param())
    }
  })

  outputOptions(output, "param1_ui", suspendWhenHidden = FALSE) # see Dev. Manual

  # Save the previously selected Parameter 1 Input when options change (Memory)
  observe({

    # save previously selected value
    isolate({
      if(input$param1 %in% c(Param())){
        save_selected <- input$param1
      }else{
        save_selected <- NULL
      }
    })

    updateRadioButtons(session, inputId = "param1",
                      choices = Param(),
                      selected = save_selected)
  })

  # Parameter Axis Choice for Secondary Axis
  output$param2_ui <- renderUI({
    radioButtons(ns("param2"), "Parameter 2 - Secondary Y-Axis",
                 choices = c(Param(), "None"),
                 selected = "None")
  })

  outputOptions(output, "param2_ui", suspendWhenHidden = FALSE) # See Dev. Manual

  # Save the previously selected Parameter 2 Input when options change (Memory)
  observe({

    # save previously selected value
    isolate({
      if(input$param2 %in% c(Param(), "None")){
        save_selected <- input$param2
      }else{
        save_selected <- "None"
      }
    })

    updateRadioButtons(session, inputId = "param2",
                      choices = c(Param(), "None"),
                      selected = save_selected)

  })


  # Color Grouping UI
  output$group_color_ui <- renderUI({
    req(input$param2)
    if(input$param2 == "None"){
      color_choices <- c("None", "LocationLabel", "Flags (needs update)")
      radioButtons(ns("group_color"), label = "Group with Colors:",
                   choices = color_choices,
                   selected = "LocationLabel")
    } else {
      tagList(
        strong("Group with Colors:"),
        p("Coloring is used for the two parameters, choose colors below.")
      )
    }
  })


  # Shape Grouping
  output$group_shape_ui <- renderUI({
    req(input$param2)

    # Make so this includes all types of flags. Probably make sticky memory
    shape_choices <- c("None/Parameter", "LocationLabel", "Flags (needs update)")

    radioButtons(ns("group_shape"), label = "Group with Shapes:",
                 choices = shape_choices,
                 selected = "None/Parameter")
  })


  # Axis UI - only show options for 1 Parameter plot
  output$axis_ui <- renderUI({
    req(input$param2 == "None")
    wellPanel(
      checkboxGroupInput(ns("axis"), "Axis Options:",
                         choices= c("Log-scale Y-Axis",
                                    "Y-axis start at zero"))
    )
  })

  # Point Color 1 UI - only show options when color = None
  output$point_color1_ui <- renderUI({
    req(input$param2 != "None" | input$group_color == "None")
      radioButtons(ns("point_color1"), "Color of Primary Axis:",
                   choices = c("black", "blue", "red", "green"),
                   inline = TRUE)
  })

  # Point Color 2 UI - only show options when color = None
  output$point_color2_ui <- renderUI({
    req(input$param2 != "None")
      radioButtons(ns("point_color2"), "Color of Secondary Axis:",
                   choices = c("black", "blue", "red", "green"),
                   selected = "blue",
                   inline = TRUE)
  })

  # Trend Line - only show options when shape = None/Parameter
  output$trend_line_ui <- renderUI({
    req(input$group_shape == "None/Parameter")
    radioButtons(ns("trend_line"), "Line Type (when not grouped by shape)",
                 choices = c("solid", "dashed", "dotted"),
                 inline = TRUE)
  })



########################################################################
### Plot UI


  ### Plot Outputs

  # ONe Y-axis interactive Plotly plot
  output$plot1_inter <- renderPlotly({
    req(input$param2 == "None")# & P2$Gplotly() == TRUE)
    ggplotly(P5(), tooltip = c("y", "x", "colour", "shape"))
  })

  # ONe Y-axis static Plot
  output$plot1_static <- renderPlot({
    req(input$param2 == "None") # & P2$Gplotly() == FALSE)
    P5() + theme(text = element_text(size = 15))
  })

  # Two Y-axis ggplot static plot
  output$plot2 <- renderPlot({
    req(input$param2 != "None")
    P5() + theme(text = element_text(size = 15))
  })

  output$plot_ui <- renderUI({
    req(input$param1, input$param2)

    if(input$param2 != "None"){
      tagList(
        plotOutput(ns("plot2"), width = "100%", height = 600),
        h4(textOutput(ns("plot2text")))
      )
    } else if(input$param2 == "None"& P2$Gplotly() == TRUE){
        plotlyOutput(ns("plot1_inter"), width = "100%", height = 600)
    }else{
        plotOutput(ns("plot1_static"), width = "100%", height = 600)
    }

  })



#########################################################################
# Plot Creation

  # Main Plot Scripts
  P1 <- reactive({

    ############# One Parameter Plots - Primary Y axis only ######################################

    if(input$param2 == "None"){

      p <- ggplot(Df1())

      # Group by both Color and Shape when both selected
      if(input$group_color != "None" & input$group_shape != "None/Parameter"){
          p <- p + geom_point(aes(x = as.POSIXct(DateTimeET, tz = "America/New_York"),
                                         y = Result,
                                         color = input$group_color,
                                         shape = input$group_shape),
                              size = input$point_size)
          if(input$trend_show == TRUE){
            p <- p + geom_smooth(aes(x = as_datetime(DateTimeET),
                                            y = Result,
                                            color = input$group_color,
                                            linetype = input$group_shape),
                                 method = input$trend_type,
                                 size = input$trend_size,
                                 alpha = input$trend_alpha,
                                 se = input$trend_ribbon,
                                 level = as.numeric(input$trend_conf))
          }
          # Group by only Color when only color grouping is selected
      } else if (input$group_color != "None"){
        p <- p + geom_point(aes(x = as_datetime(DateTimeET),
                                       y = Result,
                                       color = input$group_color),
                            shape = 16,
                            size = input$point_size)
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(aes(x = as_datetime(DateTimeET),
                                          y = Result,
                                          color = input$group_color),
                               linetype = input$trend_line,
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf))
        }
        # Group by only Shape when only shape grouping is selected
      } else if (input$group_shape != "None/Parameter"){
        p <- p + geom_point(aes(x = as_datetime(DateTimeET),
                                       y = Result,
                                       shape = input$group_shape),
                            color = input$point_color1,
                            size = input$point_size)

        if(input$trend_show == TRUE){
          p <- p + geom_smooth(aes(x = as_datetime(DateTimeET),
                                          y = Result,
                                          linetype = input$group_shape),
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf))
        }
        # No Grouping Selected
      } else {
        p <- p + geom_point(aes(x = as_datetime(DateTimeET),
                                       y = Result),
                            color = input$point_color1,
                            shape = 16,
                            size = input$point_size)
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(aes(x = DateTimeET,
                                          y = Result),
                               linetype = input$trend_line,
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf))
        }
      }


      # Facet for Sites if no grouping for site is selected and number of sites is greater than 1

      if(input$group_color != "LocationLabel" & input$group_shape != "LocationLabel" & length(c(Site())) > 1){
        p <- p + facet_wrap(~LocationLabel, ncol = ceiling(length(c(Site()))/4))
      }

      # Log Scale and Y-axis start at zero
      if(input$param2 == "None"){
        if("Log-scale Y-Axis" %in% input$axis){
          p <- p + scale_y_log10()
        } else {
          if("Y-axis start at zero" %in% input$axis){
            ymax <- max(Df1()$Result, na.rm = TRUE)
            p <- p + scale_y_continuous(limits = c(0,ymax))
          } else {
            p <- p + scale_y_continuous()
          }
        }
        # Secondary and Primary Axis (2 Parameter Plots)
      }

      ############# Two Parameter Plots - Primary and Secondary Y axes' ######################
    } else{

      p <- ggplot()

      # Grouped by Shape
      if(input$group_shape != "None/Parameter"){
        p <- p + geom_point(data = Df1(),
                            aes_string(x = "DateTimeET",
                                       y = "Result",
                                       shape = input$group_shape),
                            color = input$point_color1,
                            size = input$point_size*2) +
          geom_point(data = Df2(),
                     aes_string(x = "DateTimeET",
                                y = "Result*Mult()",
                                shape = input$group_shape),
                     color = input$point_color2,
                     size = input$point_size*2)
        if(input$trend_show == TRUE){
          p <- p + geom_smooth(data = Df1(), aes_string(x = "DateTimeET",
                                                        y = "Result",
                                                        linetype = input$group_shape),
                               color = input$point_color1,
                               method = input$trend_type,
                               size = input$trend_size,
                               alpha = input$trend_alpha,
                               se = input$trend_ribbon,
                               level = as.numeric(input$trend_conf)) +
            geom_smooth(data = Df2(), aes_string(x = "DateTimeET",
                                                 y = "Result*Mult()",
                                                 linetype = input$group_shape),
                        color = input$point_color2,
                        method = input$trend_type,
                        size = input$trend_size,
                        alpha = input$trend_alpha,
                        se = input$trend_ribbon,
                        level = as.numeric(input$trend_conf))
        }
        # Not Grouped
      } else{
        p <- p + geom_point(data = Df1(),
                            aes_string(x = "DateTimeET",
                                       y = "Result"),
                            shape = 15,
                            color = input$point_color1,
                            size = input$point_size*2) +
          geom_point(data = Df2(),
                     aes_string(x = "DateTimeET",
                                y = "Result*Mult()"),
                     shape = 17,
                     color = input$point_color2,
                     size = input$point_size*2)
      }
      if(input$trend_show == TRUE){
        p <- p + geom_smooth(data = Df1(), aes_string(x = "DateTimeET",
                                                      y = "Result"),
                             linetype = input$trend_line,
                             color = input$point_color1,
                             method = input$trend_type,
                             size = input$trend_size,
                             se = input$trend_ribbon,
                             level = as.numeric(input$trend_conf)) +
          geom_smooth(data = Df2(), aes_string(x = "DateTimeET",
                                               y = "Result*Mult()"),
                      linetype = input$trend_line,
                      color = input$point_color2,
                      method = input$trend_type,
                      size = input$trend_size,
                      se = input$trend_ribbon,
                      level = as.numeric(input$trend_conf))
      }


      # Facet for Sites if no grouping for site is selected and number of sites is greater than 1

      if(input$group_shape != "LocationLabel" & length(c(Site())) > 1){
        p <- p + facet_wrap(~LocationLabel, ncol = ceiling(length(c(Site()))/4))
      }

    }

    ### All Plots #####
    p <- p + scale_x_datetime(labels = date_format("%Y-%m-%d"), breaks = pretty_breaks(n=12))

    p

  })



  # Display OPtions Tab
  P2 <- callModule(PLOT_THEME_AND_HLINE, "theme_hline",
                   P = P1)

  # Display OPtions Tab
  P3 <- callModule(PLOT_TEXT_AND_VLINES_TIME, "text_vline",
                   P = P2$Plot)


  # Title and Labels - returns a list of two reactive expressions. One a plot object and one a text string Sec. Axis label
  P4 <- callModule(PLOT_TITLE_AND_LABELS, "title_label",
                   P = P3,
                   Title_Auto = Plot_Name, # error w/o reactive?
                   X_Lab_Auto = reactive({"Date"}),
                   Y_Lab_Auto = reactive({paste(Text_Param1(), " (", Text_Units1(),")", sep= "")}),
                   sec_y_axis = TRUE,
                   Y2_Lab_Auto = reactive({paste(Text_Param2(), " (", Text_Units2(),")", sep= "")}))


  # Secondary Axis Only (Post Axis Label Naming)
  P5 <- reactive({

    p <- P4$Plot()

    # Secondary Axis Only
    if(input$param2 != "None"){
      p <- p + scale_y_continuous(breaks = pretty_breaks(), # ylim?
                                  sec.axis = sec_axis(~./Mult(), breaks = pretty_breaks(),
                                                      name = P4$Y2_Lab()))
      # p <- p + theme(text = element_text(size = 15))

      p <- p + theme(axis.text.y = element_text(colour = input$point_color1),
                     axis.text.y.right = element_text(colour = input$point_color2))

    }

    p

  }) # end P5


  # Save Tab and Download Function
  callModule(PLOT_SAVE, "save",
             P = P5,
             Plot_Name = Plot_Name)


  # Multiplier for Secondary Y-axis
  Mult <- reactive({

    y1max <- max(Df1()$Result, na.rm = TRUE)
    y2max <- max(Df2()$Result, na.rm = TRUE)

    y1max / y2max

  })


  ### Texts For Plot

  Text_Site <- reactive({Site() %>% paste(collapse = ", ")})

  Text_Param1 <- reactive({Df1()$Parameter %>% factor() %>% levels() %>% paste()})

  Text_Param2 <- reactive({Df2()$Parameter %>% factor() %>% levels() %>% paste()})

  Text_Units1 <- reactive({Df1()$Units %>% factor() %>% levels() %>% paste(collapse = ", ")})

  Text_Units2 <- reactive({Df2()$Units %>% factor() %>% levels() %>% paste(collapse = ", ")})

  Text_Date_Start1 <- reactive({Df1()$Date %>% min(na.rm = TRUE) %>% paste()})

  Text_Date_End1 <- reactive({Df1()$Date %>% max(na.rm = TRUE) %>% paste()})

  Text_Date_Start2 <- reactive({c(Df1()$Date, Df2()$Date)  %>% min(na.rm = TRUE) %>% paste()})

  Text_Date_End2 <- reactive({c(Df1()$Date, Df2()$Date) %>% max(na.rm = TRUE) %>% paste()})

  Plot_Name <- reactive({
    if(input$param2 == "None"){
      paste0(Text_Param1(),' at Site(s) ', Text_Site(),
             ' from ', Text_Date_Start1(),' to ', Text_Date_End1())
    } else{
      paste0(Text_Param1(), ' and ', Text_Param2(), ' at Site(s) ', Text_Site(),
             ' from ', Text_Date_Start2(),' to ', Text_Date_End2())
    }
  })

  # legend for double y-axis plot
  output$plot2text <- renderText({
    paste(input$point_color1, "=", Text_Param1(), " , ", input$point_color2, "=", Text_Param2())
  })


} # end Server Function


