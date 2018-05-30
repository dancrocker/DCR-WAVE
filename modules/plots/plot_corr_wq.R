##############################################################################################################################
#     Title: Plot-Corr.R
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

PLOT_CORR_WQ_UI <- function(id) {
  
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
                                   uiOutput(ns("param_x_ui"))
                            ),
                            column(6,
                                   uiOutput(ns("param_y_ui"))
                            )
                          )
                        ),
                        wellPanel(
                          checkboxGroupInput(ns("axis"), "Axis Options:",
                                             choices= c("Log-scale X-Axis",
                                                        "X-axis start at zero",
                                                        "Log-scale Y-Axis",
                                                        "Y-axis start at zero"))
                        )
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
                            uiOutput(ns("point_color_ui")),
                            sliderInput(ns("point_size"), "Point Size:",
                                        min = 0.5, max = 4, value = 1.5, step = 0.5),
                            sliderInput(ns("point_alpha"), "Opacity:", 
                                        min = 0, max = 1, value = 1, step = 0.1),
                            sliderInput(ns("point_jitter"), "Jitter:",
                                        min = 0, max = 1, value = 0, step = 0.1)
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
                          radioButtons(ns("trend_line"), "Line Type (when not grouped by shape)",
                                       choices = c("solid", "dashed", "dotted"),
                                       inline = TRUE),
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

PLOT_CORR_WQ <- function(input, output, session, Df) {
  
  ns <- session$ns # see General Note 1
  
  # Find Choices for Param
  Param <- reactive({Df()$Parameter %>% factor() %>% levels()})
  
  
  # Dataframe Creation based on Two chosen Parameters
  Df1 <- reactive({
    
    req(input$param_x, input$param_y)
    
    df_x <- Df() %>% 
      filter(Parameter == input$param_x) %>%
      rename(X_Param = Result) %>%
      select(LocationLabel, Date, X_Param)
    
    df_y <- Df() %>% 
      filter(Parameter == input$param_y) %>%
      rename(Y_Param = Result) %>%
      select(LocationLabel, Date, Y_Param)
    
    inner_join(df_x, df_y, by = c("LocationLabel", "Date"))
    
  })
  
  # Site List  
  Site <- reactive({Df1()$LocationLabel %>% factor() %>% levels()})
  
  
  
############# Rendered User Interface ###################
  
  ### X Axis Parameter
  
  output$param_x_ui <- renderUI({
    # If data selected is greater than zero
    if(Df() %>% summarise(n()) %>% unlist() != 0){
      radioButtons(ns("param_x"), "X Parameter",
                   choices = Param())
    }
  })
  
  outputOptions(output, "param_x_ui", suspendWhenHidden = FALSE) # see Dev. Manual
  
  # update when one changes the selected dataframe (Site, date, or parameters)
  observe({
    
    # save previously selected value
    isolate({
      if(input$param_x %in% c(Param())){
        save_selected <- input$param_x
      }else{
        save_selected <- NULL
      }
    })
    
    updateRadioButtons(session, inputId = "param_x", 
                       choices = Param(),
                       selected = save_selected)
  })
  
  
  ### Y Axis Parameter
  
  output$param_y_ui <- renderUI({
    # If data selected is greater than zero
    if(Df() %>% summarise(n()) %>% unlist() != 0){
      radioButtons(ns("param_y"), "Y Parameter",
                   choices = Param())
    }
  })
  
  outputOptions(output, "param_y_ui", suspendWhenHidden = FALSE) # see Dev. Manual
  
  # update when one changes the selected dataframe (Site, date, or parameters)
  observe({
    
    # save previously selected value
    isolate({
      if(input$param_y %in% c(Param())){
        save_selected <- input$param_y
      }else{
        save_selected <- NULL
      }
    })
    
    updateRadioButtons(session, inputId = "param_y", 
                       choices = Param(),
                       selected = save_selected)
  })
  
  

  # Color Grouping UI
  output$group_color_ui <- renderUI({
    color_choices <- c("None", "LocationLabel", "Flags (needs update)")
    radioButtons(ns("group_color"), label = "Group with Colors:",
                 choices = color_choices,
                 selected = "LocationLabel")
  })
  
  # # update when new flag code changes

  
  # Shape Grouping
  output$group_shape_ui <- renderUI({
    shape_choices <- c("None", "LocationLabel", "Flags (needs update)")
    radioButtons(ns("group_shape"), label = "Group with Shapes:",
                 choices = shape_choices,
                 selected = "LocationLabel")
  })
  
  
  # Point Color- only show options when color = None
  output$point_color_ui <- renderUI({
    req(input$param2 != "None" | input$group_color == "None")
    radioButtons(ns("point_color1"), "Color of Primary Axis:",
                 choices = c("black", "blue", "red", "green"),
                 inline = TRUE)
  })
  


  
  
  
  ########################################################################
  ### Plot UI
  
  
  ### Plot Outputs 
  
  # ONe Y-axis interactive Plotly plot
  output$plot1_inter <- renderPlotly({
    ggplotly(P4$Plot(), tooltip = c("x",  "y", "colour", "shape"))
  })
  
  # ONe Y-axis static Plot
  output$plot1_static <- renderPlot({
    P4$Plot() + theme(text = element_text(size = 15))
  })
  

  
  output$plot_ui <- renderUI({
    req(input$param_x, input$param_y)
    if(P2$Gplotly() == TRUE){
      plotlyOutput(ns("plot1_inter"), width = "100%", height = 600)
    }else{
      plotOutput(ns("plot1_static"), width = "100%", height = 600)
    }
    
  })
  
  
  
  #########################################################################
  # Plot Creation

  
  # Jitter Scheme Factor Calculation
  
  jitter_x <- reactive({
    input$point_jitter*IQR(Df1()$X_Param)*0.06
  })
  
  jitter_y <- reactive({
    input$point_jitter*IQR(Df1()$Y_Param)*0.06
  })
  
  
  # Plot Creation
  
  P1 <- reactive({
    
    # Features in which all plot options have in common
    p <- ggplot(Df1(), aes_string(x = "X_Param", y = "Y_Param"))
    

    # Grouping and Trendline
    
    # Group by both Color and Shape when both selected
    if(input$group_color != "None" & input$group_shape != "None"){
      p <- p + geom_point(aes_string(color = input$group_color, 
                                     shape = input$group_shape), 
                          size = input$point_size,
                          alpha = input$point_alpha,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$trend_show == TRUE){
        p <- p + geom_smooth(aes_string(color = input$group_color, 
                                        linetype = input$group_shape),
                             method = input$trend_type,
                             size = input$trend_size,
                             alpha = input$trend_alpha,
                             se = input$trend_ribbon,
                             level = as.numeric(input$trend_conf))
      }
    }
    # Group by only Color when only color grouping is selected
    else if (input$group_color != "None"){
      p <- p + geom_point(aes(color = input$group_color),
                          size = input$point_size,
                          alpha = input$point_alpha,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$trend_show == TRUE){
        p <- p + geom_smooth(aes_string(color = input$group_color),
                             linetype = input$trend_line,
                             method = input$trend_type,
                             size = input$trend_size,
                             alpha = input$trend_alpha,
                             se = input$trend_ribbon,
                             level = as.numeric(input$trend_conf))
      }
    } 
    # Group by only Shape when only shape grouping is selected 
    else if (input$group_shape != "None"){
      p <- p + geom_point(aes(shape = input$group_shape),
                          color = input$point_color,
                          size = input$point_size,
                          alpha = input$point_alpha,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$trend_show == TRUE){
        p <- p + geom_smooth(aes_string(linetype = input$group_shape),
                             method = input$trend_type,
                             size = input$trend_size,
                             alpha = input$trend_alpha,
                             se = input$trend_ribbon,
                             level = as.numeric(input$trend_conf))
      }
    } 
    # No Grouping Selected
    else {
      p <- p + geom_point(color = input$point_color,
                          size = input$point_size,
                          alpha = input$point_alpha,
                          position = position_jitter(width = jitter_x(), height = jitter_y()))
      if(input$trend_show == TRUE){
        p <- p + geom_smooth(linetype = input$trend_line,
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
    if("Log-scale X-Axis" %in% input$axis){
      p <- p + scale_x_log10()
    } else if("X-axis start at zero" %in% input$axis){
      xmax <- max(Df1()$X_Param, na.rm = TRUE)
      p <- p + scale_x_continuous(limits = c(0,xmax))
    }
    
    if("Log-scale Y-Axis" %in% input$axis){
      p <- p + scale_y_log10()
    } else if("Y-axis start at zero" %in% input$axis){
      ymax <- max(Df1()$Y_Param, na.rm = TRUE)
      p <- p + scale_y_continuous(limits = c(0,ymax))
    }

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
                   X_Lab_Auto = reactive({paste(Text_Param_X(), " (", Text_Units_X(),")", sep= "")}),
                   Y_Lab_Auto = reactive({paste(Text_Param_Y(), " (", Text_Units_Y(),")", sep= "")}))

  
    

  
  
  # Save Tab and Download Function
  callModule(PLOT_SAVE, "save",
             P = P4$Plot,
             Plot_Name = Plot_Name)
  
  
  
  
  ### Texts For Plot
  
  Text_Site <- reactive({Site() %>% paste(collapse = ", ")})
  
  Text_Param_X <- reactive({input$param_x})
  
  Text_Param_Y <- reactive({input$param_y})
  
  Text_Units_X <- reactive({Df() %>% filter(Parameter == input$param_x) %>% .$Units %>% unique() %>% paste(collapse = ", ")})
  
  Text_Units_Y <- reactive({Df() %>% filter(Parameter == input$param_y) %>% .$Units %>% unique() %>% paste(collapse = ", ")})
  
  Text_Date_Start <- reactive({Df1()$Date %>% min(na.rm = TRUE) %>% paste()})
  
  Text_Date_End <- reactive({Df1()$Date %>% max(na.rm = TRUE) %>% paste()})
  
  Plot_Name <- reactive({
    paste0(Text_Param_X(), ' and ', Text_Param_Y(),' at Site(s) ', Text_Site(),
           ' from ', Text_Date_Start(),' to ', Text_Date_End())

  })
  


  
} # end Server Function

