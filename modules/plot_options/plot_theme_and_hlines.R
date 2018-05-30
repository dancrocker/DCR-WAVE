##############################################################################################################################
#     Title: plot_display_options.R
#     Type: Module2
#     Description: Time Series plot (for non-depth dependent data)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. 

# To-Do List:
#   1. 

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_THEME_AND_HLINE_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    column(3,
           wellPanel(
             h4("Display", align = "center"),
             wellPanel(
               radioButtons(ns("gplotly"), "Interactive or Static?",
                            choices = c("interactive Plot (Plotly)" = TRUE,
                                        "Static Plot" = FALSE)),
               p("Interactive Plot (Plotly) can be a distorted version of the plot that will saved")
             ),
             wellPanel(
               radioButtons(ns("theme"), "Theme:",
                            choices= c("Gray",
                                       "Black and White",
                                       "Line Draw",
                                       "Light",
                                       "Dark",
                                       "Minimal",
                                       "Classic"))
             )
           )
    ),
    column(9,
           wellPanel(
             fluidRow(
               h4("Horizontal Lines", align = "center"),
               column(4,
                      wellPanel(
                        strong("Horizontal Line 1:"),
                        checkboxInput(ns("hline1"), "Show Line"),
                        textInput(ns("hline1_int"), "Location of Y intercept(s)"),
                        h5("multiple numbers seperated with comma"),
                        radioButtons(ns("hline1_type"), "Line Type",
                                     choices = c("solid", "dashed", "dotted"),
                                     inline = TRUE),
                        sliderInput(ns("hline1_size"), "Thickness:",
                                    min = 0, max = 3, value = 1, step = 0.25),
                        sliderInput(ns("hline1_alpha"), "Transparency:",
                                    min = 0, max = 1, value = 1, step = 0.1)
                      )
               ), # end column
               column(4,
                      wellPanel(
                        strong("Horizontal Line 2:"),
                        checkboxInput(ns("hline2"), "Show Line"),
                        textInput(ns("hline2_int"), "Location of Y intercept(s)"),
                        h5("multiple numbers seperated with comma"),
                        radioButtons(ns("hline2_type"), "Line Type",
                                     choices = c("solid", "dashed", "dotted"),
                                     inline = TRUE),
                        sliderInput(ns("hline2_size"), "Thickness:",
                                    min = 0, max = 3, value = 1, step = 0.25),
                        sliderInput(ns("hline2_alpha"), "Transparency:",
                                    min = 0, max = 1, value = 1, step = 0.1)
                      )
               ), # end column
               column(4,
                      wellPanel(
                        strong("Horizontal Line 3:"),
                        checkboxInput(ns("hline3"), "Show Line"),
                        textInput(ns("hline3_int"), "Location of Y intercept(s)"),
                        h5("multiple numbers seperated with comma"),
                        radioButtons(ns("hline3_type"), "Line Type",
                                     choices = c("solid", "dashed", "dotted"),
                                     inline = TRUE),
                        sliderInput(ns("hline3_size"), "Thickness:",
                                    min = 0, max = 3, value = 1, step = 0.25),
                        sliderInput(ns("hline3_alpha"), "Transparency:",
                                    min = 0, max = 1, value = 1, step = 0.1)
                      )
               ) # end column
             ) # end fluidRow
           ) # end well
    ) # end col
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_THEME_AND_HLINE <- function(input, output, session, P) {
  
  ns <- session$ns # see General Note 1
  
  
  ### Plot Additions 
  P1 <- reactive({
    
    p <- P()
    
    ### Theme based on selection
    
    if(input$theme == "Gray"){
      p <- p + theme_gray()
    }
    if(input$theme == "Black and White"){
      p <- p + theme_bw()
    }
    if(input$theme == "Line Draw"){
      p <- p + theme_linedraw()
    }
    if(input$theme == "Light"){
      p <- p + theme_light()
    }
    if(input$theme == "Dark"){
      p <- p + theme_dark()
    }
    if(input$theme == "Minimal"){
      p <- p + theme_minimal()
    }
    if(input$theme == "Classic"){
      p <- p + theme_classic()
    }
  

    ### Add Lines
    
    # Horizontal Line 1
    if(input$hline1 == TRUE){
      p <- p + geom_hline(yintercept = Hline1_Int(),
                          linetype = input$hline1_type,
                          size = input$hline1_size,
                          alpha = input$hline1_alpha)
    }
    
    # Horizontal Line 2
    if(input$hline2 == TRUE){
      p <- p + geom_hline(yintercept = Hline2_Int(),
                          linetype = input$hline2_type,
                          size = input$hline2_size,
                          alpha = input$hline2_alpha)
    }
    
    # Horizontal Line 1
    if(input$hline3 == TRUE){
      p <- p + geom_hline(yintercept = Hline3_Int(),
                          linetype = input$hline3_type,
                          size = input$hline3_size,
                          alpha = input$hline3_alpha)
    }
    
  p
  
  })
  
  
  # Text to number for Line intercept inputs
  Hline1_Int <- reactive({
    req(input$hline1_int)
    str_split(input$hline1_int, ",") %>% 
      unlist() %>%
      as.numeric()
  })
  
  Hline2_Int <- reactive({
    req(input$hline2_int)
    str_split(input$hline2_int, ",") %>% 
      unlist() %>%
      as.numeric()
  })
  
  Hline3_Int <- reactive({
    req(input$hline3_int)
    str_split(input$hline3_int, ",") %>% 
      unlist() %>%
      as.numeric()
  })
  

  
  # include text input
  
return(list(Plot = P1, Gplotly = reactive({input$gplotly})))
  
} # end Server Function



#paste0(Text_Param2(), " (", Text_Units2(),")")
