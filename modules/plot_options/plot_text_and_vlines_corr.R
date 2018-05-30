##############################################################################################################################
#     Title: plot_display_options_2.R
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

PLOT_TEXT_AND_VLINES_TIME_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    column(8,
           wellPanel(
             h4("Text Boxes", align = "center"),
             fluidRow(
               column(3,
                      wellPanel(
                        strong("Custom Text 1"),
                        checkboxInput(ns("text1"), "Show Text"),
                        textInput(ns("text1_text"), "Text"),
                        numericInput(ns("text1_x"), "X Locatoin", value = 0),
                        numericInput(ns("text1_y"), "Y Locatoin", value = 0)
                      )
               ),
               column(3,
                      wellPanel(
                        strong("Custom Text 2"),
                        checkboxInput(ns("text2"), "Show Text"),
                        textInput(ns("text2_text"), "Text"),
                        numericInput(ns("text2_x"), "X Locatoin", value = 0),
                        numericInput(ns("text2_y"), "Y Locatoin", value = 0)
                      )
               ),
               column(3,
                      wellPanel(
                        strong("Custom Text 3"),
                        checkboxInput(ns("text3"), "Show Text"),
                        textInput(ns("text3_text"), "Text"),
                        numericInput(ns("text3_x"), "X Locatoin", value = 0),
                        numericInput(ns("text3_y"), "Y Locatoin", value = 0)
                      )
               ),
               column(3,
                      wellPanel(
                        strong("Custom Text 4"),
                        checkboxInput(ns("text4"), "Show Text"),
                        textInput(ns("text4_text"), "Text"),
                        numericInput(ns("text4_x"), "X Locatoin", value = 0),
                        numericInput(ns("text4_y"), "Y Locatoin", value = 0)
                      )
               )
             ) # end fluidRow
      ) # end wellPanel
    ), # end column
    column(4,
           wellPanel(
             h4("Vertical Lines", align = "center"),
             fluidRow(
               column(6,
                      wellPanel(
                        strong("Vertical Line 1"),
                        checkboxInput(ns("vline1"), "Show Line"),
                        numericInput(ns("vline1_int"), "X Locatoin", value = 0),
                        radioButtons(ns("vline1_type"), "Line Type",
                                     choices = c("solid", "dashed", "dotted"),
                                     inline = TRUE),
                        sliderInput(ns("vline1_size"), "Thickness:",
                                    min = 0, max = 3, value = 1, step = 0.25),
                        sliderInput(ns("vline1_alpha"), "Transparency:",
                                    min = 0, max = 1, value = 1, step = 0.1)
                      )
               ),
               column(6,
                      wellPanel(
                        strong("Vertical Line 2"),
                        checkboxInput(ns("vline2"), "Show Line"),
                        numericInput(ns("vline2_int"), "X Locatoin", value = 0),
                        radioButtons(ns("vline2_type"), "Line Type",
                                     choices = c("solid", "dashed", "dotted"),
                                     inline = TRUE),
                        sliderInput(ns("vline2_size"), "Thickness:",
                                    min = 0, max = 3, value = 1, step = 0.25),
                        sliderInput(ns("vline2_alpha"), "Transparency:",
                                    min = 0, max = 1, value = 1, step = 0.1)
                      )
               ) # end col
             ) # end FluidRow
           ) # end Wellpanel
    ) # end Column
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_TEXT_AND_VLINES_TIME <- function(input, output, session, P) {
  
  ns <- session$ns # see General Note 1
  
  
  ### Plot Additions 
  P1 <- reactive({
    
    p <- P()
    
    # Text outputs 
    
    # Text 1
    if(input$text1 == TRUE){
      p <- p + annotate(geom="text",
                        label = input$text1_text,
                        x = input$text1_x,
                        y = input$text1_y)
    }

    # Text 2
    if(input$text2 == TRUE){
      p <- p + annotate(geom="text",
                        label = input$text2_text,
                        x = input$text2_x,
                        y = input$text2_y)
    }

    # Text 3
    if(input$text3 == TRUE){
      p <- p + annotate(geom="text",
                        label = input$text3_text,
                        x = input$text3_x,
                        y = input$text3_y)
    }

    # Text 4
    if(input$text4 == TRUE){
      p <- p + annotate(geom="text",
                        label = input$text4_text,
                        x = input$text4_x,
                        y = input$text4_y)
    }
    
    ### Add Vertical Lines
    
    # Vertical Line 1
    if(input$vline1 == TRUE){
      p <- p + geom_vline(xintercept = input$vline1_int,
                          linetype = input$vline1_type,
                          size = input$vline1_size,
                          alpha = input$vline1_alpha)
    }
    
    # Vertical Line 2
    if(input$vline2 == TRUE){
      p <- p + geom_vline(xintercept = input$vline2_int,
                          linetype = input$vline2_type,
                          size = input$vline2_size,
                          alpha = input$vline2_alpha)
    }
    
    p
    
  })
  
  
  
  return(P1)
  
} # end Server Function
