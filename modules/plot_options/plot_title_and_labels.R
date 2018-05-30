##############################################################################################################################
#     Title: plot_title_and_labels.R
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

PLOT_TITLE_AND_LABELS_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    wellPanel(
      h4("Title and Axis Labels", align = "center"),
      fluidRow(
        column(3,
               wellPanel(
                 radioButtons(ns("title"), "Title Options:",
                              choices= c("None", "Auto", "Custom"),
                              selected = "Auto"),
                 textInput(ns("title_text"), ""),
                 sliderInput(ns("title_margin"), "adjust title location",
                             min = 0, max = 1, value = 0.5, step = 0.1)
               )
        ), # end column
        column(3,
               wellPanel(
                 radioButtons(ns("x_lab"), "X Label Options:",
                              choices= c("None", "Auto", "Custom"),
                              selected = "Auto"),
                 textInput(ns("x_lab_text"), ""),
                 sliderInput(ns("x_margin"), "adjust label location",
                             min = -1, max = 1.5, value = 0.2, step = 0.1)
               )
        ), # end column
        column(3,
               wellPanel(
                 radioButtons(ns("y_lab"), "Y Label Options:",
                              choices= c("None", "Auto", "Custom"),
                              selected = "Auto"),
                 textInput(ns("y_lab_text"), ""),
                 sliderInput(ns("y_margin"), "adjust label location",
                             min = -1, max = 1.5, value = 0.2, step = 0.1)
               )
        ), # end column
        column(3,
               wellPanel(
                 conditionalPanel(
                   condition = "sec_y_axis == TRUE", 
                   radioButtons(ns("y2_lab"), "Y Label Options:",
                                choices= c("None", "Auto", "Custom"),
                                selected = "Auto"),
                   textInput(ns("y2_lab_text"), "")
                 )
               )
        ) # end column
      )
    )
  )
}


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_TITLE_AND_LABELS <- function(input, output, session, P, Title_Auto, X_Lab_Auto, Y_Lab_Auto, sec_y_axis = FALSE, Y2_Lab_Auto = NULL) {
  
  ns <- session$ns # see General Note 1
  
  P1 <- reactive({
    
    p <- P()
    
    # Title and Axis Lables
    
    # Title
    if(input$title == "None"){
      p <- p + ggtitle("")
    } else if(input$title == "Auto"){
      p <- p + ggtitle(Title_Auto())
    } else if(input$title == "Custom"){
      p <- p + ggtitle(input$title_text)
    }
    
    p <- p + theme(plot.title = element_text(hjust = input$title_margin))
    
    
    # X Axis Label
    if(input$x_lab == "None"){
      p <- p + xlab("")
    } else if(input$x_lab == "Auto"){
      p <- p + xlab(X_Lab_Auto())
    } else if(input$x_lab == "Custom"){
      p <- p + xlab(input$x_lab_text)
    }
    
    p <- p + theme(axis.title.x = element_text(margin = unit(c(input$x_margin, 0, 0, 0), "in")))
    
    
    # Y Axis Label
    if(input$y_lab == "None"){
      p <- p + ylab("")
    } else if(input$y_lab == "Auto"){
      p <- p + ylab(Y_Lab_Auto())
    } else if(input$y_lab == "Custom"){
      p <- p + ylab(input$y_lab_text)
    }
    
    p <- p + theme(axis.title.y = element_text(margin = unit(c(0, input$y_margin, 0, 0), "in")))
    
    p
    
  })
  
  output$sec_y_axis_ui <- renderUI({
    if(sec_y_axis == TRUE){
      tagList(
        radioButtons(ns("y2_lab"), "Y Label Options:",
                     choices= c("None", "Auto", "Custom"),
                     selected = "Auto"),
        textInput(ns("y2_lab_text"), "")
      )
    }
  })
  
  # Do not suspend rendering when Main tab is not selected
  outputOptions(output, "sec_y_axis_ui", suspendWhenHidden = FALSE)
  
  # Secondary Y-axis
  Y2_Lab <- reactive({
    req(input$y2_lab)
    if(sec_y_axis == TRUE){
      if(input$y2_lab == "None"){
        ""
      } else if(input$y2_lab == "Auto"){
        Y2_Lab_Auto()
      } else if(input$y2_lab == "Custom"){
        input$y2_lab_text
      }
    }
  })

  
  return(list(Plot = P1, Y2_Lab = Y2_Lab))
  
} # end Server Function



