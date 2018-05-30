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

PLOT_SAVE_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    column(6,
           wellPanel(
             h4("Save", align = "center"),

               fluidRow(
                 column(6,
                        radioButtons(ns("save_type"), "File Type:",
                                     choices= c("pdf",
                                                "jpg",
                                                "png"),
                                     inline = TRUE),
                        br(),
                        radioButtons(ns("legend_pos"), "Legend Position:",
                                           choices= c("right",
                                                      "bottom-horizontal",
                                                      "bottom-vertical"))
                 ),
                 column(6,
                        numericInput(ns("save_width"), "Plot Width (inches):", 11,
                                     min = 5, max = 20, step = 0.5),
                        
                        numericInput(ns("save_height"), "Plot Height (inches):", 8.5,
                                     min = 5, max = 20, step = 0.5)
                        
                 )
               ),
             tags$div(
               fluidRow(hr(),
                 downloadButton(ns('save_plot'), "Save Plot")
               ), 
               align = "center"
             )
           )

    ),
    column(6,
           wellPanel(
             h4("Margins", align = "center"),
             fluidRow(
               column(6,
                      sliderInput(ns("plot_margin_top"), "adjust top margin",
                                  min = 0, max = 1.5, value = 0.2, step = 0.1),
                      sliderInput(ns("plot_margin_bottom"), "adjust bottom margin",
                                  min = 0, max = 1.5, value = 0.2, step = 0.1)
               ),
               column(6,
                      sliderInput(ns("plot_margin_left"), "adjust left margin",
                                  min = 0, max = 1.5, value = 0.5, step = 0.1),
                      sliderInput(ns("plot_margin_right"), "adjust right margin",
                                  min = 0, max = 1.5, value = 0.2, step = 0.1)
               )
             ) # fluidRow
           ) # wellPanel
    ) # end column

  )
}


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PLOT_SAVE <- function(input, output, session, P, Plot_Name) {

  ns <- session$ns # see General Note 1
  
  P1 <- reactive({
    
    p <- P()
    
    if(input$legend_pos == "bottom-horizontal"){
      p <- p + theme(legend.position="bottom")
    } else if(input$legend_pos == "bottom-vertical"){
      p <- p + theme(legend.position="bottom",legend.direction="vertical")
    }
    
    # Margin Options
    # Size dependent? Change size for saving?
    p <- p + theme(plot.margin = unit(c(input$plot_margin_top, 
                                        input$plot_margin_right, 
                                        input$plot_margin_bottom, 
                                        input$plot_margin_left), 
                                      "in"))
    

    
    
    p
    
  })
    
  # Filename
  
  Filename <- reactive({
    paste0(Plot_Name(), ".", input$save_type)
  })
  
  # Plot Print
  
  output$save_plot <- downloadHandler(
    filename = function(){Filename()},
    content = function(file){ggsave(file, plot = P1(), 
                                    width = input$save_width,
                                    height = input$save_height,
                                    device = input$save_type)}
  )
    
  
} # end Server Function


