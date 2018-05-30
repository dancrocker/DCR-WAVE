##############################################################################################################################
#     Title: Report-Custom.R
#     Type: Module for DCR Shiny App
#     Description: Report Generation via RMarkdown and knitR
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. Make Plot and Table Modules (second level)

##############################################################################################################################
# User Interface
##############################################################################################################################

REPORT_CUSTOM_UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    # INSERT
    wellPanel(
      strong("Insert Report Element"),
      fluidRow(
        actionButton(ns("insert_text_s"), "Insert Text (short)"),
        actionButton(ns("insert_text_l"), "Insert Text (long)"),
        actionButton(ns("insert_plot"), "Insert Plot"),
        actionButton(ns("insert_table"), "Insert Table"),
        actionButton(ns("insert_image"), "Insert Image")
      )
    ),
    # REMOVE
    wellPanel(
      uiOutput(ns("remove_choice_ui")),
      actionButton(ns("remove_action"), "Remove Selected Element")
    ),
    # DOWNLOAD
    wellPanel(    
      downloadButton(ns("report"), "Generate report")
    ),
    tags$div(id = 'placeholder')
  ) # end Taglist
} # end UI Function
  

##############################################################################################################################
# Server Function
##############################################################################################################################

REPORT_CUSTOM <- function(input, output, session, df, df_site) { 
  
# Initialize a vector of inserted elements
  
  values <- reactiveValues(elements = NULL)

  
# Remove Element UI
  
  output$remove_choice_ui <- renderUI({
    
    if(length(values$elements != 0)){
      radioButtons(session$ns("remove_choice"), "Remove Report Elements", choices = values$elements)
    }
    
  })
  
  
# Text (short) Input
  observeEvent(input$insert_text_s, {

    num_text_s <- input$insert_text_s
    id <- paste("textsUI", num_text_s, sep = "")
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Text (Short) Input", num_text_s)), 
        wellPanel(
          textInput(session$ns(paste("text_s", num_text_s)), "")
        ),
        id = id
      )
    )
    
    values$elements <- c(values$elements, id)
    
  })
  
  
# Text (Long) Insert
  
  observeEvent(input$insert_text_l, {
    
    num_text_l <- input$insert_text_l
    id <- paste("textlUI", num_text_l, sep = "")
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Text (Long) Input", num_text_l)), 
        wellPanel(
          textAreaInput(session$ns(paste("text_l", num_text_l)), "", cols = 8)
        ),
        id = id
      )
    )
    
    values$elements <- c(values$elements, id)
    
  })
  
  
# Plot Insert
  
  observeEvent(input$insert_plot, {
    
    num_plot <- input$insert_plot
    id <- paste("plot", num_plot, sep = "")
    
    # Most likely shoulf make this a Module inside a module
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Plot", num_plot)),
        wellPanel(
          fluidRow(
            column(3,
                   checkboxGroupInput(session$ns("site_plot"), "Sites: (Choose 1st)", 
                                      choices= levels(factor(df$Site)),
                                      selected = factor(df$Site[1]),
                                      inline=TRUE)
            ),
            #column(1),
            column(2,
                   selectInput(session$ns("param_plot"), "Water Quality Parameter:",        
                               choices=levels(factor(df$Parameter)),
                               selected = factor(df$Parameter[4]))
            ),
            #column(1),
            column(2,
                   dateRangeInput(session$ns("date_plot"), "Date Range:", 
                                  start = min(df$Date, na.rm = TRUE), 
                                  end = max(df$Date, na.rm = TRUE),
                                  min = min(df$Date, na.rm = TRUE),
                                  max = max(df$Date, na.rm = TRUE),
                                  startview = "year")
            )
          )
        ),
        id = id
        )
    )
    
    values$elements <- c(values$elements, id)
    
  })
 
  
# Table Insert
  
  observeEvent(input$insert_table, {

    num_table <- input$insert_table
    id <- paste("table", num_table)
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Table", num_table)), 
        wellPanel(
          fluidRow(
            column(3,
                   checkboxGroupInput(session$ns("site_table"), "Sites: (Choose 1st)", 
                                      choices= levels(factor(df$Site)),
                                      selected = factor(df$Site[1]),
                                      inline=TRUE)
            ),
            #column(1),
            column(2,
                   checkboxGroupInput(session$ns("param_table"), "Water Quality Parameter:",        
                                      choices=levels(factor(df$Parameter)),
                                      selected = factor(df$Parameter[4]),
                                      inline=TRUE)
            ),
            #column(1),
            column(2,
                   dateRangeInput(session$ns("date_table"), "Date Range:", 
                                  start = min(df$Date, na.rm = TRUE), 
                                  end = max(df$Date, na.rm = TRUE),
                                  min = min(df$Date, na.rm = TRUE),
                                  max = max(df$Date, na.rm = TRUE),
                                  startview = "year")
            )
          )
        ),
        id = id
      )
    )
    
    values$elements <- c(values$elements, id)
    
  })
  
# Image Insert
  observeEvent(input$insert_image, {
    
    num_image <- input$insert_image
    id <- paste("image", num_image)
    
    insertUI(
      selector = "#placeholder",
      where = "beforeEnd",
      ui = tags$div(
        h4(paste("Image", num_image)), 
        id = id)
    )
    
    values$elements <- c(values$elements, id)
    
  })


  
# Remove Report Elements
  
  observeEvent(input$remove_action, {
    removeUI(
      selector = paste("#", input$remove_choice, sep = "")
    )
    
    values$elements <- values$elements[!values$elements %in% input$remove_choice]
    
  })

  
# Output and Save Report
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    } # end content funciton
  ) # end Download Handler
  
} # end Server Function
