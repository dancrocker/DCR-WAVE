##############################################################################################################################
#     Title: Report-AWQ.R
#     Type: Module for DCR Shiny App
#     Description: Report Generation via RMarkdown adn knitR
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes: 
#   1. 
#
# To-Do List:
#   1. 

##############################################################################################################################
# User Interface
##############################################################################################################################

REPORT_AWQ_UI <- function(id, df) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    
    strong("Date Range:"), # bold text
    br(), br(),
    wellPanel(
      # Year
      selectInput(ns("year"), "Year:", 
                  choices = c(rev(year(seq(as.Date("1990-1-1"), Sys.Date(), "years")))), 
                  selected = year(Sys.Date())),
      textInput(ns("awq_text1"), "Section 1 text"),
      textInput(ns("awq_text2"), "Section 2 text"),
      textInput(ns("awq_text3"), "Section 3 text"),
      textInput(ns("awq_text4"), "Conclusion text")
    ), # end Well Panel
    br(), br(),
    downloadButton(ns("report"), "Generate report")

  ) # end Taglist
} # end UI Function
  

##############################################################################################################################
# Server Function
##############################################################################################################################

REPORT_AWQ <- function(input, output, session, df_trib, df_chem, df_prof, df_site) {  
      
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
