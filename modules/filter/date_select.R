##############################################################################################################################
#     Title: DateSelect.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:

# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

DATE_SELECT_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    wellPanel(
      em("Dates sampled at selected sites"), br(),
      uiOutput(ns("date_ui")),
      uiOutput(ns("text_ui")),
      #h5("OR", align = "center"),
      uiOutput(ns("year_ui"))
    )
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df" need to be reactive expressions, not resolved values.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

DATE_SELECT <- function(input, output, session, Df, hidden = FALSE) {

  ns <- session$ns # see General Note 1

  # Min and Max Dates for Sites Selected
  Date_Min <- reactive({Df()$Date %>% min(na.rm=TRUE)})
  Date_Max <- reactive({Df()$Date %>% max(na.rm=TRUE)})


  # Date Selection UI
  output$date_ui <- renderUI({

    # Date Input
    dateRangeInput(ns("date"), "Date Range:",
                   # start = Date_Min(),
                   # end = Date_Max(),
                   min = Date_Min(),
                   max = Sys.Date(),
                   startview = "year")
  })


  # To fill back in previously selected
  observe({
    # save the Parameter Type input for when the Site selection changes. Isolate so does not cause reactivity
    isolate({
      save_selected_lower <- input$date[1]
      save_selected_upper <- input$date[2]
    })

    # If Site list is changed but not empty then generate a Select Input with the...
    # date range for that Site(s) and autoselect previous selected date range
    if(Df() %>% summarise(n()) %>% unlist() != 0){

      updateDateRangeInput(session, inputId = "date", label = "Date Range:",
                           start = save_selected_lower,
                           end = save_selected_upper,
                           min = Date_Min(),
                           max = Sys.Date())

      # If Site list is empty than make a date range of the previously selected date range to save it.
    } else {
      updateDateRangeInput(session, inputId = "date", label = "Date Range:",
                           start = save_selected_lower,
                           end = save_selected_upper,
                           min = save_selected_lower,
                           max = save_selected_upper)
    }
  })


  # Render "Or (Includes Both)" Text
  output$text_ui <- renderUI({
    req(Df() %>% summarise(n()) %>% unlist() != 0)
    h5("OR (includes both)", align = "center")
  })


  ### Year Selection

  # Choices
  Year_Choices <- reactive({c(rev(year(seq(Date_Min(), Date_Max(), "years"))))}) # Change to first year of data

  # Parameter Selection UI
  output$year_ui <- renderUI({
    selectInput(ns("year"), "Year(s):", choices=Year_Choices(), multiple = TRUE)
  })

  # To fill back in previously selected - Memory
  observe({

    # save the Parameter Type input for when the Site selection changes. Isolate so does not cause reactivity
    isolate({
      save_selected <- input$year
    })

    # If Site list is changed but not empty then generate a Select Input with the...
    # parameters for that Site and autoselect previous selected parameter
    if(Df() %>% summarise(n()) %>% unlist() != 0){

      updateSelectInput(session, inputId = "year", label = "Year(s):",
                        choices = Year_Choices(),
                        selected = save_selected)

      # If Site list is empty than make a parameter list of just the previously listed item to save it.
    } else {
      updateSelectInput(session, inputId = "year", label = "Year(s):",
                        choices= save_selected,
                        selected = save_selected)
    }
  })



  return(list(Lower = reactive({input$date[1]}),
              Upper = reactive({input$date[2]}),
              Years = reactive({input$year})))



} # end Server Function

