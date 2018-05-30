##############################################################################################################################
#     Title: SelectInputSelectAll.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:

# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

SELECT_SELECT_ALL_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    uiOutput(ns("selectinput_ui")),
    uiOutput(ns("actionbuttons"))
  )
}
    
##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argumetns "choices" and "selected" need to be reactive expressions, not resolved values. 
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

SELECT_SELECT_ALL <- function(input, output, session, label, Choices, Selected = reactive(NULL), colwidth = 3, hidden = FALSE) { 
  
  ns <- session$ns # see General Note 1
  
  # Select Input Widget
  output$selectinput_ui <- renderUI({
    ns <- session$ns # see General Note 1
    selectInput(inputId = ns("selectinput"), label = label, choices = Choices(), selected = Selected(), multiple = TRUE)
  })
  
  ### Update the Checkbox based on Action Buttons
  observeEvent(input$select_all, {
    updateSelectInput(session = session, inputId = "selectinput", label = NULL, choices = Choices(), selected = Choices())
  })
  
  observeEvent(input$select_def, {
    updateSelectInput(session = session, inputId = "selectinput", label = NULL, choices = Choices(), selected = Selected())
  })
  
  observeEvent(input$unselect_all, {
    updateSelectInput(session = session, inputId = "selectinput", label = NULL, choices = Choices())
  })
  
  
  # Create Action Buttons to Select All/ Unselect All/ Select Default
  output$actionbuttons <- renderUI({
    # If selected is not NULL or ALL
    if(is.null(Selected()) | length(Choices()) == length(Selected())) {
      # Size Buttons according to column width (input)
      if(colwidth >= 3){
        tagList(
          fluidRow(actionButton(inputId = ns("select_all"), label = "Select All", width = "48%"), 
                   actionButton(inputId = ns("unselect_all"), label = "Unselect All", width = "48%"))
        )
      } else {
        tagList(
          fluidRow(actionButton(inputId = ns("select_all"), label = "Select All", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect_all"), label = "Unselect All", width = "98%"))
        )
      }
      # If Default Selection - Add a third button
    } else {
      if(colwidth >= 3){
        tagList(
          fluidRow(actionButton(inputId = ns("select_all"), label = "Select All", width = "32%"),
                   actionButton(inputId = ns("select_def"), label = "Select Default", width = "32%"), 
                   actionButton(inputId = ns("unselect_all"), label = "Unselect All", width = "32%"))
        )
      } else {
        tagList(
          fluidRow(actionButton(inputId = ns("select_all"), label = "Select All", width = "98%")),
          fluidRow(actionButton(inputId = ns("select_def"), label = "Select Default", width = "98%")), 
          fluidRow(actionButton(inputId = ns("unselect_all"), label = "Unselect All", width = "98%"))
        )
      }
    }
  })
  

  # Getoptions(SuspendedWhenHidden = FALSE) to selectinput.ui so when input are on serperate tabs, uiOUtput (by renderUI) does not suspend
  # this is used in Filter Tab becuase this is hidden when other tabs are open yet use this info
  if(hidden == TRUE){
    outputOptions(output, "selectinput_ui", suspendWhenHidden = FALSE)
  }
  
  
  
  return(reactive({input$selectinput}))
  
}

