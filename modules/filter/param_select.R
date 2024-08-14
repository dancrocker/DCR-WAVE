##############################################################################################################################
#     Title: ParamSelect.R
#     Type: Module2 for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#
# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

PARAM_SELECT_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    # Parameter Selection
    wellPanel(
      em("Parameters sampled at selected sites"), br(),
      uiOutput(ns("type_ui")),
      uiOutput(ns("range_ui")),
      em("Note - value range restriction applies to all parameters selected")
    ) # end Well Panel
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Arguments "Df"  and "Site" need to be reactive expressions, not resolved values.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

PARAM_SELECT <- function(input, output, session, Df, multiple = TRUE) {

  ns <- session$ns # see General Note 1

  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6

  Parameters_Non_Historical <- reactive({
    Df() %>%
      filter(Date > Sys.Date()- years(5), Date < Sys.Date()) %>%
      .$Parameter %>%
      factor() %>%
      levels()
  })


  # Parameter Choice List

  Param_Choices <- reactive({

    # Parameters which have data at any Site (in the mofule's Df) within 5 years.
    param_new_choices <- Df() %>%
      filter(Parameter %in% Parameters_Non_Historical()) %>%
      .$Parameter %>%
      factor() %>%
      levels()

    # Parameters which do NOT have data at any Site (in the mofule's Df) within 5 years.
    param_old_choices <- Df() %>%
      filter(!(Parameter %in% Parameters_Non_Historical())) %>%
      .$Parameter %>%
      factor() %>%
      levels()

    # Cmbine lists (recent parameters first and then old parameters)
    c(param_new_choices, param_old_choices)

  })



  # Parameter Selection UI
  output$type_ui <- renderUI({
    selectInput(ns("type"), "Choose Parameter(s):", choices=c(Param_Choices()), multiple = multiple)
  })


  # To fill back in previously selected - Memory
  observe({

    # save the Parameter Type input for when the Site selection changes. Isolate so does not cause reactivity
    isolate({
        save_selected <- input$type
    })

    # If Site list is changed but not empty then generate a Select Input with the...
    # parameters for that Site and autoselect previous selected parameter
    if(Df() %>% summarise(n()) %>% unlist() != 0){

      updateSelectInput(session, inputId = "type", label = "Choose Parameter(s):",
                        choices=c(Param_Choices()),
                        selected = save_selected)

      # If Site list is empty than make a parameter list of just the previously listed item to save it.
    } else {
      updateSelectInput(session, inputId = "type", label = "Choose Parameter(s):",
                        choices= save_selected,
                        selected = save_selected)
    }

  })


  # Units Texts for Selected Parameter
  # 
  Units <- reactive({
    Df() %>%
      filter(Parameter %in% input$type) %>%
      .$Units %>%
      factor() %>%
      levels() %>% 
      paste(collapse = ", ")
  })



  # Parameter Value Range Bar UI
  output$range_ui <- renderUI({
  req(!is.null(input$type))
      result <- Df() %>%
        filter(Parameter %in% input$type) %>%
        .$Result

      param_min <- result %>% min(na.rm=TRUE)
      param_max <- result %>% max(na.rm=TRUE)

      sliderInput(ns("range"), paste("Range (", Units() , ")"),
                  min = param_min, max = param_max,
                  value = c(param_min, param_max))
  })



  ### return List of reactive expressions
  return(list(Type = reactive({input$type}),
              Units = reactive({Units()}), # Units = Units
              Range_Min = reactive({input$range[1]}),
              Range_Max = reactive({input$range[2]})))


} # end Server Function

