##############################################################################################################################
#     Title: SiteCheckbox.R
#     Type: Module2 for DCR Shiny App
#     Description: Combined Seleciton Widget for Primary and NonPrimary Site seperation with optional NonPrimary Sites Shown.
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#
# To-Do List:

##############################################################################################################################
# User Interface
##############################################################################################################################

SITE_CHECKBOX_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      # Site Selection
      wellPanel(h4("LOCATION FILTER:", align = "left"),
        uiOutput(ns("site_primary_ui")),
        br(),
        uiOutput(ns("site_nonprimary_category_ui")),
        uiOutput(ns("site_nonprimary_ui"))
      ) # end Well Panel
    )
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

SITE_CHECKBOX <- function(input, output, session, df) {

  ns <- session$ns # see General Note 1

  ### Site - Primary

  # List
  site_primary_choices <- df %>%
    filter(grepl("Primary", LocationCategory)) %>%
    arrange(Site)%>% #Sorts them alphabetically by Site (eg. MD01, MD02)
    .$LocationLabel %>% unique()

  # Primary Site INput Widget
  output$site_primary_ui <- renderUI({
    checkboxGroupInput(ns("site_primary"),
                       label =  "Primary Active Sites:",
                       choices = site_primary_choices)
  })



  ### Site - Non Primary Categories

  # List
  site_nonprimary_category_choices <- reactive({
    df %>%
      filter(LocationCategory != "Primary Active") %>%
      .$LocationCategory %>% factor(exclude = FALSE) %>% levels()
    })

  # UI
  output$site_nonprimary_category_ui <- renderUI({

    checkboxGroupInput(ns("site_nonprimary_category"),
                       label = "Show Other Categories:",
                       choices = site_nonprimary_category_choices())
  })




  ### Site - NonPrimary Sites

  # List
  Site_Nonprimary_Choices <- reactive({

    df %>%
      filter(LocationCategory %in% input$site_nonprimary_category) %>%
      arrange(Site)%>% #Sorts them alphabetically by Site (eg. MD01, MD02)
      .$LocationLabel %>%
      factor() %>%
      levels()
  })

  # NonPrimary Site INput Widget
  output$site_nonprimary_ui <- renderUI({
    if(isTruthy(input$site_nonprimary_category)){
    checkboxGroupInput(ns("site_nonprimary"),
                       label =  paste0("Other Sites (", paste(input$site_nonprimary_category, collapse = ", "), ") :"),
                       choices = Site_Nonprimary_Choices())
    }
  })


  ### Return selected site list "site" from callModule
  return(reactive({c(input$site_primary, input$site_nonprimary)}))

} # end Server Function

