##############################################################################################################################
#     Title: StationLevelCheckBox.R
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

STATION_LEVEL_CHECKBOX_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      column(6,
             # Site Selection
             wellPanel(
               CHECKBOX_SELECT_ALL_UI(ns("station"))
             )
      ),
      column(6,
             wellPanel(
               CHECKBOX_SELECT_ALL_UI(ns("level"))
             ) # end Well Panel
      )
    ) # end FluidRow
  ) # end taglist

} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "Df"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

STATION_LEVEL_CHECKBOX <- function(input, output, session, df) {

  ### Station

  # Server for Checkbox Select All Module
  Station_Selected <- callModule(CHECKBOX_SELECT_ALL, "station",
                                 label = "Station:",
                                 Choices = reactive({unique(df$Station)}))



  ### Location Depth

  # Server for Checkbox Select All Module
  Level_Selected <- callModule(CHECKBOX_SELECT_ALL, "level",
                               label = "Sampling Level:",
                               Choices = reactive({unique(df$LocationDepth)}))


  
  ### Create Site (location Labels) Choices from selected Stations and Levels
  Site_Selected <- reactive({

    req(Station_Selected(), Level_Selected())

    df %>%
      filter(Station %in% Station_Selected(),
             LocationDepth %in% Level_Selected()) %>%
      .$LocationLabel %>%
      factor() %>%
      levels()

  })

  ### Return Site List
  return(Site_Selected)

} # end Server Function

