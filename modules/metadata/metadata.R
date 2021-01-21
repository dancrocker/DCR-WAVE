##############################################################################################################################
#     Title: Metadata.R
#     Type: Module for DCR Shiny App
#     Description: Shows Tables of Metadata (Location, Parameter, Flags)
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#
# To-Do List:
#   1. Make the Metero/Hydro Filters work
#   2. Plotting Features - Show Limits, Finish and Clean up Coloring options (flagged data, met filters)

##############################################################################################################################
# User Interface
##############################################################################################################################

METADATA_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    tabsetPanel(
      tabPanel("Location",
               fluidRow(
                 dataTableOutput(ns("table_site"))
               ), # end Fluid Row
               br(),
               br(),
               wellPanel(
                 fluidRow(
                   column(3,
                          SITE_MAP_UI(ns("site_map"))
                   ),
                   column(6,
                          tableOutput(ns("table_site_stat")),
                          h3(textOutput(ns("site_text_null"))),
                          h3(textOutput(ns("site_text_select")))
                   ),
                   column(3,
                          imageOutput(ns("river_image"))
                   )
                 )
               ) # end Fluid Row
      ),
      tabPanel("Parameter",
               fluidRow(
                 dataTableOutput(ns("table_param"))
               ), # end Fluid Row
               br(),
               br(),
               wellPanel(
                 fluidRow(
                   column(1),
                   column(10,
                          tableOutput(ns("table_param_stat")),
                          h3(textOutput(ns("param_text_null"))),
                          h3(textOutput(ns("param_text_select")))
                   ),
                   column(1)
                 )
               ) # end Fluid Row
      )#,
      # tabPanel("Flags Codes",
      #          fluidRow(
      #            dataTableOutput(ns("table_flag"))
      #          ), # end Fluid Row
      #          br(),
      #          br(),
      #          wellPanel(
      #            fluidRow(
      #              column(3),
      #              uiOutput(ns("df_choice_flag_ui")),
      #              uiOutput(ns("flag_grouping_ui")),
      #              column(9,
      #
      #                     tableOutput(ns("table_flag_stat")),
      #                     h3(textOutput(ns("flag_text_null"))),
      #                     h3(textOutput(ns("flag_text_select")))
      #              )
      #            )
      #          ) # end Fluid Row
      # )
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

# This module takes df as a reactive expressions. Changes will have to be made to accmodate reactive expressions
#

METADATA <- function(input, output, session, df_site = NULL, df_param = NULL, df) {

  ns <- session$ns # see General Note 1

  df_site <- df_site %>%
    select(LocationLabel,LocationCategory, LocationDescription, LocationLat, LocationLong, LocationElevFt) %>%
    dplyr::rename(Location = LocationLabel, Status = LocationCategory, Description = LocationDescription, Latitude = LocationLat, Longitude = LocationLong,
                  `Elevation (ft)` = LocationElevFt)
  ### Primary Tables

  output$table_site <- DT::renderDataTable(df_site, selection = 'single',
                                       options = list(lengthMenu = c(5, 10, 50), pageLength = 5))

  output$table_param <- DT::renderDataTable(df_param, selection = 'single',
                                        options = list(lengthMenu = c(5, 10, 50), pageLength = 5))

  #output$table_flag <- renderDataTable(df_flag, selection = 'single')


  ### Additonal Site Info

  # Selected row from DT
  Site_Selected <- reactive({
    req(input$table_site_rows_selected)
    df_site[input$table_site_rows_selected, ]
  })

  # UI for Full or filtered choice
  output$df_choice_site_ui <- renderUI({
    req(Site_Selected())
    radioButtons(ns("df_choice_site"), "Full or Filtered Data:",
                 choices = c("full", "filtered"),
                 inline = TRUE)
  })

  # Site Map
  callModule(SITE_MAP_SINGLE, "site_map", Site = Site_Selected)

  # Site Overview Table
  Df_Site_Info_a <- reactive({
    df %>%
      filter(LocationLabel %in% Site_Selected()$Location) %>%
      summarise(`Parameter` = "ALL",
                `Number of Samples` = n(),
                `Start Date` = as.character(min(Date)),
                `End Date` = as.character(max(Date)),
                `Latest Result` = NA,
                `Units` = NA)
  })

  Df_Site_Info_b <- reactive({
    df %>%
      filter(LocationLabel %in% Site_Selected()$Location) %>%
      group_by(Parameter) %>%
      summarise(`Number of Samples` = n(),
                `Start Date` = as.character(min(Date)),
                `End Date` = as.character(max(Date)),
                `Latest Result` = Result[Date == max(Date)],
                `Units` = Units[Date == max(Date)])
  })

  Df_Site_Info <- reactive({rbind(Df_Site_Info_a(), Df_Site_Info_b())})

  output$table_site_stat <- renderTable({
    req(Site_Selected())
    Df_Site_Info()
    })

  # Site Image
  output$river_image <- renderImage({
    req(Site_Selected()) # can delete once Site_Selected is used in renderImage due to earlier req()

    list(src = "images/QuinapoxetMarch2018.jpg",
         width="100%",
         height= "350")
  }, deleteFile = FALSE)

  # Site Text - when no site table is given to module
  output$site_text_null <- renderText({
    req(is.null(df_site))
    "SELECT a ROW above to see MORE INFO on a LOCATION"
  })

  # Site Text - when no site is selected
  output$site_text_select <- renderText({
    req(!isTruthy(input$table_site_rows_selected))
    "SELECT a ROW above to see MORE INFO on a LOCATION"
  })



  ### Additional Parameter Info Table

  # Selected row from DT
  Param_Selected <- reactive({
    req(input$table_param_rows_selected)
    df_param[input$table_param_rows_selected, ]
  })

  # UI for Full or filtered choice
  output$df_choice_param_ui <- renderUI({
    req(Param_Selected())
    radioButtons(ns("df_choice_param"), "Full or Filtered Data:",
                 choices = c("full", "filtered"),
                 inline = TRUE)
  })

  # param Overview Table
  Df_Param_Info_a <- reactive({
    df %>%
      filter(Parameter %in%  Param_Selected()$ParameterName) %>%
      summarise(`Location` = "ALL",
                `Number of Samples` = n(),
                `Start Date` = as.character(min(Date)),
                `End Date` = as.character(max(Date)),
                `Latest Result` = NA,
                `Units` = NA)
  })

  Df_Param_Info_b <- reactive({
    df %>%
      filter(Parameter %in%  Param_Selected()$ParameterName) %>%
      group_by(Location) %>%
      summarise(`Number of Samples` = n(),
                `Start Date` = as.character(min(Date)), # need as.character becuase of renderTable has a bug for Date Class (xtable bug)
                `End Date` = as.character(max(Date)),
                `Latest Result` = Result[Date == max(Date)],
                `Units` = Units[Date == max(Date)])
  })

  Df_Param_Info <- reactive({rbind(Df_Param_Info_a(), Df_Param_Info_b())})

  output$table_param_stat <- renderTable(Df_Param_Info())

  # Param Text - when no Parameter table is not available
  output$param_text_null <- renderText({
    req(is.null(df_param))
    "No Parameter Table available. Please set up"
  })

  # Param Text - when no site is selected
  output$param_text_select <- renderText({
    req(!isTruthy(input$table_param_rows_selected))
    "SELECT a ROW above to see MORE INFO on a PARAMETER"
  })



  # ### Additional Flag Info Table
  #
  # # Selected row from DT
  # Flag_Selected <- reactive({
  #   req(input$table_flag_rows_selected)
  #   df_flag[input$table_flag_rows_selected, ]
  # })
  #
  # # UI for Full or filtered choice
  # output$df_choice_flag_ui <- renderUI({
  #   req(Flag_Selected())
  #   radioButtons(ns("df_choice_flag"), "Full or Filtered Data:",
  #                choices = c("full", "filtered"),
  #                inline = TRUE)
  # })
  #
  #
  # # UI for grouping type
  # output$flag_grouping_ui <- renderUI({
  #   req(Flag_Selected())
  #   radioButtons(ns("flag_grouping"), "Flag Grouping:",
  #                choices = c("None" = "None",
  #                            "Site" = c("LocationLabel"),
  #                            "Parameter" = c("LocationLabel", "Parameter"),
  #                            "Site and Parameter" = c("LocationLabel", "Parameter"),
  #                            "Parameter and Site" = c("Parameter", "LocationLabel"))
  #                )
  # })
  #
  #
  # Df_Flag_Info <- reactive({
  #
  #   df_temp <- Df() %>% filter_(!is.null(paste0("Flag", Flag_Selected()$Flag_ID)))
  #
  #   if(input$flag_grouping == "None"){
  #     df_temp <- df.temp
  #   }else{
  #     df_temp <- group_by_(.dots = input$flag_grouping)
  #   }
  #
  #   df_temp %>% summarise(`Number of Samples` = n(),
  #                         `Start Date` = as.character(min(Date)), # need as.character becuase of renderTable has a bug for Date Class (xtable bug)
  #                         `End Date` = as.character(max(Date)))
  # })
  #
  #
  # # Flag Text - when no site table is given to module
  # output$flag_text_null <- renderText({
  #   req(is.null(df_flag))
  #   "No Flag Code Table available. Please set up"
  # })
  #
  # # Flag Text - when no site is selected
  # output$flag_text_select <- renderText({
  #   req(!isTruthy(input$table_flag_rows_selected))
  #   "SELECT a ROW above to see MORE INFO on a Flag Code"
  # })


} # end Server Function

