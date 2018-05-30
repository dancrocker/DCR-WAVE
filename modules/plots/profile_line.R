##############################################################################################################################
#     Title: Profile-Line.R
#     Type: Module for DCR Shiny App
#     Description: Line Plot for
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. req() will delay the rendering of a widget or other reactive object until a certain logical expression is TRUE or not NULL
#   2. Tried Progress Bar for Plot and did not work well. Used Custom Message instead

# To-Do List:
#   1. Make Loading Bar for Plot
#   2. Make option for COloring Scale (whether based on Site, Year; Site; or None)
#   3. Change Decimal Date to DOY

##############################################################################################################################
# User Interface
##

PROF_LINE_UI <- function(id, df) {

ns <- NS(id)

tagList(
  wellPanel(
    fluidRow(
      column(2,
             # SITE
             wellPanel(
               checkboxGroupInput(ns("site"), "Site: (Select First)",
                                  choices = levels(factor(df$Site)))
             )
      ),
      column(2,
             # SITE
             wellPanel(
               h3(textOutput(ns("text_site_null")), align = "center"),
               h3(textOutput(ns("text_param_null")), align = "center"),
               h4(textOutput(ns("text_num_text")), align = "center"),
               h3(textOutput(ns("text_num")), align = "center")
             )
      ),
      column(3,
             # PARAMETER
             wellPanel(
               uiOutput(ns("param_ui"))
             )
      ),
      column(5,
             # DATE
             wellPanel(
               fluidRow(
                 column(6,
                        radioButtons(ns("date_option"), "Choose Date Method:",
                                     choices=c("Calendar Range",
                                               "Select Year",
                                               "Select Month",
                                               "Select Day"),
                                     selected = "Calendar Range")
                 ),
                 column(6,
                        uiOutput(ns("date_ui"))
                 )
               ) # end Fluid Row
             ) # end Well Panel
      ) # end Column
    ) # end Fluid Row
  ), # well panel

  tabsetPanel(
    # the "Plot" tab panel where everything realted to the plot goes
    tabPanel("Custom Plot",
             PLOT_PROFLINE_CUSTOM_UI(ns("plot"))
    ),
    tabPanel("Standard Template Line Plot",
             fluidRow(
               h2("Soon to Come", align = "center")
             )
    ),
    tabPanel("Table",
             fluidRow(
               dataTableOutput(ns("table_dynamic"))
             )
    )
  ) # end tabsetpanel
) # end taglist
}

##############################################################################################################################
# Server Function
##############################################################################################################################


PROF_LINE <- function(input, output, session, df) {

  ns <- session$ns

  # filter DF for blank data

  df <- df %>% filter(!is.na(Date),
                      !is.na(Depth_m),
                      !is.na(Result))

  # Non Historical Parameters (when a Parameter has not been used in over 5 years). See General Note 6

  parameters_non_historical <- df %>%
    filter(Date > Sys.Date()-years(5), Date < Sys.Date()) %>%
    .$Parameter %>%
    factor() %>%
    levels()


  # Parameter Selection UI

  output$param_ui <- renderUI({

    req(input$site) # See General Note 5

    ns <- session$ns # see General Note 1

    # Parameters which have data at any Site (in the mofule's df) within 5 years.
    param_choices_new <- df %>%
      filter(Site %in% input$site,
             Parameter %in% parameters_non_historical) %>%
      .$Parameter %>%
      factor() %>%
      levels()

    # Parameters which do NOT have data at any Site (in the mofule's df) within 5 years.
    param_choices_old <- df %>%
      filter(Site %in% input$site,
             !(Parameter %in% parameters_non_historical)) %>%
      .$Parameter %>%
      factor() %>%
      levels()

    # Recent Parameters first and then old parameters
    param_choices <- c(param_choices_new, param_choices_old)

    # Parameter Input
    checkboxGroupInput(ns("param"), "Parameter:",
                       choices=levels(factor(df$Parameter)))

  })


# Depending on input$date.option, we'll generate a different UI date component

  output$date_ui <- renderUI({

    req(input$site) # See General Note 5

    dates <- df %>%
      filter(Site %in% input$site) %>%
      .$Date

    date_min <- dates %>% min(na.rm=TRUE)
    date_max <- dates %>% max(na.rm=TRUE)

    # Date Input

    months_unique <- levels(factor(month(dates)))
    days_unique <- levels(factor(dates))

    switch(input$date_option,

           "Calendar Range" = dateRangeInput(ns("date"), "Date Range:",
                                             start = date_max - years(1),
                                             end = date_max,
                                             min = date_min,
                                             max = date_max,
                                             startview = "year"),

           "Select Year" = selectInput(ns("date"), "Year:",
                                       choices = year(seq(date_min, date_max, "years")),
                                       selected = year(date_max)),

           "Select Month" = selectInput(ns("date"), "Month:",
                       choices = c(months_unique),
                       selected = month(Sys.Date())),

           "Select Day" = selectInput(ns("date"), "Day:",
                                        choices = days_unique)

    )
  })


# Reactive Data Frames for different date selection methods:

  Df2 <- reactive({

    req(input$site, input$param, input$date, input$date_option) # See General Note 5

    if(input$date_option == "Select Year"){
        df %>%
          filter(Parameter %in% input$param,
                 Site %in% c(input$site),
                 year(Date) == input$date)
    } else if (input$date_option == "Select Month"){
      df %>%
        filter(Parameter %in% input$param,
               Site %in% c(input$site),
               month(Date) == input$date)
    } else if (input$date_option == "Calendar Range"){
      df %>%
        filter(Parameter %in% input$param,
               Site %in% c(input$site),
               Date > input$date[1], Date < input$date[2])
    } else if (input$date_option == "Select Day"){
      df %>%
        filter(Parameter %in% input$param,
               Site %in% c(input$site),
               Date == input$date)
    }

  })


  # Text - Select Site

  output$text_site_null <- renderText({
    req(is.null(input$site)) # See General Note 1
    "Select Site(s)"
  })

  # Text - Select Parameter

  output$text_param_null <- renderText({
    req(!is.null(input$site), is.null(input$param)) # See General Note 1
    "Select Parameter(s)"
  })

  # Text - Number of Samples

  output$text_num_text <- renderText({
    req(input$site, input$param) # See General Note 1
    "Number of Samples in Selected Data"
  })

  # Text - Number of Samples

  output$text_num <- renderText({
    req(Df2()) # See General Note 1
    Df2() %>% summarise(n()) %>% paste()
  })

# Plot

  callModule(PLOT_PROFLINE_CUSTOM, "plot", Df = Df2)

# Table

  output$table_dynamic <- renderDataTable(Df2())


}
