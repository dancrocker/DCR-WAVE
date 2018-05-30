##############################################################################################################################
#     Title: stats_summary.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Summary Stats with Grouping
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

STAT_TIME_WQ_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    fluidRow(
      column(3,
             checkboxInput(ns("summary_group_site"), label = "Group by Site", value = TRUE),
             radioButtons(ns("summary_group_time"), "Temporal Groupings:",
                          choices = c("None" = 1,
                                      "Year" = 2,
                                      "Season (all years)" = 3,
                                      "Month (all years)" = 4,
                                      "Season (each year)" = 5,
                                      "month (each year)" = 6),
                          selected = 1)
      ),
      column(9,
             tableOutput(ns("summary"))
      ) # end column
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

STAT_TIME_WQ <- function(input, output, session, Df) {

# Summary Statistics

  output$summary <- renderTable({

    # # Add Year, season, and Month Columns
    # sum_1 <- Df() %>%
    #   mutate(Year = as.integer(lubridate::year(Date)),
    #          Season = getSeason(Date),
    #          Month = month.abb[lubridate::month(Date)]
    #   )


    # Group by time (year, season, month)
    if (input$summary_group_time == 1){
      sum_dots = c()
    } else if (input$summary_group_time == 2) {
      sum_dots = c("Year")
    } else if (input$summary_group_time == 3) {
      sum_dots = c("Season")
    } else if (input$summary_group_time == 4) {
      sum_dots = c("Month")
    } else if (input$summary_group_time == 5) {
      sum_dots = c("Year", "Season")
    } else if (input$summary_group_time == 6) {
      sum_dots = c("Year", "Month")
    }

    # Group by site
    if(input$summary_group_site == TRUE){
      sum_dots <- c(sum_dots, "LocationLabel")
    }

    # Group by Param
    sum_dots <- c("Parameter", sum_dots)

    # Applying Grouping (stats is always grouped by parameter)

    sum_2 <- Df() %>% group_by_(.dots = sum_dots)

    # Making the Sumamry Statistic Columns
    sum_2 %>% summarise(`number of samples` = n(),
                        average = mean(Result),
                        `stand. dev.` = sd(Result, na.rm=TRUE),
                        min = min(Result, na.rm=TRUE),
                        `1st quartile` = quantile(Result, 0.25),
                        median = median(Result),
                        `3rd quartile` = quantile(Result, 0.75),
                        max = max(Result, na.rm=TRUE),
                        `geometric mean` = gm_mean(Result))
  })



} # end Server Function

