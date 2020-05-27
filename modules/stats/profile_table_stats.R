##############################################################################################################################
#     Title: Profile-Summary - Shiny Module
#     Description: This script will create Summary Statistics for Profile Data
#     Written by: Nick Zinck, Spring 2017
#     Note: TBD
##############################################################################################################################

#===========================================================================================
# UI side
PROF_TABLE_STAT_UI <- function(id, df) {

ns <- NS(id)

tagList(

  wellPanel(br(),
    fluidRow(
      column(3,
        # Parameter Input
        selectInput(ns("param"), "Parameter:",
                   choices=levels(factor(df$Parameter)),
                   selected = factor(df$Parameter[1]))
      ),
      column(3,
        dateRangeInput(ns("date"), "Date Range:",
                       start = Sys.Date() - years(5),
                       end = Sys.Date(),   #makes todays date
                       min =  min(df$Date, na.rm = TRUE),
                       max = Sys.Date(),
                       startview = "year")
      ),
      column(3,
        # Site Input
        checkboxGroupInput(ns("site"), "Site:", inline = TRUE,
                   choices = levels(factor(df$Site)),
                   selected = factor(df$Site)[1])
      ),
      column(3,
             # Depth Input
             sliderInput(ns("depth"), "Depth:",
                         min = 0,
                         max = df %>% select(Depth_m) %>% max() %>% ceiling(),
                         value = c(0,df %>% select(Depth_m) %>% max() %>% ceiling()),
                         step = 1
             )
      )
    ) # end fluid row
  ), # end well Panel
  sidebarLayout(
    sidebarPanel(width = 3,
           checkboxInput(ns("summary_group_site"), label = "Group by Site", value = TRUE),
           radioButtons(ns("summary_group_time"), "Group by Time:",
                        choices = c("None" = 1,
                                    "Year" = 2,
                                    "Season (all years)" = 3,
                                    "Month (all years)" = 4,
                                    "Season (each year)" = 5,
                                    "month (each year)" = 6),
                        selected = 1),
           radioButtons(ns("summary_group_depth"), "Group by Depth:",
                        choices = c("None" = 1,
                                    "1m interavals (rounded up)" = 2,
                                    "2m intervals" = 3,
                                    "5m intervals" = 4,
                                    "10m intervals" = 5),
                        selected = 1),
           h5('note: "[0m,5m)" actually means "=>0m & <5m"')
    ), # end sidebarPanel
    mainPanel(width = 9,
           tabsetPanel(
             tabPanel("Summary",
                      tableOutput(ns("summary"))
             ),
             tabPanel("Table",
                        dataTableOutput(ns("table"))
             )
           ) # end tabset panel
    ) # end main panel
  ) # end sidebarlayout
) # end taglist
} # end UI

#=======================================================================================
# server side

PROF_TABLE_STAT <- function(input, output, session, df) {

# Reactive Dataframe

  Df2 <- reactive({
    df %>%
      filter(Parameter %in% input$param) %>%
      filter(Date > input$date[1], Date < input$date[2]) %>%
      filter(Site %in% c(input$site)) %>%
      filter(Depth_m > input$depth[1], Depth_m < input$depth[2])
  })

# Summary Stat

  output$summary <- renderTable({

    sum_1 <- Df2() %>%
      mutate(Year = lubridate::year(Date),
             Season = getSeason(Date),
             Month = lubridate::month(Date),
             Depth1 = ceiling(Depth_m),
             Depth2 = cut(Depth_m,
                          breaks = seq(0,44,2),
                          labels = c("[0,2)", "[2,4)", "[4,6)", "[6,8)", "[8,10)",
                                     "[10,12)", "[12,14)", "[14,16)", "[16,18)", "[18,20)",
                                     "[20,22)", "[22,24)", "[24,26)", "[26,28)", "[28,30)",
                                     "[30,32)", "[32,34)", "[34,36)", "[36,38)", "[38,40)",
                                     "[40,42)", "[42,44)"),
                          right = FALSE),
             Depth5 = cut(Depth_m,
                          breaks = seq(0,45,5),
                          labels = c("[0,5)", "[5,10)", "[10,15)", "[15,20)",
                                     "[20,25)", "[25,30)", "[30,35)", "[35-40)", "[40-45)"),
                          right = FALSE),
             Depth10 = cut(Depth_m,
                           breaks = seq(0,50,10),
                           labels = c("[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)"),
                           right = FALSE))


    # group by time

    if (input$summary_group_time == 1){
      sum_dots = c()
    } else if(input$summary_group_time == 2) {
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

    # group by site
    if(input$summary_group_site == TRUE){
      sum_dots <- c(sum_dots, "Site")
    }

    # group by depth
    if(input$summary_group_depth == 2){
      sum_dots <- c(sum_dots, "Depth1")
    } else if(input$summary_group_depth == 3){
      sum_dots <- c(sum_dots, "Depth2")
    } else if(input$summary_group_depth == 4){
      sum_dots <- c(sum_dots, "Depth5")
    } else if(input$summary_group_depth == 5){
      sum_dots <- c(sum_dots, "Depth10")
    }


    if (input$summary_group_site == FALSE & input$summary_group_time == 1 & input$summary_group_depth == 1){
      sum_2 <- sum_1
    } else {
      sum_2 <- sum_1 %>%
        group_by_(.dots = sum_dots)
    }


    sum_2 %>% summarise(average = mean(Result),
                        min = min(Result),
                        max = max(Result),
                        median = median(Result),
                        variance = var(Result),
                        `stand. dev.` = sd(Result),
                        `number of samples` = n())
  }) # end summary


# Table

  output$table <- renderDataTable(Df2())


} # end server

