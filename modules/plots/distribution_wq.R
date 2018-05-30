##############################################################################################################################
#     Title: Tributary-Regression.R
#     Type: Module for DCR Shiny App
#     Description: Regression plots and tables
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#

##############################################################################################################################
# User Interface
##############################################################################################################################

DISTRIBUTION_WQ_UI <- function(id) {

  ns <- NS(id)

  tagList(

    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("param_ui")),
        radioButtons(ns("plot_type"), "Plot Type",
                     choices = c("Histogram", "Density Curve", "Box Plot"),
                     inline = TRUE),
        strong("Outliers"),
        checkboxInput(ns("rm_outliers"), "Remove Outliers"),
        fluidRow(
          column(6,
                 radioButtons(ns("color"), "group by color",
                              choices = c("None" = "None",
                                          "Site" = "LocationLabel",
                                          "Year",
                                          "Month",
                                          "Season"))
          ),
          column(6,
                 radioButtons(ns("facet"), "group by facet",
                              choices = c("None" = "None",
                                          "Site" = "LocationLabel",
                                          "Year",
                                          "Month",
                                          "Season"))
                 )
        ),
        uiOutput(ns("histo_ui")),
        strong(textOutput(ns("text_box_plot")))
      ),
      mainPanel(
        h4(textOutput(ns("text_plot_no_data"))),
        h4(textOutput(ns("text_plot_zero_data"))),
        plotOutput(ns("plot"))
      )
    )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

DISTRIBUTION_WQ <- function(input, output, session, Df) {

  ns <- session$ns # see General Note 1

  # Find Choices for Param
  Param <- reactive({Df()$Parameter %>% factor() %>% levels()})

  # Parameter Choice
  output$param_ui <- renderUI({
    if(Df() %>% summarise(n()) %>% unlist() != 0){
      radioButtons(ns("param"), "Parameter",
                   choices = Param())
    }
  })

  # Remove Outliers - see function in functions/stat_functions.R
  Df1 <- reactive({

    df_temp <- Df() %>% filter(Parameter == input$param)

    if(input$rm_outliers == TRUE){
      df_temp %>% mutate(Result = remove_outliers(Result))
    }else{
      df_temp
    }
  })


  # render UI
  output$histo_ui <- renderUI({

    if(input$plot_type == "Histogram"){
    slide_vec <- pretty(Df1()$Result, n = 20)
    interval <- (max(slide_vec) - min(slide_vec)) / (length(slide_vec)-1)
    digit <- round(log10(interval))*-1 + 2
    slide_val <- round(interval, digit)*2
    slide_min_step <- slide_val/4
    slide_max <- slide_val*2


    tagList(
      hr(),
      sliderInput(ns("bin_size"), "Bin Size (Hstogram)",
                  min = slide_min_step, max = slide_max,
                  value = slide_val, step = slide_min_step),
      radioButtons(ns("position"), "Position Type (Histogram)",
                   choices = c("stack", "identity", "dodge"),
                   inline = TRUE)
    )
    } else{

    }

  })


  # Plot Output
  output$plot <- renderPlot({
    P()
  })

  # Plot
  P <- reactive({

    p <- ggplot(Df1())

    # Histogram
    if(input$plot_type == "Histogram"){
      if(input$color == "None"){
        p <- p + geom_histogram(aes(x = Result), size = 1.0, binwidth = input$bin_size, position = input$position)
      } else{
        if(input$position == "identity"){
        p <-  p + geom_histogram(aes_string(x = "Result", color = input$color),
                                 size =1.0,  binwidth = input$bin_size, position = input$position)
        } else{
          p <-  p + geom_histogram(aes_string(x = "Result", fill = input$color, color = input$color),
                                   alpha = 0.1, size =1.0,  binwidth = input$bin_size, position = input$position)
        }

      }
    # Density Curve
    } else if(input$plot_type == "Density Curve"){
      if(input$color == "None"){
        p <-  p + geom_density(aes(x = Result), size = 1.0)
      } else{
        p <-  p + geom_density(aes_string(x = "Result", color = input$color),
                               alpha = 0.1, size =1.0)
      }
    # Box Plot
    } else if(input$plot_type == "Box Plot"){
      if(input$color == "None"){
        p <-   p + geom_boxplot(aes(x = Parameter, y = Result),
                                alpha = 0.5, size =1.0, width = 0.8, position = position_dodge(1)) +
          coord_flip()
      } else{
        p <- p + geom_boxplot(aes_string(x = "Parameter", y = "Result", fill = input$color, color = input$color),
                              alpha = 0.5, size =1.0, width = 0.8, position = position_dodge(1)) +
          coord_flip()
      }
    }

    # Facet Wrap
    if(input$facet != "None"){
      p <- p + facet_wrap(as.formula(paste("~", input$facet)))
    }

    p <- p + xlab(paste(input$param, " (", Text_Units_X(),")", sep= ""))

    p

  })


  Text_Units_X <- reactive({Df1() %>% filter(Parameter == input$param) %>% .$Units %>% unique() %>% paste(collapse = ", ")})



  ################ Texts ###################################


  # Text - Plot - Zero data Selected for Plot
  output$text_box_plot <- renderText({
    req(input$plot_type == "Box Plot") # See General Note 1
    "Caution should be taken when there is a small number of observations represented by a box plot"
  })



} # end Server Function


