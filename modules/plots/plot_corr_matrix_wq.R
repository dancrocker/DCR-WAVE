##############################################################################################################################
#     Title: plot_corr_matrix_wq.R
#     Type: Secondary Module for DCR Shiny App
#     Description: 
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

##############################################################################################################################
# User Interface
##############################################################################################################################

PLOT_CORR_MATRIX_WQ_UI <- function(id) {
  
  ns <- NS(id) # see General Note 1
  
  tagList(
    fluidRow(
      plotOutput(ns("plot"), height = "100%")
    )
  ) # end taglist
} # end UI function



##############################################################################################################################
# Server Function
##############################################################################################################################

PLOT_CORR_MATRIX_WQ <- function(input, output, session, Df) {
  
  ns <- session$ns # see General Note 1
  
# numeric rows (Rows with Result Data)
num_rows <- reactive({sapply(Df(), is.numeric)})

# number of numeric rows / coresponds to Parameters selected
num_rows_count <- reactive({sum(num_rows(), na.rm=TRUE)})
  
  
# Correlation Matrix
Df1 <- reactive({
  if(num_rows_count() > 1){
    Df()[, num_rows()] %>%
      cor(use = "pairwise.complete.obs") %>%
      round(2) %>%
      reorder_cormat() %>% 
      get_upper_tri() %>% 
      melt(na.rm = TRUE)
  } else{
    NULL
  }
  
})


# Plot Creation
P <- reactive({
  
  ggplot(data = Df1(), aes(Var2, Var1, fill = value))+
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.y = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    theme(legend.text = element_text(size = 12)) +
    coord_fixed() + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())

})


# output Plot
observe({
  output$plot <- renderPlot({
    P()
  }, width = 250 + 35*num_rows_count(), height = 150 + 35*num_rows_count())
})

  
} # end server function
