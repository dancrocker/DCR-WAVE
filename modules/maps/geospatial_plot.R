##############################################################################################################################
#     Title: Maplot.R
#     Type: Module for DCR Shiny App
#     Description: Geospatial Plots of Tributary Data and Statistics for a Watershed
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1. If Statement makes sure that the dataframe is not empty in order for the colorpal to run.
#      The aim is to prevent potential crash. Further investigation could be: Is this neccesary?,
#      If so, better alternative? Do we need to assign an else to colorpal (in the case where no colorpal exists (the first click)).
#      Think it's only neccesary in the latter location
#   2. Factor Scheme in which is For the sizing Range of the Circles. Max Value in the determination of the Scale.
#      This allows unity of the largest circle size (corresponding to the max value of the Statistic data selected.
#      The formula also has multipliing constant "30" to make the circles a reasonable size, and adds "10" to mainly
#      make sure one cannot make the circles too small and disappear
#   3. The Base Leaflet Map contains the Map Tiles and the Boudary Lat/Long info. This Base Leaflet Map is not dependent
#      on any reactive objects or inputs (except for the map type input). Therefore the base map should not dissapear
#      and be regerated during the any change in Selected Data or Display settings except for when map type is changed.
#   4. If one changes the preffered initial map type (Tile), one needs to change it in the UI and also in the Base Leaflet
#      Map section of the Server Function. The reason why the input is not directly used in the Base Leaflet Map section
#      is becuase the Lat/Long coordinates would be reset, which is likely undesired.
#
# To-Do List:
#   2. Make a more precise sig fig method for statistic (i.e. Number of Samples should not be rounded (maybe keep all
#      non-decimal numbers))
#   3. Crashed with color numeric when selected variance

##############################################################################################################################
# User Interface
##############################################################################################################################

MAP_PLOT_UI <- function(id, df) {

ns <- NS(id)

tagList(

  # CSS for map / map legend
  tags$head(
    tags$style(type = "text/css",
               # CSS for Map height to adjust to screen height
               "#mod_trib_quab_map-map {height: calc(100vh - 235px) !important;}",
               "#mod_trib_ware_map-map {height: calc(100vh - 235px) !important;}",
               "#mod_trib_wach_map-map {height: calc(100vh - 235px) !important;}",
               "#mod_bact_wach_map-map {height: calc(100vh - 235px) !important;}",
               "#mod_chem_quab_map-map {height: calc(100vh - 235px) !important;}",
               "#mod_chem_wach_map-map {height: calc(100vh - 235px) !important;}",
               # CSS for legend
               ".leaflet .legend i{
               border-radius:50%;
               width: 10px;
               height: 10px;
               margin-top: 4px;
                }
          ") # end tags style
  ), # end tags head
  # end CSS

    sidebarLayout(position = "right",
      sidebarPanel(width = 3,
        tabsetPanel(
          # Main Panel Options
          tabPanel("Main",
                   br(), br(), # Line Breaks
                   # Parameter Selection
                   selectInput(ns("param"), "Water Quality Parameter:",
                               choices=c("-Select Parameter-", levels(factor(df$Parameter))),
                               selected = "-Select Parameter-"),
                   hr(), # Horizontal Rule/Line
                   # Statistic Selection
                   selectInput(ns("stat"), "Value Statistic:",
                               choices=c("average", "minimum", "maximum",
                                         "median", "1st quartile", "1st quartile",
                                         "variance", "stand. dev.","number of samples", "geometric mean"),
                               selected = "average"),
                   hr(),
                   # Date Selection
                   strong("Date Range:"), # bold text
                   br(), br(),
                   wellPanel(
                   # Year
                     selectInput(ns("year"), "Year:",
                                 choices = c("All Years", rev(year(seq(as.Date("1980-1-1"), Sys.Date(), "years")))),
                                 selected = "All Years"),
                     # Month
                     selectInput(ns("month"), "Month:",
                                 choices = c("All Months", month.name[1:12]),
                                 selected = "All Months")
                   ) # end Well Panel
          ),
          # Display Panel Options
          tabPanel("Display",
                   br(), br(),
                   # Map Style
                   selectInput(ns("map_type"), "Map Style:",
                               choices=c(providers$Stamen.TonerLite,
                                         providers$CartoDB.Positron,
                                         providers$Esri.NatGeoWorldMap,
                                         providers$Esri.WorldImagery),
                               selected = providers$Stamen.TonerLite), # See Note 4
                   hr(), # Horizontal Rule/Line
                   # Plot Style
                   radioButtons(ns("plot_type"), "Plot Style:",
                               choices=c("Display by Color", "Display by Size"),
                               selected = "Display by Color"),
                   br(),
                   wellPanel(
                     # Color Scheme (Dependent on which Plot Style is Selected)
                     uiOutput(ns("color_dynamic_ui")),
                     uiOutput(ns("color_static_ui")),
                     br(),
                     # Radius Slider Bar
                     sliderInput(ns("radius"), "Circle Size:",
                                 min = 0, max = 1,
                                 value=0.5),
                     # Opacity Slider Bar
                     sliderInput(ns("opacity"), "Opacity:",
                                 min = 0, max = 1,
                                 value=0.7)
                   ) # end Well Panel
          ), # end tab - Display options
          # Save Options Tab
          tabPanel("save",
            radioButtons(ns("plot_size"), "plot Size", c("small", "medium", "large")),
            downloadButton(ns('plot_save'), "Save Plot")
          ) # end tab
        ) # end tabset
      ), # end sidebar Panel
      mainPanel(width = 9,
        leafletOutput(ns("map"), height = 800)
      ) # end Main Panel
    ) # end sidebarlayout
  ) # end taglist
} # end UI function

##############################################################################################################################
# Server Function
##############################################################################################################################

# Note that Argument "df.filtered"  needs to be a reactive expression, not a resolved value.
# Thus do not use () in callModule argument for reactives
# For non reactives wrap with "reactive" to make into a reactive expression.

MAP_PLOT <- function(input, output, session, df, df_site) {

  ns <- session$ns # see General Note 1

  # Create a more condensed Site Location dataframe (with Lat,lomg,site ID)
  df_site2 <- df_site %>% select(Site, LocationLat, LocationLong)

  ### Render UIs

  # Parameter Axis Choice for Secondary Axis
  output$color_dynamic_ui <- renderUI({
    req(input$plot_type == "Display by Color")
    radioButtons(ns("color_dynamic"), "Color Scheme:",
                 choices= c("Spectral", "RdYlBu", "Greens", "Greys", "Oranges", "Blues"),
                 inline = TRUE)
  })

  outputOptions(output, "color_dynamic_ui", suspendWhenHidden = FALSE) # see Dev. Manual

  # Parameter Axis Choice for Secondary Axis
  output$color_static_ui <- renderUI({
    req(input$plot_type == "Display by Size")
    radioButtons(ns("color_static"), "Color:",
                 choices=c("black", "blue", "red"),
                 selected = "blue",
                 inline = TRUE)
  })

  outputOptions(output, "color_static_ui", suspendWhenHidden = FALSE) # see Dev. Manual


  ### Reactive Dataframe for data stats

  Df1 <- reactive({

    # Filter by Parameter and remove NAs
    df_temp <- df %>%
      filter(!is.na(Result),
             Parameter %in% input$param)

    # Filter by Date
    if(input$year != "All Years"){
      df_temp <- df_temp %>% filter(year(Date) == input$year)
    }
    if (input$month != "All Months"){
      df_temp <- df_temp %>% filter(month.name[month(Date)] == input$month)
    }

    # Group by Site and add any statistics (Make sure this matches with UI options)
    df_temp <- df_temp %>% group_by(Site) %>%
      summarise(`number of samples` = n(),
                average = mean(Result),
                minimum = min(Result, na.rm = TRUE),
                maximum = max(Result, na.rm = TRUE),
                `1st quartile` = quantile(Result, 0.25),
                median = median(Result),
                `3rd quartile` = quantile(Result, 0.75),
                variance = var(Result, na.rm = TRUE),
                `stand. dev.` = sd(Result, na.rm = TRUE),
                `geometric mean` = gm_mean(Result)) %>%
      gather(Stat, Value, -c(Site)) # Restructuring the Stat Columns into Two new Columns: "Stat" and "Value"

    # Join the two tables together mathched by site - now includes lat/long info
    df_temp <- inner_join(df_temp, df_site2, "Site") %>%
      filter(Stat %in% input$stat,
             !is.na(LocationLat),
             !is.na(LocationLong))

    # Setting a 3 digit sig fig for Statistic Values
    df_temp$Value <- signif(df_temp$Value, 3)

    # Assiging df.temp to Df1
    df_temp

  }) # end Df1


# Map - Color Pallete

  Color_Pal <- reactive({
    req(input$color_dynamic)
    # If statement to prevent potential crash, See Note 1
    if(nrow(Df1()) > 0){
      colorNumeric(palette = input$color_dynamic, range(Df1()$Value))
    }
  }) # end colorpal


# Map - Size "Pallete" - For size legend creation.

  Value_Min <- reactive({
    signif(min(Df1()$Value),3)
  })

  Value_Max <- reactive({
    signif(max(Df1()$Value),3)
  })

  # Create 8 circles for the legend. change 8 if desired number is different
  Value_List <- reactive({
    signif(seq(Value_Min(), Value_Max(), length.out = 8), 3)
  })

  # Sizing Scheme for the Circles, See Note 2
  Value_Scale <- reactive({
    ((as.numeric(input$radius)*30)+10)/sqrt(Value_Max())
  })

  Diam_List <- reactive({
    2*Value_Scale()*sqrt(Value_List())
  })


# Circle Legend function

  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes,
                             "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
  }


# Base Leaflet Map - See Note 3

  output$map <- renderLeaflet({
    leaflet(data = df_site %>% filter(!is.na(LocationLat), !is.na(LocationLong))) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min(LocationLong, na.rm = TRUE), ~min(LocationLat, na.rm = TRUE), ~max(LocationLong, na.rm = TRUE), ~max(LocationLat, na.rm = TRUE))
  })


# Map Proxy - Add the Circle Markers and Legend to the map. This is Reactive to events, unlike the Base Map.

  observe({

    # if Data Selected is empty, do not add markers. See Note 1
    if(nrow(Df1()) > 0){

      # Color
      if(input$plot_type == "Display by Color"){

        pal <- Color_Pal()                                # load colorpal function

        leafletProxy("map", data = Df1()) %>%
          clearTiles() %>%
          addProviderTiles(input$map_type,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LocationLong,
                     lat = ~LocationLat,
                     radius = input$radius*15+5,          # user selected opacity
                     weight = .5,                         # weight of the outside circle
                     color = "black",                     # color of the outside circle
                     fillColor = ~pal(Value),             # color inside
                     fillOpacity = input$opacity,         # user selected opacity
                     label= ~as.character(Value),         # show Value when hovering
                     popup = ~Site) %>%                   # Show Site name when clicked
          clearControls() %>%
          leaflet::addLegend(position = "topright",
                    pal = pal,
                    values = ~Value,
                    title = input$param,
                    opacity = 1)
      }
      # Size
      if(input$plot_type == "Display by Size"){

        leafletProxy("map", data = Df1()) %>%
          clearTiles() %>%
          addProviderTiles(input$map_type,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LocationLong,
                           lat = ~LocationLat,
                           radius = ~Value_Scale()*sqrt(Value), # radius function of Value
                           weight = 2,                          # weight of the outside circle
                           color = input$color_static,          # Color of the outside circle
                           fillColor = input$color_static,      # User selected fill Color
                           fillOpacity = input$opacity,         # user selected opacity
                           label= ~as.character(Value),         # show Value when hovering
                           popup = ~Site) %>%                   # Show Site name when clicked
          clearControls() %>%
          addLegendCustom(colors = c(input$color_static, input$color_static, input$color_static),
                          labels = Value_List(),
                          sizes = Diam_List(),
                          opacity = .7)
      }
    # If no data, then clear the existing circleMarkers and legend
    } else {
      leafletProxy("map", data = Df1()) %>%
        clearMarkers() %>%
        clearControls()
    }
  })



} # end Server Funtion

