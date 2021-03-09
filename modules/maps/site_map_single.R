##############################################################################################################################
#     Title: site_map_single.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Site Map for Site Options and Selected Sites
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

##############################################################################################################################
# User Interface
##############################################################################################################################

SITE_MAP_SINGLE_UI <- function(id) {

  ns <- NS(id) # see General Note 1

  tagList(
    leafletOutput(ns("map"), height = 350 )
  ) # end taglist
} # end UI function


##############################################################################################################################
# Server Function
##############################################################################################################################

SITE_MAP_SINGLE <- function(input, output, session, Site) {

  # Base Leaflet Map - See General Note 3

  output$map <- renderLeaflet({

    req(Site())

    leaflet(data = Site()) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude,
                       label= ~Location,
                       popup = ~paste("ID =", Location, "<br/>",
                                      "Description =", Location, "<br/>",
                                      "Lat = ", Latitude, "<br/>",
                                      "Long = ", Longitude, "<br/>",
                                      "Elev = ", `Elevation (ft)`,"ft")
                 )
  })


} # end Server Function
