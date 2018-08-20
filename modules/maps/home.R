##############################################################################################################################
#     Title: Maplot.R
#     Type: Module for DCR Shiny App
#     Description: Geospatial Plots of Tributary Data and Statistics for a Watershed
#     Written by: Nick Zinck, Spring 2017
##############################################################################################################################

# Notes:
#   1.
#
# To-Do List:
#   1. Add watershed delineations
#   2. Make use of stations for nutrient data for no overlap
#   3.

##############################################################################################################################
# User Interface
##############################################################################################################################
HOME_UI <- function(id) {

ns <- NS(id)

tagList(
  fluidRow(
    tags$style(type = "text/css", "#home-map {height: calc(100vh - 235px) !important;}"),
    leafletOutput(ns("map"))
  ),
  fluidRow(
    column(6,
           strong(textOutput(ns("text_db")), align = "left")
    ),
    column(6,
           strong(textOutput(ns("text_cred")), align = "right")
    )
  )
)

}


##############################################################################################################################
# Server Function
##############################################################################################################################

HOME <- function(input, output, session, df_site) {

  df_site$LocationType[df_site$LocationType == "Nutrient"] <- "Reservoir"
  df_site$LocationType[df_site$LocationType == "Transect"] <- "Bacteria"

  # to fix duplicated Reservoir (Nutrient locations due to multiple depths)
  df_site$Site[!is.na(df_site$Station)] <- df_site$Station[!is.na(df_site$Station)]
  df_site <- df_site[!duplicated(df_site[,c("Site", "LocationLat", "LocationLong", "LocationCategory")]),]


# levels (Categories) of Colors and Legend

  map_levels <- c("Quabbin Tributary",
                  "Ware River Tributary",
                  "Wachusett Tributary",
                  "Quabbin Bacteria",
                  "Wachusett Bacteria",
                  "Quabbin Reservoir",
                  "Wachusett Reservoir")

# Create a new column in df_site for coloring and legend purposes

  df_site2 <- df_site %>%
      mutate(MapFactor = factor(paste(Watershed, LocationType), levels = map_levels))

# Color

  color_pal <- colorFactor(palette = c("firebrick",
                                       "tomato",
                                       "red2",
                                       "orange1",
                                       "orange2",
                                       "yellow1",
                                       "yellow2"),
                           domain = factor(map_levels, levels = map_levels),
                           ordered = TRUE)

# Map

  output$map <- renderLeaflet({

    pal <- color_pal
    QWW <- readOGR("gis/QuabbinWareWachusettWatersheds.shp") %>%
      spTransform(CRS("+proj=longlat +ellps=GRS80"))

    leaflet(data = df_site2) %>%
      # Set Long/Lat (not completely neccesary)
      setView(lng = -72.0589, lat = 42.43, zoom = 11) %>%
      # 
      addTiles() %>%
      # Basemap (World Imagery from ESRI)
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      # Watershed Boundary
      addPolygons(data = QWW,
                  layerId = QWW,
                  color = "white", # "#00008B",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 0.7, fillOpacity = .1,
                  fillColor = "#00008B") %>%  # ,
                  # Removed Highlighting due to BringToFront interferring with circle Markers
                  #highlightOptions = highlightOptions(color = "white", 
                                                      #weight = 2,
                                                      #bringToFront = TRUE)) %>%
      # Site Location Markers
      addCircleMarkers(
        lng = ~LocationLong, lat = ~LocationLat,
        label=~LocationLabel,
        popup = ~paste("ID =", Site, "<br/>",
                       "Description =", LocationDescription, "<br/>",
                       "Lat = ", LocationLat, "<br/>",
                       "Long = ", LocationLong, "<br/>",
                       "Elev = ", LocationElevFt, "ft"),
        color = ~pal(MapFactor),
        radius = 5,
        weight = 3,
        opacity = 0.8,
        fillOpacity = 0.4) %>%
      # Legend
      leaflet::addLegend(
                position = "bottomleft",
                values = ~MapFactor,
                pal = pal,
                opacity = 1,
                na.label = "Not Available",
                title = "")

  })


} # end Server Function
