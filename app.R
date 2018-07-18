#__________________________________________________________________________________________________________
#     Title: app.R
#     Type: Master file for DCR Shiny App
#     Description: This Shiny App contains the "master" script for the app. The app contains a ui and server component
#           and sources R scripts from the App folder
#     Written by: Nick Zinck and Dan Crocker, Spring 2017 -  Spring 2018
#__________________________________________________________________________________________________________

### Notes:
###   1_
###
### To-Do List:
###   1_

### Load Libraries and Script (Sources, Modules, and Functions) ####

 ipak <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg))
     install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/", quiet = T, verbose = F)
   sapply(pkg, require, character.only = TRUE)
 }
### Package List ####
### NOTE - Shiny must be installed and loaded in the LaunchAppGitHub.R script - any other packages requred should be listed below

packages <- c("shiny","shinyjs", "shinyFiles","rmarkdown", "knitr", "tidyverse", "lubridate", "plotly", "leaflet", "RColorBrewer", "devtools",
              "DT", "akima", "odbc", "DBI", "scales", "stringr", "cowplot", "shinythemes","rgdal", "reshape2", "dataRetrieval", "pryr", "broom",
              "ggthemes")
ipak(packages) ### Need to add "rcmodel"' - not in any repo...load from Mhagemann's github

if("rcmodel" %in% rownames(installed.packages()) == FALSE) {
  print("rcmodel package not installed, installing now")
  install_github("markwh/rcmodel")
}
library(rcmodel)

### Specify User information ####
user <-  Sys.getenv("USERNAME")
  
  userdata <- readxl::read_xlsx(path = config[17])
    ### Directory with saved .rds files
    if (user == userdata$Username[7]) {
      datadir <- config[14]
      df_users <- readRDS(paste0(config[1],"/df_users.rds"))
    } else {
      datadir <- config[1]
    }
  if(user %in% userdata$Username){
    username <- paste(userdata$FirstName[userdata$Username %in% user],userdata$LastName[userdata$Username %in% user],sep = " ")
    useremail <- userdata$Email[userdata$Username %in% user]
    userlocation <- userdata$Location[userdata$Username %in% user]
  } else {
    username <- "non-DCR"
    useremail <- "other"
    userlocation <- "non-DCR"
  }  
### Tab Default ####
  if(userlocation == "Quabbin"){
    tab_selected = "Quabbin"
  } else {
    tab_selected = "Wachusett"
  }
### Load rds files ####
  
    ### Make a list of all the .rds files using full path
    rds_files <- list.files(datadir,full.names = TRUE ,pattern = "\\.rds$")

    ### create an object that contains all of the rds files
    data <- lapply(rds_files, readRDS)

    ### Make a list of the df names by eliminating extension from files
    df_names <- gsub(".rds", "", list.files(datadir, pattern = "\\.rds$"))

    # name each df in the data object appropriately
    names(data) <- df_names
    ### Extract each element of the data object into the global environment
    list2env(data ,.GlobalEnv)
    ### Remove data
    rm(data)
    
### SOURCE OTHER DATA ####    
gam_models_wach <- readxl::read_xlsx(path = paste0(datadir,"/GAM_loading_models_wach.xlsx"))
### SOURCE MODULES ####

### Filter - Filter Related Modules
source("modules/filter/filter_wq.R")
source("modules/filter/filter_flow.R")
### source("modules/filter/filter_precip.R")
source("modules/filter/site_checkbox.R")
source("modules/filter/station_level_checkbox.R")
source("modules/filter/param_select.R")
source("modules/filter/param_checkbox.R") # Eventually delete
source("modules/filter/date_select.R")
source("modules/filter/checkbox_select_all.R")
source("modules/filter/select_select_all.R")

### Plots - Modules that generate plots
source("modules/plots/plot_time_wq.R")
source("modules/plots/plot_time_depth_wq.R") ### Merge with Plot_Time_wq (with differeing facetting schemes)
source("modules/plots/plot_corr_wq.R")
source("modules/plots/plot_corr_depth_wq.R") ### Merge with Plot_Corr_wq (with differeing facetting schemes)
source("modules/plots/plot_corr_matrix_wq.R")
source("modules/plots/plot_profline_custom.R") ### look at Dan's script - faceting - Bring in actual function?
source("modules/plots/distribution_wq.R")
source("modules/plots/profile_heatmap.R")
source("modules/plots/profile_line.R")
source("modules/plots/phyto.R")
source("modules/plots/plot_flow.R")

### Plot Options - Modules containing plot options
source("modules/plot_options/plot_theme_and_hlines.R")
source("modules/plot_options/plot_text_and_vlines_time.R")
source("modules/plot_options/plot_text_and_vlines_corr.R")
source("modules/plot_options/plot_title_and_labels.R")
source("modules/plot_options/plot_save.R")

### Statistics - Modules that generate Statistics
  ### Make into One file
source("modules/stats/stats_summary.R")
source("modules/stats/stats_time_depth_wq.R") # Merge into stats_sumamry
source("modules/stats/profile_table_stats.R") # Merge into stats_summary
### Add Hydro Stats here?
    
### Modeling
source("modules/modeling/loading_rcmodel.R")      
source("functions/RUNRCMODEL.R")   
source("functions/Prep_rcmodelData.R")   

### Metadata - Modules for Metadata
source("modules/metadata/metadata.R") # Seperate into multiple Modules

### Maps - Modules that use Leaflet Map generation
source("modules/maps/home.R")
source("modules/maps/geospatial_plot.R")
source("modules/maps/site_map.R")
source("modules/maps/site_map_single.R")

### Reports - Modules for report and document generation
source("modules/reports/report_AWQ.R")
source("modules/reports/report_MWQ.R")
source("modules/reports/report_custom.R")

### Load Functions

source("functions/stat_functions.R")
source("functions/phyto_plots.R")
source("functions/flow_plots.R")


    
### UI  ####
### font-family: 'Lobster', cursive;
ui <- tagList(
  ### Creates padding at top for navBar space due to "fixed-top" position
  tags$style(type='text/css',
             'body {padding-top: 70px;}',
             'h2 {
               font-family: "Arial Black";
               font-weight: 500;
               line-height: 1.1;
               color: #0C4B91;
             }'
             ),
  ### Use ShinyJS for reset button in Filter Tab
  useShinyjs(),

  ### Create the Top Navigation Bar as well as define aesthetics
  navbarPage(NULL, position = "fixed-top", inverse = TRUE, collapsible = TRUE, theme = shinytheme("cerulean"), windowTitle = "WAVE",
             footer = tagList(hr(),
               column(4,strong(paste("Data last updated:", last_update)),br()),
               column(8,tags$div(tags$em("Created by Nick Zinck, University of Massachusetts; and Dan Crocker,
                                         Massachusetts Department of Conservation and Recreation"), align = "right"), br())
                ),

  ### HOME PAGE  ####

  tabPanel("Home",
         fluidRow(
           column(3, imageOutput("dcr_image", height = 80), align = "left"),
           column(6, imageOutput("wave_image1", height = 80), align = "center"),
           column(3, imageOutput("umass_image", height = 80), align = "right")
         ),
         HOME_UI("home")
  ),

  ### TRIBUTARY ####

  tabPanel("Tributary",
    ### Title
    fluidRow(
      column(3, imageOutput("wave_image3", height = 50), align = "left"),
      column(6, h2("Tributary Water Quality Data", align = "center")),
      column(3, imageOutput("DCR_BlueLeaf1", height = 50), align = "right")
    ),
    ### Qb Tribs ####
    tabsetPanel(
      tabPanel("Quabbin",
        navlistPanel(widths = c(2, 10),
            tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_trib_quab_filter")),
            tabPanel("--- Plots", icon = icon("line-chart"),
              br(),
              wellPanel(em('Plots use data from the "Select / Filter Data" tab.Each plot may have additional selections, filters, and options.')),
              tabsetPanel(
                tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_trib_quab_plot_time")),
                tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_trib_quab_plot_corr")),
                tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_trib_quab_plot_dist"))
              )
            ),
            tabPanel("--- Statistics", icon = icon("calculator"),
            br(),
              wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
              tabsetPanel(
                tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_trib_quab_stat_sum")),
                tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_trib_quab_stat_cormat"))
              )
            ),
            tabPanel("Geospatial", icon = icon("map-marker"), MAP_PLOT_UI("mod_trib_quab_map", df = df_trib_quab)),
            tabPanel("Metadata", icon = icon("table"), METADATA_UI("mod_trib_quab_meta"))
        ) ### end navlist panel
      ),
      ### Ware Tribs ####
      tabPanel("Ware",
        navlistPanel(widths = c(2, 10),
          tabPanel("Select / Filter Data", icon = icon("filter"), FILTER_WQ_UI("mod_trib_ware_filter")),
          tabPanel("--- Plots", icon = icon("line-chart"),
            br(),
            wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
              tabsetPanel(
                tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_trib_ware_plot_time")),
                tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_trib_ware_plot_corr")),
                tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_trib_ware_plot_dist"))
              )
          ),
          tabPanel("--- Statistics", icon = icon("calculator"),
            br(),
            wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
            tabsetPanel(
              tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_trib_ware_stat_sum")),
              tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
              tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_trib_ware_stat_cormat"))
            )
          ),
          tabPanel("Geospatial", icon = icon("map-marker"), MAP_PLOT_UI("mod_trib_ware_map", df = df_trib_ware)),
          tabPanel("Metadata", icon = icon("table"), fluidRow(h5("See Quabbin Tab. Can add data here in future")))
        ) ### end navlist panel
      ),
      ### Wach Tribs ####
      tabPanel("Wachusett",
        navlistPanel(widths = c(2, 10),
          tabPanel("Select / Filter Data", icon = icon("filter"), FILTER_WQ_UI("mod_trib_wach_filter")),
          tabPanel("--- Plots", icon = icon("line-chart"),
            br(),
            wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
            tabsetPanel(
              tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_trib_wach_plot_time")),
              tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_trib_wach_plot_corr")),
              tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_trib_wach_plot_dist"))
            )
          ),
          tabPanel("--- Statistics", icon = icon("calculator"),
            br(),
            wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
            tabsetPanel(
              tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_trib_wach_stat_sum")),
              tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
              tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_trib_wach_stat_cormat"))
            )
          ),
          tabPanel("Geospatial", icon = icon("map-marker"), MAP_PLOT_UI("mod_trib_wach_map", df = df_trib_wach)),
          tabPanel("Metadata", icon = icon("table"), METADATA_UI("mod_trib_wach_meta"))
        ) ### end navlist panel
      ),
      selected = tab_selected
    )
),  ### end Tributary Tabpanel (page)

### RESERVOIR ####

tabPanel("Reservoir",

   ###Title
   fluidRow(
            column(3, imageOutput("wave_image4", height = 50), align = "left"),
            column(6, h2("Reservoir Water Quality Data", align = "center")),
            column(3, imageOutput("DCR_BlueLeaf2", height = 50), align = "right")
   ),
   ### Qb res ####
   tabsetPanel(
     tabPanel("Quabbin",
              navlistPanel(widths = c(2, 10),
                           "Chemical",
                           tabPanel("Select / Filter data", icon=icon("filter"), FILTER_WQ_UI("mod_chem_quab_filter")),
                           tabPanel("--- Plots", icon = icon("line-chart"),
                                    br(), wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_chem_quab_plot_time")),
                                      tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_chem_quab_plot_corr")),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_chem_quab_plot_dist"))
                                    )
                           ),
                           tabPanel("--- Statistics", icon = icon("calculator"),
                                    br(), wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_chem_quab_stat_sum")),
                                      tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                      tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_chem_quab_stat_cormat"))
                                    )
                           ),
                           tabPanel("Metadata", icon = icon("table"), METADATA_UI("mod_chem_quab_meta")),
                           "Profile",
                           tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_prof_quab_filter")),
                           tabPanel("--- Plots", icon = icon("line-chart"),
                                    br(), wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Heat Map", PROF_HEATMAP_UI("mod_prof_quab_heat")),
                                      tabPanel("Scatter Plot", PLOT_TIME_WQ_UI("mod_prof_quab_plot_time")),
                                      tabPanel("Line Plot", PROF_LINE_UI("mod_prof_quab_line", df_prof_quab)),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_prof_quab_plot_dist"))
                                    )
                           ),
                           tabPanel("Table and Summary", PROF_TABLE_STAT_UI("mod_prof_quab_sum", df_prof_quab)),
                           tabPanel("Metadata", icon=icon("table"), METADATA_UI("mod_prof_quab_meta"))
              ) ### end navlist panel
     ),
     ### Wach res ####
     tabPanel("Wachusett",
              navlistPanel(widths = c(2, 10),
                           "Biological",
                           tabPanel("Phytoplankton", PHYTO_UI("mod_phyto_wach_plots", df_phyto_wach)),
                           "Profile",
                           tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_prof_wach_filter")),
                           tabPanel("--- Plots", icon=icon("line-chart"),
                                    br(),
                                    wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Heat Map", PROF_HEATMAP_UI("mod_prof_wach_heat")),
                                      tabPanel("Scatter Plot", PLOT_TIME_WQ_UI("mod_prof_wach_plot_time")),
                                      tabPanel("Line Plot", PROF_LINE_UI("mod_prof_wach_line", df_prof_wach)),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_prof_wach_plot_dist"))
                                    )
                           ),
                           tabPanel("Table and Summary", PROF_TABLE_STAT_UI("mod_prof_wach_sum", df_prof_wach)),
                           tabPanel("Metadata", icon=icon("table"), METADATA_UI("mod_prof_wach_meta")),
                           "Chemical",
                           tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_chem_wach_filter")),
                           tabPanel("--- Plots", icon=icon("line-chart"),
                                    br(),wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_chem_wach_plot_time")),
                                      tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_chem_wach_plot_corr")),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_chem_wach_plot_dist"))
                                    )
                           ),
                           tabPanel("--- Statistics", icon=icon("calculator"),
                                    br(),wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_chem_wach_stat_sum")),
                                      tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                      tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_chem_wach_stat_cormat"))
                                    )
                           ),
                           tabPanel("Metadata", icon=icon("table"), METADATA_UI("mod_chem_wach_meta")),
                           "Bacteria",
                           tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_bact_wach_filter")),
                           tabPanel("--- Plots", icon=icon("line-chart"),
                                    br(),wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Time-Series Scatter", PLOT_TIME_WQ_UI("mod_bact_wach_plot_time")),
                                      tabPanel("Correlation Scatter", PLOT_CORR_WQ_UI("mod_bact_wach_plot_corr")),
                                      tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_bact_wach_plot_dist"))
                                    )
                           ),
                           tabPanel("--- Statistics", icon=icon("calculator"),
                                    br(),wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                                    tabsetPanel(
                                      tabPanel("Summary Statistics", STAT_TIME_WQ_UI("mod_bact_wach_stat_sum")),
                                      tabPanel("Temporal Statistics", fluidRow(h5("Mann-Kendall Stats to come"))),
                                      tabPanel("Correlation Matrix", PLOT_CORR_MATRIX_WQ_UI("mod_bact_wach_stat_cormat"))
                                    )
                           ),
                           tabPanel("Geospatial", icon=icon("map-marker"), MAP_PLOT_UI("mod_bact_wach_map", df = df_bact_wach)),
                           tabPanel("Metadata", icon=icon("table"), METADATA_UI("mod_bact_wach_meta"))
            ) ### end navlist panel
     ), ### End TabPanel Watersheds
     selected = tab_selected
   )
 ),  ### end Tabpanel (page)

### HYDRO-MET-MODELING ####

  tabPanel("Hydro/Met",

    ### Header
    fluidRow(
      column(3, imageOutput("wave_image6", height = 50), align = "left"),
      column(6, h2("Hydrology and Meteorology Data", align = "center")),
      column(3, imageOutput("DCR_BlueLeaf3", height = 50), align = "right")
    ),
    ### Qb Hydro ####
    tabsetPanel(
      tabPanel("Quabbin",
          navlistPanel(widths = c(2, 10),
            "Streamflow",
            tabPanel("Select / Filter Data", icon = icon("filter"), FILTER_WQ_UI("mod_flow_quab_filter")),
            tabPanel("--- Plots/Ratings", icon = icon("line-chart"),
                     br(),
                     wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                     tabsetPanel(
                       # tabPanel("Hydrographs", PLOT_FLOW_UI("mod_flow_quab_hydrographs")),
                       # tabPanel("Flow Duration", PLOT_CORR_WQ_UI("mod_flow_quab_flowdur")),
                       # tabPanel("Boxplots", PLOT_CORR_WQ_UI("mod_flow_quab_boxplots")),
                       # tabPanel("Ratings", PLOT_CORR_WQ_UI("mod_flow_quab_ratings"))
                     )
            ),
            tabPanel("--- Streamflow Stats", icon = icon("calculator"),
                     br(),
                     wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                     tabsetPanel(
                       tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_flow_quab_stat_sum")),
                       tabPanel("Historical Statistics", PLOT_CORR_MATRIX_WQ_UI("mod_flow_quab_stat_hist"))
                     )
            ),
            tabPanel("--- Modeling", icon = icon("calculator"),
                     br(),
                     wellPanel(em('Run Tributary Loading Models')),
                     tabsetPanel(
                       # tabPanel("flux", FLUX_WQ_UI("mod_trib_loads_flux")),
                       # tabPanel("rcmodel", LOADING_RCMODEL_UI("mod_quab_rcmodel"))
                       # tabPanel("loadflex", LOADFLEX_WQ_UI("mod_trib_loads_loadflex"))
                     )
            ),
            # tabPanel("Ratings", icon = icon("calculator"),
            #          tabsetPanel(
            #            tabPanel("Rating Curves", STAT_TIME_DEPTH_WQ_UI("mod_flow_quab_ratcurve")),
            #            tabPanel("Rating Tables", PLOT_CORR_MATRIX_WQ_UI("mod_chem_quab_rattable"))
            #          )
            # ),
            "Precipitation",
            tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_precip_quab_filter")),
            tabPanel("--- Plots", icon = icon("line-chart"),
                     br(),
                     wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                     tabsetPanel(
                     #   tabPanel("Current Conditions", PROF_HEATMAP_UI("mod_precip_quab_current")),
                     #   tabPanel("Historical Precip", PROF_LINE_UI("mod_precip_quab_hist", df_prof_quab)),
                     #   tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_precip_quab_plot_dist"))
                     )
            ),
            tabPanel("--- Stats/Tables", icon = icon("calculator"),
                     br(),
                      wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                      tabsetPanel(
                        # tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_precip_quab_stat_sum")),
                        # tabPanel("Historical Statistics", PLOT_CORR_MATRIX_WQ_UI("mod_precip_quab_stat_hist"))
                      )
            ),
            "Water Supply",
            tabPanel("Reservoirs/Transfers", icon=icon("calculator"),
                     br(),
                     wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                     tabsetPanel(
                       # tabPanel("Flows/Transfers", PLOT_TIME_WQ_UI("mod_ws_quab_flowtrans")),
                       # tabPanel("Historical Elevation/Storage ", PLOT_CORR_WQ_UI("mod_ws_quab_elev_stor"))
                     )
            ),
            tabPanel("Snowpack", icon = icon("calculator"),
                     tabsetPanel(
                       # tabPanel("Map Overview", icon=icon("map-marker"), FILTER_WQ_UI("mod_snow_quab_map")),
                       # tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_snow_quab_stat_sum"))
                     )
            ),
            tabPanel("Groundwater", icon=icon("calculator"),
                     tabsetPanel(
                       # tabPanel("Map Overview", icon=icon("map-marker"), FILTER_WQ_UI("mod_gw_quab_map")),
                       # tabPanel("Plots", STAT_TIME_DEPTH_WQ_UI("mod_gw_quab_plots")),
                       # tabPanel("Stats/Tables", STAT_TIME_DEPTH_WQ_UI("mod_gw_quab_stats"))
                     )
            )
          ) ### end navlist panel
      ), ### End Tab Panel Quabbin
      ### Wach Hydro ####
      tabPanel("Wachusett",
          navlistPanel(widths = c(2, 10),
              "Streamflow",
              tabPanel("Select / Filter Data", icon = icon("filter"), FILTER_FLOW_UI("mod_flow_wach_filter")),
              tabPanel("--- Plots/Ratings", icon = icon("line-chart"),
                       br(),
                       wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                       tabsetPanel(
                         tabPanel("Hydrographs", PLOT_FLOW_UI("mod_flow_wach_hydrograph"))
                         # tabPanel("Flow Duration", PLOT_DURATION_UI("mod_flow_wach_flowdur")),
                         # tabPanel("Boxplots", PLOT_BOX_UI("mod_flow_wach_boxplots")),
                         # tabPanel("Ratings", RATINGS_UI("mod_flow_wach_ratings"))
                       )
              ),
              tabPanel("--- Streamflow Stats", icon = icon("calculator"),
                       br(),
                       wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                       tabsetPanel(
                         tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_flow_wach_stat_sum")),
                         tabPanel("Historical Statistics", PLOT_CORR_MATRIX_WQ_UI("mod_flow_wach_stat_hist"))
                       )
              ),
              tabPanel("--- Modeling", icon = icon("calculator"),
                       br(),
                       wellPanel(em('Run Tributary Loading Models')),
                       tabsetPanel(
                         # tabPanel("flux", FLUX_WQ_UI("mod_trib_loads_flux")),
                         tabPanel("rcmodel", LOADING_RCMODEL_UI("mod_wach_rcmodel"))
                         # tabPanel("loadflex", LOADFLEX_WQ_UI("mod_trib_loads_loadflex"))
                       )
              ),
              # tabPanel("Ratings", icon = icon("calculator"),
              #          tabsetPanel(
              #            tabPanel("Rating Curves", STAT_TIME_DEPTH_WQ_UI("mod_flow_wach_ratcurve")),
              #            tabPanel("Rating Tables", PLOT_CORR_MATRIX_WQ_UI("mod_chem_wach_rattable"))
              #          )
              # ),
              "Precipitation",
              tabPanel("Select / Filter Data", icon=icon("filter"), FILTER_WQ_UI("mod_precip_wach_filter")),
              tabPanel("--- Plots", icon = icon("line-chart"),
                       br(),
                       wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                       tabsetPanel(
                         # tabPanel("Current Conditions", PRECIP_CURRENT_UI("mod_precip_wach_current")),
                         # tabPanel("Historical Precip", PRECIP_HIST_UI("mod_precip_wach_hist", df_prof_wach)),
                         # tabPanel("Distribution Charts", DISTRIBUTION_WQ_UI("mod_precip_wach_plot_dist"))
                       )
              ),
              tabPanel("--- Stats/Tables", icon = icon("calculator"),
                       br(),
                       wellPanel(em('Statistics use data from the "Select / Filter Data" tab')),
                       tabsetPanel(
                         # tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_precip_wach_stat_sum")),
                         # tabPanel("Historical Statistics", PLOT_CORR_MATRIX_WQ_UI("mod_precip_wach_stat_hist"))
                       )
              ),
              "Water Supply",
              tabPanel("Reservoirs/Transfers", icon=icon("calculator"),
                       br(),
                       wellPanel(em('Plots use data from the "Select / Filter Data" tab')),
                       tabsetPanel(
                         # tabPanel("Flows/Transfers", PLOT_TIME_WQ_UI("mod_ws_wach_flowtrans")),
                         # tabPanel("Historical Elevation/Storage ", PLOT_CORR_WQ_UI("mod_ws_wach_elev_stor"))
                       )
              ),
              tabPanel("Snowpack", icon = icon("calculator"),
                       tabsetPanel(
                         # tabPanel("Map Overview", icon=icon("map-marker"), FILTER_WQ_UI("mod_snow_wach_map")),
                         # tabPanel("Summary Statistics", STAT_TIME_DEPTH_WQ_UI("mod_snow_wach_stat_sum"))
                       )
              ),
              tabPanel("Groundwater", icon=icon("calculator"),
                       tabsetPanel(
                         # tabPanel("Map Overview", icon=icon("map-marker"), FILTER_WQ_UI("mod_gw_wach_map")),
                         # tabPanel("Plots", STAT_TIME_DEPTH_WQ_UI("mod_gw_wach_plots")),
                         # tabPanel("Stats/Tables", STAT_TIME_DEPTH_WQ_UI("mod_gw_wach_stats"))
                       )
              )
          ) ### end navlist panel
      ), ### End TabPanel Watersheds
      selected = tab_selected
    )
  ),  ### end Tabpanel (page)

### FORESTRY ####

  tabPanel("Forestry",

         ### Title
         fluidRow(
                  column(3, imageOutput("wave_image7", height = 50), align = "left"),
                  column(6, h2("Forestry Data", align = "center")),
                  column(3, imageOutput("DCR_BlueLeaf4", height = 50), align = "right")
         )
  ), ### End Tab Panel Forestry

### REPORTS ####

  tabPanel("Report",

         ### Title
         fluidRow(
                  column(3, imageOutput("wave_image8", height = 50), align = "left"),
                  column(6, h2("Report Generation", align = "center")),
                  column(3, imageOutput("DCR_BlueLeaf5", height = 50), align = "right")
         ),
         navlistPanel(widths = c(2, 10),
                      "Preset Reports",
                      tabPanel("Annual WQ",
                               fluidRow(column(10, h4("Annual Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_AWQ_UI("mod_quab_awq", df_trib_quab)),
                                 tabPanel("Wachusett", REPORT_AWQ_UI("mod_wach_awq", df_trib_wach))
                               ) ### end tabset Panel
                      ), ### end tabpanel
                      tabPanel("Monthly WQ",
                               fluidRow(column(10, h4("Monthly Water Quality Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_MWQ_UI("mod_quab_mwq", df_trib_quab)),
                                 tabPanel("Wachusett", REPORT_MWQ_UI("mod_wach_mwq", df_trib_wach))
                               ) ### end tabset panel
                      ), ### end tabpanel
                      "Custom Reports",
                      tabPanel("Tributary",
                               fluidRow(column(10, h4("Tributary Custom Reports", align = "center")), column(2)),
                               tabsetPanel(
                                 tabPanel("Quabbin", REPORT_CUSTOM_UI("mod_trib_quab_rep", df_trib_quab)),
                                 tabPanel("Ware River", REPORT_CUSTOM_UI("mod_trib_ware_rep", df_trib_ware)),
                                 tabPanel("Wachusett", REPORT_CUSTOM_UI("mod_trib_wach_rep", df_trib_wach))
                               )
                      )
         ) ### end navlist
      )   ### End Tab Panel Reports
) ### end tagList

) ### END UI ####

### SERVER  ####

server <- function(input, output, session) {

### HOME ####

  callModule(HOME, "home", df_site = df_all_site)

### TRIBUTARY ####

  ### Quabbin ####

  ### Filter
  Df_Trib_Quab <- callModule(FILTER_WQ, "mod_trib_quab_filter", df = df_trib_quab, df_site = df_trib_quab_site, type = "wq")

  ### Plots
  callModule(PLOT_TIME_WQ, "mod_trib_quab_plot_time", Df = Df_Trib_Quab$Long)
  callModule(PLOT_CORR_WQ, "mod_trib_quab_plot_corr", Df = Df_Trib_Quab$Long)
  callModule(DISTRIBUTION_WQ, "mod_trib_quab_plot_dist", Df = Df_Trib_Quab$Stat)

  # Stats
  callModule(STAT_TIME_WQ, "mod_trib_quab_stat_sum", Df = Df_Trib_Quab$Stat)
  ### temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_trib_quab_stat_cormat", Df = Df_Trib_Quab$Wide)

  ### Geospatial
  callModule(MAP_PLOT, "mod_trib_quab_map", df = df_trib_quab, df_site = df_trib_quab_site)

  # MetaData
  callModule(METADATA, "mod_trib_quab_meta", df = df_trib_quab, df_site = df_trib_quab_site, df_param = df_quab_param)

  ### Ware ####

  ### Filter
  Df_Trib_Ware <- callModule(FILTER_WQ, "mod_trib_ware_filter", df = df_trib_ware, df_site = df_trib_ware_site, type = "wq")

  # Plots
  callModule(PLOT_TIME_WQ, "mod_trib_ware_plot_time", Df = Df_Trib_Ware$Long)
  callModule(PLOT_CORR_WQ, "mod_trib_ware_plot_corr", Df = Df_Trib_Ware$Long)
  callModule(DISTRIBUTION_WQ, "mod_trib_ware_plot_dist", Df = Df_Trib_Ware$Stat)

  ### Stats
  callModule(STAT_TIME_WQ, "mod_trib_ware_stat_sum", Df = Df_Trib_Ware$Stat)
  ### temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_trib_ware_stat_cormat", Df = Df_Trib_Ware$Wide)

  ### Geospatial
  callModule(MAP_PLOT, "mod_trib_ware_map", df = df_trib_ware, df_site = df_trib_ware_site)


  ### Wachusett ####

  ### Filter
  Df_Trib_Wach <- callModule(FILTER_WQ, "mod_trib_wach_filter",
                             df = df_trib_wach,
                             df_site = df_trib_wach_site,
                             df_flags = df_flags,
                             df_flag_index = df_wach_flag_index[df_wach_flag_index$Dataset == "df_trib_bact_wach",],
                             type = "wq")

  ### Plots
  callModule(PLOT_TIME_WQ, "mod_trib_wach_plot_time", Df = Df_Trib_Wach$Long)
  callModule(PLOT_CORR_WQ, "mod_trib_wach_plot_corr", Df = Df_Trib_Wach$Long)
  callModule(DISTRIBUTION_WQ, "mod_trib_wach_plot_dist", Df = Df_Trib_Wach$Stat)

  ### Stats
  callModule(STAT_TIME_WQ, "mod_trib_wach_stat_sum", Df = Df_Trib_Wach$Stat)
  # temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_trib_wach_stat_cormat", Df = Df_Trib_Wach$Wide)

  ### Geospatial
  callModule(MAP_PLOT, "mod_trib_wach_map", df = df_trib_wach, df_site = df_trib_wach_site)

  ### MetaData
  callModule(METADATA, "mod_trib_wach_meta", df = df_trib_wach, df_site = df_trib_wach_site, df_param = df_wach_param)

### RESERVOIR ####


##### Quabbin #####

  ### Chemical

  ### Filter
  Df_Chem_Quab <- callModule(FILTER_WQ, "mod_chem_quab_filter", df = df_chem_quab, df_site = df_chem_quab_site, type = "wq_depth")

  ### Plots
  callModule(PLOT_TIME_WQ, "mod_chem_quab_plot_time", Df = Df_Chem_Quab$Long) ### Update to Depth specific plot
  callModule(PLOT_CORR_WQ, "mod_chem_quab_plot_corr", Df = Df_Chem_Quab$Long) ### Update to Depth Specific Plot
  callModule(DISTRIBUTION_WQ, "mod_chem_quab_plot_dist", Df = Df_Chem_Quab$Stat) ### Depth Specific?

  ### Stats
  callModule(STAT_TIME_DEPTH_WQ, "mod_chem_quab_stat_sum", Df = Df_Chem_Quab$Stat)
  ### temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_chem_quab_stat_cormat", Df = Df_Chem_Quab$Wide)

  ### MetaData
  callModule(METADATA, "mod_chem_quab_meta", df = df_chem_quab, df_site = df_chem_quab_site, df_param = df_quab_param)

  ### Profile

  ### Filter
  Df_Prof_Quab <- callModule(FILTER_WQ, "mod_prof_quab_filter", df = df_prof_quab, df_site = df_prof_quab_site, type = "profile")

  ### Plots
  callModule(PROF_HEATMAP, "mod_prof_quab_heat", Df = Df_Prof_Quab$Long)
  callModule(PLOT_TIME_WQ, "mod_prof_quab_plot_time", Df = Df_Prof_Quab$Long) ### Update to Depth specific plot
  callModule(PROF_LINE, "mod_prof_quab_line", df = df_prof_quab)
  callModule(DISTRIBUTION_WQ, "mod_prof_quab_plot_dist", Df = Df_Prof_Quab$Stat)

  ### Table and Stats
  callModule(PROF_TABLE_STAT, "mod_prof_quab_sum", df = df_prof_quab)

  ### Change the Data Frames to Profile Data
  callModule(METADATA, "mod_prof_quab_meta", df = df_prof_quab, df_site = df_prof_quab_site, df_param = df_quab_param)



##### Wachusett #####

  ### Bacteria

  ### Filter
  Df_Bact_Wach <- callModule(FILTER_WQ, "mod_bact_wach_filter",
                             df = df_bact_wach,
                             df_site = df_bact_wach_site,
                             df_flags = df_flags,
                             df_flag_index = df_wach_flag_index[df_wach_flag_index$Dataset == "df_trib_bact_wach",],
                             type = "wq")

  ### Plots
  callModule(PLOT_TIME_WQ, "mod_bact_wach_plot_time", Df = Df_Bact_Wach$Long)
  callModule(PLOT_CORR_WQ, "mod_bact_wach_plot_corr", Df = Df_Bact_Wach$Long)
  callModule(DISTRIBUTION_WQ, "mod_bact_wach_plot_dist", Df = Df_Bact_Wach$Stat)

  ### Stats
  callModule(STAT_TIME_WQ, "mod_bact_wach_stat_sum", Df = Df_Bact_Wach$Stat)
  ### temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_bact_wach_stat_cormat", Df = Df_Bact_Wach$Wide)

  ### Geospatial
  callModule(MAP_PLOT, "mod_bact_wach_map", df = df_bact_wach, df_site = df_bact_wach_site)

  ### MetaData
  callModule(METADATA, "mod_bact_wach_meta", df = df_bact_wach, df_site = df_bact_wach_site, df_param = df_wach_param)

  ### Chemical

  ### Filter
  Df_Chem_Wach <- callModule(FILTER_WQ, "mod_chem_wach_filter",
                             df = df_chem_wach,
                             df_site = df_chem_wach_site,
                             df_flags = df_flags,
                             df_flag_index = df_wach_flag_index[df_wach_flag_index$Dataset == "df_chem_wach",],
                             type = "wq_depth")

  ### Plots
  callModule(PLOT_TIME_WQ, "mod_chem_wach_plot_time", Df = Df_Chem_Wach$Long) ### Update to Depth specific plot
  callModule(PLOT_CORR_WQ, "mod_chem_wach_plot_corr", Df = Df_Chem_Wach$Long) ### Update to Depth Specific Plot
  callModule(DISTRIBUTION_WQ, "mod_chem_wach_plot_dist", Df = Df_Chem_Wach$Stat) ### Depth Specific?

  ### Stats
  callModule(STAT_TIME_DEPTH_WQ, "mod_chem_wach_stat_sum", Df = Df_Chem_Wach$Stat)
  ### temp stats
  callModule(PLOT_CORR_MATRIX_WQ, "mod_chem_wach_stat_cormat", Df = Df_Chem_Wach$Wide)

  ### MetaData
  callModule(METADATA, "mod_chem_wach_meta", df = df_chem_wach, df_site = df_chem_wach_site, df_param = df_chem_param)

  ### Profile

  ### Filter
  Df_Prof_Wach <- callModule(FILTER_WQ, "mod_prof_wach_filter", df = df_prof_wach, df_site = df_prof_wach_site, type = "profile")

  ### Plots
  callModule(PLOT_TIME_WQ, "mod_prof_wach_plot_time", Df = Df_Prof_Wach$Long) ### Update to Depth specific plot
  callModule(PROF_HEATMAP, "mod_prof_wach_heat", Df = Df_Prof_Wach$Long)
  callModule(PROF_LINE, "mod_prof_wach_line", df = df_prof_wach)
  callModule(DISTRIBUTION_WQ, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Stat)

  ### Table and Stats
  callModule(PROF_TABLE_STAT, "mod_prof_wach_sum", df = df_prof_wach)

  ### Change the Data Frames to Profile Data
  callModule(METADATA, "mod_prof_wach_meta", df = df_prof_wach, df_site = df_prof_wach_site, df_param = df_wach_param)

  ### AquaBio

  callModule(PHYTO, "mod_phyto_wach_plots", df = df_phyto_wach)

  ### HYDRO ####
  ### Wachusett ####
  
    ### Streamflow ####
    # # Filter
    Df_Flow_Wach <- callModule(FILTER_FLOW, "mod_flow_wach_filter",
                               df = df_wach_flow,
                               df_site = df_trib_wach_site[!is.na(df_trib_wach_site$LocationFlow),],
                               df_wq = df_trib_wach,
                               df_flags = df_flags,
                               df_flag_index = df_wach_flag_index[df_wach_flag_index$Dataset == "df_hobo_wach",],
                               type = "wq"
                              )
  
  callModule(PLOT_FLOW, "mod_flow_wach_hydrograph",
             Df = Df_Flow_Wach$Long,
             df2 = df_trib_wach,
             df_site = df_trib_wach_site[!is.na(df_trib_wach_site$LocationFlow),],
             df_precip = df_wach_prcp_daily)
    
    # callModule(PLOT_DURATION, "mod_flow_wach_flowdur", Df = Df_Prof_Wach$Long)
    # callModule(PLOT_BOX, "mod_flow_wach_boxplots", Df = Df_Prof_Wach$Long)
    # callModule(FLOW_RATINGS, "mod_flow_wach_ratings", Df = Df_Prof_Wach$Long)
    # callModule(RATINGS, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  
  # ### Precip ####
  #
  # # Filter
  # Df_Precip_Wach <- callModule(FILTER_PRECIP, "mod_precip_wach_filter",
  #                             df_precip = df_wach_precip_daily,
  #                             type = "wq")
  #
  # callModule(PRECIP_MAP, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  # callModule(PRECIP, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  # callModule(PRECIP_PLOTS, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  # callModule(PRECIP_STATS, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  # ### PRECIP
  #
  # # Water Supply ####
  # # Filter
  # Df_wSupply_Wach <- callModule(FILTER_WSUPPLY, "mod_wsupply_wach_filter",
  #                              df_wsupply = df_wsupply_wach,
  #                              type = "wq")
  #
  # callModule(WSUPPLY_RESTRANS, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  # callModule(WSUPPLY_SNOW, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  # callModule(WSUPPLY_GWATER, "mod_prof_wach_plot_dist", Df = Df_Prof_Wach$Long)
  
  ### Modeling  ####
  
  callModule(LOADING_RCMODEL, "mod_wach_rcmodel",
             df_wq = df_trib_wach,
             df_flow = df_wach_flow,
             df_precip = df_wach_prcp_daily,
             df_locs = df_trib_wach_site,
             gam_models = gam_models_wach,
             df_flags = df_flags,
             df_flag_index = df_wach_flag_index[df_wach_flag_index$Dataset == "df_trib_bact_wach",])

  # callModule(LOADING_FLUX, "mod_trib_loads_flux", Args)
  # callModule(LOADING_LOADFLEX, "mod_trib_loads_loadflex", Args)

# Reports

  callModule(REPORT_AWQ, "mod_quab_awq", df_trib = df_trib_quab, df_chem = df_chem_quab, df_prof = df_prof_quab, df_site = df_trib_quab_site)
  callModule(REPORT_AWQ, "mod_wach_awq", df_trib = df_trib_wach, df_chem = df_chem_wach, df_prof = df_prof_wach, df_site = df_trib_wach_site)
  callModule(REPORT_MWQ, "mod_quab_mwq", df_trib = df_trib_quab, df_chem = df_chem_quab, df_prof = df_prof_quab, df_site = df_trib_quab_site)
  callModule(REPORT_MWQ, "mod_wach_mwq", df_trib = df_trib_wach, df_chem = df_chem_wach, df_prof = df_prof_wach, df_site = df_trib_wach_site)
  callModule(REPORT_CUSTOM, "mod_trib_quab_rep", df = df_trib_quab, df_site = df_trib_quab_site)
  callModule(REPORT_CUSTOM, "mod_trib_ware_rep", df = df_trib_ware, df_site = df_trib_ware_site)
  callModule(REPORT_CUSTOM, "mod_trib_wach_rep", df = df_trib_wach, df_site = df_trib_wach_site)

### IMAGES ####

  # DCR IMAGE
  output$dcr_image <- renderImage({
    list(src = "images/DCR.jpg",
         width= "160",
         height= "80")
  }, deleteFile = FALSE)

  # DCR BLUE LEAF IMAGE 1
  output$DCR_BlueLeaf1 <- renderImage({
    list(src = "images/DCR_BlueLeaf.jpg",
         width= "51",
         height= "50")
  }, deleteFile = FALSE)

  # DCR BLUE LEAF IMAGE 2
  output$DCR_BlueLeaf2 <- renderImage({
    list(src = "images/DCR_BlueLeaf.jpg",
         width= "51",
         height= "50")
  }, deleteFile = FALSE)

  # DCR BLUE LEAF IMAGE 3
  output$DCR_BlueLeaf3 <- renderImage({
    list(src = "images/DCR_BlueLeaf.jpg",
         width= "51",
         height= "50")
  }, deleteFile = FALSE)

  # DCR BLUE LEAF IMAGE 4
  output$DCR_BlueLeaf4 <- renderImage({
    list(src = "images/DCR_BlueLeaf.jpg",
         width= "51",
         height= "50")
  }, deleteFile = FALSE)

  # DCR BLUE LEAF IMAGE 5
  output$DCR_BlueLeaf5 <- renderImage({
    list(src = "images/DCR_BlueLeaf.jpg",
         width= "51",
         height= "50")
  }, deleteFile = FALSE)

  # UMass IMAGE
  output$umass_image <- renderImage({
    list(src = "images/UMass.png",
         width= "240",
         height= "80")
  }, deleteFile = FALSE)

  # WAVE IMAGE 1
  output$wave_image1 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "360",
         height= "80")
  }, deleteFile = FALSE)

  # WAVE IMAGE 2
  output$wave_image2 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 3
  output$wave_image3 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 4
  output$wave_image4 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 5
  output$wave_image5 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 6
  output$wave_image6 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 7
  output$wave_image7 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

  # WAVE IMAGE 8
  output$wave_image8 <- renderImage({
    list(src = "images/WAVE.jpg",
         width= "225",
         height= "50")
  }, deleteFile = FALSE)

### SESSION END ####
# Code to stop app when browser session window closes
session$onSessionEnded(function() {
      stopApp()
    })
} # END SERVER FUNCTION ####

#combines the user interface and server (it's a must)
shinyApp(ui = ui, server = server)
