##############################################################################################################################
#     Title: flow_plots.R
#     Type: Function for DCR Shiny App
#     Description: Parameter selection with Historical at end and Select All feature
#     Written by: Dan Crocker
#     Last Update: April, 2017
##############################################################################################################################

# library(dataRetrieval)
# library(lubridate)
# library(tidyverse)

# Hydrograph plot:
# Plot of daily HOBO/USGS flow data AND (optional) Instantaneous USGS flow data (using data retrieval) on Y1
#  and/or discrete sample results from selected locations
# UI Elements needed:
# Location Selection (1) Select input (not multiple)
# Y1 Parameter Selection (Multiple?)
# Dates - mindate and maxdate - default to last available 30 days. Use input range ui
# - add helper text that notes earliest and latest dates based on locs/pars selected
# UsGS IV overlay  - (radio button to add instantaneous values) if a location == a valid usgs station then button on fetches
#  Next to Radio Button for Days() number input - restrict values from 1-30
# and adds max 30 day of IV data to plot starting at mindate(). Need a note that says maximum 30 days may be fetched.
# Y2 Parameter Selection - subset df2 by input$locs and by mindate and maxdate


 # Function Arguments: (Only needed when running outside Shiny)
  # Df <- df_wach_flow
  # df2 <- df_trib_wach
  # df_site <- df_trib_wach_site
  # df_precip <- df_wach_prcp_daily
  # loc <- c("GATES BROOK 1 - MD04") # Multiple selections Set as default - adjust plot title
  # mindate <- "2018-01-01"
  # maxdate <- "2018-01-30"
  # Y1Par <- "Q_MEAN_CFS"
  # #OPTIONAL:
  # Y2Par <- "Discharge"
  # USGS_days <- 30
  # USGS_IV <- TRUE
  # PlotPrecip <- TRUE

#### START FUNCTION
HYDRO_G <- function(Df, df2 = NULL, df_site, df_precip,loc, Y1Par, # required args
                    Y2Par = NULL, USGS_days = NULL, USGS_IV = NULL, PlotPrecip = NULL){ # Optional args
  mindate <- min(Df$Date, na.rm = TRUE)
  maxdate <- max(Df$Date, na.rm = TRUE)
### USGS IV ###

  # Get list of USGS Stations
  usgs_sta <- df_site %>%
    filter(!is.na(LocationFlow), LocationFlow != "HOBO") %>%
    .$Site
  # Set variable to zero - will get overwritten if necessary
  usgsmaxflow <- 0
  # Test if USGS IVs are wanted and if site selected is a USGS station - if so then fetch the data
  if(USGS_IV == TRUE){
    if(df_site$Site[df_site$LocationLabel == loc] %in% usgs_sta){
      # Future enhancement - IN SHINY - make the USGS_IV button visible when TRUE, hide when FALSE
      # Specify the station to fetch data for
      sta <- df_site$LocationFlow[df_site$Site %in% usgs_sta & df_site$LocationLabel == loc]
      # Function to fetch USGS IVs
      fetchUSGS_IV <- function(usgs_sta, days,pcode){
        df_usgs_IV <- readNWISuv(siteNumbers = sta,
                                 parameterCd = pcode,
                                 startDate = mindate,
                                 endDate = as.Date(mindate) + USGS_days,
                                 tz="America/New_York") %>%
          renameNWISColumns() # Convenience function to rename columns
      }
      # Set pcode - could provide a user selection to get other parameters - discharge hard coded for now
      pcode <- "00060" # 60 is discharge
      df_usgs_IV <- fetchUSGS_IV(usgs_sta = sta ,days = USGS_days, pcode = pcode)
      usgsmaxflow <- max(df_usgs_IV$Flow_Inst, na.rm = TRUE)
    } else {
      print("The location selected is not a USGS station")
    }
  }

#####

# Parent data sets:
# Get filtered data frame and refine based on user input
  # Flow


  df <- Df %>%
    filter(Parameter %in% Y1Par & LocationLabel == loc) %>%
    filter(Date >= mindate, Date <= maxdate) %>%
    mutate(Date = force_tz(ymd_hms(paste(as.character(Date),"12:00:00", sep = " ")),tzone = "America/New_York"))
  # Precip
  df_precip <- df_precip %>%
    select(DATE, DailyPrcpAve) %>%
    filter(DATE >= mindate, DATE <= maxdate) %>%
    mutate(DATE = force_tz(ymd_hms(paste(as.character(DATE),"12:00:00", sep = " ")),tzone = "America/New_York"))
  # Discrete Data
  if(!is.null(df2)){
  dfY2 <- df2 %>%
    filter(LocationLabel == loc, Parameter == Y2Par, SampleDateTime >= mindate, SampleDateTime <= maxdate)
  } else {
  dfY2 <- NULL
    }
# Plot Options
  xlabel <- "Date"
  ylabel <- paste0(Y1Par)
  title <- paste0("DAILY ", Y1Par, " AT ",loc)
  subtitle <- paste0(mindate, " - ", maxdate)
  # Set up the Y1 Axis height
  y1lim <- max(df$Result, usgsmaxflow, na.rm = T)
  # Set plot colors: Order is - daily mean data, discrete data, USGS IV Data
  colors = setNames(c("blue", "firebrick4", "aquamarine4", "Purple"), c(Y1Par, Y2Par, "USGS Instantaneous Discharge (cfs)", "Daily Precip (inches)"))

  # Make the Plot
  p  <- ggplot(data = df, aes(x = Date)) + # This is the primary data (daily HOBO or USGS flow related)
    # Instantaneous USGS discharge data  (also on Y1 Axis)
    geom_line(data = df, aes_(y = df$Result, color = Y1Par), size = 1.5, linetype = 1)
   if(USGS_IV == TRUE){
    p <- p + geom_line(data = df_usgs_IV, aes(x = dateTime, y = Flow_Inst, color = "USGS Instantaneous Discharge (cfs)"), size = 1, linetype = 3)
   } # If a secondary Axis Parameter is chosen then it goes on Y2
   if(!is.null(df2)){
     if(Y2Par == "Discharge" & Y1Par %in% c("Q_MIN_CFS","Q_MEAN_CFS","Q_MAX_CFS")){
       p <- p + geom_point(data = dfY2, aes_(x = dfY2$SampleDateTime, y = dfY2$Result, color = Y2Par), size = 2) +
         scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1lim))
     } else {
       # In Shiny Reactively change PlotPrecip to False
          y2lim <- max(dfY2$Result)
          mult <- y1lim/y2lim
          PlotPrecip == FALSE
          p <- p + geom_point(data = dfY2, aes_(x = dfY2$SampleDateTime, y = dfY2$Result * mult, color = Y2Par), size = 2) +
            scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1lim),
                       sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), name = paste(Y2Par,dfY2$Units[1], sep = " ")))
     }
   }
    if(PlotPrecip == TRUE){
      maxprcp <- max(df_precip$DailyPrcpAve)
      multprcp  <- y1lim/(maxprcp * 2)
      p <- p + geom_bar(data = df_precip, stat = "identity", aes(x = df_precip$DATE, y = df_precip$DailyPrcpAve * multprcp,
                                                                 color = "Daily Precip (inches)"), alpha = 0.6) +
        scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1lim),
                       sec.axis = sec_axis(~./multprcp, breaks = pretty_breaks(), name = "Daily Precip (inches)"))
    }
    p <- p + scale_x_datetime(
      breaks = pretty_breaks(),
      labels = date_format("%b-%d- \n %Y", tz  = "America/New_York"),
      expand = c(0, 0),
      limits = c(as.POSIXct(mindate), as.POSIXct(maxdate))) +
    labs(y = ylabel, x = xlabel, title = title, subtitle = subtitle) +
    scale_colour_manual(values = colors) +
    theme_bw() +
    theme(plot.title = element_text(color= "black", face="bold", size = 14, vjust = 1, hjust = 0.5),
          plot.subtitle =  element_text(color= "black", size = 12, vjust = 1, hjust = 0.5),
          axis.title.x = element_text(angle = 0, face = "bold", color = "black", size = 12),
          axis.title.y = element_text(angle = 90, face = "bold", color = "black", size = 12),
          panel.grid.major.x = element_blank() , # remove the vertical grid lines
          panel.grid.major.y = element_line(size=.1, color="#808080"), # explicitly set the horizontal lines (or they will disappear too)
          legend.position="bottom",
          legend.title=element_blank())
  p
  } # End Function

# HYDRO_G(Df, df2, df_site,loc, mindate, maxdate, Y1Par, Y2Par, USGS_days, USGS_IV)


