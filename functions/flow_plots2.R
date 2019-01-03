

  


# loc <- "QUINAPOXET RIVER (CANADA MILLS) - MD69"
# type <- "Daily" # "Instantaneous"
# xmin <- as_date("2018-03-01")
# xmax <- as_date("2018-09-01")
# y1par <- "Q_CFS_MEAN"
# y2par <- "Precipitation"
# wq_param <- "Total Phosphorus"
# df_site <- df_trib_wach_site
# y2 <- "precip" # sensor, wq, precip, none
# 
# df_daily <- df_wach_flow %>%
#   dplyr::rename(Location = "LocationLabel") %>%
#   filter(Location == loc, Parameter %in% c(y1par, y2par), between(Date,xmin,xmax)) %>%
#   mutate(DateTime = force_tz(ymd_hms(paste(as.character(Date),"12:00:00", sep = " ")),tzone = "America/New_York"))
# 
# # unique(df_wach_flow$Parameter)
# 
# precip <- df_wach_prcp_daily %>%
#     select(DATE, DailyPrcpAve) %>%
#     filter(DATE >= xmin, DATE <= xmax) %>%
#     mutate(DateTime = force_tz(ymd_hms(paste(as.character(DATE),"12:00:00", sep = " ")),tzone = "America/New_York"),
#            Parameter = "Precipitation") %>%
#     dplyr::rename("Result" = DailyPrcpAve)
# 
# 
# df_inst <- df_wach_flow_inst %>%
#   gather(key = "Parameter", value = "Result", 4:6) %>%
#   dplyr::rename("DateTime" = DateTimeET) %>%
#   mutate(Location = df_site$LocationLabel[match(Location, df_site$Site)]) %>%
#   filter(Parameter %in% c(y1par, y2par), between(DateTime,xmin,xmax), Location == loc)
# 
# dfwq <- df_trib_wach %>%
#   dplyr::rename("Location" = LocationLabel) %>%
#   mutate("DateTime" = SampleDateTime) %>%
#   filter(Location == loc, Parameter ==  wq_param)#, between(DateTime, xmin, xmax))
# # Set the df for Y1 axis
# df <- df_daily
# 
# p <- HYDRO_PLOT (df = df_daily, # df for Y1 Data
#                  daily_precip = precip, # df for precip
#                  dfwq = dfwq, # df for discrete wq data
#                  loc = loc, # Site choice
#                  df_site = df_site, # Site df
#                  type = type, # Y1 plot data - Daily or instantaneous
#                  xmin = xmin, # Min date
#                  xmax = xmax, # Max date
#                  y1par = y1par, # Parameter on Y1
#                  y2par = y2par, # Parameter on Y2
#                  df2 = precip, # df for y2 data wq, precip,
#                  y2 = y2 # y2 data option
# )
# p


HYDRO_PLOT <- function(df, daily_precip, dfwq = NULL, loc, df_site, type, xmin, xmax, y1par, y2par = NULL, df2 = NULL, y2 = NULL){
    # print(head(df))
### Format precip df based on type  ####
# if(type == "Daily"){ ### If daily then use the daily df
#   df <- df %>% 
#     mutate(DateTime = force_tz(ymd_hms(paste(as.character(Date),"12:00:00", sep = " ")),tzone = "America/New_York")) 
  
  # plot_precip2 <- plot_precip %>% 
  #   select(DATE, DailyPrcpAve) %>%
  #   filter(DATE >= xmin, DATE <= xmax) %>%
  #   mutate(DateTime = force_tz(ymd_hms(paste(as.character(DATE),"12:00:00", sep = " ")),tzone = "America/New_York"), 
  #          Parameter = "Precipitation") %>% 
  #   dplyr::rename("Result" = DailyPrcpAve)

    # Colors for Daily variables
  
    cols <- c("TEMP_C_MAX"  = "plum4",
              "TEMP_C_MEAN"  = "purple4",
              "TEMP_C_MIN"= "plum3",
              "Q_CFS_MIN" = "cadetblue2",
              "Q_CFS_MEAN" = "cornflowerblue",
              "Q_CFS_MAX" = "cyan2",
              "STAGE_FT_MIN" = "chartruse4",
              "STAGE_FT_MEAN" = "darkgreen",
              "STAGE_FT_MAX" = "olivedrab4",
              "SPCD_MIN" = "orchid1",
              "SPCD_MEAN" = "orchid4",
              "SPCD_MAX"= "palevioletred2")
    
    units_daily <- c("TEMP_C_MAX"  = "Deg-C",
                      "TEMP_C_MEAN"  = "Deg-C",
                      "TEMP_C_MIN"= "Deg-C",
                      "Q_CFS_MIN" = "CFS",
                      "Q_CFS_MEAN" = "CFS",
                      "Q_CFS_MAX" = "CFS",
                      "STAGE_FT_MIN" = "ft",
                      "STAGE_FT_MEAN" = "ft",
                      "STAGE_FT_MAX" = "ft",
                      "SPCD_MIN" = "uS/cm",
                      "SPCD_MEAN" = "uS/cm",
                      "SPCD_MAX"= "uS/cm")
      
      
      

    # 
    # } else { ### Type = Instantaneous ... Determine if location is USGS or HOBO site
    #   # print(head(plot_precip))
    #   # print(xmin)
    #   # print(xmax)
    #   plot_precip2 <- plot_precip %>% 
    #     filter(DateTime >= xmin, DateTime <= xmax) 
    #   # print(head(plot_precip))
    #   df <- df
    # } # End if
    
  ### Split out Y1 from df
  # dfy1 <- df[df$Parameter == y1param,]
  
  ### Y1 variable will always be selected from df parameters
  y1lim <- max(df$Result, na.rm = TRUE)
   
  ### Toggle df for second parameter
  # dfy2 <- switch(y2,
  #                "None" = dfy2,
  #                "precip" = plot_precip,
  #                "wq" = df_wq,
  #                "sensor" = dfy2
  # )
  
  ### If y2param is sensor/wq data, then subset by y2param - otherwise it is precip or none, no subset needed               
  # if(y2 %in% c("daily","sensor")){
  #   dfy2 <- df[df$Parameter == y2param,]
  # } 

  ### Set the max of y2 if y2param is not "None"
  # if(y2param != "None"){
  #   y2lim <- max(dfy2$Result, na.rm = TRUE)
  #   mult <- y1lim / abs(y2lim)
  #   } else {
  #   y2lim <- NULL 
  # }
    ### Set colors for parameters based on default in parameter table
    cols_wq <- df_wach_param[,7]
    names(cols_wq) <- df_wach_param[,2]
    
    ### Combine all colors for variables into 1 vector
    cols <- c(cols,cols_wq)

    ### List Parameters
    if(y2par == "None"){
      params <- y1par
      } else {  
      params <- toupper(paste0(c(y1par, " and ", y2par), collapse = ""))
      }
    
    if(type == "Daily"){
      units_y1 <- units_daily[[y1par]]
      } else {
      units_y1 <- df_wach_param$ParameterUnits[df_wach_param$ParameterName == y1par]
    }

    if(y2 == "daily"){
      units_y2 <- units_daily[[y2par]]
    } else {
      units_y2 <- df_wach_param$ParameterUnits[df_wach_param$ParameterName == y2par]
    }

    ### Plot text ####
    title <- paste0(params, " AT ", loc, collapse = "")
    subtitle <- "" 
    xlabel <- "Date"
    ylabel <- paste0(y1par, " (", units_y1, ")")
    y1col <- paste0(cols[y1par])
    y2col <- paste0(cols[y2par])
     ### PLOT ###
    ### setup plot and y1param
    p  <- ggplot() +
      geom_line(data = df, aes(x = DateTime, y = Result, linetype = Parameter), color = y1col, size = 1.5)
 
    if(y2 == "wq"){
      # print(head(df_wq))
      # print(max(df_wq$Result, na.rm = TRUE))
      y2lim <- max(dfwq$Result, na.rm = TRUE)
      mult <- y1lim / abs(y2lim)
      p <- p +
        geom_point(data = dfwq, aes(x = DateTime, y = Result * mult, shape = Parameter), color = y2col, size = 1.5) +
        scale_y_continuous(breaks = pretty_breaks(),limits = c(NA, 1.2 * y1lim),
                           sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), name = paste0(y2par, " (", units_y2, ")")))
    } 
    
    if(y2 %in% c("daily", "sensor")){
      # print(head(dfy2))
      # print(max(dfy2$Result, na.rm = TRUE))
      y2lim <- max(df2$Result, na.rm = TRUE)
      mult <- y1lim / abs(y2lim)
      p <- p +
        geom_point(data = df2, aes(x = DateTime, y = Result * mult, shape = Parameter), color = y2col, size = 1.5) +
        scale_y_continuous(breaks = pretty_breaks(),limits = c(NA, 1.2 * y1lim),
                           sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), name = paste0(y2par, " (", units_y2, ")")))
    } 
      
    if(y2 == "precip"){
      # print(head(plot_precip))
      maxprcp <- max(daily_precip$Result, na.rm = T)
      multprcp  <- y1lim/(maxprcp * 2)
      p <- p +
        geom_bar(data = daily_precip, stat = "identity", aes(x = DateTime, y = Result * multprcp, color = Parameter), alpha = 0.5, size = 1) +
        scale_y_continuous(breaks = pretty_breaks(),limits = c(0, 1.2 * y1lim),
                           sec.axis = sec_axis(~./multprcp, breaks = pretty_breaks(), name = paste0(y2par, " (", units_y2, ")")))
    }
    if(y2 == "None"){
    p <- p +
      scale_y_continuous(breaks = pretty_breaks(),limits = c(NA, 1.2 * y1lim)) 
    }
    plot <- p +
      ### Plot Details
      scale_x_datetime(
        breaks = pretty_breaks(),
        labels = date_format("%b-%d- \n %Y", tz  = "America/New_York"),
        expand = c(0, 0),
        limits = c(as.POSIXct(xmin), as.POSIXct(xmax))) +
      labs(y = ylabel, x = xlabel, title = title, subtitle = subtitle) +
      scale_color_manual(values = cols) +
      scale_linetype_manual("Parameter", values = 1) +
      scale_shape_manual("Parameter", values= 19) +
      theme_linedraw() +
      theme(plot.title = element_text(color= "black", face="bold", size=14, vjust = 1, hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank(),
            axis.title.x = element_text(angle = 0, face = "bold", color = "black"),
            axis.title.y = element_text(angle = 90, face = "bold", color = "black"))
    
    plot
    return(plot)
    }
    



    
  
  
  

  