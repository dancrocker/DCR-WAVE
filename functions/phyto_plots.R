##############################################################################################################################.
#     Title: phyto_plots.R
#     Type: Secondary Module for DCR Shiny App
#     Description: Time series plots for phytoplankton data
#     Written by: Dan Crocker, Fall 2017
#     Edits: JTL added header/outline and updated code to plot Quabbin data/deal with error messages, Jan 2020
##############################################################################################################################.

#### Plot Phyto Data ####

# Libraries were loaded in app.R
# Data source was loaded in app.R

#Return the dataframes in a list
# dfs <- LoadPhytoData()
# # Extract each dataframe
# phyto <- dfs[[1]]
# secchi <- dfs[[2]]
# dflocs <- dfs[[3]]
# rm(dfs)

# Create the plots

# Plot 1
# Tab Panel with plots ready to view (default current year and BN3417, CI3409) - Plots stacked vertically for the following taxa:
# Export button with each plot
# Add Phyto Thresholds
# Colors

# Total_Diatoms 128 dodgerblue
# Asterionella 430 lightskyblue
# Cyclotella 441 lightsteelblue3
# Total_Chlorophytes 472 mediumseagreen
# Total_Chrysophytes 144 gold2
# Chrysosphaerella 78 darkgoldenrod3
# Dinobryon 145 gold3
# Synura 149 goldenrod2
# Uroglenopsis 573 sandybrown
# Total_Cyanophytes 475 mediumturquoise
# Dolichospermum 518 palegreen4
# Microcystis 523 paleturquoise4
# Grand_Total 24 black
# Other (Select dropdown) 604 slategrey

#### Taxa Plots ####

taxaplot <- function(df, locs, vyear, taxa, color){
## Function Arguments:
# df <- df_phyto_quab
# vyear <- 2019 # Selection unique(phyto)
# taxa <- "test"
# locs <- c("206", "202") # Multiple selections Set as default - adjust plot title
# color <- "dodgerblue"
  ############################################.
# Plot Conditions
  # If taxa to be plotted is not in the current db, plot message, otherwise continue with plotting data
    if (! (taxa %in% unique(df[year(df$Date) == vyear, ]$Taxa))){
      p <- ggplot(df) +
        theme_void() +
        annotate("text", x = 4, y = 25, label = paste(
          "No data is available for", taxa, "in", vyear, sep = " "),
                 color = "blue", size = 5, fontface = "bold.italic")
      return(p)
    } else {
  ############################################.
# Plot Setup

   df_thresh <- df_taxa_wach %>%
    filter(!is.na(Threshold_early)) %>%
    select("Name","Threshold_early","Threshold_Tx") %>%
    dplyr::rename(Taxa = Name)
   df <- df %>%
     filter(Result != 8888, Result != 9999) %>% 
     mutate("Year" = year(Date))
   
   df$Taxa_f <-  df_taxa_wach$Frmr_name[match(df$Taxa, df_taxa_wach$Name)]
   
  plot_taxa <- c(taxa, df$Taxa_f[df$Taxa == taxa]) %>% unique()
  
  # Set name of reservoir for plot titles based on station number
  if ("BN3417" %in% unique(df$Station)) {
    res <- "Wachusett"
  } else {
    res <- "Quabbin"
  }
  
  df2 <- df[df$Taxa_f %in% plot_taxa & df$Year %in% vyear & df$Station %in% locs,]
  title <- paste0(gsub("_", " ", taxa), " at Station(s) ", str_c(locs, collapse = ", "), " in ", vyear)
  xlabel <- "Date"
  ylabel <- paste0(gsub("_", " ", taxa)," Density (ASUs/ml)")
  xmin <- as.Date(paste0(as.numeric(vyear),"-01-01"))
  xmax <- as.Date(paste0(as.numeric(vyear),"-12-31"))
  taxathreshlist <- unique(df_thresh$Taxa)

p <- ggplot(df2, aes(x = Date, y = Result)) +
  geom_point(aes(color = factor(taxa),
                 fill = factor(taxa)), shape = 23, size=4) +
  scale_fill_manual(values= c(color)) +
  scale_color_manual(values= c("blue")) +
  ggtitle(title) + xlab(xlabel) + ylab(ylabel) +
  theme_bw() +
  theme(plot.title = element_text(color= "black", face="bold", size=12, vjust = 1, hjust = 0.5),
        axis.title.x = element_text(face="bold", color="black", size=10),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10),
        axis.title.y = element_text(face="bold", color="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10),
        legend.position = "none") +
  stat_smooth(method = "loess") +
  scale_x_date(date_labels = "%b", date_breaks(width = "1 month"), limits = c(xmin,xmax), name = "Date")

  if(taxa %in% taxathreshlist) {
    trigmon <- df_thresh$Threshold_early[match(paste0(taxa), df_thresh$Taxa)]
    trigtreat <- df_thresh$Threshold_Tx[match(paste0(taxa), df_thresh$Taxa)]
  p <- p + geom_hline(yintercept = trigmon, linetype=2 ) +
    annotate("text", min(df2$Date),trigmon - (0.02 * max(df2$Result)), label = "Early Monitoring Threshold", hjust = "left") +
    geom_hline(yintercept = trigtreat, linetype=5) +
    annotate("text", min(df2$Date), trigtreat - (0.02 * max(df2$Result)), label = "Treatment Consideration Threshold", hjust = "left")
    # p <- p +
    #   draw_label("Early Monitoring Threshold", min(df2$Date),trigmon - (0.15 * max(df2$Result)), hjust = "left") +
    #   draw_label("Treatment Consideration Threshold", min(df2$Date),trigtreat - (0.15 * max(df2$Result)), hjust = "left")
  }
return(p)
}
}

#taxaplot(df, locs, vyear, taxa, color)
#######################################################.

#### Overview Plot ####

phytoplot <- function(df,locs,vyear,epi_min,epi_max,em_min,em_max) {
  
  ## Function Arguments:
  # df <- df_phyto_quab
  # df <- df_phyto_quab %>%
  #   mutate(Year = year(df$Date))
  secchi <- df_secchi_wach # Eventually this needs to be changed to a df argument with ns()
    # vyear <- 2018
  # locs <- c("202", "206")
  # epi_min <- 1
  # epi_max <- 4
  # em_min <- 5
  # em_max <- 15

  # Set name of reservoir for plot titles based on station number
  if ("BN3417" %in% unique(df$Station)) {
    res <- "Wachusett"
  } else {
    res <- "Quabbin"
  }
  
# This plot will not work for Quabbin until Totals are added to Quabbin db
# If Quabbin data is to be plotted, plot message saying plot unavailable
if ("202" %in% unique(df$Station)) {
  p <- ggplot(df) +
    theme_void() +
    annotate("text", x = 4, y = 25, label = paste(
      "This plot will not be available for Quabbin data until totals are included in the database.", sep = " "),
      color = "blue", size = 4.5, fontface = "bold.italic")
  return(p)
} else {

  ######################################.
  
# make the data subsets

# This is the Blue Line - usually 1-4 m
GTA_epi <- df[df$Taxa == "Grand Total" & year(df$Date) %in% vyear & df$Depth_m >= epi_min & df$Depth_m <= epi_max &
      df$Station %in% locs,] %>%
  filter(Result != 8888, Result != 9999) %>% 
  group_by(Date) %>%
  drop_na() %>%
  summarize(Result = max(Result)) %>%
  mutate("epi")
names(GTA_epi) <- c("date","value","dataset")
GTA_epi$dataset <- factor(GTA_epi$dataset)

# This is the black line, 6-14 m
GTA_em <- df[df$Taxa == "Grand Total" & year(df$Date) %in% vyear & df$Depth_m >= em_min & df$Depth_m <= em_max &
      df$Station %in% locs,] %>%
  filter(Result != 8888, Result != 9999) %>% 
  group_by(Date) %>%
  drop_na() %>%
  summarize(Result = max(Result)) %>%
  mutate("em")
names(GTA_em) <- c("date","value","dataset")
GTA_em$dataset <- factor(GTA_em$dataset)

# Secchi data in ft - red line
secchi_yr <- secchi[year(secchi$Date) %in% vyear & secchi$Station %in% locs, c(2,4)] %>%
  group_by(Date) %>%
  drop_na() %>%
  summarize(value = max(Depth_ft)) %>%
  mutate("secchi") %>%
  ungroup()
names(secchi_yr) <- c("date","value","dataset")
secchi_yr$dataset <- factor(secchi_yr$dataset)

## What we want the axis to show
y1lim <- max(GTA_epi$value , GTA_em$value, na.rm = TRUE)
## What we need to specify in our graph
y2lim <- max(secchi_yr$value, na.rm = TRUE)
mult <- y1lim / y2lim
xmin <- as.Date(paste0(vyear,"-01-01"))
xmax <- as.Date(paste0(vyear,"-12-31"))
title <- paste0(vyear, " Phytoplankton Monitoring at ", res, " Reservoir\n Station(s) - ", str_c(locs, collapse = ", "))
xlabel <- "Date"
ylabel <- "Phytoplankton Density (ASUs/ml)"
#Define legend labels and colors
epi_leg <- paste0("Epi. (",epi_min,"-",epi_max,"m)")
em_leg <- paste0("Epi./Meta. (",em_min,"-",em_max,"m)")
colors <- c("#0099FF", "black", "red1")

######################################.
### MAKE THE PLOTS ###
p  <- ggplot() +
  geom_point(data = GTA_epi, aes(x = date, y = value, color = epi_leg), size = 3, shape = 15) +
  geom_line(data = GTA_epi, aes(x = date, y = value, color = epi_leg), size = 1.5) +
  geom_point(data = GTA_em, aes(x = date, y = value, color = em_leg), size = 4, shape = 18) +
  geom_line(data = GTA_em, aes(x = date, y = value, color = em_leg), size = 1.5) 
  if(nrow(secchi_yr) > 0){
  p <- p + geom_point(data = secchi_yr, aes(x = date, y = value * mult, color = "Secchi (ft)"), size = 3, shape = 17) +
            geom_line(data = secchi_yr, aes(x = date, y = value * mult, color = "Secchi (ft)"), size = 1.5) +
    scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1lim),
                       sec.axis = sec_axis(~./mult, breaks = pretty_breaks(), name = "Secchi Transparency (ft)")) 
  } else {
    p <- p + scale_y_continuous(breaks = pretty_breaks(),limits = c(0,y1lim)) 
  }
  p <- p +
  scale_x_date(date_labels = "%b", date_breaks(width = "1 month"), expand = c(0,0),limits = c(xmin,xmax), name = "Date") +
  labs(y = ylabel,x = xlabel, color = "") +
  ggtitle(title) +
  scale_colour_manual(values = colors) +
  theme_bw() +
  theme(plot.title = element_text(color= "black", face="bold", size = 14, vjust = 1, hjust = 0.5),
        axis.title.x = element_text(angle = 0, face = "bold", color = "black", size = 12),
        axis.title.y = element_text(angle = 90, face = "bold", color = "black", size = 12),
        legend.position="bottom",
        legend.title=element_blank())
  return(p)
}
}

#phytoplot(data, locs,vyear,epi_min,epi_max,em_min,em_max)
######################################################################################.

#### Historical Comparison Plot ####

# To do:
# Make year grouping lines curved
# Image Export
# Table Export

historicplot <- function(df, taxa, locs, vyear, yg1min, yg1max, yg2min, yg2max, yg3min, yg3max, stat, stat1, stat2, stat3, depthmin, depthmax) {
  # #Function Arguments for data selection
  # df <- df_phyto_quab
  # taxa <- "Urosolenia" # Entire list - Alphabetical (single choice)
  # locs <- c("BN3417", "CI3409", "202", "206") # Radio buttons  - default both toggled on
  # vyear <- 2019
  # yg1min <- 2007
  # yg1max <- 2020
  # yg2min <- 2007
  # yg2max <- 2020
  # yg3min <- 2007
  # yg3max <- 2020
  # depthmin <- 1
  # depthmax <- 21
  # # Function Arguments for Plot
  # stat <- "ave_val"
  # stat1 <- "ave_val"
  # stat2 <- "ave_val"
  # stat3<- "ave_val"
###########################################.
# Plot Options
  df <- df %>%
    filter(Result != 8888, Result != 9999, !is.na(Station), !is.na(Depth_m))# %>% 
    # select(-PA)
  df$Taxa_f <-  df_taxa_wach$Frmr_name[match(df$Taxa, df_taxa_wach$Name)]
  plot_taxa <- c(taxa, df$Taxa_f[df$Taxa == taxa]) %>% unique()
  df <- df %>% 
    filter(Depth_m >= depthmin, Depth_m <= depthmax)

  # Set name of reservoir for plot titles
  if ("BN3417" %in% unique(df$Station)) {
    res <- "Wachusett"
    } else {
    res <- "Quabbin"
    }
  
  # If taxa to be plotted is not in the current db, plot message, otherwise continue with plotting data
  if (! taxa %in% df$Taxa_f) {
    p <- ggplot(df) +
      theme_void() +
      annotate("text", x = 4, y = 25, label = paste(
        "This plot is not available for", paste(plot_taxa, collapse = "/"), "with the given parameters.", sep = " "),
        color = "green", size = 4.5, fontface = "bold.italic")
    return(p)
  } else 
    {
  
  taxalabel <- paste0(gsub("_", " ", taxa))
  title <- paste0(res, " Reservoir ", taxalabel, " Density by Month")
  subtitle <- paste0("Data included only from Stations: (", str_c(locs, collapse = ", "), "); ", depthmin, " - ", depthmax, "m")
  xlabel <- "Month"
  ylabel <- paste0(gsub("_", " ", taxa)," Density (ASUs/ml)")
  xmin <- as.Date(paste0(as.numeric(vyear),"-01-01"), format = '%Y-%m-%d')
  xmax <- as.Date(paste0(as.numeric(vyear),"-12-31"), format = '%Y-%m-%d')
  df_thresh <- df_taxa_wach %>%
    filter(!is.na(Threshold_early)) %>%
    select("Name","Threshold_early","Threshold_Tx") %>%
    dplyr::rename(Taxa = Name)
  taxathreshlist <- unique(df_thresh$Taxa)
  #Define legend labels and colors
  yg0_leg <- paste0(vyear, " ", taxalabel, " Values")
  yg1_leg <- paste0("Years (",yg1min,"-",yg1max,")")
  yg2_leg <- paste0("Years (",yg2min,"-",yg2max,")")
  yg3_leg <- paste0("Years (",yg3min,"-",yg3max,")")

  colors <- c("#F49B00", "black", "#7F7F7F", "#77933C")

  # Parent data set

  df <- df[df$Taxa_f %in% plot_taxa & df$Station %in% locs & df$Depth_m >= depthmin & df$Depth_m <= depthmax,]
  df$plotdate <- NA
  df$plotdate <- as.Date(paste0(vyear,"-",month(df$Date),"-15"), format = '%Y-%m-%d')
  df$Year <- year(df$Date)
  df <- select(df, Station, Year, Date, Depth_m, Taxa, Result, plotdate)

###  Data subsets by month ###

  dfall <- df %>%
    group_by(month(Date), Year, plotdate) %>%
    drop_na() %>%
    summarize(min_val = min(Result), ave_val = mean(Result),  max_val = max(Result)) %>% 
    ungroup()

    names(dfall) <- c("month", "year", "plotdate", "min_val", "ave_val", "max_val") 

  # Comparison year set by vyear
  df_yr <-df[df$Year %in% vyear, c(3,6)] %>%
    group_by(Date) %>%
    summarize(min_val = min(Result), ave_val = mean(Result),  max_val = max(Result)) %>%
    dplyr::rename("date" = Date) %>%
    drop_na()

  # Year group 1
  df_yg1 <- dfall[dfall$year >= yg1min & dfall$year <= yg1max, ] %>%
    group_by(month, plotdate) %>%
    summarize(min_val = min(min_val), ave_val = mean(ave_val),  max_val = max(max_val)) %>%
    drop_na() %>% ungroup()
  # Year group 2
  df_yg2 <- dfall[dfall$year >= yg2min & dfall$year <= yg2max, ] %>%
    group_by(month, plotdate) %>%
    summarize(min_val = min(min_val), ave_val = mean(ave_val),  max_val = max(max_val)) %>%
    drop_na()%>% ungroup
  # Year group 3
  df_yg3 <- dfall[dfall$year >= yg3min & dfall$year <= yg3max, ] %>%
    group_by(month, plotdate) %>%
    summarize(min_val = min(min_val), ave_val = mean(ave_val),  max_val = max(max_val)) %>%
    drop_na() %>% ungroup

### MAKE THE PLOTS ###
var <- df_yr[stat] %>% unlist()
var1 <- df_yg1[stat1] %>% unlist()
var2 <- df_yg2[stat2] %>% unlist()
var3 <- df_yg3[stat3] %>% unlist()

if(dim(df_yr)[1] == 0) {
  p <- ggplot(df) +
    theme_void() +
    annotate("text", x = 4, y = 25, label = paste(
      "This plot is not available for", plot_taxa, "with the given parameters.", sep = " "),
      color = "blue", size = 4.5, fontface = "bold.italic")
  return(p)
} else if (dim(df_yg1)[1] == 0) {
    p <- ggplot(df) +
      theme_void() +
      annotate("text", x = 4, y = 25, label = paste(
        "This plot is not available for", plot_taxa, "with the given parameters.", sep = " "),
        color = "blue", size = 4.5, fontface = "bold.italic")
    return(p)
} else if(dim(df_yg2)[1] == 0) {
  p <- ggplot(df) +
    theme_void() +
    annotate("text", x = 4, y = 25, label = paste(
      "This plot is not available for", plot_taxa, "with the given parameters.", sep = " "),
      color = "blue", size = 4.5, fontface = "bold.italic")
  return(p)
} else if (dim(df_yg3)[1] == 0) {
    p <- ggplot(df) +
      theme_void() +
      annotate("text", x = 4, y = 25, label = paste(
        "This plot is not available for", plot_taxa, "with the given parameters.", sep = " "),
        color = "blue", size = 4.5, fontface = "bold.italic")
    return(p)
  } else {

  p  <- ggplot() +
    geom_line(data = df_yg3, aes(x = plotdate, y = var3, color = yg3_leg), size = 1.5, linetype = 1) +
    geom_line(data = df_yg1, aes(x = plotdate, y = var1, color = yg1_leg), size = 2, linetype = 5) +
    geom_line(data = df_yg2, aes(x = plotdate, y = var2, color = yg2_leg), size = 2, linetype = 2) +
    geom_line(data = df_yr, aes(x = date, y = var, color = yg0_leg), size = 2, linetype = 1) +
    scale_y_continuous(breaks = pretty_breaks()) +
    # scale_x_discrete(labels = c("J","F","M","A","M","J","J",",A","S","O","N","D")) +
    scale_x_date(date_labels = "%b", date_breaks(width = "1 month"), expand = c(0,0),limits = c(xmin,xmax), name = "Date") +
    labs(y = ylabel,x = xlabel, title = title, subtitle = subtitle, color = "") +
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

  if(taxa %in% taxathreshlist) {
    trigmon <- df_thresh$Threshold_early[match(paste0(taxa), df_thresh$Taxa)]
    trigtreat <- df_thresh$Threshold_Tx[match(paste0(taxa), df_thresh$Taxa)]
    p <- p + geom_hline(yintercept = trigmon, linetype=2 ) +
      annotate("text", min(df_yr$date),trigmon - (0.02 * max(var, var1, var2, var3)), label = "Early Monitoring Threshold", hjust = "left") +
      geom_hline(yintercept = trigtreat, linetype=5) +
      annotate("text", min(df_yr$date), trigtreat - (0.02 * max(var, var1, var2, var3)), label = "Treatment Consideration Threshold", hjust = "left")
  }
    return(p)
  
   }
}
}

#historicplot(df, taxa, locs, vyear, yg1min, yg1max, yg2min, yg2max, yg3min, yg3max, stat, stat1, stat2, stat3, depthmin, depthmax)

## End of Plots ####
######################################################################.
# PLOT $ - Facet Grid Plot

# grid <- ceiling(length(unique(dfall$year))^.5)
# ggplot(dfall, aes(x = factor(month), y = ave_val, color = factor(year))) +
#   facet_wrap(~year, nrow = grid, scales = "fixed") +
#   geom_point()
#   #scale_x_discrete(date_labels = "%b", date_breaks(width = "1 month"), name = "Month")
