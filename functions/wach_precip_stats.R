######################################################################################
#     Title: wach_precip_stats
#     Description: This script will calculate some stats based on daily USGS/NOAA data in the databases
#                 1) Precip Totals (Annual, Monthly, Monthly_by_Year)
#                 2) Precip Cummulative (2D Sum, 7D Sum, 14D Sum, 30D Sum)
#     Written by: Dan Crocker
#     Last Updated: August, 2018
#
#################################################################################

# Load Libraries
library(tidyverse)
library(lubridate)
library(scales)
# library(chron)
# library(hydroTSM)
library(plotly)
library(lattice)
# Precip stats function  - processes daily precip records and caches relevant stats for WAVE App

#### THIS IS TEMPORARY FOR USE OUTSIDE OF SHINY
# Read config file to access database
# config <- read.csv("//env.govt.state.ma.us/enterprise/DCR-WestBoylston-WKGRP/WatershedJAH/EQStaff/WQDatabase/R-Shared/WAVE-WIT/Configs/WAVE_WIT_Config.csv", header = TRUE)
# config <- as.character(config$CONFIG_VALUE)
# 
# rds_files <- list.files(config[1],full.names = T)
# rds_files # Take a look at the rds files:
# df_precip <- readRDS(rds_files[34]) # NOTE: This rds file is created daily at the conclusion of the NOAA data fetch
#####


# Wachusett Precip DF

PRECIP_STATS_WACH <- function(df_precip, vyear = NULL){

# Calculate some key values for displaying data
start_precip <- min(df_precip$DATE) # First precip date
end_precip <- max(df_precip$DATE) # Last precip date
jend_precip <- df_precip$jDay[df_precip$DATE == end_precip] # Last Precip Julian day
this_year <- year(end_precip) # Current Year (based on data
this_month <- month(end_precip) # Current Month of data (this will lag for 1-3 days at the end of each month)

# Calculate Month Totals, then filter out months that are missing daily records so they are not used in stats
PrcpMonthYear <- df_precip %>%
  mutate(Month = month(DATE), Year = year(DATE)) %>%
  group_by(Month, Year) %>%
  summarize(MonthPrcpTotal = round(sum(DailyPrcpAve),2), nDays = n()) %>%
  ungroup()

PrcpMonthMean <- PrcpMonthYear[PrcpMonthYear$nDays >= 28,] %>%
  group_by(Month) %>%
  summarize(MonthPrcpAve = round(mean(MonthPrcpTotal),2), nYears = n()) %>%
  ungroup()

# Calculate Annual Totals- YTCD = Year to Calendar Date for last date in record - could make the date value selectable
YTD_C_Day <- df_precip %>%
  filter(jDay <= jend_precip) %>%
  group_by(year(DATE)) %>%
  summarize(YTCD = round(sum(DailyPrcpAve), 2),nDays = n()) %>%
  rename("Year" = `year(DATE)`) %>%
  ungroup()

# More totals - YTJD = Year to Julian Date (last 365 or 366 days) This one is tricky
#- uses lag() to combine the YTCD from each year with the precip after the Julian Date from the prior year
YTD_J_Day <- df_precip %>%
  filter(jDay > jend_precip) %>%
  group_by(year(DATE)) %>%
  summarize(After_YTCD = round(sum(DailyPrcpAve), 2),nDays = n()) %>%
  rename("Year" = `year(DATE)`) %>%
  full_join(YTD_C_Day,by = "Year") %>%
  mutate("YTJD" = round(YTCD + lag(After_YTCD,1), 2)) %>%
  mutate("AnnualTotal" = round(After_YTCD + YTCD, 2)) %>%
  ungroup()

### Current Precip Overview ####

# Year to Julian Day (Last year through most recent day)
YTJD <- YTD_J_Day$YTJD[YTD_J_Day$Year == this_year]
# Year to Julian Day (Normal)
YTJDNormal <- round(mean(YTD_J_Day$YTJD, na.rm = T), 2)
# Year to Julian Day Departure
YTJD_Depart <- round(YTJD - YTJDNormal, 2)
# Year to Calendar Day (From Jan 1 current year to most recent day)
YTCD <- YTD_J_Day$YTCD[YTD_J_Day$Year == this_year]
# Year to Calendar Day (Normal)
YTCDNormal <- round(mean(YTD_J_Day$YTCD, na.rm = T), 2)
# Year to Calendar Day Departure
YTCD_Depart <- round(YTCD - YTCDNormal, 2)
# Average annual precip
PrcpAnnualAve <- round(mean(YTD_J_Day$AnnualTotal, na.rm = T), 2)
# Total precip so far this month
PrcpThisMonthSoFar <- PrcpMonthYear$MonthPrcpTotal[PrcpMonthYear$Month == this_month & PrcpMonthYear$Year == this_year]
# Normal precip for this month -
PrcpMonthNormalSoFar <- round(mean(YTD_J_Day$YTCD, na.rm = T) - sum(PrcpMonthMean$MonthPrcpAve[PrcpMonthMean$Month < this_month]), 2)
# Current monthly departure from normal
ThisMonth_Depart <- PrcpThisMonthSoFar - PrcpMonthNormalSoFar

### Most recent precip value
last1prcp <- round(df_precip$DailyPrcpAve[df_precip$DATE == end_precip],2)
last7prcp <- round(df_precip$`7dPRCP`[df_precip$DATE == end_precip],2)
last30prcp <- round(df_precip$`30dPRCP`[df_precip$DATE == end_precip],2)

lastjday <- df_precip$jDay[df_precip$DATE == end_precip]

# Need to make begin dates for Quabbin gauges too
wor_begin <- min(df_precip$DATE[!is.na(df_precip$WORCESTER)])
fitch_begin <- min(df_precip$DATE[!is.na(df_precip$FITCHBURG)])
still_begin <- min(df_precip$DATE[!is.na(df_precip$STILLWATER)])
quin_begin <- min(df_precip$DATE[!is.na(df_precip$QUINAPOXET)])
wor_end <- max(df_precip$DATE[!is.na(df_precip$WORCESTER)])
fitch_end <- max(df_precip$DATE[!is.na(df_precip$FITCHBURG)])
still_end <- max(df_precip$DATE[!is.na(df_precip$STILLWATER)])
quin_end <- max(df_precip$DATE[!is.na(df_precip$QUINAPOXET)])

# Annual Cumulative Precip Sum (calendar day)
# Group by Jday - jday total + sum of precip on all prior days of that year.

jday_sum <- df_precip %>%
  group_by(jDay) %>%
  summarize(jDay_ave_prcp = mean(DailyPrcpAve)) %>%
  mutate(jDay_YTJD = cumsum(jDay_ave_prcp[jDay_ave_prcp <= jDay]))

jday_sum_thisyear <- df_precip %>%
  filter(year(DATE) == year(Sys.Date())) %>%
  mutate(jDay_YTJD = cumsum(DailyPrcpAve[jDay <= jDay])) %>%
  left_join(jday_sum, by = "jDay")

if(is.null(vyear)){
  vyear <- year(Sys.Date())
}
jday_sum_vyear <- df_precip %>%
  filter(year(DATE) == vyear) %>%
  mutate(jDay_YTJD = cumsum(DailyPrcpAve[jDay <= jDay])) %>%
  left_join(jday_sum, by = "jDay")


# Summary Table ####
# NOAA Stations of interest:
# USW00004780 # FITCHBURG MUNICIPAL AIRPORT, MA US
# USW00094746 # WORCESTER, MA US
# USW00054756 # ORANGE MUNICIPAL AIRPORT, MA US
# USC00198793 # WARE, MA US
# USC00190562 # BELCHERTOWN, MA US


### Need to come up with way to make this fit into module - also need a note that Worcester data is available prior to 1985,
# but it is not stored locally in databases

t_gauge_summary  <- tibble("Gauge Name" = c("WORCESTER", "FITCHBURG","STILLWATER","QUINAPOXET"),
                           "Owner" = c("NOAA", "NOAA", "USGS", "USGS"),
                           "Gauge Number" = c("USW00094746","USW00004780","01095220 (MD07)","01095375 (MD69)"),
                           "Start Date" = c(wor_begin, fitch_begin, still_begin, quin_begin),
                           "End Date" = c(wor_end, fitch_end, still_end, quin_end)
                    )


t_prcp_summary <- tibble("Stat" = c(paste0("Most recent precipitation value (", end_precip, ")"),
                                    paste0("7-Day Precipitation (", end_precip - 7, " - ", end_precip, ")"),
                                    paste0("30-Day Precipitation (", end_precip - 30, " - ", end_precip, ")"),
                                    paste0("Precipitation so far this month (", floor_date(today(),"month"), " - ", end_precip,")"),
                                    paste0("Year to calendar date (",this_year, "-01-01 - ", end_precip, ")"),
                                    paste0("Year to day-of-year (",end_precip - dyears(1), " - ", end_precip, ")")),
                         "Inches" = c(last1prcp, last7prcp, last30prcp, PrcpThisMonthSoFar, YTCD,YTJD),
                         "Normal" = c(NA, NA, NA, PrcpMonthNormalSoFar, YTCDNormal, YTJDNormal),
                         "Departure" = c(NA, NA, NA, ThisMonth_Depart, YTCD_Depart, YTJD_Depart)
                         )



### FUNCTION RETURNS ####
dfs <- list()
dfs[[1]] <- PrcpMonthYear
dfs[[2]] <- PrcpMonthMean
dfs[[3]] <- YTD_J_Day
dfs[[4]] <- jday_sum_thisyear
dfs[[5]] <- jday_sum_vyear
dfs[[6]] <- t_gauge_summary
dfs[[7]] <- t_prcp_summary

return(dfs)

} # End Function

# dfs <- PRECIP_STATS(df_precip = df_wach_prcp_daily, vyear = NULL)

 # Generate Sample date vectors for precip threshold filters:
      #* Need to have an explanation on this filter saying:
      # Cummulative precipitation totals for each day include that day's precipitation as well, which could very well have fallen after
      # a sampling event. So, use this filter with caution... as it may lead to erroneous conclusions

### ANTECEDENT WET CONDITIONS ###
# # 24 hr Precip < 0.2"
# PrcpFilter1 <- PrcpDaily$DATE[PrcpDaily$`2dPRCP` < 0.2]
# # 24 hr Precip >= 0.2" but < 0.5"
# PrcpFilter2 <- PrcpDaily$DATE[PrcpDaily$`2dPRCP` >= 0.2 & PrcpDaily$`2dPRCP` < 0.5]
# # 24 hr Precip >= 0.5" < 1.0"
# PrcpFilter3 <- PrcpDaily$DATE[PrcpDaily$`2dPRCP` >= 0.5 & PrcpDaily$`2dPRCP` < 1.0]
# # 24 hr Precip >= 1.0" < 2.0"
# PrcpFilter4 <- PrcpDaily$DATE[PrcpDaily$`2dPRCP` >= 1.0 & PrcpDaily$`2dPRCP` < 2.0]
# # 24 hr Precip >= 2.0" < 3.0"
# PrcpFilter5 <- PrcpDaily$DATE[PrcpDaily$`2dPRCP` >= 2.0 & PrcpDaily$`2dPRCP` < 3.0]
# # 24 hr Precip >= 3.0"
# PrcpFilter6 <- PrcpDaily$DATE[PrcpDaily$`2dPRCP` > 3.0]
#
# ### ANTECEDENT DRY CONDITIONS ###
# # 7 day Precip <= 0.2"
# PrcpFilter7 <- PrcpDaily$DATE[PrcpDaily$`7dPRCP` <= 0.2]
# # 14 day Precip <= 0.5"
# PrcpFilter8 <- PrcpDaily$DATE[PrcpDaily$`14dPRCP` <= 0.5]
# # 30 day Precip <= 1.0"
# PrcpFilter9 <- PrcpDaily$DATE[PrcpDaily$`30dPRCP` <= 1.0]

################################################################



