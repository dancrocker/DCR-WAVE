##############################################################################################################################
#     Title: Prep_rcmodelData.R
#     Description: This script will prep Wachusett tributary data for use with Mark Hagemann's rcmodel
#     Written by: Dan Crocker
#     Last Update: June 2018
#
##############################################################################################################################
# library(tidyverse)
# library(lubridate)

options(scipen = 999) # Eliminate Scientific notation in numerical fields

### NOTES - locs is a list of LocationLabels; 
### pars is a list of Parameters from parameter table - aggregate params (i.e.TN can be fed to Model function
PREPRCMODELDATA <- function(df_wq, df_flow2, df_precip, df_locs, df_flag_index, 
                          locs, model_pars, rm_flags = NULL,  
                          minyear_wq = NULL, maxyear_wq = NULL, 
                          minyear_q = NULL, maxyear_q = NULL, 
                          rm_storms = NULL, rm_bdl = NULL){
### PREP RAW DATA:
all_pars <- c("Ammonia-N", "Nitrite-N", "Nitrate-N", "Total Organic Carbon", "Total Phosphorus",
              "Total Suspended Solids", "Discharge", "Total Kjeldahl Nitrogen")

### Set vector of Storm sample IDs
storms <- df_flag_index$SampleID[df_flag_index$FlagCode == 114]
# Vector of Samples below detection;
bdl <- df_flag_index$SampleID[df_flag_index$FlagCode == 100]
### Other flags:
exclude <- df_flag_index$SampleID[df_flag_index$FlagCode %in% rm_flags]

### Filter WQ Data to only USGS Locations (for now) and for only the parameters to calculate loads
df_wq <- df_wq[df_wq$LocationLabel %in% locs & df_wq$Parameter %in% all_pars,-10]

if(!is.null(minyear_wq)){
  df_wq <- df_wq %>%
    filter(year(SampleDateTime) >= minyear_wq)
}
if(!is.null(maxyear_wq)){
  df_wq <- df_wq %>%
    filter(year(SampleDateTime) <= maxyear_wq)
}
if(rm_storms){
  df_wq <- df_wq %>%
    filter(!ID %in% storms)
}
if(rm_bdl){
  df_wq <- df_wq %>%
    filter(!ID %in% bdl)
}
if(!is.null(rm_flags)){
  df_wq <- df_wq %>%
    filter(!ID %in% exclude)
}

### Change column data types to match sample data
cols <- c(2:5,7)
df_wq[cols] <- lapply(df_wq[cols], factor)

### Pull out discharge data into df
flow <- df_wq %>% 
  filter(Parameter == "Discharge") %>%
  filter(!is.na(Parameter)) 
flow$Units <- "CFS"

### Calculate TN 

df_wq <- df_wq %>% 
  select(-c(1,8,9)) %>%
  filter(Parameter!="Discharge") %>% 
  spread(Parameter, Result, fill = NA) %>% 
  mutate("Total Nitrogen" = Nitrate-N + Nitrite-N  + `Total Kjeldahl Nitrogen` - Ammonia-N, "InorgN" = Nitrate-N + Nitrite-N + Ammonia-N) %>% 
  gather(key = "Parameter", value = "Result", 5:13) %>% 
  filter(!is.na(Result), Parameter %in% model_pars)

### Set the full rawdata set for the model  ####
rawdata <- df_wq %>% 
  inner_join(flow, by = c("SampleDateTime", "LocationLabel")) %>%
  dplyr::rename(Date = Date.x, flow = Result.y, flow.units = Units.y, 
                conc = Result.x, conc.units = Units.x, variable = Parameter.x, basin = LocationLabel) %>%
  select(Date, flow, flow.units, conc, conc.units, variable, basin)

# saveRDS(object = rawdata, file = paste0(dir,"rawdata.rds")) 

###==========================###
#### PREDICTION DATA (FLOW) ####
###==========================###

# df_flow2 <- df_flow2[df_flow2$LocationLabel %in% locs & 
#                    df_flow2$Parameter == "Q_MEAN_CFS" & 
#                    year(df_flow2$Date) >= minyear_q &
#                    year(df_flow2$Date) <= maxyear_q,]

df_flow2 <- df_flow2 %>% 
  filter(LocationLabel %in% locs, Parameter == "Q_MEAN_CFS", year(Date) >= minyear_q, year(Date) <= maxyear_q) %>%
  dplyr::mutate("flow.units" = "CFS") %>% 
  select(c(3,2,6,8,7))
  
names(df_flow2) <- c("Date",	"Station", "flow", "flow.units", "LocationLabel")
# saveRDS(object = df_flow, file = paste0(dir, "df_flow.rds")) 

##########################
# Multiparameter dataset #
##########################

df_full <- left_join(df_flow2,df_precip[,-c(2:6)], by = c("Date" = "DATE")) %>%
  mutate("Date" = as.factor(Date)) %>% 
  left_join(y = rawdata, by = c("Date","LocationLabel" = "basin"))


# When sample flows are unavailable, fill flow with daily Average  ### This is an important option - possible give choice through Shiny?
df_full$flow.y[is.na(df_full$flow.y)] <- df_full$flow.x[is.na(df_full$flow.y)]
# Fix headers
df_full <- df_full %>% 
  rename(flow.units = flow.units.y, flow = flow.y, basin =  LocationLabel) %>% 
  select(-c(flow.x, flow.units.x))
# Fill the missing flow units
df_full$flow.units <- "CFS"
# Fill the missing concentration units for flow records (not sure why this is necessary, but causes error if not present)
df_full$conc.units[is.na(df_full$conc.units)] <- "mg/L"

# Round vals to 2 digits
df_full[,c(4:9)] <- round(df_full[,c(4:9)],2)
# saveRDS(object = df_full, file = paste0(dir, "df_full.rds"))

### FUNCTION RETURNS ####
dfs <- list()
  dfs[[1]] <- rawdata
  dfs[[2]] <- df_flow2
  dfs[[3]] <- df_full

return(dfs)
}# END PREP RAW DATA


### Function Arguments to run manually
# df_wq <-  df_trib_wach
# df_flow <-  df_wach_flow
# df_precip <-  df_wach_prcp_daily
# df_locs <-  df_trib_wach_site
# df_flag_index <-  df_wach_flag_index[df_wach_flag_index$Dataset == "df_hobo_wach",]
# locs <- c("GATES BROOK 1 - MD04", "QUINAPOXET RIVER (CANADA MILLS) - MD69")
# model_pars <- "Total Nitrogen"
# rm_flags <- NULL
# minyear_wq <-   2012
# maxyear_wq <-  2017
# minyear_q <-  2012
# maxyear_q <-  2017
# rm_storms <-  FALSE
# rm_bdl <-  FALSE
# 
# 
# ### RUN PREP MODEL DATA ####
# dfs <- PREPRCMODELDATA(df_wq = df_wq,
#               df_flow = df_flow,
#               df_precip = df_precip,
#               df_flag_index = df_flag_index,
#               locs =  locs,
#               model_pars = model_pars,
#               rm_flags = flags,
#               minyear_wq =  minyear_wq,
#               maxyear_wq = maxyear_wq,
#               minyear_q = minyear_q,
#               maxyear_q = maxyear_q,
#               rm_storms = rm_storms,
#               rm_bdl = rm_bdl)
# rawdata <- dfs[[1]]
# df_flow <- dfs[[2]]
# df_full <- dfs[[3]]

