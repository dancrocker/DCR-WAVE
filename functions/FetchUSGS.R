###   Title: filter_flow.R ####_____________________________________
#     Type: Function for DCR Shiny App
#     Description: Fetches USGS data and converts to long format
#     Written by: Dan Crocker, December 2018
#     Notes: This script is Wachusett specific, Quabbin will need a slightly different customized script
###_________________________________________________________________

# sta <- "01095434"
# pcode <- c("00060", "00065", "00095", "00045", "00010")
# mindate <- as_date("2018-09-01")
# maxdate <- as_date("2018-10-01")

FETCH_USGS <- function(sta, pcode, mindate, maxdate){

    # Function to fetch USGS IVs
    df <- readNWISuv(siteNumbers = unique(c(sta, "01095375", "01095220")),
                           parameterCd = pcode,
                           startDate = as.character(as_date(mindate)),
                           endDate = as.character(as_date(maxdate)),
                           tz="America/New_York") %>%
      renameNWISColumns() # Convenience function to rename columns
    
    ### USGS pcode, Parameter -- Return variable name ####
    # 00010 Temperature, water	--   Wtemp_Inst 
    # 00045 Precipitation	--  Precip_Inst        
    # 00060 Discharge	    -- Flow_Inst          
    # 00065 Gage height	  -- GH_Inst          
    # 00095 Specific cond at 25C	  -- SpecCond_Inst  
    # Column for date time = "dateTime"
    
    # Reformat USGS Data - Gather, Recode, Select
    ### Note - must include stillwater and Quinapoxet to get instantaneous precip 
    df <- df %>% 
      select(vars_select(names(.), ends_with("Inst"), .include = c("site_no", "dateTime"))) %>%
      gather(key = "Parameter", value = "Result", 3:ncol(.), na.rm = T) %>%
      dplyr::rename("Location" = site_no, "DateTime" = dateTime)
    
      
    df$Parameter <- recode(df$Parameter,
                            Wtemp_Inst = "Water Temperature", 
                            Precip_Inst = "Precipitation", 
                            Flow_Inst = "Discharge", 
                            GH_Inst = "Staff Gauge Height", 
                            SpecCond_Inst = "Specific Conductance") 
    
    ### Pull out precip values and make an instantaneous precip df based on average of QP and SW gauges
    precip_inst <- df[df$Parameter == "Precipitation",]
    precip_inst <- precip_inst %>% 
      group_by(DateTime,Parameter) %>% 
      summarize("Result" = mean(Result, na.rm = TRUE)) %>% 
      mutate("Location" = sta) %>%
      ungroup()
    ### Pull out precipitation values since it is now in a separate df
    df <- df[df$Parameter != "Precipitation" & df$Location == sta,]
    ### Now merge back in the average values
    df <-  bind_rows(df, precip_inst)
    
    ### Recode Location from USGS Station number to LocationLabel
    df$Location <- recode(df$Location,
                          "01095434" = "GATES BROOK 1 - MD04", 
                          "01095375" = "QUINAPOXET RIVER (CANADA MILLS) - MD69",
                          "01095220" = "STILLWATER RIVER - MUDDY POND RD - MD07")
    ### Add dfs to return list
    usgs_df <- df
    
    return(usgs_df)
}

# usgs_df <- FETCH_USGS(sta = sta, pcode = pcode, mindate = mindate, maxdate = maxdate)
# unique(df$Location)
# summary(df)
