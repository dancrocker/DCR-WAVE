### RC MODEL FUNCTION ####

# library(rcmodel)
# library(tidyverse)
# library(lubridate)
# library(pryr)
# library(broom)
# library(ggthemes)
# library(scales)


### Source the prep model data function:
# source("functions/Prep_rcmodelData.R")
### Get Data files ####  
# Get RDS file path
# 
# # Browse files to Load
# # RDSfiles
# # 
# # ### Set at module level depoending on location:
# # 
# # df_flag_index <- df_flag_index[df_flag_index$Dataset == "df_trib_bact_wach",]
# # df_wq <- readRDS(paste0(config[1],"/",RDSfiles[22]))
# # flow <- readRDS(paste0(config[1],"/",RDSfiles[28]))
# # df_precip <- readRDS(paste0(config[1],"/",RDSfiles[30]))
# # df_flags <- readRDS(paste0(config[1],"/",RDSfiles[9]))
# 
# ### Model run options ####
# 
# loc_choices <- sort(unique(gam_models$Location)) ### location options --- can select multiple
# 
# # Make Reactive to Location Choice
# par_choices <- sort(unique(gam_models$Parameter))
# ### flag_choices <- 
# 
# ### Set by user:
# ### Directory to save outputs (browse for directory)
# dir <- "P:/DOCUMENTS/R/MarkHagemannModel/Outputs/" ### File output directory (Note -- if NULL then write to C:/users/downloads
# ### Locations to model (will loop through each location and model parameters selected)
# locs <- loc_choices[3] 
# pars <- par_choices[1:3]
# flags <- NULL 
# ### Date Ranges ####
# #Set water quality data date range:
# minyear_wq <- 2011
# maxyear_wq <- 2018
# #Set flow data date range:
# minyear_q <- 2011
# maxyear_q <- 2018
# rm_storms <- FALSE
# rm_bdl <- FALSE ### Below detection values are as follows: TSS: 5.0, Nitrate/Nitrite/Ammonia/TP: 0.005, TOC: , TKN: 0.1 
# ### GENERATE INPUT DATA ####
# dfs <- PREPRCMODELDATA(df_wq = df_wq,
#                      flow = flow,
#                      df_precip = df_precip,
#                      df_flag_index = df_flag_index,
#                      locs =  locs,
#                      model_pars = model_pars,
#                      flags = flags,
#                      minyear_wq =  minyear_wq,
#                      maxyear_wq = maxyear_wq,
#                      minyear_q = minyear_q,
#                      maxyear_q = maxyear_q,
#                      rm_storms = rm_storms,
#                      rm_bdl = rm_bdl
#                      )
# 
# ### Extract the datasets from the list:  
# rawdata <- dfs[[1]] 
# flow <- dfs[[2]]  
# df_full <- dfs[[3]] 
# 
# loc <- locs[1] 
# var <- pars[3]


# Model run loop by location for all parameters chosen. Have separate button to knit report using outputs?
RUNRCMODEL <- function(rawdata, gam_models, flow, df_full, loc, pars, dir){
  loc <- substrRight(loc, 4)
  ht <- 4.5
  wd <- 10
  ### LOOP THROUGH EACH PARAMETER AND RUN THE THE MODEL FUNCTIONS, RETURN TO NEXT LOC
  for(i in seq_along(pars)){ 
    ### Set the Variable
    var <- pars[i]
    # Make a subdirectory for the outputs
    sdir <- paste(loc)
    newdir <- paste0(dir,"/", sdir)
    dir.create(newdir)   
    newdir <- paste0(newdir,"/",var,"/")
    dir.create(newdir)  
    
### MODEL SELECTIONS #### 
    # 
    ### 1. Rating Curve Model:
      # Predictor variable selection: by defalut flow is there, options to add:
      # Precip values (Only use 1 at a time since the values are autocorrelated)
      # Time Values (Come directly from Date (opts are season, month, doy)
      # Other???
          
    
          # Observed vs predicted concentration (plot)
          # Observed vs predicted Load (plot)
          # Evaluation Metrics (summary)
                  # NSE Concentration 
                  # NSE Load
          # Time Series (daily) of predicted concentration along with observed concentrations (plotly)
          # Time Series (daily of predicted load (plotly)
          # Loads by Month: Bar plot as well as stats (group by month) Add the monthly average line accross the bar chart,
            # Fill the space between monthly average and monthly max with green shading, and space between monthly avearge 
            # and monthly minimum with orange shading)
    
    ### 2. Loadest Model:
      ###
###    
  ### Filter and format data for rating-curve model
  rcdata <- df_full %>% filter(variable == var, Station == loc) %>% mutate("is.bdl" = FALSE) %>%
    droplevels()
  
  maxconc <-  top_n(rcdata, 1, wt = conc) %>%
    select(conc) %>%
    unlist()

  rcdata <- rcdata %>%
    filter(conc < maxconc)

  rcdata_f <- makeModelData(rcdata)
  
  # rcdata_f <- rcdata %>%
  #   mutate(addays = adry(x = DailyPrcpAve, thresh = 0.1),
  #          flow = discount(flow, d = 0.9)) %>% 
  #   makeModelData()
           ### logq = log(flow),
         ### logc = log(conc)) %>%
  ################################################################################################
              ### Make the rating-curve model ####
  ################################################################################################

  ### THIS IS WHERE THE MODEL CAN BE MODIFIED WITH DIFFERENT PREDICTOR VARIABLES
  rcdata_model <- rcgam(c ~ s(q, k = 3) + s(time, k = 5) + s(doy, bs = "cc", k = 5),
                        data = rcdata_f)
  # rcdata_model <- rcgam(c ~ s(q, k = 3) + s(time, k = 5) + s(doy, bs = 'cc', k = 5) +  s(addays, k = 4),
  # data = rcdata_f)
  
  # gam_model <- gam_models$rcgam[gam_models$Location == loc & gam_models$Parameter == var]
  # 
  # rcdata_model <- rcgam(gam_model, data = rcdata_f)

  ################################################################################################
  ################################################################################################

  preddat <- makePredData(rcdata, object = rcdata_model)
  
  ### Output Plots
  # ggTermPlot(rcdata_model, xvar = "q", data = rcdata_f)
  # ggTermPlot(rcdata_model, xvar = "doy", data = rcdata_f)
  # ggTermPlot(rcdata_model, xvar = "time", data = rcdata_f)
  
  ### Compare observed to predicted concentration:
  predconc <- predict(rcdata_model, retransform = TRUE)
  ### The %<a% is from the pyr package
  # obs_v_prd_c %<a-% {plot(rcdata$conc, predconc$fit, main = paste0(var, " Measured Concentration vs Predicted Concentration\n",loc),
  #                         xlab = "Actual Concentration (mg/L)", ylab = "Predicted Concentration (mg/L)", asp = 800/600);abline(0, 1)}
  
  obs_v_prd_c <- plot(rcdata$conc, predconc$fit, main = paste0(var, " Measured Concentration vs Predicted Concentration\n",loc), 
                          xlab = "Actual Concentration (mg/L)", ylab = "Predicted Concentration (mg/L)", asp = wd/ht);abline(0, 1)
  dev.copy(png, width = wd, height = ht, units = "in", res = 300, paste0(newdir,"obs_v_prd_c.png"))
  dev.off()

  ### Compare observed to predicted daily load:
  
  predload <- predict(rcdata_model, what = "load", retransform = TRUE, smear = TRUE, 
                  newdata = rcdata_f)
  
  measload <- rcdata$conc * rcdata$flow * 2.4466 # last part is unit conversion
  
  obs_v_prd_ld <-  plot(measload, predload$fit, main = paste0(var, " Measured Load vs Predicted Load\n",loc), 
                           xlab = "Measured Load (kg/day)", ylab = "Predicted Load (kg/day)");abline(0, 1)
  
  ggsave(paste0("obs_v_prd_ld.png"), plot = obs_v_prd_ld, device = "png", path = newdir, width = wd, height = ht, units = "in", dpi = 300)
  # dev.copy(png, width = wd, height = ht, units = "in", res = 300, paste0(newdir,"obs_v_prd_ld.png"))
  # dev.off()

  ### Various evaluation metrics are shown using the `summary` function:
  rcmodel_sum <- summary(rcdata_model)
  saveRDS(rcmodel_sum, paste0(newdir, "rcmodel_sum.rds"))
  #rcmodel_sum  <- readRDS(paste0(newdir, "rcmodel_sum.rds"))

  ### We can also look at NSE for concentration and load:
  nse_conc <- NSE(rcdata_model, what = "concentration")
  saveRDS(nse_conc, paste0(newdir, "nse_conc.rds"))
  nse_load <- NSE(rcdata_model, what = "load")
  saveRDS(nse_load, paste0(newdir, "nse_load.rds"))
  
  ###  The model can now be used to make predictions (estimates) of concentration or load.
  flow <- flow %>% 
    mutate(Date = as.Date(Date))
  
  rcPredData <- subset(flow, Station == loc)
  
  concpreds <- predict(rcdata_model, what = "concentration", newdata = rcPredData)
  
  ts_conc <-  plot(rcPredData$Date, concpreds$fit, type = "l", main = paste0(" Predicted Concentration of ", var, "\n", 
                                    rcPredData$LocationLabel[1]), xlab = "Date", ylab = "concentration (mg/L)")
  dev.copy(png, width = wd, height = ht, units = "in", res = 300, paste0(newdir,"ts_conc.png"))
  dev.off()
  
  loadpreds <- predict(rcdata_model, what = "load", newdata = rcPredData)
 
  ts_load <-  plot(rcPredData$Date, loadpreds$fit, type = "l", main = paste0(" Predicted Load of ", var, "\n", 
                                  rcPredData$LocationLabel[1]), xlab = "Date", ylab = "Load (kg/day)")
  dev.copy(png, width = wd, height = ht, units = "in", res = 300, paste0(newdir,"ts_load.png"))
  dev.off() 

 ### Total load can be computed as follows:
 # totLoad(loadpreds$fit, datetime = rcPredData$Date, load.units = "kg/day")
 
 obs_conc <- select(rcdata,c("Date","conc"))
 obs_conc$Date <- as.Date(obs_conc$Date)
   
 rcPredData <- rcPredData %>% 
   mutate("PredLoad" = loadpreds$fit, "PredConc" = concpreds$fit) %>% 
   left_join(obs_conc, by = "Date")
 
 title <- paste(var, "Concentration at \n", rcPredData$LocationLabel[1])
 pred_v_obs_c_plot <- ggplot() +
   geom_line(aes(x = Date, y = PredConc, color = "Modeled Concentration"), data = rcPredData) +
   geom_point(aes(x = Date, y = conc, color = "Measured Concentration"), data = rcPredData) +
   theme(plot.title = element_text(face="bold", size=12, vjust = 1, hjust = 0.5),
        plot.background = element_rect(fill = "transparent"),
        legend.position="bottom",
        legend.title=element_blank(),
        axis.title.y=element_text(angle=90, vjust=3, face = "bold"),
        axis.title.x=element_text(face = "bold"),
        panel.background = element_rect(colour = "black", size=1.5, fill=NA),
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank()) +
        # aspect.ratio=ht/wd) +
   scale_color_manual(name = "", values = c("Modeled Concentration" = "orange3", "Measured Concentration" = "salmon4")) +
   ggtitle(title) +
   ylab(paste0(var, " (mg/L)"))
 
ggsave(paste0("pred_v_obs_c_plot.png"), plot = pred_v_obs_c_plot, device = "png", path = newdir, width = wd, height = ht, units = "in", dpi = 300)
# saveRDS(pred_v_obs_c_plot, paste0(newdir, "pred_v_obs_c_plot.rds"))       
# pred_v_obs_c_plot

# Monthly Loads ####  
rc_month  <- rcPredData %>% 
  group_by(month(Date), year(Date)) %>%
  summarize("sum_load" = sum(PredLoad), "MonthYear" = max(floor_date(Date, unit = "month")), "days" = n())

names(rc_month) <- c("Month", "Year","Load", "PlotDate", "Days")
saveRDS(rc_month, paste0(newdir, "rc_month.rds"))
# write.csv(x = rc_month,file = paste0(newdir, "MonthLoads.csv"), row.names = FALSE)

month_load_plot <- ggplot() +
  geom_bar(aes(x= PlotDate, y = Load, fill = "Monthly Load"), data = rc_month, stat = "identity") +
  scale_fill_manual(name = "", values = c("Monthly Load" = "olivedrab")) +
  labs(x = "Month-Year", y = "Load (kg/month)") +
  ggtitle(paste0("Monthly ", var, " Loads at\n", rcPredData$LocationLabel[1] )) +
  scale_x_date(expand=c(0, 0),
                   labels=scales::date_format('%m-%Y')) +
  theme(plot.title = element_text(face="bold", size=12, vjust = 1, hjust = 0.5),
        axis.title.y=element_text(angle=90, vjust=3, face = "bold"),
        axis.title.x=element_text(face = "bold"),
        panel.background = element_rect(colour = "black", size=1.5, fill=NA),
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank(),
        panel.ontop = T,
        # aspect.ratio=ht/wd,
        legend.position = "top")
ggsave(paste0("month_load_plot.png"),plot = month_load_plot, device = "png", path = newdir, width = wd, height = ht, units = "in",dpi = 300)
# saveRDS(mnth_load_plot, paste0(newdir, "month_load_plot.rds"))
# month_load_plot

# Annual Loads ####  
rc_year  <- rcPredData %>% 
  group_by(year(Date)) %>%
  summarize("sum_load" = sum(PredLoad), "days" = n()) 
names(rc_year) <- c("Year","Load", "Days")
saveRDS(rc_year, paste0(newdir, "rc_year.rds")) 

annual_load_plot <- ggplot() +
  geom_bar(aes(x = Year, y = Load, fill = "Annual Load"), data = rc_year, stat = "identity") +
  scale_fill_manual(name = "", values = c("Annual Load" = "lightskyblue4")) +
  labs(x = "Year", y = "Load (kg/year)") +
  ggtitle(paste0("Annual ", var, " Loads at\n ", rcPredData$LocationLabel[1])) +
  theme(plot.title = element_text(face="bold", size=12, vjust = 1, hjust = 0.5),
        axis.title.y=element_text(angle=90, vjust=3, face = "bold"),
        axis.title.x=element_text(face = "bold"),
        panel.background = element_rect(colour = "black", size = 1.5, fill=NA),
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank(),
        panel.ontop = T,
        # aspect.ratio=ht/wd,
        legend.position = "top")
ggsave(paste0("annual_load_plot.png"),plot = annual_load_plot, device = "png", path = newdir, width = wd, height = ht, units = "in", dpi = 300)
# saveRDS(annual_load_plot, paste0(newdir, "annual_load_plot.rds"))
# annual_load_plot
  

  ### Compare to LOADEST model:
#  le1 <- loadest_cal(data = rcdata_f, timeout = 2)
#  summary(le1)
#   
#  ggTermPlot(le1, "q", data = le1$data)
#  ggTermPlot(le1, "Date", data = le1$data)
#  ggTermPlot(le1, "time", data = le1$data)
#   
#   # Other model parameters: Options include:
#   
#  modpars <-  names(df_full[,c(4:8)])
#   ### Use other predictor variables...
#  rcdata <- df_full %>% filter(variable == var, LocationLabel == loc) %>% mutate("is.bdl" = FALSE) %>% droplevels()
#   
#   ### Prep and make the model data 
#   rcdata_f <- rcdata %>%
#     mutate(addays = adry(x = DailyPrcpAve, thresh = 0.1),
#            flow = discount(flow, d = 0.9)) %>%
#  makeModelData()
#  ### Now we can try fitting a model using these predictor variables:
#  sModel_1 <- rcgam(c ~ s(q, k = 3) + s(time, k = 5) + s(doy, bs = "cc", k = 5) +
#                       s(addays, k = 4), data = rcdata_f)
#   
#  ggTermPlot(sModel_1, xvar = "q")
#  ggTermPlot(sModel_1, xvar = "time")
#  ggTermPlot(sModel_1, xvar = "doy")
#  ggTermPlot(sModel_1, xvar = "addays")
#  
#  summary(sModel_1)

renderMyDocument <- function(loc, par,dir, oformat) {
  rmarkdown::render(input = "modules/reports/RC_MODEL_REPORT.Rmd", 
                    output_format = paste0(oformat,"_document"),
                    output_file = paste0("RC_Model_Report_",sdir,"_", par,".", oformat), 
                    output_dir = newdir,
                    params = list(
                      loc = loc,
                      par = var,
                      dir = newdir
                    ))
}
renderMyDocument(loc = loc, par = var, dir = newdir, oformat = "html")
# renderMyDocument(loc = loc, par = var, dir = newdir, oformat = "pdf")

  } # End parameter loop
} # End Function

### Run Model for Locations Selected 
# for(i in seq_along(locs)){  
#   RUNRCMODEL(rawdata = rawdata, gam_models = gam_models, flow = flow,  df_full = df_full, loc = locs[i], pars = vars, dir = dir)
# }
