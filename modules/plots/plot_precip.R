# Precip Plots ####


### MONTHLY PLOT ####
PRECIP_MONTH_BAR <- function(df, vyear, type = NULL){
# Plot Args
# df <- PrcpMonthYear
# vyear <- 2017

this_year <- year(Sys.Date()) 
  
p1data <- filter(df, Year == vyear) %>%
  left_join(PrcpMonthMean, by = "Month") %>%
  rename(!!paste0(vyear) := MonthPrcpTotal, "Normal" = MonthPrcpAve) %>%
  select(c(1,3,5)) %>%
  gather(2:3, key = "Year" , value = "Precip")

month_ave <- PrcpMonthMean

if(this_year == vyear){
  p1data <- filter(p1data, Month < this_month)
  month_ave <- filter(PrcpMonthMean, Month < this_month)
}

t1data <- p1data[p1data$Year == vyear,]

t_precip_month <- t1data %>%
  mutate("Normal" = month_ave$MonthPrcpAve,
         "Years" = month_ave$nYears) %>%
  mutate("Departure" = Precip - Normal) %>%
  select(Month, Precip, Normal, Departure, Years)

# Make months character abbreviations
t_precip_month$Month <- as.character(factor(month.abb[t_precip_month$Month], levels=month.abb[1:12]))


p1data$Month <- factor(month.abb[p1data$Month], levels=month.abb[1:12])
p1data$Year <- factor(p1data$Year)

t_precip_month <- t(t_precip_month) %>% as.data.frame()

# Rename Columns using first row values and then remove the first row
names(t_precip_month) <- unlist(t_precip_month[1,])
t_precip_month <- t_precip_month[-1,]
# Now convert all the data back to numeric
id = 1:ncol(t_precip_month) # column ids to change
t_precip_month[id] = as.numeric(as.character(unlist(t_precip_month[id])))

t_precip_month$Total <- rowSums(t_precip_month)
t_precip_month[4,ncol(t_precip_month)] <- NA

p <- ggplot(p1data, aes(x = Month, y = Precip)) +
  geom_bar(aes(fill = p1data$Year, text = paste("Precip:", Precip)), position = "dodge", stat = "identity") +
  labs(title = paste0("Wachusett Watershed Monthly Precipitation for ", vyear),
       caption="Source: USGS and NOAA") +
  xlab("Month") +
  ylab("Precipitation (Inches)") +
  theme_light() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 2, face = "bold"),
        axis.title.x = element_text(vjust = 2, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values=c("deepskyblue3", "bisque4")) +
  scale_y_continuous(breaks = pretty_breaks())

### Plot return type ####
  if(isTruthy(type)){
    return(plotly::ggplotly(p, tooltip = c("x","y")))
    } else {
    return(p)
    }
} # End of function
# PRECIP_MONTH_BAR(df = PrcpMonthYear,vyear = 2017, type = TRUE)

PRECIP_DAILY_BAR <- function(df, vyear, type = NULL){
  # Plot Args
  df <- PrcpMonthYear
  vyear <- 2018

  
  this_year <- year(Sys.Date()) 
  
  p1data <- filter(df, Year == vyear) %>%
    left_join(PrcpMonthMean, by = "Month") %>%
    rename(!!paste0(vyear) := MonthPrcpTotal, "Normal" = MonthPrcpAve) %>%
    select(c(1,3,5)) %>%
    gather(2:3, key = "Year" , value = "Precip")
  
  month_ave <- PrcpMonthMean
  
  if(this_year == vyear){
    p1data <- filter(p1data, Month < this_month)
    month_ave <- filter(PrcpMonthMean, Month < this_month)
  }
  
  t1data <- p1data[p1data$Year == vyear,]
  
  t_precip_month <- t1data %>%
    mutate("Normal" = month_ave$MonthPrcpAve,
           "Years" = month_ave$nYears) %>%
    mutate("Departure" = Precip - Normal) %>%
    select(Month, Precip, Normal, Departure, Years)
  
  # Make months character abbreviations
  t_precip_month$Month <- as.character(factor(month.abb[t_precip_month$Month], levels=month.abb[1:12]))
  
  
  p1data$Month <- factor(month.abb[p1data$Month], levels=month.abb[1:12])
  p1data$Year <- factor(p1data$Year)
  
  t_precip_month <- t(t_precip_month) %>% as.data.frame()
  
  # Rename Columns using first row values and then remove the first row
  names(t_precip_month) <- unlist(t_precip_month[1,])
  t_precip_month <- t_precip_month[-1,]
  # Now convert all the data back to numeric
  id = 1:ncol(t_precip_month) # column ids to change
  t_precip_month[id] = as.numeric(as.character(unlist(t_precip_month[id])))
  
  t_precip_month$Total <- rowSums(t_precip_month)
  t_precip_month[4,ncol(t_precip_month)] <- NA
  
  p <- ggplot(p1data, aes(x = Month, y = Precip)) +
    geom_bar(aes(fill = p1data$Year, text = paste("Precip:", Precip)), position = "dodge", stat = "identity") +
    labs(title = paste0("Wachusett Watershed Monthly Precipitation for ", vyear),
         caption="Source: USGS and NOAA") +
    xlab("Month") +
    ylab("Precipitation (Inches)") +
    theme_light() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.title.y = element_text(vjust = 2),
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("deepskyblue3", "bisque4")) +
    scale_y_continuous(breaks = pretty_breaks())
  
  ### Plot return type ####
  if(isTruthy(type)){
    return(plotly::ggplotly(p, tooltip = c("x","y")))
  } else {
    return(p)
  }
} # End of function
# PRECIP_DAILY_BAR()

PRECIP_YEAR_BAR <- function(df, vyear1, vyear2, type){
  # Plot Args
  # df <- YTD_J_Day
  # vyear1 <- 1985
  # vyear2 <- 2017
  # type <- "plotly"
  
  this_year <- year(Sys.Date()) 
  
  df <- filter(df, Year >= vyear1 & Year <= vyear2) %>%
    select(c(1,7)) %>%
    rename("Precip" = AnnualTotal)
    

  p <- ggplot(df, aes(x = Year, y = Precip)) +
    geom_bar(aes(fill = df$Year, text = paste("Precip:", Precip)), stat = "identity", color = "deepskyblue3") +
    geom_hline(yintercept = mean(df$Precip), color = "red") +
    labs(title = paste0("Wachusett Watershed Annual Precipitation Totals"),
         caption="Source: USGS and NOAA") +
    xlab("Year") +
    ylab("Precipitation (Inches)") +
    theme_light() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(vjust = 2),
          axis.title.x = element_text(vjust = 2),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq.int(vyear1,vyear2, by = 1)) +
    scale_y_continuous(breaks = pretty_breaks()) 
  
  ### Plot return type ####
  if(isTruthy(type)){
    return(plotly::ggplotly(p, tooltip = c("x","y")))
  } else {
    return(p)
  }
} # End of function
# PRECIP_YEAR_BAR()
###______________________________________________________________

### CUMULATIVE PRECIP PLOT ####
# Julian Day plot - Days 1 - 366 showing cumulative precip
PRECIP_LINE <-  function(df, vyear, type = NULL){ 
source("functions/wach_precip_stats.R") # Function args (df)  
dfs <- PRECIP_STATS(df_precip = df, vyear = vyear)
df <- dfs[[5]]
  
# df <- jday_sum_vyear
p <- ggplot() 
if(isTruthy(type)){  
  p <- p +
  geom_line(data = df, aes(df$DATE, df$jDay_YTJD.y, color = "Normal", text = paste("Date:", df$DATE, "<br>", "Precip (in):", round(df$jDay_YTJD.y,2)), group=1), size = 1.5, linetype = 2) +
  geom_line(data = df, aes(df$DATE, df$jDay_YTJD.x, color = paste(vyear), text = paste("Date:", df$DATE, "<br>", "Precip (in):", round(df$jDay_YTJD.x,2)),group=1), size = 1.5) 
} else {
  p <- p +
    geom_line(data = df, aes(df$DATE, df$jDay_YTJD.y, color = "Normal"), size = 1.5, linetype = 2) +
    geom_line(data = df, aes(df$DATE, df$jDay_YTJD.x, color = paste(vyear)), size = 1.5) 
}
p <- p + 
  labs(title = paste0("Cummulative Watershed Precipitation for ", vyear),
       caption="Source: USGS and NOAA") +
  xlab("Date") +
  ylab("Precipitation (Inches)") +
  scale_color_manual(name = "", labels = c(paste(vyear), "Normal"), values = c("deepskyblue3","bisque4")) +
  theme_light() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.title.y = element_text(vjust = 2, face = "bold"),
        axis.title.x = element_text(vjust = 2, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_date(date_labels = "%b", date_breaks(width = "1 month"), name = "Date") +
  scale_y_continuous(breaks = pretty_breaks())

### Plot return type ####
if(isTruthy(type)){
  return(plotly::ggplotly(p, tooltip = "text"))
} else {
  return(p)
}

} # End function
# PRECIP_LINE(df = df_wach_prcp_daily, vyear = 2016, type = TRUE)

### HydroTSM ####
# PRECIP_MATRIX <- function(df){
# require(lattice)
# # df <- df_wach_prcp_daily
# 
# tsm_data <- df[year(df$DATE) < year(Sys.Date()) ,c(1,7)]
# tsm_data <-  zoo(x = tsm_data[,2], order.by = tsm_data[,1])
# 
# x <- window(tsm_data, start = as.Date("1985-01-01"))
# m <- daily2monthly(x, FUN=sum)
# dates <- time(x)
# # smry(x)
# 
# # hydroplot(x, var.type="Precipitation", main="at Wachusett Watershed",
#           # pfreq = "dm", from="1985-01-01")
# 
# # Daily zoo to monthly zoo
# m <- daily2monthly(x, FUN=sum, na.rm=TRUE)
# # Creating a matrix with monthly values per year in each column
# M <- matrix(m, ncol=12, byrow=TRUE)
# colnames(M) <- month.abb
# rownames(M) <- unique(format(time(m), "%Y"))
# 
# # Plotting the monthly precipitation values
# 
# ## Loading required package: lattice
# mplot <- print(matrixplot(M, ColorRamp="Precipitation",
#                  main="Wachusett Watershed Monthly Precipitation, [in/month]"))
# 
# return(mplot)
# } # End function

# PRECIP_BOX <- 

