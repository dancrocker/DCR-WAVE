# PRECIPITATION PLOTS ####

### MONTHLY PLOT1 (just 1 year ####
PRECIP_MONTH_BAR <- function(df1, df2, vyear, type = NULL){
# Plot Args
  # df1 <- PrcpMonthYear
  # df2 <- PrcpMonthMean
  # vyear <- 2018

this_year <- year(Sys.Date()) 
  
p1data <- filter(df1, Year == vyear) %>%
  left_join(df2, by = "Month") %>%
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
        axis.title.x = element_text(vjust = -1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values=c("deepskyblue3", "bisque4")) +
  scale_y_continuous(breaks = pretty_breaks())

out <- list()
### Plot return type
  if(isTruthy(type)){
    out[[1]] <- (plotly::ggplotly(p, tooltip = c("x","y")))
    out[[2]] <- t_precip_month
    } else {
      out[[1]] <- p
      out[[2]] <- t_precip_month
    }
return(out)
} # End of function
# dfs <- PRECIP_MONTH_BAR(df1 = PrcpMonthYear, df2 = PrcpMonthMean, vyear = 2017, type = TRUE)
# p <- dfs[[1]]
# p
# t <- dfs[[2]]
# t

###__________________________________________________________________________________________________________

### MONTHLY PLOT2 ####
PRECIP_MONTH_BAR2 <- function(df, date_min, date_max, type = NULL){
  # Plot Args
  # df <- PrcpMonthYear
  # date_min <- today() - 1500
  # date_max <- today()
  # type <-  FALSE
  
  df$filter_date <- as.Date(paste0(df$Year,"-",df$Month,"-01"))

  p1data <- df %>% 
    filter(filter_date >= floor_date(date_min, unit = "month"), filter_date <= floor_date(date_max, unit = "month"), nDays >=28) %>%
    select(c(5,3)) %>%
    dplyr::rename("Month" = filter_date, "Precip" = MonthPrcpTotal)
  
  
  p <- ggplot(p1data, aes(x = Month, y = Precip)) +
    geom_bar(aes(fill = "deepskyblue3", text = paste("Month:", format(Month,"%b-%Y"), "<br>","Precip:", Precip)), stat = "identity", color = "black") +
    labs(title = paste0("Wachusett Watershed Monthly Precipitation\n", format(min(p1data$Month),"%b-%Y"), " to ", format(max(p1data$Month),"%b-%Y")),
         caption="Source: USGS and NOAA") +
    xlab("Month") +
    ylab("Precipitation (Inches)") +
    theme_light() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.title.y = element_text(vjust = 2, face = "bold"),
          axis.title.x = element_text(vjust = 2, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_fill_manual(values=c("deepskyblue3", "bisque4")) +
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_x_date(date_labels = "%b\n%Y", breaks =  pretty_breaks(n=12))
  
  # p
  
  ### Plot return type 
  if(isTruthy(type)){
    return(plotly::ggplotly(p, tooltip = c("text")))
  } else {
    return(p)
  }
} # End of function
# PRECIP_MONTH_BAR2(df = PrcpMonthYear, date_min = date_min, date_max = date_max, type = TRUE)

### DAILY PLOT ####
PRECIP_DAILY_BAR <- function(df, date_min, date_max, type){
  # Plot Args
  # df = df_wach_prcp_daily
  # date_min <- as.Date("2018-05-03")
  # date_max <- as.Date("2018-08-08")
  
  df <- select(df,c(1,7)) %>%
    dplyr::rename("Date" = DATE, "Precip" = DailyPrcpAve) %>% 
    filter(Date >= date_min, Date <= date_max)
  df$Precip <- round(df$Precip,2)
  
  p <- ggplot(df, aes(x = Date, y = Precip)) +
    geom_bar(fill = "deepskyblue3", position = "dodge", stat = "identity", color = "black") +
    labs(title = paste0("Wachusett Watershed Daily Precipitation"),
         caption="Source: USGS and NOAA") +
    xlab("Date") +
    ylab("Precipitation (Inches)") +
    theme_light() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.title.y = element_text(vjust = 2, face = "bold"),
          axis.title.x = element_text(vjust = 2, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_fill_manual(values=c("deepskyblue3")) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_x_date(date_labels = "%b %d\n%Y", breaks =  pretty_breaks(n=12))
  # p
  ### Plot return type 
  if(isTruthy(type)){
    return(plotly::ggplotly(p, tooltip = c("x","y")))
  } else {
    return(p)
  }
} # End of function
# PRECIP_DAILY_BAR(df = df_wach_prcp_daily, date_min = date_min, date_max = date_max, type = FALSE)

### YEAR PLOT ####
PRECIP_YEAR_BAR <- function(df, date_min, date_max, type = FALSE){
  # Plot Args
  # date_min <- as.Date("1985-05-03")
  # date_max <- as.Date("2018-08-08")
  this_year <- year(Sys.Date()) 
  years <- seq.int(from = year(date_min), to = min(this_year - 1 , year(date_max)))
  
  df2 <- filter(df, Year %in% years) %>%
    select(c(1,7)) %>%
    dplyr::rename("Precip" = AnnualTotal)
  PrcpAnnualAve <- round(mean(df$AnnualTotal, na.rm = T), 2)
  plotmax <- ceiling(max(PrcpAnnualAve, max(df2$Precip, na.rm = T)) + 1)

  p <- ggplot(df2, aes(x = Year, y = Precip)) +
    geom_bar(aes(fill = "deepskyblue3"), stat = "identity", color = "black") +
    geom_hline(yintercept = PrcpAnnualAve, color = "red") +
    labs(title = paste0("Wachusett Watershed Annual Precipitation Totals")) +
    xlab("Year") +
    ylab("Precipitation (Inches)") +
    theme_light() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90,face = "bold"),
          axis.title.y = element_text(vjust = 2, face = "bold"),
          axis.title.x = element_text(vjust = -1,face = "bold"),
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("deepskyblue3")) +
    scale_x_continuous(breaks = seq.int(min(years),max(years), by = 1)) +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, plotmax)) +
    annotate("text", x = (min(years) + max(years))/2, y = 4, label = paste0("Average Annual Watershed Precipitation = ", PrcpAnnualAve, " Inches \nSource: USGS and NOAA"))
  
  ### Plot return type 
  if(isTruthy(type)){
    return(plotly::ggplotly(p, tooltip = c("x","y")))
  } else {
    return(p)
  }
} # End of function
# PRECIP_YEAR_BAR(df = YTD_J_Day, date_min, date_max, type = FALSE)

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
        axis.title.x = element_text(vjust = -1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_date(date_labels = "%b", date_breaks(width = "1 month"), name = "Date") +
  scale_y_continuous(breaks = pretty_breaks())

### Plot return type
if(isTruthy(type)){
  return(plotly::ggplotly(p, tooltip = "text"))
} else {
  return(p)
}

} # End function
# PRECIP_LINE(df = df_wach_prcp_daily, vyear = 1987, type = TRUE)

## MATRIX MONTH PLOT ####
PRECIP_MATRIX <- function(df){
require(lattice)
require(hydroTSM)
# df <- df_wach_prcp_daily

tsm_data <- df[year(df$DATE) < year(Sys.Date()) ,c(1,7)]
tsm_data <-  zoo(x = tsm_data[,2], order.by = tsm_data[,1])

x <- window(tsm_data, start = as.Date("1985-01-01"))
m <- daily2monthly(x, FUN=sum)
dates <- time(x)
# smry(x)

# hydroplot(x, var.type="Precipitation", var.unit = "in", main="at Wachusett Watershed",
#           pfreq = "dm", from="1985-01-01")

# Daily zoo to monthly zoo
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)
# Creating a matrix with monthly values per year in each column
M <- matrix(m, ncol=12, byrow=TRUE)
colnames(M) <- month.abb
rownames(M) <- unique(format(time(m), "%Y"))

# Plotting the monthly precipitation values

## Loading required package: lattice
mplot <- print(matrixplot(M, ColorRamp="Precipitation",
                 main="Wachusett Watershed Monthly Precipitation, [in/month]"))
return(mplot)
} # End function
# PRECIP_MATRIX(df = df_wach_prcp_daily)


