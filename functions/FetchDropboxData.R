# This function will fetch WAVE data that is stored on Dropbox and save it to a directory to be accessed by the WAVE App. 

fetchDropbox <- function(url,dir){
  require(httr)
  # Set the data url from dropbox account
  dropbox_url <-  url
  # Set the data directory:
  zipdir <- dir
  #Fetch the WAVE data on dropbox
  GET(dropbox_url, 
      write_disk(paste0(zipdir, "/WAVE_DATA.zip"), overwrite = TRUE))
  #Unzip the file
  unzip(zipfile = paste0(zipdir, "/WAVE_DATA.zip"), exdir = zipdir, overwrite = TRUE, junkpaths = TRUE)
  # Remove the zipfile
  file.remove(paste0(zipdir, "/WAVE_DATA.zip")) # tidy up by removing the zip file
}


# df_flags_url <- config[30]
# datadir <- paste0(getwd(), "/rds_files")
# 
# GET(df_flags_url, 
#     write_disk(paste0(datadir, "/df_flags.rds"), overwrite = T))
# 
# df_wach_params_url <- config[31]
# 
# GET(df_wach_params_url, 
#     write_disk(paste0(datadir, "/df_wach_param.rds"), overwrite = T))