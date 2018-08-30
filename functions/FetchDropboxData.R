# This function will fetch WAVE data that is stored on Dropbox and save it to a directory to be accessed by the WAVE App. 

fetchDropbox <- function(dir){
  require(httr)
  # Set the data url from dropbox account
  dropbox_url <-  "https://www.dropbox.com/sh/d1n1uzd58sgubcu/AADtTFohN5oFNbKjbLPaA5mya?raw=1"
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

