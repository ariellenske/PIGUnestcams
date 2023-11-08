###############################################################################
#project: PIGU nestcam datasheet autofill
#1. extract video metadata
#date: Dec 2, 2022
#author: Ariel Lenske
###############################################################################

##notes: 

#A) to extract the metadata for a year of video data, change the value of year
#in #1 and run script, results are output in #7 to an .rds file

#B) remember to set correct path in 0.1 and 0.2 --> comment out either the 
#windows or mac line before running depending on the system your running the code on

##Packages
library(tidyverse)
library(stringr)
library(doParallel)
library(foreach)
library(readr)

#source functions
source("R_scripts/functions/outputs_loc.R")

#0.1 set data output base path to the projects google drive output folder####
outputbasepath <- outputs_loc("PIGUnestcams_outputs")

#0.2 set the path to the PIGU nestbox videos folder in your local google drive location####
# #windows
pigufp <- file.path("G:", "My Drive", "ECCC work", "Data", "PIGU", "PIGU nestbox videos", "PIGU nestbox video data")

#0.3 set the year you want to extract meta data for####
year <- "2023"

#0.4 set the project you want to extract meta data for####
site <- "EastLimestone"

#0.5 set up stuff for running things in parallel####
parallel::detectCores()

n.cores <- parallel::detectCores() - 1

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)

#check cluster definition (optional)
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#list all the files in the main folder
list.files(pigufp)

#list all the files in the project folder for the year of interest
list.files(file.path(pigufp, site, paste0(year, "_deployments")))

#1. pull out the box IDs for the specified year###########
boxIDs <- data.frame(name = list.files(file.path(pigufp, site, paste0(year, "_deployments")))) %>% 
  dplyr::filter(str_detect(name, "Unit")) %>%
  arrange(name) 

#2.pull out the file names for each recording event for each box in the selected year###############
revents <- foreach(i = 1:nrow(boxIDs),
                   .packages = c("tidyverse"),
                   .final = function(x) setNames(x, boxIDs$name)) %do%{ 
                     
                     temp <- data.frame(name = list.files(file.path(pigufp, site, paste0(year, "_deployments"), boxIDs[i,]))) %>% 
                       dplyr::filter(name != "desktop.ini") %>%
                       arrange(name)
                     
                     return(temp)
                     
                   }

rm(temp, i)

#turn into a dataframe
#this is the main dataframe that will be used to update the video datasheet
maindf <- enframe(revents) %>%
  rename(boxID = name) %>%
  unnest(cols = c(boxID, value)) %>%
  mutate(ID = paste0(boxID, "/", name)) 

rm(revents)

###########################
# maindf <- maindf[1:10, ] #remove when running on full dataset
###########################
  
#3.loop through each recording event for each nestbox and pull out info on video file names#### 
vids <- foreach(i = 1:nrow(maindf),
                .packages = c("tidyverse"),
                .final = function(x) setNames(x, maindf$ID)) %dopar%{ 
                  
                  
                  tempvids <- data.frame(name = list.files(file.path(pigufp, site, paste0(year, "_deployments"), maindf$ID[i]))) %>%
                    dplyr::filter(name != "metadata.txt") %>%
                    dplyr::filter(name != "desktop.ini") %>%
                    arrange(name)
                  
                  if(nrow(tempvids) == 2) {
                    
                    tempvids <- tempvids %>%
                      mutate(colname = c("video1_filename", "video2_filename")) %>%
                      pivot_wider(names_from = colname, values_from = name)
                    
                  } else if(nrow(tempvids) == 1) {
                    
                    tempvids <- tempvids %>%
                      add_row(name = NA) %>%
                      mutate(colname = c("video1_filename", "video2_filename")) %>%
                      pivot_wider(names_from = colname, values_from = name)
                    
                  } else {
                    
                    tempvids <- data.frame(video1_filename = NA,
                                           video2_filename = NA)
                    
                  }
                  
                  return(tempvids)
                  
                }

#stick video file names in a df --> will be joined with maindf
vidsdf <- enframe(vids) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value))

rm(vids)


#4.loop through each recording event for each nestbox and pull out info from the text files

#get df of text file locations
txt <- foreach(i = 1:nrow(maindf),
                .packages = c("tidyverse"),
                .final = function(x) setNames(x, maindf$ID)) %dopar%{ 
                  
                  temptxt <- data.frame(name = list.files(file.path(pigufp, site, paste0(year, "_deployments"), maindf$ID[i])))%>%
                    dplyr::filter(name == "metadata.txt")
                  
                  
                  return(temptxt)
                  
                }

#make a txt file location df
txtdf <- enframe(txt) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value))


#use the txt file location df to pull out the contents of the text files
txtc <- foreach(i = 1:nrow(txtdf),
                .packages = c("tidyverse", "stringr"),
                .final = function(x) setNames(x, txtdf$ID)) %dopar%{ 
                  
                  txtctemp <- data.frame(value = read_file(file.path(pigufp, site, paste0(year, "_deployments"),
                                                                     txtdf$ID[i], txtdf$name[i])))
                  
                  return(txtctemp)
                  
                  
                }


#make a df of each text files contents
txtcdf <- enframe(txtc) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value))

rm(txt, txtc, txtdf)

#split up text file contents into columns
txtcdf<- txtcdf %>% 
  mutate(value = str_replace_all(value, "[\n]" , "; "),
         value = str_replace_all(value, "[()]", ""),
         value = str_replace_all(value, "[@]", "_"))

txtcdf <- txtcdf %>%
  mutate(datetime_UTC = as.POSIXct(str_extract(value, "(?<=Date:)([^UTC;]+)"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         MCP9804atTS_Temperature_degC = str_extract(value, "(?<=MCP9804_TS Temperature:)([^degC;]+)"),
         HDC2010atSENSO30A_Temperature_degC = str_extract(value, "(?<=HDC2010_SENSO30A Temperature:)([^degC;]+)"),
         HDC2010atSENSO30A_Humidity_percent = str_extract(value, "(?<=HDC2010_SENSO30A Humidity:)([^%;]+)"),
         LPS22HBatSENSO30A_Temperature_degC = str_extract(value, "(?<=LPS22HB_SENSO30A Temperature:)([^degC;]+)"),
         LPS22HBatSENSO30A_Pressure_hPa = str_extract(value, "(?<=LPS22HB_SENSO30A Pressure:)([^hPa;]+)"))

#5. add text file info and video file 'names' column to the main df
maindf <- left_join(maindf, vidsdf) %>%
  left_join(., txtcdf) %>%
  mutate(year = year)

#6. fill in missing datetimes based on name column
maindf <- maindf %>%
  mutate(dt_from_name = paste0(str_sub(name, 1, 4), "-",
                               str_sub(name, 5, 6), "-",
                               str_sub(name, 7, 8), " ",
                               str_sub(name, 10, 11), ":",
                               str_sub(name, 12, 13), ":",
                               str_sub(name, 14, 15)),
         dt_from_name = as.POSIXct(dt_from_name, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         datetime_UTC = if_else(is.na(datetime_UTC), dt_from_name, datetime_UTC))

#7. extract file size info for each video and set video quality column for 0 mb files to 'unusable - video error'

#make df with the video file names and path info
sizedf <- vidsdf

#use the video file location df to pull out the file sizes
#video file #1s
size1 <- foreach(i = 1:nrow(sizedf),
                .packages = c("tidyverse", "stringr"),
                .final = function(x) setNames(x, sizedf$ID)) %dopar%{ 
                  
                  sizetemp <- data.frame(value = file.size(file.path(pigufp, site, paste0(year, "_deployments"),
                                                                     sizedf$ID[i], sizedf$video1_filename[i])))
                  
                  return(sizetemp)
                  
                }

#make a df of each videos file size and add video 'quality' column
vq1 <- enframe(size1) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value)) %>%
  mutate(video1_quality = if_else(value == 0, "unusable - video error", "")) %>%
  dplyr::select(-value)

#video file #2s
size2 <- foreach(i = 1:nrow(sizedf),
                  .packages = c("tidyverse", "stringr"),
                  .final = function(x) setNames(x, sizedf$ID)) %dopar%{ 
                    
                    sizetemp <- data.frame(value = file.size(file.path(pigufp, site, paste0(year, "_deployments"),
                                                                        sizedf$ID[i], sizedf$video2_filename[i])))
                    
                    return(sizetemp)
                    
                  }

#make a df of each videos file size and add video 'quality' column
vq2 <- enframe(size2) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value)) %>%
  mutate(video2_quality = if_else(value == 0, "unusable - video error", "")) %>%
  dplyr::select(-value)

rm(sizedf, size1, size2)


#8.add video quality columns to the main df
maindf <- left_join(maindf, vq1) %>%
  left_join(., vq2) 

#9. select relevant columns 
names(maindf)

maindf <- maindf %>%
  dplyr::select(boxID, year, video1_filename, video2_filename, video1_quality, video2_quality, datetime_UTC,
                MCP9804atTS_Temperature_degC, HDC2010atSENSO30A_Temperature_degC, 
                HDC2010atSENSO30A_Humidity_percent, LPS22HBatSENSO30A_Temperature_degC,
                LPS22HBatSENSO30A_Pressure_hPa) 


#8. save extracted data to a rds file
saveRDS(maindf, file.path(outputbasepath, "data_working", paste0(year, "_pigu_video_metadata.rds")))
  

