###############################################################################
#project: PIGU nestcam datasheet autofill
#0. extract video metadata
#date: Dec 2, 2022
#author: Ariel Lenske
###############################################################################

##notes: to extract the metadata for a year of video data, change the value of year
#in #1 and run script, results are output in #7 to an .rds file

##Packages
library(tidyverse)
library(stringr)
library(doParallel)
library(foreach)
library(readr)

#source functions
source("R_scripts/functions/outputs_loc.R")
source("R_scripts/functions/outputs_loc_mac.R")

#set data output base path to the projects google drive output folder
# #windows
# outputbasepath <- outputs_loc("PIGUnestcams_outputs")

#mac
outputbasepath <- outputs_loc_mac("PIGUnestcams_outputs")

#set the path to the PIGU nestbox videos folder in your local google drive location
# #windows
# pigufp <- file.path("G:", "My Drive", "ECCC work", "Data", "PIGU", "PIGU nestbox videos")

#mac
pigufp <- file.path("~", "Google Drive", "My Drive", "ECCC work", "Data", "PIGU", "PIGU nestbox videos")

#set up stuff for running things in parallel
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

#1. pull out the box IDs for a year (currently using 2022 as a test)###########
year <- "2021"

boxIDs <- data.frame(name = list.files(file.path(pigufp, paste0("PIGU", year)))) %>% 
  dplyr::filter(name != "desktop.ini") %>%
  arrange(name) 

#2.pull out the file names for each recording event for each box in the selected year###############
revents <- foreach(i = 1:nrow(boxIDs),
                   .packages = c("tidyverse"),
                   .final = function(x) setNames(x, boxIDs$name)) %do%{ 
                     
                     temp <- data.frame(name = list.files(file.path(pigufp, paste0("PIGU", year), boxIDs[i,]))) %>% 
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
#maindf <- maindf[1:10, ] #remove when running on full dataset
###########################
  
#3.loop through each recording event for each nestbox and pull out info on video file names#### 
vids <- foreach(i = 1:nrow(maindf),
                .packages = c("tidyverse"),
                .final = function(x) setNames(x, maindf$ID)) %dopar%{ 
                  
                  
                  tempvids <- data.frame(name = list.files(file.path(pigufp, paste0("PIGU", year), maindf$ID[i]))) %>%
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
                  
                  temptxt <- data.frame(name = list.files(file.path(pigufp, paste0("PIGU", year), maindf$ID[i])))%>%
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
                  
                  txtctemp <- data.frame(value = read_file(file.path(pigufp, paste0("PIGU", year),
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
  mutate(datetime_UTC = str_extract(value, "(?<=Date:)([^UTC;]+)"),
         MCP9804atTS_Temperature_degC = str_extract(value, "(?<=MCP9804_TS Temperature:)([^degC;]+)"),
         HDC2010atSENSO30A_Temperature_degC = str_extract(value, "(?<=HDC2010_SENSO30A Temperature:)([^degC;]+)"),
         HDC2010atSENSO30A_Humidity_percent = str_extract(value, "(?<=HDC2010_SENSO30A Humidity:)([^%;]+)"),
         LPS22HBatSENSO30A_Temperature_degC = str_extract(value, "(?<=LPS22HB_SENSO30A Temperature:)([^degC;]+)"),
         LPS22HBatSENSO30A_Pressure_hPa = str_extract(value, "(?<=LPS22HB_SENSO30A Pressure:)([^hPa;]+)"))

#5. add text file info and video file names to the main df
maindf <- left_join(maindf, vidsdf) %>%
  left_join(., txtcdf) %>%
  mutate(year = year)
  
  
#6. select relevant columns 
names(maindf)

maindf <- maindf %>%
  dplyr::select(boxID, year, video1_filename, video2_filename, datetime_UTC,
                MCP9804atTS_Temperature_degC, HDC2010atSENSO30A_Temperature_degC, 
                HDC2010atSENSO30A_Humidity_percent, LPS22HBatSENSO30A_Temperature_degC,
                LPS22HBatSENSO30A_Pressure_hPa) %>%
  na_if("")

#7. save extracted data to a rds file
saveRDS(maindf, file.path(outputbasepath, "data_working", paste0(year, "_pigu_video_metadata.rds")))
  

