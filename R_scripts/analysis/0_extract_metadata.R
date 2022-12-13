###############################################################################
#project: PIGU nestcam datasheet autofill
#0. extract video metadata
#date: Dec 2, 2022
#author: Ariel Lenske
###############################################################################

##Packages
library(googledrive)
library(tidyverse)
library(stringr)
library(doParallel)
library(foreach)

#source functions
source("R_scripts/functions/outputs_loc.R")

#set data output base path to the projects google drive output folder
outputbasepath <- outputs_loc("PIGUnestcams_outputs")

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
drive_ls("PIGU nestbox videos")

#1. pull out the box IDs for a year (currently using 2022 as a test)###########
year <- "2022"

boxIDs <- drive_ls(paste0("PIGU", year)) %>% 
  arrange(name)

#2.pull out the file names for each recording event for each box in the selected year###############
revents <- foreach(i = 1:2,
                   .packages = c("googledrive", "tidyverse"),
                   .final = function(x) setNames(x, boxIDs$name[1:2])) %dopar%{ #need to update for full dataset
                     
                     temp <- drive_ls(boxIDs[i,]) %>% 
                       dplyr::filter(name != ".acrosync") %>%
                       arrange(name)
                     
                     return(temp)
                     
                   }

#turn into a dataframe
#this is the main dataframe that will be used to update the video datasheet
maindf <- enframe(revents) %>%
  rename(boxID = name) %>%
  unnest(cols = c(boxID, value)) %>%
  mutate(ID = paste0(boxID, "-", name)) 

###########################
# maindf <- maindf[1:10, ] #remove when running on full dataset
###########################
  
#3.loop through each recording event for each nestbox and pull out info on video file names#### 
system.time({

vids <- foreach(i = 1:nrow(maindf),
                .packages = c("googledrive", "tidyverse"),
                .final = function(x) setNames(x, maindf$ID)) %dopar%{ #need to update for full dataset
                  
                  
                  tempvids <- drive_ls(maindf[i, ]) %>%
                    dplyr::select(name) %>%
                    dplyr::filter(name != "metadata.txt") %>%
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
                .packages = c("googledrive", "tidyverse"),
                .final = function(x) setNames(x, maindf$ID)) %dopar%{ #need to update for full dataset
                  
                  temptxt <- drive_ls(maindf[i,]) %>%
                    dplyr::filter(name == "metadata.txt")
                  
                  
                  return(temptxt)
                  
                }

#make a txt file location df
txtdf <- enframe(txt) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value))


#use the txt file location df to pull out the contents of the text files
txtc <- foreach(i = 1:nrow(txtdf),
                .packages = c("googledrive", "tidyverse", "stringr"),
                .final = function(x) setNames(x, txtdf$ID)) %dopar%{ #need to update for full dataset
                  
                  txtctemp <- drive_read_string(txtdf[i,])
                  
                  return(txtctemp)
                  
                  print(i)
                  
                }

#make a df of each text files contents
txtcdf <- enframe(txtc) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value))

rm(txt, txtc, txtdf)

#split up text file contents into columns
txtcdf <- txtcdf %>%
  separate(col = value, into = c("datetime", "MCP9804atTS_Temperature",
                                  "HDC2010atSENSO30A_Temperature", "HDC2010atSENSO30A_Humidity",
                                  "LPS22HBatSENSO30A_Temperature", "LPS22HBatSENSO30A_Pressure"), sep = "\n") %>%
  mutate(datetime = str_remove(datetime, "^.*?\\: "),
         MCP9804atTS_Temperature = str_remove(MCP9804atTS_Temperature, "^.*?\\: "),
         HDC2010atSENSO30A_Temperature = str_remove(HDC2010atSENSO30A_Temperature, "^.*?\\: "),
         HDC2010atSENSO30A_Humidity = str_remove(HDC2010atSENSO30A_Humidity, "^.*?\\: "),
         LPS22HBatSENSO30A_Temperature = str_remove(LPS22HBatSENSO30A_Temperature, "^.*?\\: "),
         LPS22HBatSENSO30A_Pressure = str_remove(LPS22HBatSENSO30A_Pressure, "^.*?\\: "))

#5. add text file info and video file names to the main df

maindf <- left_join(maindf, vidsdf) %>%
  left_join(., txtcdf) %>%
  mutate(year = year)
  
  
#6. select relevant columns 
names(maindf)

maindf <- maindf %>%
  dplyr::select(boxID, year, video1_filename, video2_filename, datetime, MCP9804atTS_Temperature, 
                HDC2010atSENSO30A_Temperature, HDC2010atSENSO30A_Humidity, LPS22HBatSENSO30A_Temperature,
                LPS22HBatSENSO30A_Pressure) %>%
  na_if("")

#7. save extracted data to a rds file
saveRDS(maindf, file.path(outputbasepath, "data_working", paste0(year, "pigu_video_metadata.rds")))
  
})
