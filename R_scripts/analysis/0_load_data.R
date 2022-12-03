###############################################################################
#project: PIGU nestcam datasheet autofill
#load data
#date: Dec 2, 2022
#author: Ariel Lenske
###############################################################################

##Packages
library(googledrive)
library(tidyverse)

#download the datasheet
drive_download("PIGU_nestbox_video_data.xlsx", overwrite = TRUE)

#retrieve file names 
#all the files in the main folder
drive_ls("PIGU nestbox videos")

#pull out the box IDs for 2022
boxIDs <- drive_ls("PIGU2022") %>% 
  dplyr::select(name) %>% pull()

#pull out the file names for each recording event
revents <- list()

for(i in 1:length(boxIDs)){
  
  revents[[i]] <- drive_ls("Unit132841") %>% 
    dplyr::select(name) %>% pull()
  
}


#all the files in each recording instance folder



#load datasheet into R


