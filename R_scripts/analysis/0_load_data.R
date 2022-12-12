###############################################################################
#project: PIGU nestcam datasheet autofill
#load data
#date: Dec 2, 2022
#author: Ariel Lenske
###############################################################################

##Packages
library(googledrive)
library(tidyverse)
library(stringr)

#download the datasheet
drive_download("PIGU_nestbox_video_data.xlsx", overwrite = TRUE)

#all the files in the main folder
drive_ls("PIGU nestbox videos")

#pull out the box IDs for 2022
boxIDs <- drive_ls("PIGU2022") %>% 
  arrange(name)

#pull out the file names for each recording event for each box in 2022
revents <- list()

for(i in 1:nrow(boxIDs)){
  
  revents[[i]] <- drive_ls(boxIDs[2,]) %>% 
    dplyr::filter(name != ".acrosync") %>%
    arrange(name)
  
  print(i)
  
}

rm(i)

names(revents) <- boxIDs %>% 
  dplyr::select(name) %>%
  pull()

#stick in a df
maindf <- enframe(revents) %>%
  rename(boxID = name) %>%
  unnest(cols = c(boxID, value)) %>%
  mutate(ID = paste0(boxID, "-", name))

#loop through each recording event for each box and pull out info on vid file names and text files
vids <- list()
txt <- list()

for(i in 1:nrow(maindf)){
  
  vids[[i]] <- drive_ls(maindf[i,]) %>%
    dplyr::select(name) %>%
    dplyr::filter(name != "metadata.txt") %>%
    arrange(name) %>%
    mutate(colname = c("video1_filename", "video2_filename")) %>%
    pivot_wider(names_from = colname, values_from = name)
  
  txt[[i]] <- drive_ls(maindf[i,]) %>%
    dplyr::filter(name == "metadata.txt")
  
  print(i)
  
}

rm(i)

names(vids) <- maindf %>% 
  dplyr::select(ID) %>%
  pull()

names(txt) <- maindf %>% 
  dplyr::select(ID) %>%
  pull()

#stick in a df
vidsdf <- enframe(vids) %>%
  unnest(cols = c(name, value))

#stick in a df
txtdf <- enframe(txt) %>%
  rename(ID = name) %>%
  unnest(cols = c(ID, value))


######################################################
#pull out info from text files
test <- drive_read_string(txtdf[5,])


