###############################################################################
#project: Timplapse PIGU nestcam database metadata fill
#1. add extracted metadata to a Timelapse database
#date: Feb 7, 2023
#author: Ariel Lenske
###############################################################################

##Packages
library(tidyverse)
library(googledrive)

#source functions
source("R_scripts/functions/outputs_loc.R")
source("R_scripts/functions/outputs_loc_mac.R")

#0.1 set data output base path to the projects google drive output folder####
outputbasepath <- outputs_loc("PIGUnestcams_outputs")

#0.2 set the year you want to update meta data for####
year <- "2023"

#0.3 set the study site you want to update meta data for####
site <- "EastLimestone"

#1. download a copy of the Timelapse database in csv form from google drive and read into R####
drive_download(file = paste0("TimelapseDatabase_", site, "_PIGUvideos_", year, ".csv"),
               path = paste0("data_raw/TimelapseDatabase_", site, "_PIGUvideos_", year, "_temp.csv"), overwrite = TRUE)

#read in the database
db <- read_csv(file = paste0("data_raw/TimelapseDatabase_", site, "_PIGUvideos_", year, "_temp.csv"),
               col_types = cols(.default = "c", DateTime = "D", DeleteFlag = "l", 
                                P1_inEntrance = "l", P1_entersNestbox = "l", P1_leavesNestbox = "l",
                                P1_inNestbox = "l", P1_incubating = "l", P1_brooding = "l",
                                P1_nestBuilding = "l", P1_hasFood = "l", P1_feedsChick = "l", P1_feedsMate = "l",
                                P2_inEntrance = "l", P2_entersNestbox = "l", P2_leavesNestbox = "l",
                                P2_inNestbox = "l", P2_incubating = "l", P2_brooding = "l",
                                P2_nestBuilding = "l", P2_hasFood = "l", P2_feedsChick = "l", P2_feedsMate = "l",
                                fishPresent = "l"))

str(db)

#remove extra column
names(db)

db <- db %>%
  dplyr::select(-...44)

#2. fill in boxID based on filepath
db <- db %>%
  mutate(boxID = str_extract(RelativePath, "[^\\\\]+"))


#3. read in the year of metadata extracting using script 1
meta <- readRDS(file.path(outputbasepath, "data_working", paste0(year, "_pigu_video_metadata.rds")))


#4. pivot metadata to long format to match the database and remove missing video file rows
meta <- meta %>%
  pivot_longer(cols = c(video1_filename, video2_filename, video1_quality, video2_quality),
               names_to = c("File_num", ".value"),
               names_pattern = "(.+)_(.+)") %>%
  dplyr::filter(!is.na(filename)) %>%
  dplyr::select(-File_num) %>%
  rename(DateTime = datetime_UTC,
         video_quality = quality,
         File = filename) 

#5. update video_quality db column based on file size info
meta <- left_join(meta, 
                db %>% dplyr::select(RootFolder, File, RelativePath, boxID, video_quality) %>%
                  rename(video_qualitydb = video_quality)) %>%
  mutate(video_quality = ifelse(video_quality == "", video_qualitydb, video_quality)) %>%
  dplyr::select(-video_qualitydb) 
  


#6. remove overlapping columns from the database
dbs <- db %>% dplyr::select(-year, -DateTime, -MCP9804atTS_Temperature_degC,
                     -HDC2010atSENSO30A_Temperature_degC, -HDC2010atSENSO30A_Humidity_percent,
                     -LPS22HBatSENSO30A_Temperature_degC, -LPS22HBatSENSO30A_Pressure_hPa,
                     -video_quality)

#7. merge database with metadata to fill in relevant data
dbnew <- left_join(dbs, meta) 

#8. convert DateTime column to character
dbnew <- dbnew %>%
  mutate(DateTime = as.character(DateTime))

#9. reorder columns in dbnew to match order in db
dbnew <- dbnew[names(db)]

#10. select the metadata and file info columns
dbnew_vq <- dbnew %>% 
  dplyr::select(File, RelativePath, DeleteFlag, boxID, year, DateTime,
                MCP9804atTS_Temperature_degC, HDC2010atSENSO30A_Temperature_degC,
                HDC2010atSENSO30A_Humidity_percent, LPS22HBatSENSO30A_Temperature_degC,
                LPS22HBatSENSO30A_Pressure_hPa, video_quality) %>%
  dplyr::filter(!is.na(video_quality))

dbnew_nvq <- dbnew %>% 
  dplyr::select(File, RelativePath, DeleteFlag, boxID, year, DateTime,
                MCP9804atTS_Temperature_degC, HDC2010atSENSO30A_Temperature_degC,
                HDC2010atSENSO30A_Humidity_percent, LPS22HBatSENSO30A_Temperature_degC,
                LPS22HBatSENSO30A_Pressure_hPa) 
  

#11. save updated database as a csv for upload to timelapse
write_csv(dbnew_vq, file.path(outputbasepath, "data_processed",
                           paste0("TimelapseDatabase_", site, "_PIGUvideos_", year, "_metadata_update_file1.csv")))

write_csv(dbnew_nvq, file.path(outputbasepath, "data_processed",
                              paste0("TimelapseDatabase_", site, "_PIGUvideos_", year, "_metadata_update_file2.csv")))


