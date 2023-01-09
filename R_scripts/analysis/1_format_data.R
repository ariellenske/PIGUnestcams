###############################################################################
#project: PIGU nestcam datasheet autofill
#1. add extracted metadata to master datasheet
#date: Jan 6, 2023
#author: Ariel Lenske
###############################################################################

##Packages
library(tidyverse)
library(googledrive)
library(readxl)
library(openxlsx)

#source functions
source("R_scripts/functions/outputs_loc.R")
source("R_scripts/functions/outputs_loc_mac.R")

#0.1 set data output base path to the projects google drive output folder####
#windows
outputbasepath <- outputs_loc("PIGUnestcams_outputs")

# #mac
# outputbasepath <- outputs_loc_mac("PIGUnestcams_outputs")

#0.2 set the year you want to extract meta data for####
year <- "2021"

#1. download a copy of the datasheet from google drive and read into R####
drive_download(file = "PIGU_nestbox_video_data.xlsx",
               path = "data_raw/PIGU_nestbox_video_data_temp.xlsx", overwrite = TRUE)

#read in master datasheet
ds <- read_excel("data_raw/PIGU_nestbox_video_data_temp.xlsx", sheet = "data",
                 col_types = c(rep("text", 10), 
                               rep("text", 4), 
                               rep("numeric", 3),
                               rep("text", 8)))

str(ds)

#2. read in a year of metadata extracting using script 0
meta <- readRDS(file.path(outputbasepath, "data_working", paste0(year, "_pigu_video_metadata.rds")))

#convert posixct datetime to character string
meta <- meta %>%
  mutate(datetime_UTC = as.character(datetime_UTC))

#convert NAs to "-"
meta <- meta %>%
  replace(is.na(.), "-")

#3. join master datasheet and metadata
ds <- bind_rows(meta, ds)

#4. write updated datasheet to the workbook as a data table
wb <- loadWorkbook("data_raw/PIGU_nestbox_video_data_temp.xlsx", xlsxFile = NULL, isUnzipped = FALSE)

addWorksheet(wb, "data_updated")

#write the data table
writeDataTable(wb, "data_updated", x = ds,
               tableStyle = "none")

#5. add data validation
nr <- c(getNamedRegions(wb))
nr

cn <- data.frame(cname = names(ds),
                 cpos = seq(1:ncol(ds)))

for(i in 1:length(nr)) {

  x <- cn %>% dplyr::filter(cname == nr[i])

  dataValidation(wb, "data_updated", col = x$cpos, rows = 2:nrow(meta), type = "list", value = x$cname)

}
rm(i)

#6. add autofill formulas
#videoTrigger (column 13), nestStage (column 14), number_of_eggs (column 15), number_of_chicks (column 16), number_of_adults (column 17), fishPresent (23)
v1 <- c("IF(AND($C2=\"-\", $D2=\"-\"),\"NA\", IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), ISNUMBER(SEARCH(\"unusable\", $L2))), \"cannot determine\", \"\"))") 

for(i in c(13:17,23)){
  writeFormula(wb, sheet = "data_updated", x = v1, startCol = i, startRow = 2)
}
rm(i)

#parent1_ID, #parent1_activity
v2 <- c("IF(AND($C2=\"-\", $D2=\"-\"),\"NA\", IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), ISNUMBER(SEARCH(\"unusable\", $L2))), \"cannot determine\", IF(ISBLANK($Q2), \"\", IF($Q2=0, \"NA\", \"\"))))")

for(i in 18:19){
  writeFormula(wb, sheet = "data_updated", x = v1, startCol = i, startRow = 2)
}
rm(i)

##parent2_ID, #parent2_activity
v3 <- c("IF(AND($C2=\"-\", $D2=\"-\"),\"NA\", IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), ISNUMBER(SEARCH(\"unusable\", $L2))), \"cannot determine\", IF(ISBLANK($Q2), \"\", IF($Q2<2, \"NA\", \"\"))))")

for(i in 20:21){
  writeFormula(wb, sheet = "data_updated", x = v1, startCol = i, startRow = 2)
}
rm(i)

##chick_activity
v4 <- c("IF(AND($C2=\"-\", $D2=\"-\"),\"NA\", IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), ISNUMBER(SEARCH(\"unusable\", $L2))), \"cannot determine\", IF(ISBLANK($P2), \"\", IF($P2=0, \"NA\", \"\"))))")

writeFormula(wb, sheet = "data_updated", x = v1, startCol = 22, startRow = 2)

##fishSpecies
v5 <- c("IF(AND($C2=\"-\", $D2=\"-\"),\"NA\", IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), ISNUMBER(SEARCH(\"unusable\", $L2))), \"cannot determine\", IF($W2 = \"no\",  \"NA\", \"\")))")

writeFormula(wb, sheet = "data_updated", x = v1, startCol = 24, startRow = 2)


#7. save updated workbook
saveWorkbook(wb,
             file.path(outputbasepath, "data_processed", "PIGU_nestbox_video_data_temp.xlsx"),
             overwrite = TRUE)

