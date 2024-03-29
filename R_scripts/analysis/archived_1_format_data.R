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
drive_download(file = "PIGU_data_template_do_not_edit.xlsx",
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

addWorksheet(wb, year)

#write the data table
writeDataTable(wb, year, x = ds,
               tableStyle = "none")

#5. add data validation
nr <- c(getNamedRegions(wb))
nr

cn <- data.frame(cname = names(ds),
                 cpos = seq(1:ncol(ds)))

for(i in 1:length(nr)) {

  x <- cn %>% dplyr::filter(cname == nr[i])

  dataValidation(wb, year, col = x$cpos, rows = 2:nrow(meta), type = "list", value = x$cname)

}
rm(i)

#6. add autofill formulas
#videoTrigger (column 13), nestStage (column 14), number_of_adults (column 17), fishPresent (23)

v1 <- c("IF(OR($K2=\"test video\", $L2=\"test video\"), \"NA\", 
        IF(AND($C2=\"-\", $D2=\"-\"), \"NA\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), ISNUMBER(SEARCH(\"unusable\",$L2))), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), $C2=\"-\"), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), $D2=\"-\"), \"cannot determine\", \"\")))))")

for(i in c(13:14,17,23)){
  writeFormula(wb, sheet = year, x = v1, startCol = i, startRow = 2)
}
rm(i)

#parent1_ID (column 18), #parent1_activity (column 19)
v2 <- c("IF(OR($K2=\"test video\", $L2=\"test video\"), \"NA\", 
        IF(AND($C2=\"-\", $D2=\"-\"), \"NA\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), ISNUMBER(SEARCH(\"unusable\",$L2))), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), $C2=\"-\"), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), $D2=\"-\"), \"cannot determine\", 
        IF(ISBLANK($Q2), \"\", IF($Q2=0, \"NA\", \"\")))))))")

for(i in 18:19){
  writeFormula(wb, sheet = year, x = v2, startCol = i, startRow = 2)
}
rm(i)

##parent2_ID (column 20), #parent2_activity (column 21)
v3 <- c("IF(OR($K2=\"test video\", $L2=\"test video\"), \"NA\", 
        IF(AND($C2=\"-\", $D2=\"-\"), \"NA\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), ISNUMBER(SEARCH(\"unusable\",$L2))), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), $C2=\"-\"), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), $D2=\"-\"), \"cannot determine\", 
        IF(ISBLANK($Q2), \"\", IF($Q2<2, \"NA\", \"\")))))))")


for(i in 20:21){
  writeFormula(wb, sheet = year, x = v3, startCol = i, startRow = 2)
}
rm(i)

##chick_activity (column 22)
v4 <- c("IF(OR($K2=\"test video\", $L2=\"test video\"), \"NA\", 
        IF(AND($C2=\"-\", $D2=\"-\"), \"NA\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), ISNUMBER(SEARCH(\"unusable\",$L2))), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), $C2=\"-\"), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), $D2=\"-\"), \"cannot determine\", 
        IF(ISBLANK($P2), \"\", IF($P2=0, \"NA\", \"\")))))))")


writeFormula(wb, sheet = year, x = v4, startCol = 22, startRow = 2)

##fishSpecies (column 24)
v5 <- c("IF(OR($K2=\"test video\", $L2=\"test video\"), \"NA\", 
        IF(AND($C2=\"-\", $D2=\"-\"), \"NA\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), ISNUMBER(SEARCH(\"unusable\",$L2))), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), $C2=\"-\"), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), $D2=\"-\"), \"cannot determine\", 
        IF($W2 = \"no\",  \"NA\", \"\"))))))")

writeFormula(wb, sheet = year, x = v5, startCol = 24, startRow = 2)

#number_of_eggs (column 15), number_of_chicks (column 16)
v6 <- c("IF(OR($K2=\"test video\", $L2=\"test video\"), \"NA\", 
        IF(AND($C2=\"-\", $D2=\"-\"), \"NA\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), ISNUMBER(SEARCH(\"unusable\",$L2))), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\",$K2)), $C2=\"-\"), \"cannot determine\", 
        IF(AND(ISNUMBER(SEARCH(\"unusable\", $K2)), $D2=\"-\"), \"cannot determine\", 
        IF($N2=\"empty\", 0, \"\"))))))")

for(i in c(15:16)){
  writeFormula(wb, sheet = year, x = v6, startCol = i, startRow = 2)
}
rm(i)


#video_quality1 (column 11)
v7 <- c("IF($C2=\"-\", \"video missing\", \"\")")
writeFormula(wb, sheet = year, x = v7, startCol = 11, startRow = 2)

#video_quality2 (column 12)
v8 <- c("IF($D2=\"-\", \"video missing\", \"\")")
writeFormula(wb, sheet = year, x = v8, startCol = 12, startRow = 2)

#7. remove data worksheet
removeWorksheet(wb, sheet = "data")

#8. save updated workbook
saveWorkbook(wb,
             file.path(outputbasepath, "data_processed", paste0("PIGU_nestbox_video_data_", year, ".xlsx")),
             overwrite = TRUE)

