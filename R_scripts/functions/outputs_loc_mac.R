###############################################################################
#Set path location of r output files to a folder in the 
#google drive 'R project outputs' folder 
#2022-12-06
###############################################################################
outputs_loc_mac <- function(outputfolder) {
  file.path("~", "Google Drive", "My Drive", "Personal", "R stuff", "R project outputs", outputfolder)
}
