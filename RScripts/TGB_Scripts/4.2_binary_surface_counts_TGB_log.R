########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: 4.2_binary_surface_counts_TGB_log.R

### Goal of this Script: 

# Makes frequency tables of the amount of 0s and 1s for each binary surface for the region of interest (WLNP & EINP) for each species

### Notes:  

# Using logistic surfaces from TGB models

### Date: October 19, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA


########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


##### WLNP #####

## Read in list of raster names 

WLNP_names_list <- gsub(pattern=".tif", replacement="", list.files("./tgb/binary_surfaces/WLNP/cropped/", pattern='.tif$', all.files=TRUE, full.names=FALSE))
WLNP_names_list


## Loop through each raster in the stack using the raster name list to make an individual frequency table and save with corresponding name

for (i in 1:length(WLNP_names_list)){
  
  
  ## read in binary raster cropped to WLNP
  
  prediction_surface <- rast(paste("./tgb/binary_surfaces/WLNP/cropped/",WLNP_names_list[i],".tif", sep=""))
  
  
  ## Making frequency table (value and count)
  
  Frequency_table <- terra::freq(prediction_surface, bylayer=FALSE)
  Frequency_table <- as.data.frame(Frequency_table)
  
  
  ## Saving frequency table as csv 
  
  write.csv(Frequency_table, file = (paste("./tgb/binary_surfaces/WLNP/binary_surface_counts/",WLNP_names_list[i],"_counts.csv", sep="")), row.names = FALSE)
  
}


##### einp #####

## Read in list of raster names 

einp_names_list <- gsub(pattern=".tif", replacement="", list.files("./tgb/binary_surfaces/einp/cropped/", pattern='.tif$', all.files=TRUE, full.names=FALSE))
einp_names_list


## Loop through each raster in the stack using the raster name list to make an individual frequency table and save with corresponding name

for (i in 1:length(einp_names_list)){
  
  
  ## read in binary raster cropped to einp
  
  prediction_surface <- rast(paste("./tgb/binary_surfaces/einp/cropped/",einp_names_list[i],".tif", sep=""))
  
  
  ## Making frequency table (value and count)
  
  Frequency_table <- terra::freq(prediction_surface, bylayer=FALSE)
  Frequency_table <- as.data.frame(Frequency_table)
  
  
  ## Saving frequency table as csv 
  
  write.csv(Frequency_table, file = (paste("./tgb/binary_surfaces/einp/binary_surface_counts/",einp_names_list[i],"_counts.csv", sep="")), row.names = FALSE)
  
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################

