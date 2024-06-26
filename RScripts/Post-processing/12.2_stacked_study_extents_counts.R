########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: 12.2_stacked_study_extents_counts.R

### Goal of this Script: 

#  Makes frequency tables of the number of models that agree on if a cell is suitable for each species in the region of interest (WLNP)

### Notes:  

# Uses a loop for each region of interest (WLNP)

### Date: November 16, 2023

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(raster)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA


########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


########################### WLNP ##############################

## Read in list of raster names 

WLNP_names_list <- gsub(pattern=".tif", replacement="", list.files("./binary_surfaces/surfaces/WLNP/study_extent_stacked/", pattern='.tif$', all.files=TRUE, full.names=FALSE))
WLNP_names_list


## Loop through each raster in the stack using the raster name list to make an individual frequency table and save with corresponding name

for (i in 1:length(WLNP_names_list)){
  
  
  ## read in binary raster cropped to WLNP
  
  prediction_surface <- rast(paste("./binary_surfaces/surfaces/WLNP/study_extent_stacked/",WLNP_names_list[i],".tif", sep=""))
  
  
  ## Making frequency table (value and count)
  
  Frequency_table <- terra::freq(prediction_surface, bylayer=FALSE)
  Frequency_table <- as.data.frame(Frequency_table)
  
  
  ## Saving frequency table as csv 
  
  write.csv(Frequency_table, file = (paste("./binary_surfaces/surfaces/WLNP/study_extent_stacked/counts/",WLNP_names_list[i],"_counts.csv", sep="")), row.names = FALSE)
  
}

########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
