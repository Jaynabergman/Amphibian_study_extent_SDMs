########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman 

### Script name: 8.2_binary_surface_counts

### Goal of this Script: 

# Makes frequency tables of the amount of 0s and 1s for each binary surface for the region of interest (WLNP & EINP) for each species

### Notes:  

#

### Date: December 10, 2022

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


##### WLNP #####

## Read in list of raster names 

WLNP_names_list <- gsub(pattern=".tif", replacement="", list.files("./binary_surfaces/surfaces/WLNP/cropped/", pattern='.tif$', all.files=TRUE, full.names=FALSE))
WLNP_names_list


## Loop through each raster in the stack using the raster name list to make an individual frequency table and save with corresponding name

for (i in 1:length(WLNP_names_list)){
  
  
  ## read in binary raster cropped to WLNP
  
  prediction_surface <- rast(paste("./binary_surfaces/surfaces/WLNP/cropped/",WLNP_names_list[i],".tif", sep=""))
  
  
  ## Making frequency table (value and count)
  
  Frequency_table <- terra::freq(prediction_surface, bylayer=FALSE)
  Frequency_table <- as.data.frame(Frequency_table)
  
  
  ## Saving frequency table as csv 
  
  write.csv(Frequency_table, file = (paste("./binary_surfaces/binary_counts/",WLNP_names_list[i],"_counts.csv", sep="")), row.names = FALSE)

}



##### EINP #####

## Read in list of raster names 

EINP_names_list <- gsub(pattern=".tif", replacement="", list.files("./binary_surfaces/surfaces/EINP/cropped/", pattern='.tif$', all.files=TRUE, full.names=FALSE))
EINP_names_list


## Loop through each raster in the stack using the raster name list to make an individual frequency table and save with corresponding name

for (i in 1:length(EINP_names_list)){
  
  
  ## read in binary raster cropped to EINP
  
  prediction_surface <- rast(paste("./binary_surfaces/surfaces/EINP/cropped/",EINP_names_list[i],".tif", sep=""))
  
  
  ## Making frequency table (value and count)
  
  Frequency_table <- terra::freq(prediction_surface, bylayer=FALSE)
  Frequency_table <- as.data.frame(Frequency_table)
  
  
  ## Saving frequency table as csv 
  
  write.csv(Frequency_table, file = (paste("./binary_surfaces/binary_counts/",EINP_names_list[i],"_counts.csv", sep="")), row.names = FALSE)
  
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
