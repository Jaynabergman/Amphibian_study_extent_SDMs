########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman

### Goal of this Script: 

# Takes an input csv with multiple amphibian species and filters by user set columns
# For each species a separte csv is saved that excludes the target species. 

### Notes:  

#need locality records in csv format - double check names of columns. 

### Date: May 28, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(tidyr)
library(tidyverse)
library(sf)

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## set working directory
setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## read in unfiltered csv of all target-group background localities
tgb <- read.csv("./tgb/TGB_amphibs_unfiltered.csv")
View(tgb)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m
## removing exact duplicate records based on lat&long

tgb_filtered <- tgb %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000) %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered tgb points with all Amphibians

write.csv(tgb_filtered, file = "./tgb/tgb_filtered.csv", row.names = FALSE)


################################# LTSA ################################

## filtering out target species (LTSA)

LTSA_tgb <- tgb_filtered %>%
  filter(CommonName != "Long-toed Salamander")


## write new csv for filtered tgb points without focal species 

write.csv(LTSA_tgb, file = "./tgb/pre_processing/LTSA/LTSA_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## filtering out target species (BCFR)

BCFR_tgb <- tgb_filtered %>%
  filter(CommonName != "Boreal Chorus Frog")


## write new csv for filtered tgb points without focal species 

write.csv(BCFR_tgb, file = "./tgb/pre_processing/BCFR/BCFR_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## filtering out target species (BCFR)

CATO_tgb <- tgb_filtered %>%
  filter(CommonName != "Canadian Toad")


## write new csv for filtered tgb points without focal species 

write.csv(CATO_tgb, file = "./tgb/pre_processing/CATO/CATO_tgb.csv", row.names = FALSE)

########################### END SECTION ###############################

################################# CSFR ################################

## filtering out target species (CSFR)

CSFR_tgb <- tgb_filtered %>%
  filter(CommonName != "Columbia Spotted Frog")


## write new csv for filtered tgb points without focal species 

write.csv(CSFR_tgb, file = "./tgb/pre_processing/CSFR/CSFR_tgb.csv", row.names = FALSE)

########################### END SECTION ###############################

################################# TISA ################################

## filtering out target species (TISA)

TISA_tgb <- tgb_filtered %>%
  filter(CommonName != "Western Tiger Salamander") %>%
  filter(CommonName != "Eastern Tiger Salamander")


## write new csv for filtered tgb points without focal species 

write.csv(TISA_tgb, file = "./tgb/pre_processing/TISA/TISA_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## filtering out target species (WETO)

WETO_tgb <- tgb_filtered %>%
  filter(CommonName != "Western Toad") 


## write new csv for filtered tgb points without focal species 

write.csv(WETO_tgb, file = "./tgb/pre_processing/WETO/WETO_tgb.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
