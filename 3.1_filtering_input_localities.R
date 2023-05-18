########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman

### Script name: 3.1_filtering_input_localities

### Goal of this Script: 

# Takes an input csv of locality records and filters them according to rules set by user
# Output is a csv 

### Notes:  

# need locality records in csv format - double check names of columns and set accordingly. 

### Date: May 12, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(tidyr)
library(tidyverse)

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## set working directory
setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

## Read in unfiltered locality data
LTSA_unfiltered <- read.csv("./input_localities/pre_processing/LTSA/LTSA_unfiltered.csv")
View(LTSA_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
LTSA_filtered <- LTSA_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
LTSA_filtered <- LTSA_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(LTSA_filtered, file = "./input_localities/pre_processing/LTSA/LTSA_filtered.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in unfiltered locality data
BCFR_unfiltered <- read.csv("./input_localities/pre_processing/BCFR/BCFR_unfiltered.csv")
View(BCFR_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
BCFR_filtered <- BCFR_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
BCFR_filtered <- BCFR_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(BCFR_filtered, file = "./input_localities/pre_processing/BCFR/BCFR_filtered.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in unfiltered locality data
CATO_unfiltered <- read.csv("./input_localities/pre_processing/CATO/CATO_unfiltered.csv")
View(CATO_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
CATO_filtered <- CATO_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
CATO_filtered <- CATO_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(CATO_filtered, file = "./input_localities/pre_processing/CATO/CATO_filtered.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in unfiltered locality data
CSFR_unfiltered <- read.csv("./input_localities/pre_processing/CSFR/CSFR_unfiltered.csv")
View(CSFR_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
CSFR_filtered <- CSFR_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
CSFR_filtered <- CSFR_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(CSFR_filtered, file = "./input_localities/pre_processing/CSFR/CSFR_filtered.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in unfiltered locality data
TISA_unfiltered <- read.csv("./input_localities/pre_processing/TISA/TISA_unfiltered.csv")
View(TISA_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
TISA_filtered <- TISA_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
TISA_filtered <- TISA_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(TISA_filtered, file = "./input_localities/pre_processing/TISA/TISA_filtered.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in unfiltered locality data
WETO_unfiltered <- read.csv("./input_localities/pre_processing/WETO/WETO_unfiltered.csv")
View(WETO_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
WETO_filtered <- WETO_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
WETO_filtered <- WETO_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(WETO_filtered, file = "./input_localities/pre_processing/WETO/WETO_filtered.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################



########################### END SCRIPT ###############################
