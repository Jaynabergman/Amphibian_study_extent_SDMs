########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman

### Goal of this Script: 

# Takes an input csv with all TGB localities for each species (separately) and subsets the localities for each study extent
# A new csv with TGB is saved for each study extent.  

### Notes:  

# Uses input localities "species_tgb_dupl_rm.csv"
# need locality records in csv format - double check names of columns. 

### Date: May 31, 2022

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
setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

## Read in input localities and set them as a special feature 

LTSA_tgb <- read.csv("./tgb/pre_processing/LTSA/LTSA_tgb_dupl_rm.csv")

LTSA_tgb <- st_as_sf(LTSA_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(LTSA_tgb))


## Read in all study extents for the species as shapefiles

LTSA_genetic <- st_read("./study_extents/model_subsets/LTSA/LTSA_genetic.shp")
LTSA_ecotone <- st_read("./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")
LTSA_political <- st_read("./study_extents/model_subsets/LTSA/LTSA_political.shp")
LTSA_range <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")

plot(st_geometry(LTSA_range))
plot(st_geometry(LTSA_genetic), add=TRUE)
plot(st_geometry(LTSA_ecotone), add=TRUE)
plot(st_geometry(LTSA_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

LTSA_genetic_pts_tgb <- st_join(LTSA_tgb, LTSA_genetic, join = st_within, left = FALSE)
plot(st_geometry(LTSA_genetic_pts_tgb))

LTSA_ecotone_pts_tgb <- st_join(LTSA_tgb, LTSA_ecotone, join = st_within, left = FALSE)
plot(st_geometry(LTSA_ecotone_pts_tgb))

LTSA_political_pts_tgb <- st_join(LTSA_tgb, LTSA_political, join = st_within, left = FALSE)
plot(st_geometry(LTSA_political_pts_tgb))

LTSA_range_pts_tgb <- st_join(LTSA_tgb,LTSA_range, join = st_within, left = FALSE)
 plot(st_geometry(LTSA_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Genetic
LTSA_genetic_pts_tgb <- LTSA_genetic_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_genetic_pts_tgb <- LTSA_genetic_pts_tgb %>%
  st_drop_geometry()

LTSA_genetic_pts_tgb <- subset(LTSA_genetic_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Ecotone
LTSA_ecotone_pts_tgb <- LTSA_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_ecotone_pts_tgb <- LTSA_ecotone_pts_tgb %>%
  st_drop_geometry()

LTSA_ecotone_pts_tgb <- subset(LTSA_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
LTSA_political_pts_tgb <- LTSA_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_political_pts_tgb <- LTSA_political_pts_tgb %>%
  st_drop_geometry()

LTSA_political_pts_tgb <- subset(LTSA_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
LTSA_range_pts_tgb <- LTSA_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_range_pts_tgb <- LTSA_range_pts_tgb %>%
  st_drop_geometry()

LTSA_range_pts_tgb <- subset(LTSA_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))


## Saving locality subsets as csv

write.csv(LTSA_genetic_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_genetic_tgb.csv", row.names = FALSE)
write.csv(LTSA_ecotone_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_ecotone_tgb.csv", row.names = FALSE)
write.csv(LTSA_political_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_political_tgb.csv", row.names = FALSE)
write.csv(LTSA_range_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_range_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in input localities and set them as a special feature 

BCFR_tgb <- read.csv("./tgb/pre_processing/BCFR/BCFR_tgb_dupl_rm.csv")

BCFR_tgb <- st_as_sf(BCFR_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(BCFR_tgb))


## Read in all study extents for the species as shapefiles

BCFR_ecotone <- st_read("./study_extents/model_subsets/BCFR/BCFR_ecotone.shp")
BCFR_political <- st_read("./study_extents/model_subsets/BCFR/BCFR_political.shp")
BCFR_range <- st_read("./study_extents/model_subsets/BCFR/BCFR_range.shp")

plot(st_geometry(BCFR_range))
plot(st_geometry(BCFR_ecotone), add=TRUE)
plot(st_geometry(BCFR_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

BCFR_ecotone_pts_tgb <- st_join(BCFR_tgb, BCFR_ecotone, join = st_within, left = FALSE)
plot(st_geometry(BCFR_ecotone_pts_tgb))

BCFR_political_pts_tgb <- st_join(BCFR_tgb, BCFR_political, join = st_within, left = FALSE)
plot(st_geometry(BCFR_political_pts_tgb))

BCFR_range_pts_tgb <- st_join(BCFR_tgb, BCFR_range, join = st_within, left = FALSE)
plot(st_geometry(BCFR_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
BCFR_ecotone_pts_tgb <- BCFR_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

BCFR_ecotone_pts_tgb <- BCFR_ecotone_pts_tgb %>%
  st_drop_geometry()

BCFR_ecotone_pts_tgb <- subset(BCFR_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
BCFR_political_pts_tgb <- BCFR_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

BCFR_political_pts_tgb <- BCFR_political_pts_tgb %>%
  st_drop_geometry()

BCFR_political_pts_tgb <- subset(BCFR_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
BCFR_range_pts_tgb <- BCFR_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

BCFR_range_pts_tgb <- BCFR_range_pts_tgb %>%
  st_drop_geometry()

BCFR_range_pts_tgb <- subset(BCFR_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))


## Saving locality subsets as csv

write.csv(BCFR_ecotone_pts_tgb, file = "./tgb/model_subsets/BCFR/BCFR_ecotone_tgb.csv", row.names = FALSE)
write.csv(BCFR_political_pts_tgb, file = "./tgb/model_subsets/BCFR/BCFR_political_tgb.csv", row.names = FALSE)
write.csv(BCFR_range_pts_tgb, file = "./tgb/model_subsets/BCFR/BCFR_range_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in input localities and set them as a special feature 

CATO_tgb <- read.csv("./tgb/pre_processing/CATO/CATO_tgb_dupl_rm.csv")

CATO_tgb <- st_as_sf(CATO_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(CATO_tgb))


## Read in all study extents for the species as shapefiles

CATO_ecotone <- st_read("./study_extents/model_subsets/CATO/CATO_ecotone.shp")
CATO_political <- st_read("./study_extents/model_subsets/CATO/CATO_political.shp")
CATO_range <- st_read("./study_extents/model_subsets/CATO/CATO_range.shp")

plot(st_geometry(CATO_range))
plot(st_geometry(CATO_ecotone), add=TRUE)
plot(st_geometry(CATO_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

CATO_ecotone_pts_tgb <- st_join(CATO_tgb, CATO_ecotone, join = st_within, left = FALSE)
plot(st_geometry(CATO_ecotone_pts_tgb))

CATO_political_pts_tgb <- st_join(CATO_tgb, CATO_political, join = st_within, left = FALSE)
plot(st_geometry(CATO_political_pts_tgb))

CATO_range_pts_tgb <- st_join(CATO_tgb, CATO_range, join = st_within, left = FALSE)
plot(st_geometry(CATO_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
CATO_ecotone_pts_tgb <- CATO_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CATO_ecotone_pts_tgb <- CATO_ecotone_pts_tgb %>%
  st_drop_geometry()

CATO_ecotone_pts_tgb <- subset(CATO_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
CATO_political_pts_tgb <- CATO_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CATO_political_pts_tgb <- CATO_political_pts_tgb %>%
  st_drop_geometry()

CATO_political_pts_tgb <- subset(CATO_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
CATO_range_pts_tgb <- CATO_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CATO_range_pts_tgb <- CATO_range_pts_tgb %>%
  st_drop_geometry()

CATO_range_pts_tgb <- subset(CATO_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))


## Saving locality subsets as csv

write.csv(CATO_ecotone_pts_tgb, file = "./tgb/model_subsets/CATO/CATO_ecotone_tgb.csv", row.names = FALSE)
write.csv(CATO_political_pts_tgb, file = "./tgb/model_subsets/CATO/CATO_political_tgb.csv", row.names = FALSE)
write.csv(CATO_range_pts_tgb, file = "./tgb/model_subsets/CATO/CATO_range_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in input localities and set them as a special feature 

CSFR_tgb <- read.csv("./tgb/pre_processing/CSFR/CSFR_tgb_dupl_rm.csv")

CSFR_tgb <- st_as_sf(CSFR_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(CSFR_tgb))


## Read in all study extents for the species as shapefiles

CSFR_ecotone <- st_read("./study_extents/model_subsets/CSFR/CSFR_ecotone.shp")
CSFR_political <- st_read("./study_extents/model_subsets/CSFR/CSFR_political.shp")
CSFR_range <- st_read("./study_extents/model_subsets/CSFR/CSFR_range.shp")

plot(st_geometry(CSFR_range))
plot(st_geometry(CSFR_ecotone), add=TRUE)
plot(st_geometry(CSFR_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

CSFR_ecotone_pts_tgb <- st_join(CSFR_tgb, CSFR_ecotone, join = st_within, left = FALSE)
plot(st_geometry(CSFR_ecotone_pts_tgb))

CSFR_political_pts_tgb <- st_join(CSFR_tgb, CSFR_political, join = st_within, left = FALSE)
plot(st_geometry(CSFR_political_pts_tgb))

CSFR_range_pts_tgb <- st_join(CSFR_tgb, CSFR_range, join = st_within, left = FALSE)
plot(st_geometry(CSFR_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
CSFR_ecotone_pts_tgb <- CSFR_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CSFR_ecotone_pts_tgb <- CSFR_ecotone_pts_tgb %>%
  st_drop_geometry()

CSFR_ecotone_pts_tgb <- subset(CSFR_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
CSFR_political_pts_tgb <- CSFR_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CSFR_political_pts_tgb <- CSFR_political_pts_tgb %>%
  st_drop_geometry()

CSFR_political_pts_tgb <- subset(CSFR_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
CSFR_range_pts_tgb <- CSFR_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CSFR_range_pts_tgb <- CSFR_range_pts_tgb %>%
  st_drop_geometry()

CSFR_range_pts_tgb <- subset(CSFR_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))


## Saving locality subsets as csv

write.csv(CSFR_ecotone_pts_tgb, file = "./tgb/model_subsets/CSFR/CSFR_ecotone_tgb.csv", row.names = FALSE)
write.csv(CSFR_political_pts_tgb, file = "./tgb/model_subsets/CSFR/CSFR_political_tgb.csv", row.names = FALSE)
write.csv(CSFR_range_pts_tgb, file = "./tgb/model_subsets/CSFR/CSFR_range_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in input localities and set them as a special feature 

TISA_tgb <- read.csv("./tgb/pre_processing/TISA/TISA_tgb_dupl_rm.csv")

TISA_tgb <- st_as_sf(TISA_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(TISA_tgb))


## Read in all study extents for the species as shapefiles

TISA_ecotone <- st_read("./study_extents/model_subsets/TISA/TISA_ecotone.shp")
TISA_political <- st_read("./study_extents/model_subsets/TISA/TISA_political.shp")
TISA_range <- st_read("./study_extents/model_subsets/TISA/TISA_range.shp")

plot(st_geometry(TISA_range))
plot(st_geometry(TISA_ecotone), add=TRUE)
plot(st_geometry(TISA_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

TISA_ecotone_pts_tgb <- st_join(TISA_tgb, TISA_ecotone, join = st_within, left = FALSE)
plot(st_geometry(TISA_ecotone_pts_tgb))

TISA_political_pts_tgb <- st_join(TISA_tgb, TISA_political, join = st_within, left = FALSE)
plot(st_geometry(TISA_political_pts_tgb))

TISA_range_pts_tgb <- st_join(TISA_tgb, TISA_range, join = st_within, left = FALSE)
plot(st_geometry(TISA_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
TISA_ecotone_pts_tgb <- TISA_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

TISA_ecotone_pts_tgb <- TISA_ecotone_pts_tgb %>%
  st_drop_geometry()

TISA_ecotone_pts_tgb <- subset(TISA_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
TISA_political_pts_tgb <- TISA_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

TISA_political_pts_tgb <- TISA_political_pts_tgb %>%
  st_drop_geometry()

TISA_political_pts_tgb <- subset(TISA_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
TISA_range_pts_tgb <- TISA_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

TISA_range_pts_tgb <- TISA_range_pts_tgb %>%
  st_drop_geometry()

TISA_range_pts_tgb <- subset(TISA_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))


## Saving locality subsets as csv

write.csv(TISA_ecotone_pts_tgb, file = "./tgb/model_subsets/TISA/TISA_ecotone_tgb.csv", row.names = FALSE)
write.csv(TISA_political_pts_tgb, file = "./tgb/model_subsets/TISA/TISA_political_tgb.csv", row.names = FALSE)
write.csv(TISA_range_pts_tgb, file = "./tgb/model_subsets/TISA/TISA_range_tgb.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in input localities and set them as a special feature 

WETO_tgb <- read.csv("./tgb/pre_processing/WETO/WETO_tgb_dupl_rm.csv")

WETO_tgb <- st_as_sf(WETO_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(WETO_tgb))


## Read in all study extents for the species as shapefiles

WETO_genetic <- st_read("./study_extents/model_subsets/WETO/WETO_genetic.shp")
WETO_ecotone <- st_read("./study_extents/model_subsets/WETO/WETO_ecotone.shp")
WETO_political <- st_read("./study_extents/model_subsets/WETO/WETO_political.shp")
WETO_range <- st_read("./study_extents/model_subsets/WETO/WETO_range.shp")

plot(st_geometry(WETO_range))
plot(st_geometry(WETO_genetic), add=TRUE)
plot(st_geometry(WETO_ecotone), add=TRUE)
plot(st_geometry(WETO_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

WETO_genetic_pts_tgb <- st_join(WETO_tgb, WETO_genetic, join = st_within, left = FALSE)
plot(st_geometry(WETO_genetic_pts_tgb))

WETO_ecotone_pts_tgb <- st_join(WETO_tgb, WETO_ecotone, join = st_within, left = FALSE)
plot(st_geometry(WETO_ecotone_pts_tgb))

WETO_political_pts_tgb <- st_join(WETO_tgb, WETO_political, join = st_within, left = FALSE)
plot(st_geometry(WETO_political_pts_tgb))

WETO_range_pts_tgb <- st_join(WETO_tgb, WETO_range, join = st_within, left = FALSE)
plot(st_geometry(WETO_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Genetic
WETO_genetic_pts_tgb <- WETO_genetic_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_genetic_pts_tgb <- WETO_genetic_pts_tgb %>%
  st_drop_geometry()

WETO_genetic_pts_tgb <- subset(WETO_genetic_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Ecotone
WETO_ecotone_pts_tgb <- WETO_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_ecotone_pts_tgb <- WETO_ecotone_pts_tgb %>%
  st_drop_geometry()

WETO_ecotone_pts_tgb <- subset(WETO_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
WETO_political_pts_tgb <- WETO_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_political_pts_tgb <- WETO_political_pts_tgb %>%
  st_drop_geometry()

WETO_political_pts_tgb <- subset(WETO_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
WETO_range_pts_tgb <- WETO_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_range_pts_tgb <- WETO_range_pts_tgb %>%
  st_drop_geometry()

WETO_range_pts_tgb <- subset(WETO_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))


## Saving locality subsets as csv

write.csv(WETO_genetic_pts_tgb, file = "./tgb/model_subsets/WETO/WETO_genetic_tgb.csv", row.names = FALSE)
write.csv(WETO_ecotone_pts_tgb, file = "./tgb/model_subsets/WETO/WETO_ecotone_tgb.csv", row.names = FALSE)
write.csv(WETO_political_pts_tgb, file = "./tgb/model_subsets/WETO/WETO_political_tgb.csv", row.names = FALSE)
write.csv(WETO_range_pts_tgb, file = "./tgb/model_subsets/WETO/WETO_range_tgb.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################