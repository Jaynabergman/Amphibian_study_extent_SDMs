########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman

### Script name: 3.5_subsetting_input_localities

### Goal of this Script: 

# Takes an input csv with all input localities for each species (separately) and subsets the localities for each study extent
# A new csv is saved for each study extent.  

### Notes:  

# Uses input localities "species_dupl_WLNP_rm.csv"
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

LTSA_pts <- read.csv("./input_localities/pre_processing/LTSA/LTSA_dupl_WLNP_rm.csv")

LTSA_pts <- st_as_sf(LTSA_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(LTSA_pts))


## Read in all study extents for the species as shapefiles

LTSA_ecotone <- st_read("./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")
LTSA_political <- st_read("./study_extents/model_subsets/LTSA/LTSA_political.shp")
LTSA_range <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")

plot(st_geometry(LTSA_range))
plot(st_geometry(LTSA_ecotone), add=TRUE)
plot(st_geometry(LTSA_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

LTSA_ecotone_pts <- st_join(LTSA_pts, LTSA_ecotone, join = st_within, left = FALSE)
plot(st_geometry(LTSA_ecotone_pts))

LTSA_political_pts <- st_join(LTSA_pts, LTSA_political, join = st_within, left = FALSE)
plot(st_geometry(LTSA_political_pts))

LTSA_range_pts <- st_join(LTSA_pts, LTSA_range, join = st_within, left = FALSE)
plot(st_geometry(LTSA_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
LTSA_ecotone_pts <- LTSA_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_ecotone_pts <- LTSA_ecotone_pts %>%
  st_drop_geometry()

LTSA_ecotone_pts <- subset(LTSA_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
LTSA_political_pts <- LTSA_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_political_pts <- LTSA_political_pts %>%
  st_drop_geometry()

LTSA_political_pts <- subset(LTSA_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
LTSA_range_pts <- LTSA_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_range_pts <- LTSA_range_pts %>%
  st_drop_geometry()

LTSA_range_pts <- subset(LTSA_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))


## Saving locality subsets as csv

write.csv(LTSA_ecotone_pts, file = "./input_localities/model_subsets/LTSA/LTSA_ecotone_locs.csv", row.names = FALSE)
write.csv(LTSA_political_pts, file = "./input_localities/model_subsets/LTSA/LTSA_political_locs.csv", row.names = FALSE)
write.csv(LTSA_range_pts, file = "./input_localities/model_subsets/LTSA/LTSA_range_locs.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# BCFR ################################

## Read in input localities and set them as a special feature 

BCFR_pts <- read.csv("./input_localities/pre_processing/BCFR/BCFR_dupl_WLNP_rm.csv")

BCFR_pts <- st_as_sf(BCFR_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(BCFR_pts))


## Read in all study extents for the species as shapefiles

BCFR_ecotone <- st_read("./study_extents/model_subsets/BCFR/BCFR_ecotone.shp")
BCFR_political <- st_read("./study_extents/model_subsets/BCFR/BCFR_political.shp")
BCFR_range <- st_read("./study_extents/model_subsets/BCFR/BCFR_range.shp")

plot(st_geometry(BCFR_range))
plot(st_geometry(BCFR_ecotone), add=TRUE)
plot(st_geometry(BCFR_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

BCFR_ecotone_pts <- st_join(BCFR_pts, BCFR_ecotone, join = st_within, left = FALSE)
plot(st_geometry(BCFR_ecotone_pts))

BCFR_political_pts <- st_join(BCFR_pts, BCFR_political, join = st_within, left = FALSE)
plot(st_geometry(BCFR_political_pts))

BCFR_range_pts <- st_join(BCFR_pts, BCFR_range, join = st_within, left = FALSE)
plot(st_geometry(BCFR_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
BCFR_ecotone_pts <- BCFR_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

BCFR_ecotone_pts <- BCFR_ecotone_pts %>%
  st_drop_geometry()

BCFR_ecotone_pts <- subset(BCFR_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
BCFR_political_pts <- BCFR_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

BCFR_political_pts <- BCFR_political_pts %>%
  st_drop_geometry()

BCFR_political_pts <- subset(BCFR_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
BCFR_range_pts <- BCFR_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

BCFR_range_pts <- BCFR_range_pts %>%
  st_drop_geometry()

BCFR_range_pts <- subset(BCFR_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))


## Saving locality subsets as csv

write.csv(BCFR_ecotone_pts, file = "./input_localities/model_subsets/BCFR/BCFR_ecotone_locs.csv", row.names = FALSE)
write.csv(BCFR_political_pts, file = "./input_localities/model_subsets/BCFR/BCFR_political_locs.csv", row.names = FALSE)
write.csv(BCFR_range_pts, file = "./input_localities/model_subsets/BCFR/BCFR_range_locs.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CATO ################################

## Read in input localities and set them as a special feature 

CATO_pts <- read.csv("./input_localities/pre_processing/CATO/CATO_dupl_rm.csv")

CATO_pts <- st_as_sf(CATO_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(CATO_pts))


## Read in all study extents for the species as shapefiles

CATO_ecotone <- st_read("./study_extents/model_subsets/CATO/CATO_ecotone.shp")
CATO_political <- st_read("./study_extents/model_subsets/CATO/CATO_political.shp")
CATO_range <- st_read("./study_extents/model_subsets/CATO/CATO_range.shp")

plot(st_geometry(CATO_range))
plot(st_geometry(CATO_ecotone), add=TRUE)
plot(st_geometry(CATO_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

CATO_ecotone_pts <- st_join(CATO_pts, CATO_ecotone, join = st_within, left = FALSE)
plot(st_geometry(CATO_ecotone_pts))

CATO_political_pts <- st_join(CATO_pts, CATO_political, join = st_within, left = FALSE)
plot(st_geometry(CATO_political_pts))

CATO_range_pts <- st_join(CATO_pts, CATO_range, join = st_within, left = FALSE)
plot(st_geometry(CATO_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
CATO_ecotone_pts <- CATO_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CATO_ecotone_pts <- CATO_ecotone_pts %>%
  st_drop_geometry()

CATO_ecotone_pts <- subset(CATO_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
CATO_political_pts <- CATO_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CATO_political_pts <- CATO_political_pts %>%
  st_drop_geometry()

CATO_political_pts <- subset(CATO_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
CATO_range_pts <- CATO_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CATO_range_pts <- CATO_range_pts %>%
  st_drop_geometry()

CATO_range_pts <- subset(CATO_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))


## Saving locality subsets as csv

write.csv(CATO_ecotone_pts, file = "./input_localities/model_subsets/CATO/CATO_ecotone_locs.csv", row.names = FALSE)
write.csv(CATO_political_pts, file = "./input_localities/model_subsets/CATO/CATO_political_locs.csv", row.names = FALSE)
write.csv(CATO_range_pts, file = "./input_localities/model_subsets/CATO/CATO_range_locs.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CSFR ################################

## Read in input localities and set them as a special feature 

CSFR_pts <- read.csv("./input_localities/pre_processing/CSFR/CSFR_dupl_WLNP_rm.csv")

CSFR_pts <- st_as_sf(CSFR_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(CSFR_pts))


## Read in all study extents for the species as shapefiles

CSFR_ecotone <- st_read("./study_extents/model_subsets/CSFR/CSFR_ecotone.shp")
CSFR_political <- st_read("./study_extents/model_subsets/CSFR/CSFR_political.shp")
CSFR_range <- st_read("./study_extents/model_subsets/CSFR/CSFR_range.shp")

plot(st_geometry(CSFR_range))
plot(st_geometry(CSFR_ecotone), add=TRUE)
plot(st_geometry(CSFR_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

CSFR_ecotone_pts <- st_join(CSFR_pts, CSFR_ecotone, join = st_within, left = FALSE)
plot(st_geometry(CSFR_ecotone_pts))

CSFR_political_pts <- st_join(CSFR_pts, CSFR_political, join = st_within, left = FALSE)
plot(st_geometry(CSFR_political_pts))

CSFR_range_pts <- st_join(CSFR_pts, CSFR_range, join = st_within, left = FALSE)
plot(st_geometry(CSFR_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
CSFR_ecotone_pts <- CSFR_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CSFR_ecotone_pts <- CSFR_ecotone_pts %>%
  st_drop_geometry()

CSFR_ecotone_pts <- subset(CSFR_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
CSFR_political_pts <- CSFR_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CSFR_political_pts <- CSFR_political_pts %>%
  st_drop_geometry()

CSFR_political_pts <- subset(CSFR_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
CSFR_range_pts <- CSFR_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

CSFR_range_pts <- CSFR_range_pts %>%
  st_drop_geometry()

CSFR_range_pts <- subset(CSFR_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))


## Saving locality subsets as csv

write.csv(CSFR_ecotone_pts, file = "./input_localities/model_subsets/CSFR/CSFR_ecotone_locs.csv", row.names = FALSE)
write.csv(CSFR_political_pts, file = "./input_localities/model_subsets/CSFR/CSFR_political_locs.csv", row.names = FALSE)
write.csv(CSFR_range_pts, file = "./input_localities/model_subsets/CSFR/CSFR_range_locs.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# TISA ################################

## Read in input localities and set them as a special feature 

TISA_pts <- read.csv("./input_localities/pre_processing/TISA/TISA_dupl_rm.csv")

TISA_pts <- st_as_sf(TISA_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(TISA_pts))


## Read in all study extents for the species as shapefiles

TISA_ecotone <- st_read("./study_extents/model_subsets/TISA/TISA_ecotone.shp")
TISA_political <- st_read("./study_extents/model_subsets/TISA/TISA_political.shp")
TISA_range <- st_read("./study_extents/model_subsets/TISA/TISA_range.shp")

plot(st_geometry(TISA_range))
plot(st_geometry(TISA_ecotone), add=TRUE)
plot(st_geometry(TISA_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

TISA_ecotone_pts <- st_join(TISA_pts, TISA_ecotone, join = st_within, left = FALSE)
plot(st_geometry(TISA_ecotone_pts))

TISA_political_pts <- st_join(TISA_pts, TISA_political, join = st_within, left = FALSE)
plot(st_geometry(TISA_political_pts))

TISA_range_pts <- st_join(TISA_pts, TISA_range, join = st_within, left = FALSE)
plot(st_geometry(TISA_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
TISA_ecotone_pts <- TISA_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

TISA_ecotone_pts <- TISA_ecotone_pts %>%
  st_drop_geometry()

TISA_ecotone_pts <- subset(TISA_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
TISA_political_pts <- TISA_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

TISA_political_pts <- TISA_political_pts %>%
  st_drop_geometry()

TISA_political_pts <- subset(TISA_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
TISA_range_pts <- TISA_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

TISA_range_pts <- TISA_range_pts %>%
  st_drop_geometry()

TISA_range_pts <- subset(TISA_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))


## Saving locality subsets as csv

write.csv(TISA_ecotone_pts, file = "./input_localities/model_subsets/TISA/TISA_ecotone_locs.csv", row.names = FALSE)
write.csv(TISA_political_pts, file = "./input_localities/model_subsets/TISA/TISA_political_locs.csv", row.names = FALSE)
write.csv(TISA_range_pts, file = "./input_localities/model_subsets/TISA/TISA_range_locs.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# WETO ################################

## Read in input localities and set them as a special feature 

WETO_pts <- read.csv("./input_localities/pre_processing/WETO/WETO_dupl_WLNP_rm.csv")

WETO_pts <- st_as_sf(WETO_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(WETO_pts))


## Read in all study extents for the species as shapefiles

WETO_ecotone <- st_read("./study_extents/model_subsets/WETO/WETO_ecotone.shp")
WETO_political <- st_read("./study_extents/model_subsets/WETO/WETO_political.shp")
WETO_range <- st_read("./study_extents/model_subsets/WETO/WETO_range.shp")

plot(st_geometry(WETO_range))
plot(st_geometry(WETO_ecotone), add=TRUE)
plot(st_geometry(WETO_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

WETO_ecotone_pts <- st_join(WETO_pts, WETO_ecotone, join = st_within, left = FALSE)
plot(st_geometry(WETO_ecotone_pts))

WETO_political_pts <- st_join(WETO_pts, WETO_political, join = st_within, left = FALSE)
plot(st_geometry(WETO_political_pts))

WETO_range_pts <- st_join(WETO_pts, WETO_range, join = st_within, left = FALSE)
plot(st_geometry(WETO_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Ecotone
WETO_ecotone_pts <- WETO_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_ecotone_pts <- WETO_ecotone_pts %>%
  st_drop_geometry()

WETO_ecotone_pts <- subset(WETO_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
WETO_political_pts <- WETO_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_political_pts <- WETO_political_pts %>%
  st_drop_geometry()

WETO_political_pts <- subset(WETO_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
WETO_range_pts <- WETO_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

WETO_range_pts <- WETO_range_pts %>%
  st_drop_geometry()

WETO_range_pts <- subset(WETO_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))


## Saving locality subsets as csv

write.csv(WETO_genetic_pts, file = "./input_localities/model_subsets/WETO/WETO_genetic_locs.csv", row.names = FALSE)
write.csv(WETO_ecotone_pts, file = "./input_localities/model_subsets/WETO/WETO_ecotone_locs.csv", row.names = FALSE)
write.csv(WETO_political_pts, file = "./input_localities/model_subsets/WETO/WETO_political_locs.csv", row.names = FALSE)
write.csv(WETO_range_pts, file = "./input_localities/model_subsets/WETO/WETO_range_locs.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
