########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# reads in localities with extracted environmental variable values, creates x amount of kfolds, saves as separate csvs

### Notes:  

# 

### Date: June 6, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)

rm(list=ls())

########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


################################# LTSA ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data
  
LTSA_range_occ <- read.csv("./extracted_envi_vars_values/LTSA/range/LTSA_locs_range.csv")
LTSA_range_tgb <- read.csv("./extracted_envi_vars_values/LTSA/range/LTSA_tgb_range.csv")
  
  
## creating kfolds (occ & tgb)
  
LTSA_range_fold <- kfold(LTSA_range_occ, k=5)
LTSA_range_fold_tgb <- kfold(LTSA_range_tgb, k=5)
  
  
### For loop to run through multiple kfolds ###
  
x <- c(1,2,3,4,5)
  
for (i in 1:length(x)){
    
  ## creating testing and training sets (holding 20%)
    
  LTSA_range_occtest <- LTSA_range_occ[LTSA_range_fold == i, ]
  LTSA_range_occtrain <- LTSA_range_occ[LTSA_range_fold != i, ]
    
  LTSA_range_tgbtest <- LTSA_range_tgb[LTSA_range_fold_tgb == i, ]
  LTSA_range_tgbtrain <- LTSA_range_tgb[LTSA_range_fold_tgb != i, ]
    
    
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
    
  LTSA_range_fulltestingdat <- rbind(LTSA_range_occtest, LTSA_range_tgbtest)
  LTSA_range_fulltrainingdat <- rbind(LTSA_range_occtrain, LTSA_range_tgbtrain)
    
    
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
    
  write.csv(LTSA_range_occtest, file=paste("./kfolds/Random_bg/LTSA/range/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_range_occtrain, file=paste("./kfolds/Random_bg/LTSA/range/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
    
  write.csv(LTSA_range_fulltestingdat, file=paste("./kfolds/SWD_mode/LTSA/range/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_range_fulltrainingdat, file=paste("./kfolds/SWD_mode/LTSA/range/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
    
}


## political study extent 

## Read input localities and tgb points with extracted envi data

LTSA_political_occ <- read.csv("./extracted_envi_vars_values/LTSA/political/LTSA_locs_political.csv")
LTSA_political_tgb <- read.csv("./extracted_envi_vars_values/LTSA/political/LTSA_tgb_political.csv")


## creating kfolds (occ & tgb)

LTSA_political_fold <- kfold(LTSA_political_occ, k=5)
LTSA_political_fold_tgb <- kfold(LTSA_political_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_political_occtest <- LTSA_political_occ[LTSA_political_fold == i, ]
  LTSA_political_occtrain <- LTSA_political_occ[LTSA_political_fold != i, ]
  
  LTSA_political_tgbtest <- LTSA_political_tgb[LTSA_political_fold_tgb == i, ]
  LTSA_political_tgbtrain <- LTSA_political_tgb[LTSA_political_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_political_fulltestingdat <- rbind(LTSA_political_occtest, LTSA_political_tgbtest)
  LTSA_political_fulltrainingdat <- rbind(LTSA_political_occtrain, LTSA_political_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_political_occtest, file=paste("./kfolds/Random_bg/LTSA/political/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_political_occtrain, file=paste("./kfolds/Random_bg/LTSA/political/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_political_fulltestingdat, file=paste("./kfolds/SWD_mode/LTSA/political/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_political_fulltrainingdat, file=paste("./kfolds/SWD_mode/LTSA/political/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

LTSA_ecotone_occ <- read.csv("./extracted_envi_vars_values/LTSA/ecotone/LTSA_locs_ecotone.csv")
LTSA_ecotone_tgb <- read.csv("./extracted_envi_vars_values/LTSA/ecotone/LTSA_tgb_ecotone.csv")


## creating kfolds (occ & tgb)

LTSA_ecotone_fold <- kfold(LTSA_ecotone_occ, k=5)
LTSA_ecotone_fold_tgb <- kfold(LTSA_ecotone_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_ecotone_occtest <- LTSA_ecotone_occ[LTSA_ecotone_fold == i, ]
  LTSA_ecotone_occtrain <- LTSA_ecotone_occ[LTSA_ecotone_fold != i, ]
  
  LTSA_ecotone_tgbtest <- LTSA_ecotone_tgb[LTSA_ecotone_fold_tgb == i, ]
  LTSA_ecotone_tgbtrain <- LTSA_ecotone_tgb[LTSA_ecotone_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_ecotone_fulltestingdat <- rbind(LTSA_ecotone_occtest, LTSA_ecotone_tgbtest)
  LTSA_ecotone_fulltrainingdat <- rbind(LTSA_ecotone_occtrain, LTSA_ecotone_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_ecotone_occtest, file=paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_occtrain, file=paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_ecotone_fulltestingdat, file=paste("./kfolds/SWD_mode/LTSA/ecotone/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_fulltrainingdat, file=paste("./kfolds/SWD_mode/LTSA/ecotone/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}

## genetic study extent 

## Read input localities and tgb points with extracted envi data

LTSA_genetic_occ <- read.csv("./extracted_envi_vars_values/LTSA/genetic/LTSA_locs_genetic.csv")
LTSA_genetic_tgb <- read.csv("./extracted_envi_vars_values/LTSA/genetic/LTSA_tgb_genetic.csv")


## creating kfolds (occ & tgb)

LTSA_genetic_fold <- kfold(LTSA_genetic_occ, k=5)
LTSA_genetic_fold_tgb <- kfold(LTSA_genetic_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_genetic_occtest <- LTSA_genetic_occ[LTSA_genetic_fold == i, ]
  LTSA_genetic_occtrain <- LTSA_genetic_occ[LTSA_genetic_fold != i, ]
  
  LTSA_genetic_tgbtest <- LTSA_genetic_tgb[LTSA_genetic_fold_tgb == i, ]
  LTSA_genetic_tgbtrain <- LTSA_genetic_tgb[LTSA_genetic_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_genetic_fulltestingdat <- rbind(LTSA_genetic_occtest, LTSA_genetic_tgbtest)
  LTSA_genetic_fulltrainingdat <- rbind(LTSA_genetic_occtrain, LTSA_genetic_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_genetic_occtest, file=paste("./kfolds/Random_bg/LTSA/genetic/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_occtrain, file=paste("./kfolds/Random_bg/LTSA/genetic/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_genetic_fulltestingdat, file=paste("./kfolds/SWD_mode/LTSA/genetic/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_fulltrainingdat, file=paste("./kfolds/SWD_mode/LTSA/genetic/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# BCFR ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

BCFR_range_occ <- read.csv("./extracted_envi_vars_values/BCFR/range/BCFR_locs_range.csv")
BCFR_range_tgb <- read.csv("./extracted_envi_vars_values/BCFR/range/BCFR_tgb_range.csv")


## creating kfolds (occ & tgb)

BCFR_range_fold <- kfold(BCFR_range_occ, k=5)
BCFR_range_fold_tgb <- kfold(BCFR_range_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  BCFR_range_occtest <- BCFR_range_occ[BCFR_range_fold == i, ]
  BCFR_range_occtrain <- BCFR_range_occ[BCFR_range_fold != i, ]
  
  BCFR_range_tgbtest <- BCFR_range_tgb[BCFR_range_fold_tgb == i, ]
  BCFR_range_tgbtrain <- BCFR_range_tgb[BCFR_range_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  BCFR_range_fulltestingdat <- rbind(BCFR_range_occtest, BCFR_range_tgbtest)
  BCFR_range_fulltrainingdat <- rbind(BCFR_range_occtrain, BCFR_range_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(BCFR_range_occtest, file=paste("./kfolds/Random_bg/BCFR/range/BCFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(BCFR_range_occtrain, file=paste("./kfolds/Random_bg/BCFR/range/BCFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(BCFR_range_fulltestingdat, file=paste("./kfolds/SWD_mode/BCFR/range/BCFR_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(BCFR_range_fulltrainingdat, file=paste("./kfolds/SWD_mode/BCFR/range/BCFR_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

BCFR_political_occ <- read.csv("./extracted_envi_vars_values/BCFR/political/BCFR_locs_political.csv")
BCFR_political_tgb <- read.csv("./extracted_envi_vars_values/BCFR/political/BCFR_tgb_political.csv")


## creating kfolds (occ & tgb)

BCFR_political_fold <- kfold(BCFR_political_occ, k=5)
BCFR_political_fold_tgb <- kfold(BCFR_political_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  BCFR_political_occtest <- BCFR_political_occ[BCFR_political_fold == i, ]
  BCFR_political_occtrain <- BCFR_political_occ[BCFR_political_fold != i, ]
  
  BCFR_political_tgbtest <- BCFR_political_tgb[BCFR_political_fold_tgb == i, ]
  BCFR_political_tgbtrain <- BCFR_political_tgb[BCFR_political_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  BCFR_political_fulltestingdat <- rbind(BCFR_political_occtest, BCFR_political_tgbtest)
  BCFR_political_fulltrainingdat <- rbind(BCFR_political_occtrain, BCFR_political_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(BCFR_political_occtest, file=paste("./kfolds/Random_bg/BCFR/political/BCFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(BCFR_political_occtrain, file=paste("./kfolds/Random_bg/BCFR/political/BCFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(BCFR_political_fulltestingdat, file=paste("./kfolds/SWD_mode/BCFR/political/BCFR_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(BCFR_political_fulltrainingdat, file=paste("./kfolds/SWD_mode/BCFR/political/BCFR_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

BCFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/BCFR/ecotone/BCFR_locs_ecotone.csv")
BCFR_ecotone_tgb <- read.csv("./extracted_envi_vars_values/BCFR/ecotone/BCFR_tgb_ecotone.csv")


## creating kfolds (occ & tgb)

BCFR_ecotone_fold <- kfold(BCFR_ecotone_occ, k=5)
BCFR_ecotone_fold_tgb <- kfold(BCFR_ecotone_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  BCFR_ecotone_occtest <- BCFR_ecotone_occ[BCFR_ecotone_fold == i, ]
  BCFR_ecotone_occtrain <- BCFR_ecotone_occ[BCFR_ecotone_fold != i, ]
  
  BCFR_ecotone_tgbtest <- BCFR_ecotone_tgb[BCFR_ecotone_fold_tgb == i, ]
  BCFR_ecotone_tgbtrain <- BCFR_ecotone_tgb[BCFR_ecotone_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  BCFR_ecotone_fulltestingdat <- rbind(BCFR_ecotone_occtest, BCFR_ecotone_tgbtest)
  BCFR_ecotone_fulltrainingdat <- rbind(BCFR_ecotone_occtrain, BCFR_ecotone_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(BCFR_ecotone_occtest, file=paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(BCFR_ecotone_occtrain, file=paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(BCFR_ecotone_fulltestingdat, file=paste("./kfolds/SWD_mode/BCFR/ecotone/BCFR_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(BCFR_ecotone_fulltrainingdat, file=paste("./kfolds/SWD_mode/BCFR/ecotone/BCFR_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# CATO ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

CATO_range_occ <- read.csv("./extracted_envi_vars_values/CATO/range/CATO_locs_range.csv")
CATO_range_tgb <- read.csv("./extracted_envi_vars_values/CATO/range/CATO_tgb_range.csv")


## creating kfolds (occ & tgb)

CATO_range_fold <- kfold(CATO_range_occ, k=5)
CATO_range_fold_tgb <- kfold(CATO_range_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CATO_range_occtest <- CATO_range_occ[CATO_range_fold == i, ]
  CATO_range_occtrain <- CATO_range_occ[CATO_range_fold != i, ]
  
  CATO_range_tgbtest <- CATO_range_tgb[CATO_range_fold_tgb == i, ]
  CATO_range_tgbtrain <- CATO_range_tgb[CATO_range_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  CATO_range_fulltestingdat <- rbind(CATO_range_occtest, CATO_range_tgbtest)
  CATO_range_fulltrainingdat <- rbind(CATO_range_occtrain, CATO_range_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CATO_range_occtest, file=paste("./kfolds/Random_bg/CATO/range/CATO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CATO_range_occtrain, file=paste("./kfolds/Random_bg/CATO/range/CATO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(CATO_range_fulltestingdat, file=paste("./kfolds/SWD_mode/CATO/range/CATO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(CATO_range_fulltrainingdat, file=paste("./kfolds/SWD_mode/CATO/range/CATO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

CATO_political_occ <- read.csv("./extracted_envi_vars_values/CATO/political/CATO_locs_political.csv")
CATO_political_tgb <- read.csv("./extracted_envi_vars_values/CATO/political/CATO_tgb_political.csv")


## creating kfolds (occ & tgb)

CATO_political_fold <- kfold(CATO_political_occ, k=5)
CATO_political_fold_tgb <- kfold(CATO_political_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CATO_political_occtest <- CATO_political_occ[CATO_political_fold == i, ]
  CATO_political_occtrain <- CATO_political_occ[CATO_political_fold != i, ]
  
  CATO_political_tgbtest <- CATO_political_tgb[CATO_political_fold_tgb == i, ]
  CATO_political_tgbtrain <- CATO_political_tgb[CATO_political_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  CATO_political_fulltestingdat <- rbind(CATO_political_occtest, CATO_political_tgbtest)
  CATO_political_fulltrainingdat <- rbind(CATO_political_occtrain, CATO_political_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CATO_political_occtest, file=paste("./kfolds/Random_bg/CATO/political/CATO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CATO_political_occtrain, file=paste("./kfolds/Random_bg/CATO/political/CATO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(CATO_political_fulltestingdat, file=paste("./kfolds/SWD_mode/CATO/political/CATO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(CATO_political_fulltrainingdat, file=paste("./kfolds/SWD_mode/CATO/political/CATO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

CATO_ecotone_occ <- read.csv("./extracted_envi_vars_values/CATO/ecotone/CATO_locs_ecotone.csv")
CATO_ecotone_tgb <- read.csv("./extracted_envi_vars_values/CATO/ecotone/CATO_tgb_ecotone.csv")


## creating kfolds (occ & tgb)

CATO_ecotone_fold <- kfold(CATO_ecotone_occ, k=5)
CATO_ecotone_fold_tgb <- kfold(CATO_ecotone_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CATO_ecotone_occtest <- CATO_ecotone_occ[CATO_ecotone_fold == i, ]
  CATO_ecotone_occtrain <- CATO_ecotone_occ[CATO_ecotone_fold != i, ]
  
  CATO_ecotone_tgbtest <- CATO_ecotone_tgb[CATO_ecotone_fold_tgb == i, ]
  CATO_ecotone_tgbtrain <- CATO_ecotone_tgb[CATO_ecotone_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  CATO_ecotone_fulltestingdat <- rbind(CATO_ecotone_occtest, CATO_ecotone_tgbtest)
  CATO_ecotone_fulltrainingdat <- rbind(CATO_ecotone_occtrain, CATO_ecotone_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CATO_ecotone_occtest, file=paste("./kfolds/Random_bg/CATO/ecotone/CATO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CATO_ecotone_occtrain, file=paste("./kfolds/Random_bg/CATO/ecotone/CATO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(CATO_ecotone_fulltestingdat, file=paste("./kfolds/SWD_mode/CATO/ecotone/CATO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(CATO_ecotone_fulltrainingdat, file=paste("./kfolds/SWD_mode/CATO/ecotone/CATO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# CSFR ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

CSFR_range_occ <- read.csv("./extracted_envi_vars_values/CSFR/range/CSFR_locs_range.csv")
CSFR_range_tgb <- read.csv("./extracted_envi_vars_values/CSFR/range/CSFR_tgb_range.csv")


## creating kfolds (occ & tgb)

CSFR_range_fold <- kfold(CSFR_range_occ, k=5)
CSFR_range_fold_tgb <- kfold(CSFR_range_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CSFR_range_occtest <- CSFR_range_occ[CSFR_range_fold == i, ]
  CSFR_range_occtrain <- CSFR_range_occ[CSFR_range_fold != i, ]
  
  CSFR_range_tgbtest <- CSFR_range_tgb[CSFR_range_fold_tgb == i, ]
  CSFR_range_tgbtrain <- CSFR_range_tgb[CSFR_range_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  CSFR_range_fulltestingdat <- rbind(CSFR_range_occtest, CSFR_range_tgbtest)
  CSFR_range_fulltrainingdat <- rbind(CSFR_range_occtrain, CSFR_range_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CSFR_range_occtest, file=paste("./kfolds/Random_bg/CSFR/range/CSFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CSFR_range_occtrain, file=paste("./kfolds/Random_bg/CSFR/range/CSFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(CSFR_range_fulltestingdat, file=paste("./kfolds/SWD_mode/CSFR/range/CSFR_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(CSFR_range_fulltrainingdat, file=paste("./kfolds/SWD_mode/CSFR/range/CSFR_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

CSFR_political_occ <- read.csv("./extracted_envi_vars_values/CSFR/political/CSFR_locs_political.csv")
CSFR_political_tgb <- read.csv("./extracted_envi_vars_values/CSFR/political/CSFR_tgb_political.csv")


## creating kfolds (occ & tgb)

CSFR_political_fold <- kfold(CSFR_political_occ, k=5)
CSFR_political_fold_tgb <- kfold(CSFR_political_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CSFR_political_occtest <- CSFR_political_occ[CSFR_political_fold == i, ]
  CSFR_political_occtrain <- CSFR_political_occ[CSFR_political_fold != i, ]
  
  CSFR_political_tgbtest <- CSFR_political_tgb[CSFR_political_fold_tgb == i, ]
  CSFR_political_tgbtrain <- CSFR_political_tgb[CSFR_political_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  CSFR_political_fulltestingdat <- rbind(CSFR_political_occtest, CSFR_political_tgbtest)
  CSFR_political_fulltrainingdat <- rbind(CSFR_political_occtrain, CSFR_political_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CSFR_political_occtest, file=paste("./kfolds/Random_bg/CSFR/political/CSFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CSFR_political_occtrain, file=paste("./kfolds/Random_bg/CSFR/political/CSFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(CSFR_political_fulltestingdat, file=paste("./kfolds/SWD_mode/CSFR/political/CSFR_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(CSFR_political_fulltrainingdat, file=paste("./kfolds/SWD_mode/CSFR/political/CSFR_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

CSFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/CSFR/ecotone/CSFR_locs_ecotone.csv")
CSFR_ecotone_tgb <- read.csv("./extracted_envi_vars_values/CSFR/ecotone/CSFR_tgb_ecotone.csv")


## creating kfolds (occ & tgb)

CSFR_ecotone_fold <- kfold(CSFR_ecotone_occ, k=5)
CSFR_ecotone_fold_tgb <- kfold(CSFR_ecotone_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CSFR_ecotone_occtest <- CSFR_ecotone_occ[CSFR_ecotone_fold == i, ]
  CSFR_ecotone_occtrain <- CSFR_ecotone_occ[CSFR_ecotone_fold != i, ]
  
  CSFR_ecotone_tgbtest <- CSFR_ecotone_tgb[CSFR_ecotone_fold_tgb == i, ]
  CSFR_ecotone_tgbtrain <- CSFR_ecotone_tgb[CSFR_ecotone_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  CSFR_ecotone_fulltestingdat <- rbind(CSFR_ecotone_occtest, CSFR_ecotone_tgbtest)
  CSFR_ecotone_fulltrainingdat <- rbind(CSFR_ecotone_occtrain, CSFR_ecotone_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CSFR_ecotone_occtest, file=paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CSFR_ecotone_occtrain, file=paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(CSFR_ecotone_fulltestingdat, file=paste("./kfolds/SWD_mode/CSFR/ecotone/CSFR_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(CSFR_ecotone_fulltrainingdat, file=paste("./kfolds/SWD_mode/CSFR/ecotone/CSFR_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# TISA ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

TISA_range_occ <- read.csv("./extracted_envi_vars_values/TISA/range/TISA_locs_range.csv")
TISA_range_tgb <- read.csv("./extracted_envi_vars_values/TISA/range/TISA_tgb_range.csv")


## creating kfolds (occ & tgb)

TISA_range_fold <- kfold(TISA_range_occ, k=5)
TISA_range_fold_tgb <- kfold(TISA_range_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  TISA_range_occtest <- TISA_range_occ[TISA_range_fold == i, ]
  TISA_range_occtrain <- TISA_range_occ[TISA_range_fold != i, ]
  
  TISA_range_tgbtest <- TISA_range_tgb[TISA_range_fold_tgb == i, ]
  TISA_range_tgbtrain <- TISA_range_tgb[TISA_range_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  TISA_range_fulltestingdat <- rbind(TISA_range_occtest, TISA_range_tgbtest)
  TISA_range_fulltrainingdat <- rbind(TISA_range_occtrain, TISA_range_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(TISA_range_occtest, file=paste("./kfolds/Random_bg/TISA/range/TISA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(TISA_range_occtrain, file=paste("./kfolds/Random_bg/TISA/range/TISA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(TISA_range_fulltestingdat, file=paste("./kfolds/SWD_mode/TISA/range/TISA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(TISA_range_fulltrainingdat, file=paste("./kfolds/SWD_mode/TISA/range/TISA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

TISA_political_occ <- read.csv("./extracted_envi_vars_values/TISA/political/TISA_locs_political.csv")
TISA_political_tgb <- read.csv("./extracted_envi_vars_values/TISA/political/TISA_tgb_political.csv")


## creating kfolds (occ & tgb)

TISA_political_fold <- kfold(TISA_political_occ, k=5)
TISA_political_fold_tgb <- kfold(TISA_political_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  TISA_political_occtest <- TISA_political_occ[TISA_political_fold == i, ]
  TISA_political_occtrain <- TISA_political_occ[TISA_political_fold != i, ]
  
  TISA_political_tgbtest <- TISA_political_tgb[TISA_political_fold_tgb == i, ]
  TISA_political_tgbtrain <- TISA_political_tgb[TISA_political_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  TISA_political_fulltestingdat <- rbind(TISA_political_occtest, TISA_political_tgbtest)
  TISA_political_fulltrainingdat <- rbind(TISA_political_occtrain, TISA_political_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(TISA_political_occtest, file=paste("./kfolds/Random_bg/TISA/political/TISA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(TISA_political_occtrain, file=paste("./kfolds/Random_bg/TISA/political/TISA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(TISA_political_fulltestingdat, file=paste("./kfolds/SWD_mode/TISA/political/TISA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(TISA_political_fulltrainingdat, file=paste("./kfolds/SWD_mode/TISA/political/TISA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

TISA_ecotone_occ <- read.csv("./extracted_envi_vars_values/TISA/ecotone/TISA_locs_ecotone.csv")
TISA_ecotone_tgb <- read.csv("./extracted_envi_vars_values/TISA/ecotone/TISA_tgb_ecotone.csv")


## creating kfolds (occ & tgb)

TISA_ecotone_fold <- kfold(TISA_ecotone_occ, k=5)
TISA_ecotone_fold_tgb <- kfold(TISA_ecotone_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  TISA_ecotone_occtest <- TISA_ecotone_occ[TISA_ecotone_fold == i, ]
  TISA_ecotone_occtrain <- TISA_ecotone_occ[TISA_ecotone_fold != i, ]
  
  TISA_ecotone_tgbtest <- TISA_ecotone_tgb[TISA_ecotone_fold_tgb == i, ]
  TISA_ecotone_tgbtrain <- TISA_ecotone_tgb[TISA_ecotone_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  TISA_ecotone_fulltestingdat <- rbind(TISA_ecotone_occtest, TISA_ecotone_tgbtest)
  TISA_ecotone_fulltrainingdat <- rbind(TISA_ecotone_occtrain, TISA_ecotone_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(TISA_ecotone_occtest, file=paste("./kfolds/Random_bg/TISA/ecotone/TISA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(TISA_ecotone_occtrain, file=paste("./kfolds/Random_bg/TISA/ecotone/TISA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(TISA_ecotone_fulltestingdat, file=paste("./kfolds/SWD_mode/TISA/ecotone/TISA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(TISA_ecotone_fulltrainingdat, file=paste("./kfolds/SWD_mode/TISA/ecotone/TISA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# WETO ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

WETO_range_occ <- read.csv("./extracted_envi_vars_values/WETO/range/WETO_locs_range.csv")
WETO_range_tgb <- read.csv("./extracted_envi_vars_values/WETO/range/WETO_tgb_range.csv")


## creating kfolds (occ & tgb)

WETO_range_fold <- kfold(WETO_range_occ, k=5)
WETO_range_fold_tgb <- kfold(WETO_range_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_range_occtest <- WETO_range_occ[WETO_range_fold == i, ]
  WETO_range_occtrain <- WETO_range_occ[WETO_range_fold != i, ]
  
  WETO_range_tgbtest <- WETO_range_tgb[WETO_range_fold_tgb == i, ]
  WETO_range_tgbtrain <- WETO_range_tgb[WETO_range_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  WETO_range_fulltestingdat <- rbind(WETO_range_occtest, WETO_range_tgbtest)
  WETO_range_fulltrainingdat <- rbind(WETO_range_occtrain, WETO_range_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_range_occtest, file=paste("./kfolds/Random_bg/WETO/range/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_range_occtrain, file=paste("./kfolds/Random_bg/WETO/range/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(WETO_range_fulltestingdat, file=paste("./kfolds/SWD_mode/WETO/range/WETO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(WETO_range_fulltrainingdat, file=paste("./kfolds/SWD_mode/WETO/range/WETO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

WETO_political_occ <- read.csv("./extracted_envi_vars_values/WETO/political/WETO_locs_political.csv")
WETO_political_tgb <- read.csv("./extracted_envi_vars_values/WETO/political/WETO_tgb_political.csv")


## creating kfolds (occ & tgb)

WETO_political_fold <- kfold(WETO_political_occ, k=5)
WETO_political_fold_tgb <- kfold(WETO_political_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_political_occtest <- WETO_political_occ[WETO_political_fold == i, ]
  WETO_political_occtrain <- WETO_political_occ[WETO_political_fold != i, ]
  
  WETO_political_tgbtest <- WETO_political_tgb[WETO_political_fold_tgb == i, ]
  WETO_political_tgbtrain <- WETO_political_tgb[WETO_political_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  WETO_political_fulltestingdat <- rbind(WETO_political_occtest, WETO_political_tgbtest)
  WETO_political_fulltrainingdat <- rbind(WETO_political_occtrain, WETO_political_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_political_occtest, file=paste("./kfolds/Random_bg/WETO/political/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_political_occtrain, file=paste("./kfolds/Random_bg/WETO/political/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(WETO_political_fulltestingdat, file=paste("./kfolds/SWD_mode/WETO/political/WETO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(WETO_political_fulltrainingdat, file=paste("./kfolds/SWD_mode/WETO/political/WETO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

WETO_ecotone_occ <- read.csv("./extracted_envi_vars_values/WETO/ecotone/WETO_locs_ecotone.csv")
WETO_ecotone_tgb <- read.csv("./extracted_envi_vars_values/WETO/ecotone/WETO_tgb_ecotone.csv")


## creating kfolds (occ & tgb)

WETO_ecotone_fold <- kfold(WETO_ecotone_occ, k=5)
WETO_ecotone_fold_tgb <- kfold(WETO_ecotone_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_ecotone_occtest <- WETO_ecotone_occ[WETO_ecotone_fold == i, ]
  WETO_ecotone_occtrain <- WETO_ecotone_occ[WETO_ecotone_fold != i, ]
  
  WETO_ecotone_tgbtest <- WETO_ecotone_tgb[WETO_ecotone_fold_tgb == i, ]
  WETO_ecotone_tgbtrain <- WETO_ecotone_tgb[WETO_ecotone_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  WETO_ecotone_fulltestingdat <- rbind(WETO_ecotone_occtest, WETO_ecotone_tgbtest)
  WETO_ecotone_fulltrainingdat <- rbind(WETO_ecotone_occtrain, WETO_ecotone_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_ecotone_occtest, file=paste("./kfolds/Random_bg/WETO/ecotone/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_ecotone_occtrain, file=paste("./kfolds/Random_bg/WETO/ecotone/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(WETO_ecotone_fulltestingdat, file=paste("./kfolds/SWD_mode/WETO/ecotone/WETO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(WETO_ecotone_fulltrainingdat, file=paste("./kfolds/SWD_mode/WETO/ecotone/WETO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## genetic study extent 

## Read input localities and tgb points with extracted envi data

WETO_genetic_occ <- read.csv("./extracted_envi_vars_values/WETO/genetic/WETO_locs_genetic.csv")
WETO_genetic_tgb <- read.csv("./extracted_envi_vars_values/WETO/genetic/WETO_tgb_genetic.csv")


## creating kfolds (occ & tgb)

WETO_genetic_fold <- kfold(WETO_genetic_occ, k=5)
WETO_genetic_fold_tgb <- kfold(WETO_genetic_tgb, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_genetic_occtest <- WETO_genetic_occ[WETO_genetic_fold == i, ]
  WETO_genetic_occtrain <- WETO_genetic_occ[WETO_genetic_fold != i, ]
  
  WETO_genetic_tgbtest <- WETO_genetic_tgb[WETO_genetic_fold_tgb == i, ]
  WETO_genetic_tgbtrain <- WETO_genetic_tgb[WETO_genetic_fold_tgb != i, ]
  
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  WETO_genetic_fulltestingdat <- rbind(WETO_genetic_occtest, WETO_genetic_tgbtest)
  WETO_genetic_fulltrainingdat <- rbind(WETO_genetic_occtrain, WETO_genetic_tgbtrain)
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_genetic_occtest, file=paste("./kfolds/Random_bg/WETO/genetic/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_genetic_occtrain, file=paste("./kfolds/Random_bg/WETO/genetic/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(WETO_genetic_fulltestingdat, file=paste("./kfolds/SWD_mode/WETO/genetic/WETO_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(WETO_genetic_fulltrainingdat, file=paste("./kfolds/SWD_mode/WETO/genetic/WETO_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

### Next steps
# run maxent using the fulltrainingdat1
# then run evaluate function using the resulting model and the fulltestingdat1
# repeat all steps above, creating a new "fulltesting" and "fulltraining" by setting the fold== and fold != values above to 2, then 3, then 4...etc.


########################### END SCRIPT ###############################
