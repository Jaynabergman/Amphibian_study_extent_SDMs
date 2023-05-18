########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 5.1_subsetting_kfolds

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
  
  
## creating kfolds (occ & tgb)
  
LTSA_range_fold <- kfold(LTSA_range_occ, k=5)
  
  
### For loop to run through multiple kfolds ###
  
x <- c(1,2,3,4,5)
  
for (i in 1:length(x)){
    
  ## creating testing and training sets (holding 20%)
    
  LTSA_range_occtest <- LTSA_range_occ[LTSA_range_fold == i, ]
  LTSA_range_occtrain <- LTSA_range_occ[LTSA_range_fold != i, ]
    
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
    
  write.csv(LTSA_range_occtest, file=paste("./kfolds/Random_bg/LTSA/range/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_range_occtrain, file=paste("./kfolds/Random_bg/LTSA/range/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
    
}


## political study extent 

## Read input localities and tgb points with extracted envi data

LTSA_political_occ <- read.csv("./extracted_envi_vars_values/LTSA/political/LTSA_locs_political.csv")


## creating kfolds (occ & tgb)

LTSA_political_fold <- kfold(LTSA_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_political_occtest <- LTSA_political_occ[LTSA_political_fold == i, ]
  LTSA_political_occtrain <- LTSA_political_occ[LTSA_political_fold != i, ]

  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_political_occtest, file=paste("./kfolds/Random_bg/LTSA/political/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_political_occtrain, file=paste("./kfolds/Random_bg/LTSA/political/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

LTSA_ecotone_occ <- read.csv("./extracted_envi_vars_values/LTSA/ecotone/LTSA_locs_ecotone.csv")


## creating kfolds (occ & tgb)

LTSA_ecotone_fold <- kfold(LTSA_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_ecotone_occtest <- LTSA_ecotone_occ[LTSA_ecotone_fold == i, ]
  LTSA_ecotone_occtrain <- LTSA_ecotone_occ[LTSA_ecotone_fold != i, ]
  
 
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_ecotone_occtest, file=paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_occtrain, file=paste("./kfolds/Random_bg/LTSA/ecotone/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)

}


########################### END SECTION ###############################

################################# BCFR ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

BCFR_range_occ <- read.csv("./extracted_envi_vars_values/BCFR/range/BCFR_locs_range.csv")


## creating kfolds (occ & tgb)

BCFR_range_fold <- kfold(BCFR_range_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  BCFR_range_occtest <- BCFR_range_occ[BCFR_range_fold == i, ]
  BCFR_range_occtrain <- BCFR_range_occ[BCFR_range_fold != i, ]
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(BCFR_range_occtest, file=paste("./kfolds/Random_bg/BCFR/range/BCFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(BCFR_range_occtrain, file=paste("./kfolds/Random_bg/BCFR/range/BCFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

BCFR_political_occ <- read.csv("./extracted_envi_vars_values/BCFR/political/BCFR_locs_political.csv")


## creating kfolds (occ & tgb)

BCFR_political_fold <- kfold(BCFR_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  BCFR_political_occtest <- BCFR_political_occ[BCFR_political_fold == i, ]
  BCFR_political_occtrain <- BCFR_political_occ[BCFR_political_fold != i, ]
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(BCFR_political_occtest, file=paste("./kfolds/Random_bg/BCFR/political/BCFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(BCFR_political_occtrain, file=paste("./kfolds/Random_bg/BCFR/political/BCFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

BCFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/BCFR/ecotone/BCFR_locs_ecotone.csv")


## creating kfolds (occ & tgb)

BCFR_ecotone_fold <- kfold(BCFR_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  BCFR_ecotone_occtest <- BCFR_ecotone_occ[BCFR_ecotone_fold == i, ]
  BCFR_ecotone_occtrain <- BCFR_ecotone_occ[BCFR_ecotone_fold != i, ]

  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(BCFR_ecotone_occtest, file=paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(BCFR_ecotone_occtrain, file=paste("./kfolds/Random_bg/BCFR/ecotone/BCFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# CATO ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

CATO_range_occ <- read.csv("./extracted_envi_vars_values/CATO/range/CATO_locs_range.csv")


## creating kfolds (occ & tgb)

CATO_range_fold <- kfold(CATO_range_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CATO_range_occtest <- CATO_range_occ[CATO_range_fold == i, ]
  CATO_range_occtrain <- CATO_range_occ[CATO_range_fold != i, ]
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CATO_range_occtest, file=paste("./kfolds/Random_bg/CATO/range/CATO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CATO_range_occtrain, file=paste("./kfolds/Random_bg/CATO/range/CATO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

CATO_political_occ <- read.csv("./extracted_envi_vars_values/CATO/political/CATO_locs_political.csv")


## creating kfolds (occ & tgb)

CATO_political_fold <- kfold(CATO_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CATO_political_occtest <- CATO_political_occ[CATO_political_fold == i, ]
  CATO_political_occtrain <- CATO_political_occ[CATO_political_fold != i, ]

  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CATO_political_occtest, file=paste("./kfolds/Random_bg/CATO/political/CATO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CATO_political_occtrain, file=paste("./kfolds/Random_bg/CATO/political/CATO_trainingdat",i,".csv",sep=""), row.names= FALSE)
   
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

CATO_ecotone_occ <- read.csv("./extracted_envi_vars_values/CATO/ecotone/CATO_locs_ecotone.csv")


## creating kfolds (occ & tgb)

CATO_ecotone_fold <- kfold(CATO_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CATO_ecotone_occtest <- CATO_ecotone_occ[CATO_ecotone_fold == i, ]
  CATO_ecotone_occtrain <- CATO_ecotone_occ[CATO_ecotone_fold != i, ]
  
 
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CATO_ecotone_occtest, file=paste("./kfolds/Random_bg/CATO/ecotone/CATO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CATO_ecotone_occtrain, file=paste("./kfolds/Random_bg/CATO/ecotone/CATO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# CSFR ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

CSFR_range_occ <- read.csv("./extracted_envi_vars_values/CSFR/range/CSFR_locs_range.csv")


## creating kfolds (occ & tgb)

CSFR_range_fold <- kfold(CSFR_range_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CSFR_range_occtest <- CSFR_range_occ[CSFR_range_fold == i, ]
  CSFR_range_occtrain <- CSFR_range_occ[CSFR_range_fold != i, ]
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CSFR_range_occtest, file=paste("./kfolds/Random_bg/CSFR/range/CSFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CSFR_range_occtrain, file=paste("./kfolds/Random_bg/CSFR/range/CSFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

CSFR_political_occ <- read.csv("./extracted_envi_vars_values/CSFR/political/CSFR_locs_political.csv")


## creating kfolds (occ & tgb)

CSFR_political_fold <- kfold(CSFR_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CSFR_political_occtest <- CSFR_political_occ[CSFR_political_fold == i, ]
  CSFR_political_occtrain <- CSFR_political_occ[CSFR_political_fold != i, ]
  

  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CSFR_political_occtest, file=paste("./kfolds/Random_bg/CSFR/political/CSFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CSFR_political_occtrain, file=paste("./kfolds/Random_bg/CSFR/political/CSFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

CSFR_ecotone_occ <- read.csv("./extracted_envi_vars_values/CSFR/ecotone/CSFR_locs_ecotone.csv")


## creating kfolds (occ & tgb)

CSFR_ecotone_fold <- kfold(CSFR_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  CSFR_ecotone_occtest <- CSFR_ecotone_occ[CSFR_ecotone_fold == i, ]
  CSFR_ecotone_occtrain <- CSFR_ecotone_occ[CSFR_ecotone_fold != i, ]

  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(CSFR_ecotone_occtest, file=paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(CSFR_ecotone_occtrain, file=paste("./kfolds/Random_bg/CSFR/ecotone/CSFR_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# TISA ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

TISA_range_occ <- read.csv("./extracted_envi_vars_values/TISA/range/TISA_locs_range.csv")


## creating kfolds (occ & tgb)

TISA_range_fold <- kfold(TISA_range_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  TISA_range_occtest <- TISA_range_occ[TISA_range_fold == i, ]
  TISA_range_occtrain <- TISA_range_occ[TISA_range_fold != i, ]
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(TISA_range_occtest, file=paste("./kfolds/Random_bg/TISA/range/TISA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(TISA_range_occtrain, file=paste("./kfolds/Random_bg/TISA/range/TISA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

TISA_political_occ <- read.csv("./extracted_envi_vars_values/TISA/political/TISA_locs_political.csv")


## creating kfolds (occ & tgb)

TISA_political_fold <- kfold(TISA_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  TISA_political_occtest <- TISA_political_occ[TISA_political_fold == i, ]
  TISA_political_occtrain <- TISA_political_occ[TISA_political_fold != i, ]
  
 
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(TISA_political_occtest, file=paste("./kfolds/Random_bg/TISA/political/TISA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(TISA_political_occtrain, file=paste("./kfolds/Random_bg/TISA/political/TISA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

TISA_ecotone_occ <- read.csv("./extracted_envi_vars_values/TISA/ecotone/TISA_locs_ecotone.csv")


## creating kfolds (occ & tgb)

TISA_ecotone_fold <- kfold(TISA_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  TISA_ecotone_occtest <- TISA_ecotone_occ[TISA_ecotone_fold == i, ]
  TISA_ecotone_occtrain <- TISA_ecotone_occ[TISA_ecotone_fold != i, ]

  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(TISA_ecotone_occtest, file=paste("./kfolds/Random_bg/TISA/ecotone/TISA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(TISA_ecotone_occtrain, file=paste("./kfolds/Random_bg/TISA/ecotone/TISA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


########################### END SECTION ###############################

################################# WETO ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

WETO_range_occ <- read.csv("./extracted_envi_vars_values/WETO/range/WETO_locs_range.csv")


## creating kfolds (occ & tgb)

WETO_range_fold <- kfold(WETO_range_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_range_occtest <- WETO_range_occ[WETO_range_fold == i, ]
  WETO_range_occtrain <- WETO_range_occ[WETO_range_fold != i, ]
  
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_range_occtest, file=paste("./kfolds/Random_bg/WETO/range/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_range_occtrain, file=paste("./kfolds/Random_bg/WETO/range/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

WETO_political_occ <- read.csv("./extracted_envi_vars_values/WETO/political/WETO_locs_political.csv")


## creating kfolds (occ & tgb)

WETO_political_fold <- kfold(WETO_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_political_occtest <- WETO_political_occ[WETO_political_fold == i, ]
  WETO_political_occtrain <- WETO_political_occ[WETO_political_fold != i, ]

  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_political_occtest, file=paste("./kfolds/Random_bg/WETO/political/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_political_occtrain, file=paste("./kfolds/Random_bg/WETO/political/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
 
}


## ecotone study extent 

## Read input localities and tgb points with extracted envi data

WETO_ecotone_occ <- read.csv("./extracted_envi_vars_values/WETO/ecotone/WETO_locs_ecotone.csv")


## creating kfolds (occ & tgb)

WETO_ecotone_fold <- kfold(WETO_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  WETO_ecotone_occtest <- WETO_ecotone_occ[WETO_ecotone_fold == i, ]
  WETO_ecotone_occtrain <- WETO_ecotone_occ[WETO_ecotone_fold != i, ]
 
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(WETO_ecotone_occtest, file=paste("./kfolds/Random_bg/WETO/ecotone/WETO_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(WETO_ecotone_occtrain, file=paste("./kfolds/Random_bg/WETO/ecotone/WETO_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################
