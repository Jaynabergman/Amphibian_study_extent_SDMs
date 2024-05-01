########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# 1) Run Maxent in SWD mode for each kfold (training localities)
# 2) Evaluate each model with kfold (testing localities)

### Notes:  

# For loop for each kfold
# Possibly loop for each background??

### Date: April 11, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")


## Basic maxent arguments used for EVERY model

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")


################################# LTSA ################################

## Set consistent variable names (stays same with all study extents)

LTSA_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/LTSA", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


###### range study extent ######


## Setting parameters from 6.1_Tuning script

LTSA_range_max_args <- c(basicargs, features = c("nohinge", "Threshold"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_range_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/LTSA/range/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_range_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/LTSA/range/LTSA_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  LTSA_range_od <- paste("./tgb/Maxent_SWD_mode/LTSA/range/LTSA_range_kfold_",i,sep="")
  dir.create(LTSA_range_od)
  
  
  ## Maxent command and save
  
  LTSA_range_me <- maxent(x=LTSA_range_training_locs[,LTSA_vars], p=LTSA_range_training_locs[,index], removeDuplicates = FALSE, args = LTSA_range_max_args, path=LTSA_range_od)
  
  save(LTSA_range_me, file = paste(LTSA_range_od,"/LTSA_range_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  LTSA_range_e <- evaluate(p=LTSA_range_testing_locs[which(LTSA_range_testing_locs$Type == 1), LTSA_vars], a=LTSA_range_testing_locs[which(LTSA_range_testing_locs$Type == 0), LTSA_vars], model=LTSA_range_me)
  print(LTSA_range_e)
  
  LTSA_range_internal_auc[i] <- LTSA_range_e@auc
  
  dput(LTSA_range_e, file=paste("./tgb/Maxent_SWD_mode/LTSA/range/LTSA_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_range_aucdat <- as.data.frame(LTSA_range_internal_auc)
LTSA_range_aucdat$fold <- row.names(LTSA_range_aucdat)

write.csv(LTSA_range_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/LTSA/range/LTSA_range_Internal_AUC.csv", row.names = FALSE)


###### political study extent ######

## Setting parameters from 6.1_Tuning script

LTSA_political_max_args <- c(basicargs, features = c("nohinge"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_political_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/LTSA/political/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_political_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/LTSA/political/LTSA_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  LTSA_political_od <- paste("./tgb/Maxent_SWD_mode/LTSA/political/LTSA_political_kfold_",i,sep="")
  dir.create(LTSA_political_od)
  
  
  ## Maxent command and save
  
  LTSA_political_me <- maxent(x=LTSA_political_training_locs[,LTSA_vars], p=LTSA_political_training_locs[,index], removeDuplicates = FALSE, args = LTSA_political_max_args, path=LTSA_political_od)
  
  save(LTSA_political_me, file = paste(LTSA_political_od,"/LTSA_political_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  LTSA_political_e <- evaluate(p=LTSA_political_testing_locs[which(LTSA_political_testing_locs$Type == 1), LTSA_vars], a=LTSA_political_testing_locs[which(LTSA_political_testing_locs$Type == 0), LTSA_vars], model=LTSA_political_me)
  print(LTSA_political_e)
  
  LTSA_political_internal_auc[i] <- LTSA_political_e@auc
  
  dput(LTSA_political_e, file=paste("./tgb/Maxent_SWD_mode/LTSA/political/LTSA_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_political_aucdat <- as.data.frame(LTSA_political_internal_auc)
LTSA_political_aucdat$fold <- row.names(LTSA_political_aucdat)

write.csv(LTSA_political_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/LTSA/political/LTSA_political_Internal_AUC.csv", row.names = FALSE)


###### ecotone study extent ######


## Setting parameters from 6.1_Tuning script

LTSA_ecotone_max_args <- c(basicargs, features = c("Threshold", "nohinge"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_ecotone_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/LTSA/ecotone/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/LTSA/ecotone/LTSA_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  LTSA_ecotone_od <- paste("./tgb/Maxent_SWD_mode/LTSA/ecotone/LTSA_ecotone_kfold_",i,sep="")
  dir.create(LTSA_ecotone_od)
  
  
  ## Maxent command and save
  
  LTSA_ecotone_me <- maxent(x=LTSA_ecotone_training_locs[,LTSA_vars], p=LTSA_ecotone_training_locs[,index], removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=LTSA_ecotone_od)
  
  save(LTSA_ecotone_me, file = paste(LTSA_ecotone_od,"/LTSA_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  LTSA_ecotone_e <- evaluate(p=LTSA_ecotone_testing_locs[which(LTSA_ecotone_testing_locs$Type == 1), LTSA_vars], a=LTSA_ecotone_testing_locs[which(LTSA_ecotone_testing_locs$Type == 0), LTSA_vars], model=LTSA_ecotone_me)
  print(LTSA_ecotone_e)
  
  LTSA_ecotone_internal_auc[i] <- LTSA_ecotone_e@auc
  
  dput(LTSA_ecotone_e, file=paste("./tgb/Maxent_SWD_mode/LTSA/ecotone/LTSA_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_ecotone_aucdat <- as.data.frame(LTSA_ecotone_internal_auc)
LTSA_ecotone_aucdat$fold <- row.names(LTSA_ecotone_aucdat)

write.csv(LTSA_ecotone_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/LTSA/ecotone/LTSA_ecotone_Internal_AUC.csv", row.names = FALSE)



########################### END SECTION ###############################

################################# BCFR ################################


## Set consistent variable names (stays same with all study extents)

BCFR_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/BCFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


###### range study extent ######


## Setting parameters from 6.1_Tuning script

BCFR_range_max_args <- c(basicargs, features = c("Threshold"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
BCFR_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  BCFR_range_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/BCFR/range/BCFR_fulltrainingdat",i,".csv",sep=""))
  BCFR_range_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/BCFR/range/BCFR_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  BCFR_range_od <- paste("./tgb/Maxent_SWD_mode/BCFR/range/BCFR_range_kfold_",i,sep="")
  dir.create(BCFR_range_od)
  
  
  ## Maxent command and save
  
  BCFR_range_me <- maxent(x=BCFR_range_training_locs[,BCFR_vars], p=BCFR_range_training_locs[,index], removeDuplicates = FALSE, args = BCFR_range_max_args, path=BCFR_range_od)
  
  save(BCFR_range_me, file = paste(BCFR_range_od,"/BCFR_range_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  BCFR_range_e <- evaluate(p=BCFR_range_testing_locs[which(BCFR_range_testing_locs$Type == 1), BCFR_vars], a=BCFR_range_testing_locs[which(BCFR_range_testing_locs$Type == 0), BCFR_vars], model=BCFR_range_me)
  print(BCFR_range_e)
  
  BCFR_range_internal_auc[i] <- BCFR_range_e@auc
  
  dput(BCFR_range_e, file=paste("./tgb/Maxent_SWD_mode/BCFR/range/BCFR_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

# Saving auc scores as a csv
BCFR_range_aucdat <- as.data.frame(BCFR_range_internal_auc)
BCFR_range_aucdat$fold <- row.names(BCFR_range_aucdat)

write.csv(BCFR_range_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/BCFR/range/BCFR_range_Internal_AUC.csv", row.names = FALSE)


###### political study extent ######

## Setting parameters from 6.1_Tuning script

BCFR_political_max_args <- c(basicargs, features = c(""), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
BCFR_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  BCFR_political_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/BCFR/political/BCFR_fulltrainingdat",i,".csv",sep=""))
  BCFR_political_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/BCFR/political/BCFR_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  BCFR_political_od <- paste("./tgb/Maxent_SWD_mode/BCFR/political/BCFR_political_kfold_",i,sep="")
  dir.create(BCFR_political_od)
  
  
  ## Maxent command and save
  
  BCFR_political_me <- maxent(x=BCFR_political_training_locs[,BCFR_vars], p=BCFR_political_training_locs[,index], removeDuplicates = FALSE, args = BCFR_political_max_args, path=BCFR_political_od)
  
  save(BCFR_political_me, file = paste(BCFR_political_od,"/BCFR_political_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  BCFR_political_e <- evaluate(p=BCFR_political_testing_locs[which(BCFR_political_testing_locs$Type == 1), BCFR_vars], a=BCFR_political_testing_locs[which(BCFR_political_testing_locs$Type == 0), BCFR_vars], model=BCFR_political_me)
  print(BCFR_political_e)
  
  BCFR_political_internal_auc[i] <- BCFR_political_e@auc
  
  dput(BCFR_political_e, file=paste("./tgb/Maxent_SWD_mode/BCFR/political/BCFR_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
BCFR_political_aucdat <- as.data.frame(BCFR_political_internal_auc)
BCFR_political_aucdat$fold <- row.names(BCFR_political_aucdat)

write.csv(BCFR_political_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/BCFR/political/BCFR_political_Internal_AUC.csv", row.names = FALSE)


###### ecotone study extent ######


## Setting parameters from 6.1_Tuning script

BCFR_ecotone_max_args <- c(basicargs, features = c("nohinge", "noproduct", "noquadratic"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
BCFR_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  BCFR_ecotone_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/BCFR/ecotone/BCFR_fulltrainingdat",i,".csv",sep=""))
  BCFR_ecotone_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/BCFR/ecotone/BCFR_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  BCFR_ecotone_od <- paste("./tgb/Maxent_SWD_mode/BCFR/ecotone/BCFR_ecotone_kfold_",i,sep="")
  dir.create(BCFR_ecotone_od)
  
  
  ## Maxent command and save
  
  BCFR_ecotone_me <- maxent(x=BCFR_ecotone_training_locs[,BCFR_vars], p=BCFR_ecotone_training_locs[,index], removeDuplicates = FALSE, args = BCFR_ecotone_max_args, path=BCFR_ecotone_od)
  
  save(BCFR_ecotone_me, file = paste(BCFR_ecotone_od,"/BCFR_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  BCFR_ecotone_e <- evaluate(p=BCFR_ecotone_testing_locs[which(BCFR_ecotone_testing_locs$Type == 1), BCFR_vars], a=BCFR_ecotone_testing_locs[which(BCFR_ecotone_testing_locs$Type == 0), BCFR_vars], model=BCFR_ecotone_me)
  print(BCFR_ecotone_e)
  
  BCFR_ecotone_internal_auc[i] <- BCFR_ecotone_e@auc
  
  dput(BCFR_ecotone_e, file=paste("./tgb/Maxent_SWD_mode/BCFR/ecotone/BCFR_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
BCFR_ecotone_aucdat <- as.data.frame(BCFR_ecotone_internal_auc)
BCFR_ecotone_aucdat$fold <- row.names(BCFR_ecotone_aucdat)

write.csv(BCFR_ecotone_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/BCFR/ecotone/BCFR_ecotone_Internal_AUC.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CATO ################################


## Set consistent variables (stays same with all study extents)

CATO_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CATO", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


###### range study extent ######


## Setting parameters from 6.1_Tuning script

CATO_range_max_args <- c(basicargs, features = c("noproduct"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CATO_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CATO_range_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/CATO/range/CATO_fulltrainingdat",i,".csv",sep=""))
  CATO_range_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/CATO/range/CATO_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CATO_range_od <- paste("./tgb/Maxent_SWD_mode/CATO/range/CATO_range_kfold_",i,sep="")
  dir.create(CATO_range_od)
  
  
  ## Maxent command and save
  
  CATO_range_me <- maxent(x=CATO_range_training_locs[,CATO_vars], p=CATO_range_training_locs[,index], removeDuplicates = FALSE, args = CATO_range_max_args, path=CATO_range_od)
  
  save(CATO_range_me, file = paste(CATO_range_od,"/CATO_range_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  CATO_range_e <- evaluate(p=CATO_range_testing_locs[which(CATO_range_testing_locs$Type == 1), CATO_vars], a=CATO_range_testing_locs[which(CATO_range_testing_locs$Type == 0), CATO_vars], model=CATO_range_me)
  print(CATO_range_e)
  
  CATO_range_internal_auc[i] <- CATO_range_e@auc
  
  dput(CATO_range_e, file=paste("./tgb/Maxent_SWD_mode/CATO/range/CATO_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
CATO_range_aucdat <- as.data.frame(CATO_range_internal_auc)
CATO_range_aucdat$fold <- row.names(CATO_range_aucdat)

write.csv(CATO_range_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/CATO/range/CATO_range_Internal_AUC.csv", row.names = FALSE)


###### political study extent ######

## Setting parameters from 6.1_Tuning script

CATO_political_max_args <- c(basicargs, features = c("noproduct","nohinge"), "betamultiplier=2")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CATO_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CATO_political_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/CATO/political/CATO_fulltrainingdat",i,".csv",sep=""))
  CATO_political_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/CATO/political/CATO_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CATO_political_od <- paste("./tgb/Maxent_SWD_mode/CATO/political/CATO_political_kfold_",i,sep="")
  dir.create(CATO_political_od)
  
  
  ## Maxent command and save
  
  CATO_political_me <- maxent(x=CATO_political_training_locs[,CATO_vars], p=CATO_political_training_locs[,index], removeDuplicates = FALSE, args = CATO_political_max_args, path=CATO_political_od)
  
  save(CATO_political_me, file = paste(CATO_political_od,"/CATO_political_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  CATO_political_e <- evaluate(p=CATO_political_testing_locs[which(CATO_political_testing_locs$Type == 1), CATO_vars], a=CATO_political_testing_locs[which(CATO_political_testing_locs$Type == 0), CATO_vars], model=CATO_political_me)
  print(CATO_political_e)
  
  CATO_political_internal_auc[i] <- CATO_political_e@auc
  
  dput(CATO_political_e, file=paste("./tgb/Maxent_SWD_mode/CATO/political/CATO_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
CATO_political_aucdat <- as.data.frame(CATO_political_internal_auc)
CATO_political_aucdat$fold <- row.names(CATO_political_aucdat)

write.csv(CATO_political_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/CATO/political/CATO_political_Internal_AUC.csv", row.names = FALSE)


###### ecotone study extent ######


## Setting parameters from 6.1_Tuning script

CATO_ecotone_max_args <- c(basicargs, features = c("Threshold", "noproduct", "nohinge"), "betamultiplier=1")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CATO_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CATO_ecotone_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/CATO/ecotone/CATO_fulltrainingdat",i,".csv",sep=""))
  CATO_ecotone_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/CATO/ecotone/CATO_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CATO_ecotone_od <- paste("./tgb/Maxent_SWD_mode/CATO/ecotone/CATO_ecotone_kfold_",i,sep="")
  dir.create(CATO_ecotone_od)
  
  
  ## Maxent command and save
  
  CATO_ecotone_me <- maxent(x=CATO_ecotone_training_locs[,CATO_vars], p=CATO_ecotone_training_locs[,index], removeDuplicates = FALSE, args = CATO_ecotone_max_args, path=CATO_ecotone_od)
  
  save(CATO_ecotone_me, file = paste(CATO_ecotone_od,"/CATO_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  CATO_ecotone_e <- evaluate(p=CATO_ecotone_testing_locs[which(CATO_ecotone_testing_locs$Type == 1), CATO_vars], a=CATO_ecotone_testing_locs[which(CATO_ecotone_testing_locs$Type == 0), CATO_vars], model=CATO_ecotone_me)
  print(CATO_ecotone_e)
  
  CATO_ecotone_internal_auc[i] <- CATO_ecotone_e@auc
  
  dput(CATO_ecotone_e, file=paste("./tgb/Maxent_SWD_mode/CATO/ecotone/CATO_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
CATO_ecotone_aucdat <- as.data.frame(CATO_ecotone_internal_auc)
CATO_ecotone_aucdat$fold <- row.names(CATO_ecotone_aucdat)

write.csv(CATO_ecotone_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/CATO/ecotone/CATO_ecotone_Internal_AUC.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# CSFR ################################


## Set consistent variables (stays same with all study extents)

CSFR_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/CSFR", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


###### range study extent ######


## Setting parameters from 6.1_Tuning script

CSFR_range_max_args <- c(basicargs, features = c("nohinge", "noproduct"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CSFR_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CSFR_range_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/CSFR/range/CSFR_fulltrainingdat",i,".csv",sep=""))
  CSFR_range_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/CSFR/range/CSFR_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CSFR_range_od <- paste("./tgb/Maxent_SWD_mode/CSFR/range/CSFR_range_kfold_",i,sep="")
  dir.create(CSFR_range_od)
  
  
  ## Maxent command and save
  
  CSFR_range_me <- maxent(x=CSFR_range_training_locs[,CSFR_vars], p=CSFR_range_training_locs[,index], removeDuplicates = FALSE, args = CSFR_range_max_args, path=CSFR_range_od)
  
  save(CSFR_range_me, file = paste(CSFR_range_od,"/CSFR_range_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  CSFR_range_e <- evaluate(p=CSFR_range_testing_locs[which(CSFR_range_testing_locs$Type == 1), CSFR_vars], a=CSFR_range_testing_locs[which(CSFR_range_testing_locs$Type == 0), CSFR_vars], model=CSFR_range_me)
  print(CSFR_range_e)
  
  CSFR_range_internal_auc[i] <- CSFR_range_e@auc
  
  dput(CSFR_range_e, file=paste("./tgb/Maxent_SWD_mode/CSFR/range/CSFR_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
CSFR_range_aucdat <- as.data.frame(CSFR_range_internal_auc)
CSFR_range_aucdat$fold <- row.names(CSFR_range_aucdat)

write.csv(CSFR_range_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/CSFR/range/CSFR_range_Internal_AUC.csv", row.names = FALSE)


###### political study extent ######

## Setting parameters from 6.1_Tuning script

CSFR_political_max_args <- c(basicargs, features = c("noproduct"), "betamultiplier=0.5")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CSFR_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CSFR_political_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/CSFR/political/CSFR_fulltrainingdat",i,".csv",sep=""))
  CSFR_political_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/CSFR/political/CSFR_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CSFR_political_od <- paste("./tgb/Maxent_SWD_mode/CSFR/political/CSFR_political_kfold_",i,sep="")
  dir.create(CSFR_political_od)
  
  
  ## Maxent command and save
  
  CSFR_political_me <- maxent(x=CSFR_political_training_locs[,CSFR_vars], p=CSFR_political_training_locs[,index], removeDuplicates = FALSE, args = CSFR_political_max_args, path=CSFR_political_od)
  
  save(CSFR_political_me, file = paste(CSFR_political_od,"/CSFR_political_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  CSFR_political_e <- evaluate(p=CSFR_political_testing_locs[which(CSFR_political_testing_locs$Type == 1), CSFR_vars], a=CSFR_political_testing_locs[which(CSFR_political_testing_locs$Type == 0), CSFR_vars], model=CSFR_political_me)
  print(CSFR_political_e)
  
  CSFR_political_internal_auc[i] <- CSFR_political_e@auc
  
  dput(CSFR_political_e, file=paste("./tgb/Maxent_SWD_mode/CSFR/political/CSFR_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
CSFR_political_aucdat <- as.data.frame(CSFR_political_internal_auc)
CSFR_political_aucdat$fold <- row.names(CSFR_political_aucdat)

write.csv(CSFR_political_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/CSFR/political/CSFR_political_Internal_AUC.csv", row.names = FALSE)


###### ecotone study extent ######


## Setting parameters from 6.1_Tuning script

CSFR_ecotone_max_args <- c(basicargs, features = c("Threshold", "nohinge", "noproduct"), "betamultiplier=2")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
CSFR_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  CSFR_ecotone_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/CSFR/ecotone/CSFR_fulltrainingdat",i,".csv",sep=""))
  CSFR_ecotone_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/CSFR/ecotone/CSFR_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  CSFR_ecotone_od <- paste("./tgb/Maxent_SWD_mode/CSFR/ecotone/CSFR_ecotone_kfold_",i,sep="")
  dir.create(CSFR_ecotone_od)
  
  
  ## Maxent command and save
  
  CSFR_ecotone_me <- maxent(x=CSFR_ecotone_training_locs[,CSFR_vars], p=CSFR_ecotone_training_locs[,index], removeDuplicates = FALSE, args = CSFR_ecotone_max_args, path=CSFR_ecotone_od)
  
  save(CSFR_ecotone_me, file = paste(CSFR_ecotone_od,"/CSFR_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  CSFR_ecotone_e <- evaluate(p=CSFR_ecotone_testing_locs[which(CSFR_ecotone_testing_locs$Type == 1), CSFR_vars], a=CSFR_ecotone_testing_locs[which(CSFR_ecotone_testing_locs$Type == 0), CSFR_vars], model=CSFR_ecotone_me)
  print(CSFR_ecotone_e)
  
  CSFR_ecotone_internal_auc[i] <- CSFR_ecotone_e@auc
  
  dput(CSFR_ecotone_e, file=paste("./tgb/Maxent_SWD_mode/CSFR/ecotone/CSFR_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
CSFR_ecotone_aucdat <- as.data.frame(CSFR_ecotone_internal_auc)
CSFR_ecotone_aucdat$fold <- row.names(CSFR_ecotone_aucdat)

write.csv(CSFR_ecotone_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/CSFR/ecotone/CSFR_ecotone_Internal_AUC.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# TISA ################################


## Set consistent variables (stays same with all study extents)

TISA_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/TISA", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


###### range study extent ######


## Setting parameters from 6.1_Tuning script

TISA_range_max_args <- c(basicargs, features = c(""), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
TISA_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  TISA_range_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/TISA/range/TISA_fulltrainingdat",i,".csv",sep=""))
  TISA_range_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/TISA/range/TISA_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  TISA_range_od <- paste("./tgb/Maxent_SWD_mode/TISA/range/TISA_range_kfold_",i,sep="")
  dir.create(TISA_range_od)
  
  
  ## Maxent command and save
  
  TISA_range_me <- maxent(x=TISA_range_training_locs[,TISA_vars], p=TISA_range_training_locs[,index], removeDuplicates = FALSE, args = TISA_range_max_args, path=TISA_range_od)
  
  save(TISA_range_me, file = paste(TISA_range_od,"/TISA_range_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  TISA_range_e <- evaluate(p=TISA_range_testing_locs[which(TISA_range_testing_locs$Type == 1), TISA_vars], a=TISA_range_testing_locs[which(TISA_range_testing_locs$Type == 0), TISA_vars], model=TISA_range_me)
  print(TISA_range_e)
  
  TISA_range_internal_auc[i] <- TISA_range_e@auc
  
  dput(TISA_range_e, file=paste("./tgb/Maxent_SWD_mode/TISA/range/TISA_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
TISA_range_aucdat <- as.data.frame(TISA_range_internal_auc)
TISA_range_aucdat$fold <- row.names(TISA_range_aucdat)

write.csv(TISA_range_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/TISA/range/TISA_range_Internal_AUC.csv", row.names = FALSE)


###### political study extent ######

## Setting parameters from 6.1_Tuning script

TISA_political_max_args <- c(basicargs, features = c("nohinge", "noproduct"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
TISA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  TISA_political_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/TISA/political/TISA_fulltrainingdat",i,".csv",sep=""))
  TISA_political_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/TISA/political/TISA_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  TISA_political_od <- paste("./tgb/Maxent_SWD_mode/TISA/political/TISA_political_kfold_",i,sep="")
  dir.create(TISA_political_od)
  
  
  ## Maxent command and save
  
  TISA_political_me <- maxent(x=TISA_political_training_locs[,TISA_vars], p=TISA_political_training_locs[,index], removeDuplicates = FALSE, args = TISA_political_max_args, path=TISA_political_od)
  
  save(TISA_political_me, file = paste(TISA_political_od,"/TISA_political_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  TISA_political_e <- evaluate(p=TISA_political_testing_locs[which(TISA_political_testing_locs$Type == 1), TISA_vars], a=TISA_political_testing_locs[which(TISA_political_testing_locs$Type == 0), TISA_vars], model=TISA_political_me)
  print(TISA_political_e)
  
  TISA_political_internal_auc[i] <- TISA_political_e@auc
  
  dput(TISA_political_e, file=paste("./tgb/Maxent_SWD_mode/TISA/political/TISA_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
TISA_political_aucdat <- as.data.frame(TISA_political_internal_auc)
TISA_political_aucdat$fold <- row.names(TISA_political_aucdat)

write.csv(TISA_political_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/TISA/political/TISA_political_Internal_AUC.csv", row.names = FALSE)


###### ecotone study extent ######


## Setting parameters from 6.1_Tuning script

TISA_ecotone_max_args <- c(basicargs, features = c("Threshold", "nohinge", "noproduct"), "betamultiplier=0.5")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
TISA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  TISA_ecotone_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/TISA/ecotone/TISA_fulltrainingdat",i,".csv",sep=""))
  TISA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/TISA/ecotone/TISA_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  TISA_ecotone_od <- paste("./tgb/Maxent_SWD_mode/TISA/ecotone/TISA_ecotone_kfold_",i,sep="")
  dir.create(TISA_ecotone_od)
  
  
  ## Maxent command and save
  
  TISA_ecotone_me <- maxent(x=TISA_ecotone_training_locs[,TISA_vars], p=TISA_ecotone_training_locs[,index], removeDuplicates = FALSE, args = TISA_ecotone_max_args, path=TISA_ecotone_od)
  
  save(TISA_ecotone_me, file = paste(TISA_ecotone_od,"/TISA_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  TISA_ecotone_e <- evaluate(p=TISA_ecotone_testing_locs[which(TISA_ecotone_testing_locs$Type == 1), TISA_vars], a=TISA_ecotone_testing_locs[which(TISA_ecotone_testing_locs$Type == 0), TISA_vars], model=TISA_ecotone_me)
  print(TISA_ecotone_e)
  
  TISA_ecotone_internal_auc[i] <- TISA_ecotone_e@auc
  
  dput(TISA_ecotone_e, file=paste("./tgb/Maxent_SWD_mode/TISA/ecotone/TISA_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
TISA_ecotone_aucdat <- as.data.frame(TISA_ecotone_internal_auc)
TISA_ecotone_aucdat$fold <- row.names(TISA_ecotone_aucdat)

write.csv(TISA_ecotone_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/TISA/ecotone/TISA_ecotone_Internal_AUC.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# WETO ################################


## Set consistent variables (stays same with all study extents)

WETO_vars <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/model_subsets/WETO", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"


###### range study extent ######


## Setting parameters from 6.1_Tuning script

WETO_range_max_args <- c(basicargs, features = c("Threshold"), "betamultiplier=1.5")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
WETO_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  WETO_range_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/WETO/range/WETO_fulltrainingdat",i,".csv",sep=""))
  WETO_range_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/WETO/range/WETO_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  WETO_range_od <- paste("./tgb/Maxent_SWD_mode/WETO/range/WETO_range_kfold_",i,sep="")
  dir.create(WETO_range_od)
  
  
  ## Maxent command and save
  
  WETO_range_me <- maxent(x=WETO_range_training_locs[,WETO_vars], p=WETO_range_training_locs[,index], removeDuplicates = FALSE, args = WETO_range_max_args, path=WETO_range_od)
  
  save(WETO_range_me, file = paste(WETO_range_od,"/WETO_range_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  WETO_range_e <- evaluate(p=WETO_range_testing_locs[which(WETO_range_testing_locs$Type == 1), WETO_vars], a=WETO_range_testing_locs[which(WETO_range_testing_locs$Type == 0), WETO_vars], model=WETO_range_me)
  print(WETO_range_e)
  
  WETO_range_internal_auc[i] <- WETO_range_e@auc
  
  dput(WETO_range_e, file=paste("./tgb/Maxent_SWD_mode/WETO/range/WETO_range_kfold_",i,"/internal_eval_",i,sep=""))
  
}



## Saving auc scores as a csv
WETO_range_aucdat <- as.data.frame(WETO_range_internal_auc)
WETO_range_aucdat$fold <- row.names(WETO_range_aucdat)

write.csv(WETO_range_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/WETO/range/WETO_range_Internal_AUC.csv", row.names = FALSE)


###### political study extent ######

## Setting parameters from 6.1_Tuning script

WETO_political_max_args <- c(basicargs, features = c("nohinge", "noquadratic", "noproduct"), "betamultiplier=0.25")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
WETO_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  WETO_political_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/WETO/political/WETO_fulltrainingdat",i,".csv",sep=""))
  WETO_political_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/WETO/political/WETO_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  WETO_political_od <- paste("./tgb/Maxent_SWD_mode/WETO/political/WETO_political_kfold_",i,sep="")
  dir.create(WETO_political_od)
  
  
  ## Maxent command and save
  
  WETO_political_me <- maxent(x=WETO_political_training_locs[,WETO_vars], p=WETO_political_training_locs[,index], removeDuplicates = FALSE, args = WETO_political_max_args, path=WETO_political_od)
  
  save(WETO_political_me, file = paste(WETO_political_od,"/WETO_political_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  WETO_political_e <- evaluate(p=WETO_political_testing_locs[which(WETO_political_testing_locs$Type == 1), WETO_vars], a=WETO_political_testing_locs[which(WETO_political_testing_locs$Type == 0), WETO_vars], model=WETO_political_me)
  print(WETO_political_e)
  
  WETO_political_internal_auc[i] <- WETO_political_e@auc
  
  dput(WETO_political_e, file=paste("./tgb/Maxent_SWD_mode/WETO/political/WETO_political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
WETO_political_aucdat <- as.data.frame(WETO_political_internal_auc)
WETO_political_aucdat$fold <- row.names(WETO_political_aucdat)

write.csv(WETO_political_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/WETO/political/WETO_political_Internal_AUC.csv", row.names = FALSE)


###### ecotone study extent ######


## Setting parameters from 6.1_Tuning script

WETO_ecotone_max_args <- c(basicargs, features = c(""), "betamultiplier=1")


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
WETO_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  WETO_ecotone_training_locs <- read.csv(file = paste("./kfolds/SWD_mode/WETO/ecotone/WETO_fulltrainingdat",i,".csv",sep=""))
  WETO_ecotone_testing_locs <- read.csv(file = paste("./kfolds/SWD_mode/WETO/ecotone/WETO_fulltestingdat",i,".csv", sep=""))
  
  
  ## Create new directory 
  
  WETO_ecotone_od <- paste("./tgb/Maxent_SWD_mode/WETO/ecotone/WETO_ecotone_kfold_",i,sep="")
  dir.create(WETO_ecotone_od)
  
  
  ## Maxent command and save
  
  WETO_ecotone_me <- maxent(x=WETO_ecotone_training_locs[,WETO_vars], p=WETO_ecotone_training_locs[,index], removeDuplicates = FALSE, args = WETO_ecotone_max_args, path=WETO_ecotone_od)
  
  save(WETO_ecotone_me, file = paste(WETO_ecotone_od,"/WETO_ecotone_modelObject_",i,sep=""))
  
  
  ## Evaluate
  
  WETO_ecotone_e <- evaluate(p=WETO_ecotone_testing_locs[which(WETO_ecotone_testing_locs$Type == 1), WETO_vars], a=WETO_ecotone_testing_locs[which(WETO_ecotone_testing_locs$Type == 0), WETO_vars], model=WETO_ecotone_me)
  print(WETO_ecotone_e)
  
  WETO_ecotone_internal_auc[i] <- WETO_ecotone_e@auc
  
  dput(WETO_ecotone_e, file=paste("./tgb/Maxent_SWD_mode/WETO/ecotone/WETO_ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
WETO_ecotone_aucdat <- as.data.frame(WETO_ecotone_internal_auc)
WETO_ecotone_aucdat$fold <- row.names(WETO_ecotone_aucdat)

write.csv(WETO_ecotone_aucdat[,c(2,1)], file = "./tgb/Maxent_SWD_mode/WETO/ecotone/WETO_ecotone_Internal_AUC.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
