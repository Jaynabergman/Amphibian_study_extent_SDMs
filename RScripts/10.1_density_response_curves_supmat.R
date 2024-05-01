########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Script name: 

### Goal of this Script: 

#

### Notes:  

# Made from Julie's script "lyrata niche models"

### Date: July 24, 2023

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(scales)
library(raster)
library(dismo)
library(sf)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd("C:/Users/jayna/OneDrive/Desktop/School/Lee_Yaw_Lab/Amphibian_SDMs/Data_Analysis")

path<-getwd()

################################# LTSA ################################

##### Prepare data

## 1) Crop environmental layers to study extent

LTSA_envi <- raster(paste(path,"/envi_variables/model_subsets/LTSA/Normal_1991_2020_MWMT_aea.tif",sep=""))


LTSA_political <- st_read(paste(path,"/study_extents/model_subsets/LTSA/LTSA_political.shp",sep=""))
LTSA_range <- st_read(paste(path,"/study_extents/model_subsets/LTSA/LTSA_range.shp",sep=""))

LTSA_envi_political <- mask(LTSA_envi, LTSA_political)
LTSA_envi_range <- mask(LTSA_envi, LTSA_range)


## 2) extract random points from layers

LTSA_backpts_political <- data.frame(sampleRandom(LTSA_envi_political, size=5000, na.rm=TRUE, xy=TRUE, row.names=FALSE))
LTSA_backpts_range <- data.frame(sampleRandom(LTSA_envi_range, size=10000, na.rm=TRUE, xy=TRUE, row.names=FALSE))

write.csv(LTSA_backpts_political, "./back_pts/LTSA/MWMT_back_pts_political.csv")
write.csv(LTSA_backpts_range, "./back_pts/LTSA/MWMT_back_pts_range.csv")

LTSA_backpts_political <- read.csv("./back_pts/LTSA/MWMT_back_pts_political.csv")
LTSA_backpts_range <- read.csv("./back_pts/LTSA/MWMT_back_pts_range.csv")


## 3) read in extracted envi values for input localities

LTSA_input_locs_political <- read.csv(paste(path,"./extracted_envi_vars_values/LTSA/political/LTSA_locs_political.csv", sep=""))
LTSA_input_locs_political <- subset(LTSA_input_locs_political, select = c(Long_m, Lat_m, Normal_1991_2020_MWMT_aea))

LTSA_input_locs_political <- na.omit(LTSA_input_locs_political)
LTSA_input_locs_political$Normal_1991_2020_MWMT_aea <- as.numeric(LTSA_input_locs_political$Normal_1991_2020_MWMT_aea)

LTSA_input_locs_range <- read.csv(paste(path,"./extracted_envi_vars_values/LTSA/range/LTSA_locs_range.csv", sep=""))
LTSA_input_locs_range <- subset(LTSA_input_locs_range, select = c(Long_m, Lat_m, Normal_1991_2020_MWMT_aea))

LTSA_input_locs_range <- na.omit(LTSA_input_locs_range)


##### Make Plots

### density plots

LTSA_m1_political <- max(c(LTSA_input_locs_political$Normal_1991_2020_MWMT_aea,LTSA_backpts_political$Normal_1991_2020_MWMT_aea))
LTSA_m2_political <-max(LTSA_m1_political)

LTSA_n1_political <- min(c(LTSA_input_locs_political$Normal_1991_2020_MWMT_aea,LTSA_backpts_political$Normal_1991_2020_MWMT_aea))
LTSA_n2_political <- min(LTSA_n1_political)


LTSA_m1_range <- max(c(LTSA_input_locs_range$Normal_1991_2020_MWMT_aea, LTSA_backpts_range$Normal_1991_2020_MWMT_aea))
LTSA_m2_range <-max(LTSA_m1_range)

LTSA_n1_range <- min(c(LTSA_input_locs_range$Normal_1991_2020_MWMT_aea, LTSA_backpts_range$Normal_1991_2020_MWMT_aea))
LTSA_n2_range <- min(LTSA_n1_range)


# Background data

LTSA_d1_political <- density(LTSA_backpts_political[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=LTSA_n2_political+-10,to=LTSA_m2_political+10)
LTSA_d1_range <- density(LTSA_backpts_range[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=LTSA_n2_range+-10,to=LTSA_m2_range+10)


# Locality data

LTSA_d2_political <- density(LTSA_input_locs_political[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=LTSA_n2_political+-10,to=LTSA_m2_political+10)
LTSA_d2_range <- density(LTSA_input_locs_range[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=LTSA_n2_range+-10,to=LTSA_m2_range+10)


### Plot political 

ymax1<-round(max(LTSA_d1_political$y,LTSA_d2_political$y)*1.10,digits=3)


pdf("./Density_plots/LTSA_response_MWMT_political.pdf")
par(mar = c(5,5,2,5))
#plot(LTSA_d1_political, main="",xlim=c(LTSA_n2_political+-10,LTSA_m2_political+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
plot(LTSA_d1_political, main="",xlim=c(LTSA_n2_range+-10,LTSA_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Mean Warmest-Month Temperature (Political Study Extent)", ylab="Kernel Probability Density")
polygon(LTSA_d1_political, col="black")
polygon(LTSA_d2_political, col=alpha("lightgrey",0.5),border=NA)

dev.off()

### Plot range

#ymax1<-round(max(LTSA_d1_range$y,LTSA_d2_range$y)*1.10,digits=3)

pdf("./Density_plots/LTSA_response_MWMT_range.pdf")
par(mar = c(5,5,2,5))
plot(LTSA_d1_range, main="",xlim=c(LTSA_n2_range+-10,LTSA_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Mean Warmest-Monthly Temperature (Range-Wide Study Extent)", ylab="Kernel Probability Density")
polygon(LTSA_d1_range, col="black")
polygon(LTSA_d2_range, col=alpha("lightgrey",0.5),border=NA)

dev.off()


################################# WETO ################################

##### Prepare data

## 1) Crop environmental layers to study extent

WETO_envi <- raster(paste(path,"/envi_variables/model_subsets/WETO/Normal_1991_2020_PPT_sm_aea.tif",sep=""))


WETO_political <- st_read(paste(path,"/study_extents/model_subsets/WETO/WETO_political.shp",sep=""))
WETO_range <- st_read(paste(path,"/study_extents/model_subsets/WETO/WETO_range.shp",sep=""))

WETO_envi_political <- mask(WETO_envi, WETO_political)
WETO_envi_range <- mask(WETO_envi, WETO_range)


## 2) extract random points from layers

WETO_backpts_political <- data.frame(sampleRandom(WETO_envi_political, size=5000, na.rm=TRUE, xy=TRUE, row.names=FALSE))
WETO_backpts_range <- data.frame(sampleRandom(WETO_envi_range, size=10000, na.rm=TRUE, xy=TRUE, row.names=FALSE))

write.csv(WETO_backpts_political, "./back_pts/WETO/PPT_sm_back_pts_political.csv")
write.csv(WETO_backpts_range, "./back_pts/WETO/PPT_sm_back_pts_range.csv")

WETO_backpts_political <- read.csv("./back_pts/WETO/PPT_sm_back_pts_political.csv")
WETO_backpts_range <- read.csv("./back_pts/WETO/PPT_sm_back_pts_range.csv")


## 3) read in extracted envi values for input localities

WETO_input_locs_political <- read.csv(paste(path,"./extracted_envi_vars_values/WETO/political/WETO_locs_political.csv", sep=""))
WETO_input_locs_political <- subset(WETO_input_locs_political, select = c(Long_m, Lat_m, Normal_1991_2020_PPT_sm_aea))

WETO_input_locs_political <- na.omit(WETO_input_locs_political)

WETO_input_locs_range <- read.csv(paste(path,"./extracted_envi_vars_values/WETO/range/WETO_locs_range.csv", sep=""))
WETO_input_locs_range <- subset(WETO_input_locs_range, select = c(Long_m, Lat_m, Normal_1991_2020_PPT_sm_aea))

WETO_input_locs_range <- na.omit(WETO_input_locs_range)


##### Make Plots

### density plots

WETO_m1_political <- max(c(WETO_input_locs_political$Normal_1991_2020_PPT_sm_aea,WETO_backpts_political$Normal_1991_2020_PPT_sm_aea))
WETO_m2_political <-max(WETO_m1_political)

WETO_n1_political <- min(c(WETO_input_locs_political$Normal_1991_2020_PPT_sm_aea,WETO_backpts_political$Normal_1991_2020_PPT_sm_aea))
WETO_n2_political <- min(WETO_n1_political)


WETO_m1_range <- max(c(WETO_input_locs_range$Normal_1991_2020_PPT_sm_aea, WETO_backpts_range$Normal_1991_2020_PPT_sm_aea))
WETO_m2_range <-max(WETO_m1_range)

WETO_n1_range <- min(c(WETO_input_locs_range$Normal_1991_2020_PPT_sm_aea, WETO_backpts_range$Normal_1991_2020_PPT_sm_aea))
WETO_n2_range <- min(WETO_n1_range)


# Background data

WETO_d1_political <- density(WETO_backpts_political[,"Normal_1991_2020_PPT_sm_aea"],bw="SJ",from=WETO_n2_political+-10,to=WETO_m2_political+10)
WETO_d1_range <- density(WETO_backpts_range[,"Normal_1991_2020_PPT_sm_aea"],bw="SJ",from=WETO_n2_range+-10,to=WETO_m2_range+10)


# Locality data

WETO_d2_political <- density(WETO_input_locs_political[,"Normal_1991_2020_PPT_sm_aea"],bw="SJ",from=WETO_n2_political+-10,to=WETO_m2_political+10)
WETO_d2_range <- density(WETO_input_locs_range[,"Normal_1991_2020_PPT_sm_aea"],bw="SJ",from=WETO_n2_range+-10,to=WETO_m2_range+10)


### Plot political 

ymax1<-round(max(WETO_d1_political$y,WETO_d2_political$y)*1.10,digits=3)


pdf("./Density_plots/WETO_response_PPT_sm_political.pdf")
par(mar = c(5,5,2,5))
#plot(WETO_d1_political, main="",xlim=c(WETO_n2_political+-10,WETO_m2_political+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
plot(WETO_d1_political, main="",xlim=c(WETO_n2_range+-10,WETO_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Precipitation Summer (Political Study Extent)", ylab="Kernel Probability Density")
polygon(WETO_d1_political, col="black")
polygon(WETO_d2_political, col=alpha("lightgrey",0.5),border=NA)

dev.off()


### Plot range

#ymax1<-round(max(WETO_d1_range$y,WETO_d2_range$y)*1.10,digits=3)

pdf("./Density_plots/WETO_response_PPT_sm_range.pdf")
par(mar = c(5,5,2,5))
plot(WETO_d1_range, main="",xlim=c(WETO_n2_range+-10,WETO_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Precipitation Summer (Range-Wide Study Extent)", ylab="Kernel Probability Density")
polygon(WETO_d1_range, col="black")
polygon(WETO_d2_range, col=alpha("lightgrey",0.5),border=NA)

dev.off()


################################# BCFR ################################

##### Prepare data

## 1) Crop environmental layers to study extent

BCFR_envi <- raster(paste(path,"/envi_variables/model_subsets/BCFR/Normal_1991_2020_Tave_sp_aea.tif",sep=""))


BCFR_political <- st_read(paste(path,"/study_extents/model_subsets/BCFR/BCFR_political.shp",sep=""))
BCFR_range <- st_read(paste(path,"/study_extents/model_subsets/BCFR/BCFR_range.shp",sep=""))

BCFR_envi_political <- mask(BCFR_envi, BCFR_political)
BCFR_envi_range <- mask(BCFR_envi, BCFR_range)


## 2) extract random points from layers

BCFR_backpts_political <- data.frame(sampleRandom(BCFR_envi_political, size=5000, na.rm=TRUE, xy=TRUE, row.names=FALSE))
BCFR_backpts_range <- data.frame(sampleRandom(BCFR_envi_range, size=10000, na.rm=TRUE, xy=TRUE, row.names=FALSE))

write.csv(BCFR_backpts_political, "./back_pts/BCFR/Tave_sp_back_pts_political.csv")
write.csv(BCFR_backpts_range, "./back_pts/BCFR/Tave_sp_back_pts_range.csv")

BCFR_backpts_political <- read.csv("./back_pts/BCFR/Tave_sp_back_pts_political.csv")
BCFR_backpts_range <- read.csv("./back_pts/BCFR/Tave_sp_back_pts_range.csv")


## 3) read in extracted envi values for input localities

BCFR_input_locs_political <- read.csv(paste(path,"./extracted_envi_vars_values/BCFR/political/BCFR_locs_political.csv", sep=""))
BCFR_input_locs_political <- subset(BCFR_input_locs_political, select = c(Long_m, Lat_m, Normal_1991_2020_Tave_sp_aea))

BCFR_input_locs_political <- na.omit(BCFR_input_locs_political)

BCFR_input_locs_range <- read.csv(paste(path,"./extracted_envi_vars_values/BCFR/range/BCFR_locs_range.csv", sep=""))
BCFR_input_locs_range <- subset(BCFR_input_locs_range, select = c(Long_m, Lat_m, Normal_1991_2020_Tave_sp_aea))

BCFR_input_locs_range <- na.omit(BCFR_input_locs_range)


##### Make Plots

### density plots

BCFR_m1_political <- max(c(BCFR_input_locs_political$Normal_1991_2020_Tave_sp_aea,BCFR_backpts_political$Normal_1991_2020_Tave_sp_aea))
BCFR_m2_political <-max(BCFR_m1_political)

BCFR_n1_political <- min(c(BCFR_input_locs_political$Normal_1991_2020_Tave_sp_aea,BCFR_backpts_political$Normal_1991_2020_Tave_sp_aea))
BCFR_n2_political <- min(BCFR_n1_political)


BCFR_m1_range <- max(c(BCFR_input_locs_range$Normal_1991_2020_Tave_sp_aea, BCFR_backpts_range$Normal_1991_2020_Tave_sp_aea))
BCFR_m2_range <-max(BCFR_m1_range)

BCFR_n1_range <- min(c(BCFR_input_locs_range$Normal_1991_2020_Tave_sp_aea, BCFR_backpts_range$Normal_1991_2020_Tave_sp_aea))
BCFR_n2_range <- min(BCFR_n1_range)


# Background data

BCFR_d1_political <- density(BCFR_backpts_political[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=BCFR_n2_political+-10,to=BCFR_m2_political+10)
BCFR_d1_range <- density(BCFR_backpts_range[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=BCFR_n2_range+-10,to=BCFR_m2_range+10)


# Locality data

BCFR_d2_political <- density(BCFR_input_locs_political[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=BCFR_n2_political+-10,to=BCFR_m2_political+10)
BCFR_d2_range <- density(BCFR_input_locs_range[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=BCFR_n2_range+-10,to=BCFR_m2_range+10)


### Plot political 

ymax1<-round(max(BCFR_d1_political$y,BCFR_d2_political$y)*1.10,digits=3)


pdf("./Density_plots/BCFR_response_Tave_sp_political.pdf")
par(mar = c(5,5,2,5))
#plot(BCFR_d1_political, main="",xlim=c(BCFR_n2_political+-10,BCFR_m2_political+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
plot(BCFR_d1_political, main="",xlim=c(BCFR_n2_range+-10,BCFR_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Average Temperature Spring (Political Study Extent)", ylab="Kernel Probability Density")
polygon(BCFR_d1_political, col="black")
polygon(BCFR_d2_political, col=alpha("lightgrey",0.5),border=NA)

dev.off()

### Plot range

#ymax1<-round(max(BCFR_d1_range$y,BCFR_d2_range$y)*1.10,digits=3)

pdf("./Density_plots/BCFR_response_Tave_sp_range.pdf")
par(mar = c(5,5,2,5))
plot(BCFR_d1_range, main="",xlim=c(BCFR_n2_range+-10,BCFR_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Average Temperature Spring (Range-Wide Study Extent)", ylab="Kernel Probability Density")
polygon(BCFR_d1_range, col="black")
polygon(BCFR_d2_range, col=alpha("lightgrey",0.5),border=NA)

dev.off()


################################# CSFR ################################

##### Prepare data

## 1) Crop environmental layers to study extent

CSFR_envi <- raster(paste(path,"/envi_variables/model_subsets/CSFR/Normal_1991_2020_MWMT_aea.tif",sep=""))


CSFR_political <- st_read(paste(path,"/study_extents/model_subsets/CSFR/CSFR_political.shp",sep=""))
CSFR_range <- st_read(paste(path,"/study_extents/model_subsets/CSFR/CSFR_range.shp",sep=""))

CSFR_envi_political <- mask(CSFR_envi, CSFR_political)
CSFR_envi_range <- mask(CSFR_envi, CSFR_range)


## 2) extract random points from layers

CSFR_backpts_political <- data.frame(sampleRandom(CSFR_envi_political, size=5000, na.rm=TRUE, xy=TRUE, row.names=FALSE))
CSFR_backpts_range <- data.frame(sampleRandom(CSFR_envi_range, size=10000, na.rm=TRUE, xy=TRUE, row.names=FALSE))

write.csv(CSFR_backpts_political, "./back_pts/CSFR/MWMT_back_pts_political.csv")
write.csv(CSFR_backpts_range, "./back_pts/CSFR/MWMT_back_pts_range.csv")

CSFR_backpts_political <- read.csv("./back_pts/CSFR/MWMT_back_pts_political.csv")
CSFR_backpts_range <- read.csv("./back_pts/CSFR/MWMT_back_pts_range.csv")


## 3) read in extracted envi values for input localities

CSFR_input_locs_political <- read.csv(paste(path,"./extracted_envi_vars_values/CSFR/political/CSFR_locs_political.csv", sep=""))
CSFR_input_locs_political <- subset(CSFR_input_locs_political, select = c(Long_m, Lat_m, Normal_1991_2020_MWMT_aea))

CSFR_input_locs_political <- na.omit(CSFR_input_locs_political)

CSFR_input_locs_range <- read.csv(paste(path,"./extracted_envi_vars_values/CSFR/range/CSFR_locs_range.csv", sep=""))
CSFR_input_locs_range <- subset(CSFR_input_locs_range, select = c(Long_m, Lat_m, Normal_1991_2020_MWMT_aea))

CSFR_input_locs_range <- na.omit(CSFR_input_locs_range)


##### Make Plots

### density plots

CSFR_m1_political <- max(c(CSFR_input_locs_political$Normal_1991_2020_MWMT_aea,CSFR_backpts_political$Normal_1991_2020_MWMT_aea))
CSFR_m2_political <-max(CSFR_m1_political)

CSFR_n1_political <- min(c(CSFR_input_locs_political$Normal_1991_2020_MWMT_aea,CSFR_backpts_political$Normal_1991_2020_MWMT_aea))
CSFR_n2_political <- min(CSFR_n1_political)


CSFR_m1_range <- max(c(CSFR_input_locs_range$Normal_1991_2020_MWMT_aea, CSFR_backpts_range$Normal_1991_2020_MWMT_aea))
CSFR_m2_range <-max(CSFR_m1_range)

CSFR_n1_range <- min(c(CSFR_input_locs_range$Normal_1991_2020_MWMT_aea, CSFR_backpts_range$Normal_1991_2020_MWMT_aea))
CSFR_n2_range <- min(CSFR_n1_range)


# Background data

CSFR_d1_political <- density(CSFR_backpts_political[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=CSFR_n2_political+-10,to=CSFR_m2_political+10)
CSFR_d1_range <- density(CSFR_backpts_range[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=CSFR_n2_range+-10,to=CSFR_m2_range+10)


# Locality data

CSFR_d2_political <- density(CSFR_input_locs_political[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=CSFR_n2_political+-10,to=CSFR_m2_political+10)
CSFR_d2_range <- density(CSFR_input_locs_range[,"Normal_1991_2020_MWMT_aea"],bw="SJ",from=CSFR_n2_range+-10,to=CSFR_m2_range+10)


### Plot political 

ymax1<-round(max(CSFR_d1_political$y,CSFR_d2_political$y)*1.10,digits=3)


pdf("./Density_plots/CSFR_response_MWMT_political.pdf")
par(mar = c(5,5,2,5))
#plot(CSFR_d1_political, main="",xlim=c(CSFR_n2_political+-10,CSFR_m2_political+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
plot(CSFR_d1_political, main="",xlim=c(CSFR_n2_range+-10,CSFR_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Mean Warmest-Monthly Temperature (Political Study Extent)", ylab="Kernel Probability Density")
polygon(CSFR_d1_political, col="black")
polygon(CSFR_d2_political, col=alpha("lightgrey",0.5),border=NA)

dev.off()

### Plot range

#ymax1<-round(max(CSFR_d1_range$y,CSFR_d2_range$y)*1.10,digits=3)

pdf("./Density_plots/CSFR_response_MWMT_range.pdf")
par(mar = c(5,5,2,5))
plot(CSFR_d1_range, main="",xlim=c(CSFR_n2_range+-10,CSFR_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Mean Warmest-Monthly Temperature (Range-Wide Study Extent)", ylab="Kernel Probability Density")
polygon(CSFR_d1_range, col="black")
polygon(CSFR_d2_range, col=alpha("lightgrey",0.5),border=NA)

dev.off()


################################# CATO ################################

##### Prepare data

## 1) Crop environmental layers to study extent

CATO_envi <- raster(paste(path,"/envi_variables/model_subsets/CATO/Normal_1991_2020_Tave_sp_aea.tif",sep=""))


CATO_political <- st_read(paste(path,"/study_extents/model_subsets/CATO/CATO_political.shp",sep=""))
CATO_range <- st_read(paste(path,"/study_extents/model_subsets/CATO/CATO_range.shp",sep=""))

CATO_envi_political <- mask(CATO_envi, CATO_political)
CATO_envi_range <- mask(CATO_envi, CATO_range)


## 2) extract random points from layers

CATO_backpts_political <- data.frame(sampleRandom(CATO_envi_political, size=5000, na.rm=TRUE, xy=TRUE, row.names=FALSE))
CATO_backpts_range <- data.frame(sampleRandom(CATO_envi_range, size=10000, na.rm=TRUE, xy=TRUE, row.names=FALSE))

write.csv(CATO_backpts_political, "./back_pts/CATO/Tave_sp_back_pts_political.csv")
write.csv(CATO_backpts_range, "./back_pts/CATO/Tave_sp_back_pts_range.csv")

CATO_backpts_political <-read.csv("./back_pts/CATO/Tave_sp_back_pts_political.csv")
CATO_backpts_range <-read.csv("./back_pts/CATO/Tave_sp_back_pts_range.csv")


## 3) read in extracted envi values for input localities

CATO_input_locs_political <- read.csv(paste(path,"./extracted_envi_vars_values/CATO/political/CATO_locs_political.csv", sep=""))
CATO_input_locs_political <- subset(CATO_input_locs_political, select = c(Long_m, Lat_m, Normal_1991_2020_Tave_sp_aea))

CATO_input_locs_political <- na.omit(CATO_input_locs_political)

CATO_input_locs_range <- read.csv(paste(path,"./extracted_envi_vars_values/CATO/range/CATO_locs_range.csv", sep=""))
CATO_input_locs_range <- subset(CATO_input_locs_range, select = c(Long_m, Lat_m, Normal_1991_2020_Tave_sp_aea))

CATO_input_locs_range <- na.omit(CATO_input_locs_range)


##### Make Plots

### density plots

CATO_m1_political <- max(c(CATO_input_locs_political$Normal_1991_2020_Tave_sp_aea,CATO_backpts_political$Normal_1991_2020_Tave_sp_aea))
CATO_m2_political <-max(CATO_m1_political)

CATO_n1_political <- min(c(CATO_input_locs_political$Normal_1991_2020_Tave_sp_aea,CATO_backpts_political$Normal_1991_2020_Tave_sp_aea))
CATO_n2_political <- min(CATO_n1_political)


CATO_m1_range <- max(c(CATO_input_locs_range$Normal_1991_2020_Tave_sp_aea, CATO_backpts_range$Normal_1991_2020_Tave_sp_aea))
CATO_m2_range <-max(CATO_m1_range)

CATO_n1_range <- min(c(CATO_input_locs_range$Normal_1991_2020_Tave_sp_aea, CATO_backpts_range$Normal_1991_2020_Tave_sp_aea))
CATO_n2_range <- min(CATO_n1_range)


# Background data

CATO_d1_political <- density(CATO_backpts_political[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=CATO_n2_political+-10,to=CATO_m2_political+10)
CATO_d1_range <- density(CATO_backpts_range[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=CATO_n2_range+-10,to=CATO_m2_range+10)


# Locality data

CATO_d2_political <- density(CATO_input_locs_political[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=CATO_n2_political+-10,to=CATO_m2_political+10)
CATO_d2_range <- density(CATO_input_locs_range[,"Normal_1991_2020_Tave_sp_aea"],bw="SJ",from=CATO_n2_range+-10,to=CATO_m2_range+10)


### Plot political 

ymax1<-round(max(CATO_d1_political$y,CATO_d2_political$y)*1.10,digits=3)


pdf("./Density_plots/CATO_response_Tave_sp_political.pdf")
par(mar = c(5,5,2,5))
#plot(CATO_d1_political, main="",xlim=c(CATO_n2_political+-10,CATO_m2_political+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
plot(CATO_d1_political, main="",xlim=c(CATO_n2_range+-10,CATO_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Average Temperature Spring (Political Study Extent)", ylab="Kernel Probability Density")
polygon(CATO_d1_political, col="black")
polygon(CATO_d2_political, col=alpha("lightgrey",0.5),border=NA)

dev.off()

### Plot range

ymax1<-round(max(CATO_d1_range$y,CATO_d2_range$y)*1.10,digits=3)

pdf("./Density_plots/CATO_response_Tave_sp_range.pdf")
par(mar = c(5,5,2,5))
plot(CATO_d1_range, main="",xlim=c(CATO_n2_range+-10,CATO_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Average Temperature Spring (Range-Wide Study Extent)", ylab="Kernel Probability Density")
polygon(CATO_d1_range, col="black")
polygon(CATO_d2_range, col=alpha("lightgrey",0.5),border=NA)

dev.off()


################################# TISA ################################

##### Prepare data

## 1) Crop environmental layers to study extent

TISA_envi <- raster(paste(path,"/envi_variables/model_subsets/TISA/Normal_1991_2020_Tave_at_aea.tif",sep=""))


TISA_political <- st_read(paste(path,"/study_extents/model_subsets/TISA/TISA_political.shp",sep=""))
TISA_range <- st_read(paste(path,"/study_extents/model_subsets/TISA/TISA_range.shp",sep=""))

TISA_envi_political <- mask(TISA_envi, TISA_political)
TISA_envi_range <- mask(TISA_envi, TISA_range)


## 2) extract random points from layers

TISA_backpts_political <- data.frame(sampleRandom(TISA_envi_political, size=5000, na.rm=TRUE, xy=TRUE, row.names=FALSE))
TISA_backpts_range <- data.frame(sampleRandom(TISA_envi_range, size=10000, na.rm=TRUE, xy=TRUE, row.names=FALSE))

write.csv(TISA_backpts_political, "./back_pts/TISA/Tave_at_back_pts_political.csv")
write.csv(TISA_backpts_range, "./back_pts/TISA/Tave_at_back_pts_range.csv")

TISA_backpts_political <- read.csv("./back_pts/TISA/Tave_at_back_pts_political.csv")
TISA_backpts_range <- read.csv("./back_pts/TISA/Tave_at_back_pts_range.csv")


## 3) read in extracted envi values for input localities

TISA_input_locs_political <- read.csv(paste(path,"./extracted_envi_vars_values/TISA/political/TISA_locs_political.csv", sep=""))
TISA_input_locs_political <- subset(TISA_input_locs_political, select = c(Long_m, Lat_m, Normal_1991_2020_Tave_at_aea))

TISA_input_locs_political <- na.omit(TISA_input_locs_political)

TISA_input_locs_range <- read.csv(paste(path,"./extracted_envi_vars_values/TISA/range/TISA_locs_range.csv", sep=""))
TISA_input_locs_range <- subset(TISA_input_locs_range, select = c(Long_m, Lat_m, Normal_1991_2020_Tave_at_aea))

TISA_input_locs_range <- na.omit(TISA_input_locs_range)


##### Make Plots

### density plots

TISA_m1_political <- max(c(TISA_input_locs_political$Normal_1991_2020_Tave_at_aea,TISA_backpts_political$Normal_1991_2020_Tave_at_aea))
TISA_m2_political <-max(TISA_m1_political)

TISA_n1_political <- min(c(TISA_input_locs_political$Normal_1991_2020_Tave_at_aea,TISA_backpts_political$Normal_1991_2020_Tave_at_aea))
TISA_n2_political <- min(TISA_n1_political)


TISA_m1_range <- max(c(TISA_input_locs_range$Normal_1991_2020_Tave_at_aea, TISA_backpts_range$Normal_1991_2020_Tave_at_aea))
TISA_m2_range <-max(TISA_m1_range)

TISA_n1_range <- min(c(TISA_input_locs_range$Normal_1991_2020_Tave_at_aea, TISA_backpts_range$Normal_1991_2020_Tave_at_aea))
TISA_n2_range <- min(TISA_n1_range)


# Background data

TISA_d1_political <- density(TISA_backpts_political[,"Normal_1991_2020_Tave_at_aea"],bw="SJ",from=TISA_n2_political+-10,to=TISA_m2_political+10)
TISA_d1_range <- density(TISA_backpts_range[,"Normal_1991_2020_Tave_at_aea"],bw="SJ",from=TISA_n2_range+-10,to=TISA_m2_range+10)


# Locality data

TISA_d2_political <- density(TISA_input_locs_political[,"Normal_1991_2020_Tave_at_aea"],bw="SJ",from=TISA_n2_political+-10,to=TISA_m2_political+10)
TISA_d2_range <- density(TISA_input_locs_range[,"Normal_1991_2020_Tave_at_aea"],bw="SJ",from=TISA_n2_range+-10,to=TISA_m2_range+10)


### Plot political 

ymax1<-round(max(TISA_d1_political$y,TISA_d2_political$y)*1.10,digits=3)


pdf("./Density_plots/TISA_response_Tave_at_political.pdf")
par(mar = c(5,5,2,5))
#plot(TISA_d1_political, main="",xlim=c(TISA_n2_political+-10,TISA_m2_political+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
plot(TISA_d1_political, main="",xlim=c(TISA_n2_range+-10,TISA_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Average Temperature Autumn (Political Study Extent)", ylab="Kernel Probability Density")
polygon(TISA_d1_political, col="black")
polygon(TISA_d2_political, col=alpha("lightgrey",0.5),border=NA)

dev.off()

### Plot range

ymax1<-round(max(TISA_d1_range$y,TISA_d2_range$y)*1.10,digits=3)

pdf("./Density_plots/TISA_response_Tave_at_range.pdf")
par(mar = c(5,5,2,5))
plot(TISA_d1_range, main="",xlim=c(TISA_n2_range+-10,TISA_m2_range+10),ylim=c(0,ymax1), xlab="", ylab="", xaxs="i", yaxs="i")
title(xlab="Average Temperature Autumn (Range-Wide Study Extent)", ylab="Kernel Probability Density")
polygon(TISA_d1_range, col="black")
polygon(TISA_d2_range, col=alpha("lightgrey",0.5),border=NA)

dev.off()


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################


