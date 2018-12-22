#------------------------------------------------------------------------------#

#			                          Ghana

#------------------------------------------------------------------------------#

## PURPOSE: 	This is code for measuring built up and population in 3 * 3 and 5* 5 neighboorhood pixels in 1975,1990,2000,2014.

# 1. SETTINGS
# 1.1. Load packages - Load main packages, but other may be loaded in
# each code.


# 2. FILE PATHS
# 2.1. One Drive path - Automatically defines OneDrive path which are references for all other paths in the project
# 2.2. Folder paths - Define sub folders paths


# 3. SECTIONS
# 3.1 Load shape files 
# 3.2 Calculate population mean value and sum in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5)
# 3.3 Calculate built up mean value and count of pixels with builtup in spatial lag 1-(3 x3) and spatial lag 2-(5 X5).
# 3.4 Output the dataframe


## TTL       :                         Forhad S.
## Written by:	   Originally:         Brian Blankenspoor
#                  Modified:           Varnitha Kurli Reddy
 
## Last updated:                                        December 21st 2018

#------------------------------------------------------------------------------#
#### 1. SETTINGS ####

# Set warnings 
options(warn = 0)

#### Delete everything already in R memory
# (Equivalent to clear all in meana)
rm(list = ls())

#-------------------------#
#### 1.1 Load packages ####
library(raster)
library(rgdal)
library(readstata13)
library(gdalUtils)
library(stringr)
library(foreign)
#------------------------------------------------------------------------------#
#### 2. FILE PATHS 	####

#### 2.1. OneDrive paths	####

# Varnitha



# Varnitha Laptop
if (Sys.getenv("USERNAME") == "WB538005" ){
  OneDrive<-file.path("C:/Users/WB538005/WBG")
  
}

# Brian Blankespoor

if (Sys.getenv("USERNAME") =="" ){
  
  OneDrive<-file.path("E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_")
}

#-------------------------------------------------------------------------------
#### 2.2. Folder paths ####
#-------------------------------------------------------------------------------
#PRIMARY FOLDERS

GHANA<-file.path(OneDrive, "Brian Blankespoor - Ghana_")

GIS<- file.path(GHANA, "gis_data/003_boundaries/gh_eas_dissolve_correction")

ROADS<- file.path(GHANA, "gis_data/018_transportation/roads_gha.gdb")

#------------------------------------------------------------------------------
# output files#

OUTPUTS   			        <- file.path(GHANA,"Statistics  Tables" )


#-------------------------------------------------------------------------------
#### 3.1 Load the shape file
#-------------------------------------------------------------------------------
#ghana shapefile
Ghana.admin.0 <- readOGR(dsn=GIS, layer="gh_adm0_merge2_clean_aux_wgs84")

#-------------------------------------------------------------------------------
#### 3.2 Calculate population mean value and sum value in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5)
#-------------------------------------------------------------------------------
# read GHS POP at 250m #
years <- c(1975,1990,2000,2014)
  
for (year in years) {
  print(year)

  outname <- paste0("ghs_data",year,".dta")
  pop.year <- year

  ##
  ## POP
  ## GHS_POP_GPW41975_GLOBE_R2015A_54009_250_v1_0
  if (year==2014) {
	pop.year = 2015
  }
  ghspop_t <- paste0("E:/data/GLB_00/016_society/GHS_POP/GHS_POP_GPW4/GHS_POP_GPW4",pop.year,"_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW4",pop.year,"_GLOBE_R2015A_54009_250_v1_0.tif")
  ghspopzip_t <- paste0(dirname(ghspop_t),".zip")
  if(!file.exists(ghspop_t)){
    zipF<- ghspopzip_t
    OUTPUTS<- dirname(dirname(ghspop_t))
    unzip(zipF,exdir=OUTPUTS)
  }

  myraster <- raster(ghspop_t)

  # ghana cleaned with coast #
  ply.prj <- spTransform(Ghana.admin.0,proj4string(myraster))
  e <- extent(ply.prj)
  myraster.crop <- crop(myraster, e, snap="out")
  rast.pop <- mask(myraster.crop,myshp)
  names(rast.pop) <- paste0("POPGHS",pop.year)
  
  ## Creating 3x3 window (spatial lag = 1)

  M3 <- matrix(c(1,1,1,
                 1,0,1,
                 1,1,1), nrow=3)
  ## Calculate mean 3x3

  pop.mean.3x3 <- focal(rast.pop, w=M3, fun=mean,na.rm=T)
  names(pop.mean) <- paste0("mean3x3_POPGHS",pop.year)
  ##spplot(pop.mean)

  ## Sum in focal 3x3
  pop.sum.3x3 <- focal(rast.pop, w=M3, fun=sum,na.rm=T)
  names(pop.sum) <- paste0("sum3x3_POPGHS",pop.year)

  ## Creating 5x5 window (spatial lag = 2)
  ## 
  
  ## weights matrix, exclude itself
  M5 <- matrix(c(1,1,1,1,1,
                 1,0,0,0,1,
                 1,0,0,0,1,
                 1,0,0,0,1,
                 1,1,1,1,1), nrow=5)
  
  ## Calculate mean 5x5
  
  pop.mean.5x5 <- focal(rast.pop, w=M5, fun=mean,na.rm=T)
  names(pop.mean) <- paste0("mean5x5_POPGHS",pop.year)
  ##spplot(pop.mean)
  
  ## Sum in focal 5x5
  pop.sum.5x5 <- focal(rast.pop, w=M5, fun=sum,na.rm=T)
  names(pop.sum) <- paste0("sum5x5_POPGHS",pop.year) 
  
  #-------------------------------------------------------------------------------
  #### 3.3 Calculate built up mean value and count of pixels with builtup in spatial lag 1-(3 x3) and spatial lag 2-(5 X5).
  #---------------------------------------------------------------------------------
  ##
  ## share of builtup in pixel 
  ## global raster file, proj=mol
  rast <- raster(paste0("C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/gis_data/016_society/GHS_Built/GHS_BUILT_LDS",yyyy,"_GLOBE_R2016A_54009_250_v1_0/GHS_BUILT_LDS",yyyy,"_GLOBE_R2016A_54009_250_v1_0.tif"))
  ## crop by poly
  rast.crop <- crop(rast, e, snap="out")
  builtup <- mask(rast.crop,ply.prj)
  names(builtup) <- paste0("BUILTGHS",year)

  ## Calculate mean 3x3
  builtup.mean.3x3 <- focal(builtup, w=M3, fun=mean,na.rm=T)
  names(builtup.mean) <- paste0("mean3x3_BUILTGHS",year)
  ##spplot(rast.mean)
  
  
  ## Sum in focal 3x3
 
  builtup.sum.3x3 <- focal(builtup, w=M3, fun=sum,na.rm=T)
  names(builtup.sum) <- paste0("sum3x3_BUILTGHS",year)
  
  
  ## Calculate mean 5x5
  builtup.mean.5x5 <- focal(builtup, w=M5, fun=mean,na.rm=T)
  names(builtup.mean) <- paste0("mean5x5_BUILTGHS",year)
  ##spplot(rast.mean)
  
  
  ## Sum in focal 5x5
  
  builtup.sum.5x5 <- focal(builtup, w=M5, fun=sum,na.rm=T)
  names(builtup.sum) <- paste0("sum5x5_BUILTGHS",year)

  ##
  ## raster stack
  ## 
  st <- stack(rast.pop,pop.mean3x3,pop.sum3x3,builtup,builtup.mean.3x3,builtup.sum.3x3,pop.mean5x5,pop.sum5x5,builtup.mean5x5,builtup.sum5x5)

  ##
  ## transform raster to data.frame with x,y
  ##
  rast.df <- as.data.frame(rasterToPoints(st))
  colnames(rast.df)
  #-------------------------------------------------------------------------------
  #### 3.4. Output the data file. ####
  #-------------------------------------------------------------------------------
  ## export to meana 13 
  save.dta13(rast.df,paste(OUTPUTS,outname,sep="/"))
}
