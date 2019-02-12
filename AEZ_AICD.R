#------------------------------------------------------------------------------#

#			                          Ghana

#------------------------------------------------------------------------------#

## PURPOSE: 	This is code for determining the AEZ soil category in each pixel and adding Primary roads from AICD 2008 data

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
## Written by:	   Originally:         Varnitha Kurli reddy
#                  Modified:           

## Last updated:                                        January 27th 2019

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
library(geosphere)
#------------------------------------------------------------------------------#
#### 2. FILE PATHS 	####

#### 2.1. OneDrive paths	####

# Varnitha



# Varnitha Laptop
if (Sys.getenv("USERNAME") == "WB538005" ){
  OneDrive<-file.path("C:/Users/WB538005/WBG")
  
}

# Brian Blankespoor

if (Sys.getenv("USERNAME") =="WB328156" ){
  
  OneDrive<-file.path("E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_")
}

#-------------------------------------------------------------------------------
#### 2.2. Folder paths ####
#-------------------------------------------------------------------------------
#PRIMARY FOLDERS
if (Sys.getenv("USERNAME") == "WB538005" ){
  GHANA<-file.path(OneDrive, "Brian Blankespoor - Ghana_")
}

if (Sys.getenv("USERNAME") =="WB328156" ){
  GHANA<-file.path(OneDrive)
}

GIS<- file.path(GHANA, "gis_data/003_boundaries/gh_eas_dissolve_correction")

ROADS<- file.path(GHANA, "gis_data/018_transportation/roads_gha.gdb")

ROADS_2008<- file.path(GHANA, "gis_data/018_transportation/roads_AICD_2008")

SOIL<-file.path(GHANA, "gis_data/001_farming/fao_gaez_aez/aez_gha_fao")

#------------------------------------------------------------------------------
# output files#
OUTPUTS   			        <- file.path(GHANA,"Statistics Tables" )


#-------------------------------------------------------------------------------
#### 3.1 Load the shape file
#-------------------------------------------------------------------------------
#ghana shapefile
Ghana.admin.0 <- readOGR(dsn=GIS, layer="gh_adm0_merge2_clean_aux_wgs84")

#-------------------------------------------------------------------------------
##### 3.2 Load the AEZ files
#-------------------------------------------------------------------------------
setwd(paste0(file.path(GHANA, "gis_data/001_farming/fao_gaez_aez/aez_gha_fao")))
dblbnd<-raster("dblbnd.adf")
hdr<-raster("hdr.adf")
prj<-raster("prj.adf")
sta<-raster("sta.adf")
vat<-raster("vat.adf")
w001001<-raster("w001001.adf")
w001001x<-raster("w001001x.adf")
AEZ<-stack(dblbnd,hdr,prj,sta,vat,w001001,w001001x)

# ghana cleaned with AEZ #
ply.prj <- spTransform(Ghana.admin.0,proj4string(AEZ))
e <- extent(ply.prj)
myraster.crop <- crop(AEZ, e, snap="out")
rast.pop <- mask(myraster.crop,ply.prj)
#Checking by plotting
#plot(rast.pop)
#-------------------------------------------------------------------------------
##### 3.3 Convert the raster to dataframe and save
#-------------------------------------------------------------------------------
#Convert raster to dataframe
aez.df <- as.data.frame(rasterToPoints(AEZ))
outname <- paste0("aez_data",".dta")
save.dta13(data=aez.df,file=paste(OUTPUTS,outname,sep="/"))

#---------------------------------------------------------------------------------
######### 3.4 Convert the Mooliwide projection system of cleaned all data file to WGS84 projection.
#Merge AEZ and the original datasets by closest lat and long because the resolutions of the two daatsets are different.
#(Package:Geosphere)
#---------------------------------------------------------------------------------
aez.df<-read_dta("C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/Statistics Tables/aez_data.dta")
all<-read_dta("C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/Statistics Tables/cleaned_all.dta")
coordinates(all) <- c("longitude", "latitude")
proj4string(all) <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
wgs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
wgscoords <- spTransform(all, wgs)
all_new<-coordinates(wgscoords)
all<-as.data.frame(all)
all_new<-as.data.frame(all_new)
all$longitude<-NULL
all$latitude<-NULL
all<-cbind(all_new,all)
#---------------------------------------------------------------------------------
######### 3.5 Merge AEZ and the original datasets by closest lat and long because the resolutions of the two daatsets are different.
#(Package:Geosphere)
#---------------------------------------------------------------------------------
gc()
memory.limit()
merge<-distm(aez.df[,c('longitude','latitude')], all[,c('longitude','latitude')], fun=distVincentyEllipsoid)



#------------------------------------------------------------------------------------------
############ 3.6 Add AICD 2008 Primary Roads
#-----------------------------------------------------------------------------------------
##Load the country shape file
Ghana.admin.0brd <- raster('C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/gis_data/003_boundaries/gh_eas_dissolve_correction/gh_adm0_merge2_clean_ln_mol.tif')
extent(Ghana.admin.0brd)<-extent(rast.sub)
res(Ghana.admin.0brd)==res(rast.sub)

rast.mask.fn = paste(OUTPUTS,"xyrastmask.RData",sep="/")
if(file.exists(rast.mask.fn)) {
  load(file = rast.mask.fn)
}

road2grid <- function(roadtype.i) {
  print(roadtype.i)
  # roadtype.i <- 1
  # convert line to raster same dim as GHS builtup
  road.i <- subset(road,ROAD_TYPE==roadtype.i)
  #roadtype.i==unique(road.i@data$GW_Class)
  
  # convert string to dummy
  road.i@data$ROAD_TYPE<- 1
  summary(road.i)
  
  # reproject to built-up grid
  road.i.prj <- spTransform(road.i,crs(rast.sub))
  
  # raster and add name
  rdgrid.i <- rasterize(road.i.prj,rast.sub,field="ROAD_TYPE")
  
  # add Rd
  roadtype.i <- paste("Rd",roadtype.i)
  
  # elim whitespace
  roadtype.i <- str_replace_all(roadtype.i, fixed(" "), "")
  names(rdgrid.i) <- paste(roadtype.i)
  return(rdgrid.i)
}


## UDF Spatial Lag 1 (3x3)
road2splag1 <- function(roadgrid.i) {
  gridname <- names(roadgrid.i)
  paste("grid name: ",gridname)
  ## weights matrix, exclude itself
  M <- matrix(c(1,1,1,
                1,0,1,
                1,1,1), nrow=3)
  
  ## Mean
  road.mean.splag1 <- raster::focal(roadgrid.i, w=M, fun=mean,na.rm=TRUE)
  names(road.mean.splag1) <- paste("Mean3x3",gridname)
  ##spplot(rast.stat)
  
  ## Sum
  road.sum.splag1 <- raster::focal(roadgrid.i, w=M, fun=sum,na.rm=TRUE)
  names(road.sum.splag1) <- paste("Sum3x3",gridname)  
  
  ###Number of pixels
  
  ## transform value to dummy: rast.pop.count
  rast.dummy <- roadgrid.i
  rast.dummy[!is.na(rast.dummy)] <-1
  ## count pixels in focal 3x3
  road.cnt.splag1 <- focal(rast.dummy, w=M, fun=sum,na.rm=TRUE)
  names(road.cnt.splag1) <- paste("Cnt3x3",gridname)
  
  # stack mean + sum raster  +count pixel
  road.splag1 <-  stack(road.mean.splag1,road.sum.splag1,road.cnt.splag1)
  return(road.splag1)
}

##
## spatial lag 2
road2splag2 <- function(roadgrid.i) {
  gridname <- names(roadgrid.i)
  paste("grid name: ",gridname)
  
  ## weights matrix, exclude itself
  M <- matrix(c(1,1,1,1,1,
                1,0,0,0,1,
                1,0,0,0,1,
                1,0,0,0,1,
                1,1,1,1,1), nrow=5)
  
  ## calculate mean 5x5
  road.mean.splag2 <- raster::focal(roadgrid.i, w=M, fun=mean,na.rm=TRUE)
  names(road.mean.splag2) <- paste("Mean5x5",gridname)
  
  ## Sum in 5x5
  road.sum.splag2 <- focal(roadgrid.i, w=M, fun=sum,na.rm=TRUE)
  names(road.sum.splag2) <- paste("Sum5x5",gridname)
  
  ## transform value to dummy: rast.pop.count
  rast.dummy <- roadgrid.i
  rast.dummy[!is.na(rast.dummy)] <-1
  ## count pixels in focal 5x5
  road.cnt.splag2 <- focal(rast.dummy, w=M, fun=sum,na.rm=TRUE)
  names(road.cnt.splag2) <- paste("Cnt5x5",gridname)
  
  road.splag2 <-  stack(road.mean.splag2,road.sum.splag2,road.cnt.splag2)
  return(road.splag2)
}



## AICD road_type= Primary
#road.year <- 2008

  road <- readOGR(dsn=ROADS_2008,layer='Ghana_Roads',stringsAsFactors=FALSE)
  
  roadtype <- unique(road@data$ROAD_TYPE)
  
  ## construct raster stack of same dims by applying userdef to road type list #
  road.stack <- stack(lapply(roadtype,road2grid))
  
  ##
  ## all road rasters
  x <- road.stack
  for(i in 1:nlayers(road.stack)){
    rdsplag1 <- road2splag1(road.stack[[i]])
    x <- stack(x, rdsplag1)
    
    # add spatial lag 2
    rdsplag2 <- road2splag2(road.stack[[i]])
    x <- stack(x, rdsplag2)
  }
  names(x)
  
  ##
  ## transform raster to data.frame with x,y
  ##
  road.df <- as.data.frame(rasterToPoints(x))
  colnames(road.df)
  
  ## export to Stata 13 
  save.dta13(road.df,paste(OUTPUTS,paste0("AICD_roads.dta"),sep="/"))
  
