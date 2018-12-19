#------------------------------------------------------------------------------#

#			                          Ghana

#------------------------------------------------------------------------------#

## PURPOSE: 	This is code for measuring built up- water bodies, road presence, sea boundary, and land boundary in 3 * 3 and 5* 5 neighboorhood pixels.

# 1. SETTINGS
# 1.1. Load packages - Load main packages, but other may be loaded in
# each code.


# 2. FILE PATHS
# 2.1. One Drive path - Automatically defines OneDrive path which are references for all other paths in the project
# 2.2. Folder paths - Define sub folders paths


# 3. SECTIONS
# 3.1 Load shape files 
# 3.2 Calculate built up-road,land,and water mean value and count of pixels with built up in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5)
# 3.3 Load and Modify the road shape file.
# 3.4 Calculate road grid  mean value and count of pixels with road network in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5)
# 3.5 Calculate water bodies mean value and count of pixels with waterbodies in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5).
# 3.6 Calculate land border mean value and count of pixels with land border in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5).
# 3.7 Calculate sea border mean value and count of pixels with sea border in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5).
# 3.8 Output final file to Stata

## Written by:	   Originally:         Brian Blankenspoor
#                  Modified:           Varnitha Kurli Reddy

## Last updated:                                        December 17th 2018

#------------------------------------------------------------------------------#
#### 1. SETTINGS ####

# Set warnings 
options(warn = 0)

#### Delete everything already in R memory
# (Equivalent to clear all in Stata)
rm(list = ls())

#-------------------------#
#### 1.1 Load packages ####
library(raster)
library(rgdal)
library(readstata13)
library(gdalUtils)
library(stringr)
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

OUTPUTS   			        <- file.path(GHANA,"Statistics Tables" )


#-----------------------------------------------------

#Output Created

OutputStata <- paste0("builtup_sp12lags_database.dta")

#-------------------------------------------------------------------------------
#### 3.1 Load the shape file
#-------------------------------------------------------------------------------
#We are interested in years 1975, 1990, 2000, and 2014.

years <- c(1975,1990,2000,2014)

####Load theshape file####

##Load the country shape file
Ghana.admin.0 <- readOGR(dsn=GIS,layer="gh_adm0_merge2_clean_geo")

# initiate stack
x <- stack()
#-------------------------------------------------------------------------------
#### 3.2 Calculate built up-road,land,water mean value and sum value with built up in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5)
#-------------------------------------------------------------------------------
for (yyyy in years) {
  print(yyyy)

  ## global raster file, proj=mol
  rast <- raster(paste0("C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/gis_data/016_society/GHS_Built/GHS_BUILT_LDS",yyyy,"_GLOBE_R2016A_54009_250_v1_0/GHS_BUILT_LDS",yyyy,"_GLOBE_R2016A_54009_250_v1_0.tif"))
  rast.proj <- proj4string(rast)
  
  ## proj=mol
  ply.prj <- spTransform(Ghana.admin.0,rast.proj)
  
  ## crop by poly
  e <- extent(ply.prj)
  rast.crop <- crop(rast, e, snap="out")
  rast.sub <- mask(rast.crop,ply.prj)
  
  ## check 
  ##spplot(rast.sub)
  
  ## 
  ## Creating 3x3 window (spatial lag = 1)
  ## 
  
  ## weights matrix, exclude itself
  M3 <- matrix(c(1,1,1,
                 1,0,1,
                 1,1,1), nrow=3)
  
  ## Calculate mean 3x3
  rast.mean3x3 <- focal(rast.sub, w=M3, fun=mean,na.rm=T)
  names(rast.mean3x3) <- paste0("Mean3x3built",yyyy)
  
  ##spplot(rast.stat)
  
  ## number of pixels in focal 3x3
  rast.sum3x3 <- focal(rast.sub, w=M3, fun=sum,na.rm=T)
  names(rast.sum3x3) <- paste0("Sum3x3built",yyyy)
  
  ## spplot(rast.sum)
  
  ## 
  ## Creating 5x5 window (spatial lag = 2)
  ## 
  
  ## weights matrix, exclude itself
  M5 <- matrix(c(1,1,1,1,1,
                 1,0,0,0,1,
                 1,0,0,0,1,
                 1,0,0,0,1,
                 1,1,1,1,1), nrow=5)
  
  ## Calculate mean 5x5
  rast.mean5x5 <- focal(rast.sub, w=M5, fun=mean,na.rm=T)
  names(rast.mean5x5) <- paste0("Mean5x5built",yyyy)
  
  
  ##spplot(rast.stat)
  
  ## number of pixels in focal 5x5
  rast.sum5x5 <- focal(rast.sub, w=M5, fun=sum,na.rm=T)
  names(rast.sum5x5) <- paste0("Sum5x5built",yyyy)
  
  ## spplot(rast.count)
  
  ##
  ## raster stack
  ## 
  x <- stack(x, rast.mean3x3, rast.sum3x3, rast.mean5x5, rast.sum5x5)
}


##
## MANUAL EDIT IN ARC
## R Too SLOW
#Ghana.admin.0mask <- rast.sub
#Ghana.admin.0mask[Ghana.admin.0mask>=0]<-1
#writeRaster(Ghana.admin.0mask,filename='C:/Users/wb328156/Downloads/temp/Ghana.admin.0mask.tif')
#output line to grid
Ghana.admin.0brd <- raster('C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/gis_data/003_boundaries/gh_eas_dissolve_correction/gh_adm0_merge2_clean_ln_mol.tif')
extent(Ghana.admin.0brd)<-extent(rast.sub)
res(Ghana.admin.0brd)==res(rast.sub)
#-------------------------------------------------------------------------------
#### 3.3 Load and Modify the road shape file.
#-------------------------------------------------------------------------------
## 1976 GW_Class = Primary, Secondary, Track and NA
road.year <- 1976

road <- readOGR(dsn=ROADS,layer='GHA_Roads_1976',stringsAsFactors=FALSE)

# reassign NA to Unknown - not coded from paper maps
road@data$GW_Class[is.na(road@data$GW_Class)] <- "Unknown"
roadtype <- unique(road@data$GW_Class)

#roadtype <- roadtype[c(4,3)]

## UDF
## Roads by type
road2grid <- function(roadtype.i) {
  print(roadtype.i)
  # roadtype.i <- 1
  # convert line to raster same dim as GHS builtup
  road.i <- subset(road,GW_Class==roadtype.i)
  #roadtype.i==unique(road.i@data$GW_Class)
  
  # convert string to dummy
  road.i@data$d_roadtype <- 1
  summary(road.i)
  
  # reproject to built-up grid
  road.i.prj <- spTransform(road.i,crs(rast.mean3x3))
  
  # raster and add name
  rdgrid.i <- rasterize(road.i.prj,rast.mean3x3,field="d_roadtype")
  
  # add Rd
  roadtype.i <- paste("Rd",roadtype.i,sep="_")
  
  # elim whitespace
  roadtype.i <- str_replace_all(roadtype.i, fixed(" "), "")
  names(rdgrid.i) <- paste(roadtype.i,road.year,sep='_')
  return(rdgrid.i)
}
#-------------------------------------------------------------------------------
#### 3.4 Calculate road grid mean value and count of pixels with road network in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5).
#-------------------------------------------------------------------------------

## UDF Spatial Lag 1 (3x3)
road2splag1 <- function(roadgrid.i) {
  gridname <- names(roadgrid.i)
  paste("grid name: ",gridname,sep='')
  ## weights matrix, exclude itself
  M <- matrix(c(1,1,1,
                1,0,1,
                1,1,1), nrow=3)
  
  ## Mean
  road.mean.splag1 <- raster::focal(roadgrid.i, w=M, fun=mean,na.rm=TRUE)
  names(road.mean.splag1) <- paste("Mean3x3",gridname,sep='_')
  ##spplot(rast.stat)
  
  ## number of pixels
  road.sum.splag1 <- raster::focal(roadgrid.i, w=M, fun=sum,na.rm=TRUE)
  names(road.sum.splag1) <- paste("Sum3x3rd",gridname,sep='_')  
  
  # stack mean + sum raster  
  road.splag1 <-  stack(road.mean.splag1,road.sum.splag1)
  return(road.splag1)
}

##
## spatial lag 2
road2splag2 <- function(roadgrid.i) {
  gridname <- names(roadgrid.i)
  paste("grid name: ",gridname,sep='')
  
  ## weights matrix, exclude itself
  M <- matrix(c(1,1,1,1,1,
                1,0,0,0,1,
                1,0,0,0,1,
                1,0,0,0,1,
                1,1,1,1,1), nrow=5)
  
  ## calculate mean 5x5
  road.mean.splag2 <- raster::focal(roadgrid.i, w=M, fun=mean,na.rm=TRUE)
  names(road.mean.splag2) <- paste("Mean5x5",gridname,sep='_')
  
  ## number of pixels in focal 5x5
  road.sum.splag2 <- focal(roadgrid.i, w=M, fun=sum,na.rm=TRUE)
  names(road.sum.splag2) <- paste("Sum5x5",gridname,sep='_')
  
  road.splag2 <-  stack(road.mean.splag2,road.sum.splag2)
  return(road.splag2)
}


## construct raster stack of same dims by applying userdef to road type list #
road.stack <- stack(lapply(roadtype,road2grid))


##
## all road rasters
## Roads spatial lag 1 and spatial lag 2
x <- road.stack
for(i in 1:nlayers(road.stack)){
  rdsplag1 <- road2splag1(road.stack[[i]])
  x <- stack(x, rdsplag1)
  
 
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
save.dta13(road.df,paste(OUTPUTS,"road_sp12lags_1976.dta",sep="/"))

#-------------------------------------------------------------------------------
#### 3.5 Calculate water bodies mean value and count of pixels with waterbodies in spatial lag 1-(3 x3) and spatial lag 2-(5 X5).
# Extract water
# 0 = no data
# 1 = water surface
# 2 = land no built-up in any epoch
# 3 = built-up from 2000 to 2014 epochs
# 4 = built-up from 1990 to 2000 epochs
# 5 = built-up from 1975 to 1990 epochs
# 6 = built-up up to 1975 epoch
# GHS_BUILT_LDSMT_GLOBE_R2015B_3857_38_v1_0.zip
# only avaible at 38m resolution
# aggregate to 250m 
#-------------------------------------------------------------------------------
# filename subset of Ghana
ghsmt_gha_fn <- 'C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/gis_data/016_society/GHS_Built/ghsmt_mask_gha.tif'
if(!file.exists(ghsmt_gha_fn)){
  ghsmt <- raster('C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/gis_data/016_society/GHSL/GHS_BUILT_LDSMT_GLOBE_R2015B_3857_38_v1_0.vrt')
  
  ## proj=ghsmt
  ply.prj <- spTransform(Ghana.admin.0,crs(ghsmt))
  writeOGR(ply.prj,dsn='C:/Users/wb328156/Downloads/temp',layer='gha_prj.shp',driver='ESRI Shapefile',overwrite=TRUE)
  ply.prj.fn = 'C:/Users/wb328156/Downloads/temp/gha_prj.shp'
  ## crop by poly
  #e <- extent(ply.prj)
  #ghsmt.crop <- crop(ghsmt, e, snap="out")
  #ghsmt.sub <- mask(ghsmt.crop,ply.prj)
  
  # gdalwarp -cutline clipper.shp -crop_to_cutline input.tif output.tif
  gdalUtils::gdalwarp(ghsmt,cutline=ply.prj.fn,crop_to_cutline=TRUE,dstfile=ghsmt_gha_fn,output_Raster=TRUE,overwrite=TRUE,verbose=TRUE)
}


ghswater <- raster(ghsmt_gha_fn)
ghswater[ghswater!=1] <- 0

# aggregate 38m water
ghswater.mol <- projectRaster(ghswater,crs=crs(rast.mean3x3),res=res(ghswater),method="bilinear")
# proj merc -> moll #
ghswat <- resample(ghswater.mol, rast.mean3x3, method="bilinear")

# 30% presence of water
ghswat[ghswat>=.3] <- 1
ghswat[ghswat<.3] <- 0
names(ghswat) <- "Water"
#spplot(ghswat)

## Water Bodies spatial lag 1 and spatial lag 2
x <- stack(x,ghswat)
watsplag1 <- road2splag1(ghswat)
names(watsplag1) <- c("Mean3x3_water","Sum3x3_water")
x <- stack(x, watsplag1)


watsplag2 <- road2splag2(ghswat)
names(watsplag2) <- c("Mean5x5_water","Sum5x5_water")
x <- stack(x,watsplag2)
names(x)
#-------------------------------------------------------------------------------
#### 3.6 Calculate land border mean value and count of pixels with land border in spatial lag 1-(3 x 3) and spatial lag 2-(5 X 5).
## Border
## Manual Edits in Arc see Above
#-------------------------------------------------------------------------------
## Land Border
Ghana.admin.0_land <- Ghana.admin.0brd
names(Ghana.admin.0_land) <- "Land_border" 
Ghana.admin.0_land[Ghana.admin.0_land==0]<-NA


## Land boundary spatial lag 1 and spatial lag 2
x <- stack(x,Ghana.admin.0_land)
landsplag1 <- road2splag1(Ghana.admin.0_land)
names(landsplag1) <- c("Mean3x3splag1_land","Sum3x3splag1_land")
x <- stack(x, landsplag1)
landsplag2 <- road2splag2(Ghana.admin.0_land)
names(landsplag2) <- c("Mean5x5splag2_land","Sum5x5splag2_land")
x <- stack(x,landsplag2)
names(x)

#-------------------------------------------------------------------------------
#### 3.7 Calculate sea border mean value and count of pixels with sea border in spatial lag 1-(3 x3) and spatial lag 2-(5 X5).
#-------------------------------------------------------------------------------
### Sea Border
Ghana.admin.0_sea <- Ghana.admin.0brd
names(Ghana.admin.0_sea) <- "Sea_border"
Ghana.admin.0_sea[Ghana.admin.0_sea==1] <- NA
Ghana.admin.0_sea[Ghana.admin.0_sea==0] <- 1

## Sea boundary spatial lag 1 and spatial lag 2
x <- stack(x,Ghana.admin.0_sea)
seasplag1 <- road2splag1(Ghana.admin.0_sea)
names(seasplag1) <- c("Mean3x3splag1_sea","Sum3x3splag1_sea")
x <- stack(x, seasplag1)
seasplag2 <- road2splag2(Ghana.admin.0_sea)
names(seasplag2) <- c("Mean5x5splag2_sea","Sum5x5splag2_sea")
x <- stack(x,seasplag2)
names(x)

#-------------------------------------------------------------------------------
#### 3.8 Output final file to Stata
#-------------------------------------------------------------------------------
## Transform raster to data.frame with x,y

rast.df <- as.data.frame(rasterToPoints(x))
colnames(rast.df)   #[38]<-"Sea_border"

## Export to Stata 13 
save.dta13(rast.df,paste(OUTPUTS,OutputStata,sep="/"))

