## B Blankespoor 2018-12-08

library(raster)
library(rgdal)
library(readstata13)
library(gdalUtils)
library(stringr)


outdir <- "E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_/Statistics Tables"

outname <- paste0("builtup_sp12lags_database.dta")


years <- c(1975,1990,2000, 2014)

##
ctr <- readOGR(dsn="E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_/gis_data/003_boundaries/gh_eas_dissolve_correction",layer="gh_adm0_merge2_clean_geo")

# initiate stack
x <- stack()


for (yyyy in years) {
  print(yyyy)

  ## share of builtup in pixel 
  ## global raster file, proj=mol
  ## downloaded from https://ghsl.jrc.ec.europa.eu/ghs_bu.php
  rast <- raster(paste0("V:/00_GLB/016_society/GHS_BUILT/GHS_BUILT_LDS",yyyy,"_GLOBE_R2016A_54009_250_v1_0/GHS_BUILT_LDS",yyyy,"_GLOBE_R2016A_54009_250_v1_0.tif"))
  rast.proj <- proj4string(rast)

  ## proj=mol
  ply.prj <- spTransform(ctr,rast.proj)

  ## crop by poly
  e <- extent(ply.prj)
  rast.crop <- crop(rast, e, snap="out")

  ## Masked raster
  rast.sub <- mask(rast.crop,ply.prj)
  rast.mask.fn = paste(outdir,"xyrastmask.RData",sep="/")
  if(!file.exists(rast.mask.fn)) {
    save(rast.sub,file = rast.mask.fn)
  }

  ## check 
  ## spplot(rast.sub)

  ## weights matrix, exclude itself
  M <- matrix(c(1,1,1,
			1,0,1,
			1,1,1), nrow=3)

  ## calculate mean 3x3
  rast.stat <- focal(rast.sub, w=M, fun=mean,na.rm=T)
  names(rast.stat) <- paste0("Mean3x3built",yyyy)

  ##spplot(rast.stat)

  ## Use: Masked raster (rast.sub)
  ## number of pixels in focal 3x3
  rast.count.dummy <- rast.sub
  #rast.count.dummy[rast.count.dummy>0] <- 1
  ##rast.count.dummy[rast.count.dummy==0] <- NA
  rast.count <- focal(rast.count.dummy, w=M, fun=sum,na.rm=T)
  names(rast.count) <- paste0("Sum3x3built",yyyy)

  ## spplot(rast.count)

  ## 
  ## 5x5 window (spatial lag = 2
  ##   
  
  ## weights matrix, exclude itself
  M5 <- matrix(c(1,1,1,1,1,
				1,0,0,0,1,
				1,0,0,0,1,
				1,0,0,0,1,
				1,1,1,1,1), nrow=5)

  ## calculate mean 5x5
  rast.stat5x5 <- focal(rast.sub, w=M5, fun=mean,na.rm=T)
  names(rast.stat5x5) <- paste0("Mean5x5built",yyyy)


  ##spplot(rast.stat)

  ## number of pixels in focal 5x5
  rast.count.dummy5x5 <- rast.sub
  #rast.count.dummy[rast.count.dummy>0] <- 1
  ##rast.count.dummy[rast.count.dummy==0] <- NA
  rast.count5x5 <- focal(rast.count.dummy5x5, w=M5, fun=sum,na.rm=T)
  names(rast.count5x5) <- paste0("Sum5x5built",yyyy)

  ## spplot(rast.count)

  ##
  ## raster stack
  ## 
  x <- stack(x, rast.stat, rast.count, rast.stat5x5, rast.count5x5)
}


##
## MANUAL EDIT IN ARC
## R Too SLOW
#ctrmask <- rast.sub
#ctrmask[ctrmask>=0]<-1
#writeRaster(ctrmask,filename='C:/Users/wb328156/Downloads/temp/ctrmask.tif')
#'E:\OneDrive\OneDrive - WBG\West Africa Project - Forhad\Ghana_\gis_data\016_society/ctrmask.tif')
#output line to grid
ctrbrd <- raster('E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_/gis_data/003_boundaries/gh_eas_dissolve_correction/gh_adm0_merge2_clean_ln_mol.tif')
extent(ctrbrd)<-extent(rast.sub)
res(ctrbrd)==res(rast.sub)

##
## Roads
## 

rast.mask.fn = paste(outdir,"xyrastmask.RData",sep="/")
if(file.exists(rast.mask.fn)) {
    load(file = rast.mask.fn)
}


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
  road.i.prj <- spTransform(road.i,crs(rast.sub))

  # raster and add name
  rdgrid.i <- rasterize(road.i.prj,rast.sub,field="d_roadtype")
  
  # add Rd
  roadtype.i <- paste("Rd",roadtype.i,sep="_")
  
  # elim whitespace
  roadtype.i <- str_replace_all(roadtype.i, fixed(" "), "")
  names(rdgrid.i) <- paste(roadtype.i,road.year,sep='_')
  return(rdgrid.i)
}


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

  ## Sum
  road.sum.splag1 <- raster::focal(roadgrid.i, w=M, fun=sum,na.rm=TRUE)
  names(road.sum.splag1) <- paste("Sum3x3",gridname,sep='_')  
  
  ###Number of pixels
  
  ## transform value to dummy: rast.pop.count
  rast.dummy <- roadgrid.i
  rast.dummy[!is.na(rast.dummy)] <-1
  ## count pixels in focal 3x3
  road.cnt.splag1 <- focal(rast.dummy, w=M, fun=sum,na.rm=TRUE)
  names(road.cnt.splag1) <- paste("Cnt3x3",gridname,sep='_')

  # stack mean + sum raster  +count pixel
  road.splag1 <-  stack(road.mean.splag1,road.sum.splag1,road.cnt.splag1)
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

  ## Sum in 5x5
  road.sum.splag2 <- focal(roadgrid.i, w=M, fun=sum,na.rm=TRUE)
  names(road.sum.splag2) <- paste("Sum5x5",gridname,sep='_')
  
  ## transform value to dummy: rast.pop.count
  rast.dummy <- roadgrid.i
  rast.dummy[!is.na(rast.dummy)] <-1
  ## count pixels in focal 5x5
  road.cnt.splag2 <- focal(rast.dummy, w=M, fun=sum,na.rm=TRUE)
  names(road.cnt.splag2) <- paste("Cnt5x5",gridname,sep='_')
  
  road.splag2 <-  stack(road.mean.splag2,road.sum.splag2,road.cnt.splag2)
  return(road.splag2)
}



## 1976 GW_Class = Primary, Secondary, Track and NA
#road.year <- 1976
road.years <- c(1976,1986)

for (road.year in road.years) {
  print(road.year)

  ## road 
  road <- readOGR(dsn='E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_/gis_data/018_transportation/roads_gha.gdb',layer=paste0('GHA_Roads_',road.year),stringsAsFactors=FALSE)

  # reassign NA to Unknown - not coded from paper maps
  road@data$GW_Class[is.na(road@data$GW_Class)] <- "Unknown"
  roadtype <- unique(road@data$GW_Class)

  #roadtype <- roadtype[c(4,3)]

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
  save.dta13(road.df,paste(outdir,paste0("road_sp12lags_",road.year,".dta"),sep="/"))

}


##
## Water
##

# GHS_BUILT_LDSMT_GLOBE_R2015B_3857_38_v1_0.zip
# only avaible at 38m resolution
# aggregate to 250m 

# filename subset of Ghana
ghsmt_gha_fn <- 'E:/OneDrive/OneDrive - WBG/West Africa Project - Forhad/Ghana_/gis_data/016_society/GHS_Built/ghsmt_mask_gha.tif'
if(!file.exists(ghsmt_gha_fn)){
  ghsmt <- raster('C:/Users/wb328156/Downloads/temp/GHS_BUILT_LDSMT_GLOBE_R2015B_3857_38_v1_0/GHS_BUILT_LDSMT_GLOBE_R2015B_3857_38_v1_0/GHS_BUILT_LDSMT_GLOBE_R2015B_3857_38_v1_0.vrt')

  ## proj=ghsmt
  ply.prj <- spTransform(ctr,crs(ghsmt))
  writeOGR(ply.prj,dsn='C:/Users/wb328156/Downloads/temp',layer='gha_prj.shp',driver='ESRI Shapefile',overwrite=TRUE)
  ply.prj.fn = 'C:/Users/wb328156/Downloads/temp/gha_prj.shp'
  ## crop by poly
  #e <- extent(ply.prj)
  #ghsmt.crop <- crop(ghsmt, e, snap="out")
  #ghsmt.sub <- mask(ghsmt.crop,ply.prj)
  
  # gdalwarp -cutline clipper.shp -crop_to_cutline input.tif output.tif
  gdalUtils::gdalwarp(ghsmt,cutline=ply.prj.fn,crop_to_cutline=TRUE,dstfile=ghsmt_gha_fn,output_Raster=TRUE,overwrite=TRUE,verbose=TRUE)
}

# Extract water
# 0 = no data
# 1 = water surface
# 2 = land no built-up in any epoch
# 3 = built-up from 2000 to 2014 epochs
# 4 = built-up from 1990 to 2000 epochs
# 5 = built-up from 1975 to 1990 epochs
# 6 = built-up up to 1975 epoch
ghswater <- raster(ghsmt_gha_fn)
# subset 1 = water surface 
ghswater[ghswater!=1] <- 0

# aggregate 38m water#
ghswater.mol <- projectRaster(ghswater,crs=crs(rast.sub),res=res(ghswater),method="bilinear")

# proj merc -> moll #
ghswat <- resample(ghswater.mol, rast.sub, method="bilinear")

# 30% presence of water
ghswat[ghswat>=.3] <- 1
ghswat[ghswat<.3] <- 0
names(ghswat) <- "Water"
#spplot(ghswat)
x <- stack()
x <- stack(x,ghswat)
watsplag1 <- road2splag1(ghswat)
names(watsplag1) <- c("Mean3x3splag1_water","Sum3x3splag1_water")
x <- stack(x, watsplag1)

# add spatial lag 2
watsplag2 <- road2splag2(ghswat)
#names(watsplag2) <- c("Mean5x5splag2_water","Sum5x5splag2_water")
x <- stack(x,watsplag2)
names(x)


##
## Border
## MANUAL EDITS IN ARC SEE ABOVE

## land border
ctr_land <- ctrbrd
names(ctr_land) <- "Land_border" 
ctr_land[ctr_land==0]<-NA

##
## LAND SP LAG
x <- stack(x,ctr_land)
landsplag1 <- road2splag1(ctr_land)
names(landsplag1) <- c("Mean3x3splag1_land","Sum3x3splag1_land")
x <- stack(x, landsplag1)

# add spatial lag 2
landsplag2 <- road2splag2(ctr_land)
names(landsplag2) <- c("Mean5x5splag2_land","Sum5x5splag2_land")
x <- stack(x,landsplag2)
names(x)

##
## Inland water
##


##
## SEA SP LAG
##

##
## sea border
ctr_sea <- ctrbrd
names(ctr_sea) <- "Sea_border"
ctr_sea[ctr_sea==1] <- NA
ctr_sea[ctr_sea==0] <- 1

x <- stack(x,ctr_sea)
seasplag1 <- road2splag1(ctr_sea)
names(seasplag1) <- c("Mean3x3splag1_sea","Sum3x3splag1_sea")
x <- stack(x, seasplag1)

# add spatial lag 2
seasplag2 <- road2splag2(ctr_sea)
names(seasplag2) <- c("Mean5x5splag2_sea","Sum5x5splag2_sea")
x <- stack(x,seasplag2)
names(x)

rast.df <- as.data.frame(rasterToPoints(x))
## export to Stata 13 
save.dta13(rast.df,paste(outdir,"landsea_sp12lags.dta",sep="/"))




##
## OUTPUT FINAL FILE TO STATA
##

##
## transform raster to data.frame with x,y
##
rast.df <- as.data.frame(rasterToPoints(x))
colnames(rast.df)   #[38]<-"Sea_border"

## export to Stata 13 
save.dta13(rast.df,paste(outdir,outname,sep="/"))



