# REASSESSMENT OF THE ALFRESCO CARIBOU FIRE RELATIONSHIP
# This script is going to count the numbers of pixels in each veg clas through time and also to count the number of pixels that are > 60 years
# and then write out these matrices so they can be used later.

# bring in some packages we need:
library(raster)
library(sp)
library(maptools)

setwd("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/working")

# set an output path
out_path <- "/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/DerivedTables/Table01/"

# read in the 2009 data
alf.veg <- raster("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/alf_veg_2005/NA_LandCover_2005_PRISM_extent_AKAlbers_1km_ALFRESCO_FINAL.tif")
# alf.veg <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CGCM31_Maps2009to2100/Maps/Veg_0_2009.tif")
# echam5.2009 <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps/Veg_0_2009.tif")

# read in the polygons needed for the analysis
pch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/PCH_winter_90kernal.shp")
cch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/CAH_winter_expert.shp")

# rasterize the input polygons to the raster extent and scale of the output maps
pch.ras <- rasterize(pch, alf.veg)
cch.ras <- rasterize(cch, alf.veg)

# this is the total cell count from each of the caribou herd winter ranges 
pch.totalCells <- length(which(values(pch.ras) == 1))
cch.totalCells <- length(which(values(cch.ras) == 1))

# get the values from the rasters
pch.ras.v <- getValues(pch.ras)
cch.ras.v <- getValues(cch.ras)
alf.veg.v <- getValues(alf.veg)

m <- matrix(NA, nrow=5, ncol=2)
for(i in unique(alf.veg)){
	print(i)
	n <- i+1
	m[n,1] <- length(which(alf.veg.v == i & cch.ras.v == 1))
	m[n,2] <- length(which(alf.veg.v == i & pch.ras.v == 1))
}

colnames(m) <- c("Central Arctic Herd (winter range)","Porcupine Herd (winter range)")
rownames(m) <- c("Rock/Ice","Tundra","Black Spruce","White Spruce","Deciduous")

write.csv(m, file=paste(out_path,"VegCounts_inputVegMap_km2_cccma_echam5_caribouHerds.csv", sep=""))

