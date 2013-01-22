# REASSESSMENT OF THE ALFRESCO CARIBOU FIRE RELATIONSHIP
# This script is going to count the numbers of pixels in 2 veg classes (aggregated Spruce Class & Tundra Class) 
# through timesteps c(2012, 2025,2050, 2100) and count the number of pixels that are > 60 years
# and then write this data out into a matrix to be used in the report.

# bring in some packages we need:
library(raster)
library(sp)
library(maptools)

########################################################################################
# this is the info on the representative run for the caribou work
# "echam5 =  highest correlation is with rep49 with a value of: 0.921665792116161"
# "cccma =  highest correlation is with rep72 with a value of: 0.904498980356914"
########################################################################################

setwd("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/working")

# set an output path
out_path <- "/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/Table_02/"

# here I am using some vectors to grab what I want from each of the data groups for the table 2
select_years <- c(c(2000:2009),c(2020:2029),c(2050:2059),c(2090:2099))
echam5.rep <- 49
cccma.rep <- 72

# read in the data
cccma.veg.rep <- list.files("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps", pattern=paste("Veg_.*",cccma.rep,"_.*",sep=""), full.names=T) #regex="Veg_.*72_.*"
cccma.age.rep <- list.files("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps", pattern=paste("Age_.*",cccma.rep,"_.*",sep=""), full.names=T)
echam5.veg.rep <- list.files("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps", pattern=paste("Veg_.*",echam5.rep,"_.*",sep=""), full.names=T)
echam5.age.rep <- list.files("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps", pattern=paste("Age_.*",echam5.rep,"_.*",sep=""), full.names=T)

# a template raster to use in rasterization
template <- raster(cccma.veg.rep[1])

# read in the polygons needed for the analysis
pch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/PCH_winter_90kernal.shp")
cch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/CAH_winter_expert.shp")

# rasterize the input polygons to the raster extent and scale of the output maps
pch.ras <- rasterize(pch, template)
cch.ras <- rasterize(cch, template)

# get the values from the rasterized winter range polygons
pch.ras.v <- getValues(pch.ras)
cch.ras.v <- getValues(cch.ras)

# create a new matrix that will hold the results of the queries
final.matrix <- matrix(NA, nrow=4, ncol=8)
colnames(final.matrix) <- c("cccma spruce pch >60 years","cccma tundra pch >60 years","cccma spruce cah >60 years","cccma tundra cah >60 years","echam5 spruce pch >60 years","echam5 tundra pch >60 years","echam5 spruce cah >60 years","echam5 tundra cah >60 years")
rownames(final.matrix) <- c("2012","2025","2050","2100")

# now we loop through the layers and start asking the right questions to the data
for(j in 1:length(select_years)){
	year_selector <- numeric()	
	curYears <- year_selector[[j]]
	for(year in curYears){
		year_selector <- append(year_selector, grep(paste(".*",as.character(cccma.rep),".*",as.character(year),sep=""),cccma.veg.rep), after=length(year_selector))
		# now stack the selected data
		cccma.veg.rep <- mean(stack(cccma.veg.rep[year_selector]))
		cccma.age.rep <- mean(stack(cccma.age.rep[year_selector]))
		echam5.veg.rep <- mean(stack(echam5.veg.rep[year_selector]))
		echam5.age.rep <- mean(stack(echam5.age.rep[year_selector]))
		
		for(i in 1:nlayers(cccma.veg.rep)){
			# get some values we need to use for indexing
			cccma.veg <- getValues(cccma.veg.rep)
			cccma.age <- getValues(cccma.age.rep)
			echam5.veg <- getValues(echam5.veg.rep)
			echam5.age <- getValues(echam5.age.rep)
			# now fill in the matrix with the data we have harvested from the which() commands below
			# the data are 1km resolution therefore the count is in km2 
			final.matrix[i,1] <- length(which((cccma.veg == 2 | cccma.veg == 3) & cccma.age > 60 & pch.ras.v == 1)) # cccma spruce pch
			final.matrix[i,2] <- length(which(cccma.veg == 1 & cccma.age > 60 & pch.ras.v == 1)) # cccma tundra pch
			final.matrix[i,3] <- length(which((cccma.veg == 2 | cccma.veg == 3) & cccma.age > 60 & cch.ras.v == 1))  #cccma spruce cah
			final.matrix[i,4] <- length(which(cccma.veg == 1 & cccma.age > 60 & cch.ras.v == 1)) # cccma tundra cah
			final.matrix[i,5] <- length(which((echam5.veg == 2 | echam5.veg == 3) & echam5.age > 60 & pch.ras.v == 1))# echam5 spruce pch
			final.matrix[i,6] <- length(which(echam5.veg == 1 & echam5.age > 60 & pch.ras.v == 1)) # echam5 tundra pch
			final.matrix[i,7] <- length(which((echam5.veg == 2 | echam5.veg == 3) & echam5.age > 60 & cch.ras.v == 1)) # echam5 spruce cah
		}	final.matrix[i,8] <- length(which(echam5.veg == 1 & echam5.age > 60 & cch.ras.v == 1)) # echam5 tundra cah
	}
}
# write out the table result
write.csv(final.matrix, file=paste(out_path,"VegCounts_representativeRuns_future_km2_cccma_echam5_caribouHerds_Table02.csv", sep=""))

