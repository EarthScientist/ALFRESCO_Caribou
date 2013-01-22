# REASSESSMENT OF THE ALFRESCO CARIBOU FIRE RELATIONSHIP
# This script is going to count the avg fire size, number of fires, and total area burned 
# within the combined winter ranges of the PCH and CAH.   

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
out_path <- "/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/DerivedTables/Table03/"

# here are the names used to identify the maps in the aflf otput s
alf_names <- c("CGCM31", "ECHAM")

# representative reps
median_reps <- c(72,49) #format c(cccma, echam5)
# echam5.rep <- 49
# cccma.rep <- 72

# these are the years we will be calculating over
select_years <- list(list(2000:2009),list(2010:2019),list(2020:2029),list(2030:2039),list(2040:2049),list(2050:2059),list(2060:2069),list(2070:2079),list(2080:2089),list(2090:2099)) # years <- 2009:2100

# read in the polygons needed for the analysis
pch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/PCH_winter_90kernal.shp")
cch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/CAH_winter_expert.shp")

template <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps/Age_0_2009.tif")

# rasterize the input polygons to the raster extent and scale of the output maps
pch.ras <- rasterize(pch, template)
cch.ras <- rasterize(cch, template)

full.range <- cover(pch.ras,cch.ras)
values(full.range)[which(values(full.range)!=1)] <- 0
# full.range <- !is.na(full.range)

output <- matrix(NA, nrow=length(select_years),ncol=6)
rownames(output) <- c("")

for(i in 1:length(alf_names)){
	alf_name <- alf_names[i]
	median_rep <- median_reps[i]
	if(alf_name == "CGCM31"){
		colnames(output) <- c("CCCMA Spruce Number of Fires", "CCCMA Tundra Number of Fires","CCCMA Spruce Avg Patch Size km2","CCCMA Tundra Avg Patch Size km2", "CCCMA Spruce Total Area Burned km2", "CCCMA Tundra Total Area Burned km2")
	}else{
		colnames(output) <- c("ECHAM5 Spruce Number of Fires", "ECHAM5 Tundra Number of Fires","ECHAM5 Spruce Avg Patch Size km2","ECHAM5 Tundra Avg Patch Size km2", "ECHAM5 Spruce Total Area Burned km2", "ECHAM5 Tundra Total Area Burned km2")
	}
	
	print(alf_name)
	# base path
	in_path_past <- paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CalibrationResults_Maps1950to2009/Maps/",sep="")
	in_path_future <- paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/",alf_name,"_Maps2009to2100/Maps/",sep="")
	
	for(k in 1:length(select_years)){
		years <- unlist(select_years[[k]])

		for(year in years){
			if(year < 2009){
				decadeList <- append(decadeList,paste(in_path_past,"Age_",median_rep,"_",year,".tif",sep=""),after=length(decadeList))
			}else{
				decadeList <- append(decadeList,paste(in_path_future,"Age_",median_rep,"_",year,".tif",sep=""),after=length(decadeList))
			}
		}

		# now stack the needed decade data 
		age <- mean(stack(decadeList))
		veg <- mean(stack(decadeList))

		# subset to the 2 regions we are interested in, within the winter ranges of the herds
		spruce.age <- age[which(values(full.range) == 1 & (values(veg) == 2 | values(veg) == 3)), drop=F]
		tundra.age <- age[which(values(veg) == 1 & values(full.range) == 1), drop=F]
		
		# here we remove all of the values that are not zero
		values(spruce.age)[which(values(spruce.age) != 0)] <- NA
		values(spruce.age)[which(values(spruce.age) == 0)] <- 1

		values(tundra.age)[which(values(tundra.age) != 0)] <- NA
		values(tundra.age)[which(values(tundra.age) == 0)] <- 1

		# lets clump them to count the nunmber of patches
		spruce.patch <- clump(spruce.age, directions=8)
		tundra.patch <- clump(tundra.age, directions=8)

		# spruce error trapping
		if(all(is.na(values(spruce.patch))) == TRUE){
			output[j,1] <- 0
			output[j,3]	<- 0
			output[j,5] <- 0
		}else{
			output[j,1] <- length(unique(spruce.patch))
			output[j,3] <- mean(table(values(spruce.patch)))
			output[j,5] <- sum(table(values(spruce.patch)))
		}

		# tundra error trapping
		if(all(is.na(values(tundra.patch))) == TRUE){
			output[j,2] <- 0
			output[j,4] <- 0
			output[j,6] <- 0
		}else{
			output[j,2] <- length(unique(tundra.patch))
			output[j,4] <- mean(table(values(tundra.patch)))
			output[j,6] <- sum(table(values(tundra.patch)))
		}
	}

	write.csv(output, file=paste(out_path,"Table03_",alf_name,".csv",sep=""))
}







