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
years <- 1951:2100

# read in the polygons needed for the analysis
pch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/PCH_winter_90kernal.shp")
cch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/CAH_winter_expert.shp")

# just a template raster pay no mind to this
template <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps/Age_0_2009.tif")

# rasterize the input polygons to the raster extent and scale of the output maps
pch.ras <- rasterize(pch, template)
cch.ras <- rasterize(cch, template)

full.range <- cover(pch.ras,cch.ras)
values(full.range)[which(values(is.na(full.range)) == TRUE)] <- 0
# full.range <- !is.na(full.range)

output <- matrix(NA, nrow=length(years),ncol=6)

for(i in 1:length(alf_names)){
	alf_name <- alf_names[i]
	median_rep <- median_reps[i]
	if(alf_name == "CGCM31"){
		colnames(output) <- c("CCCMA Spruce Number of Fires", "CCCMA Tundra Number of Fires","CCCMA Spruce Avg Patch Size km2","CCCMA Tundra Avg Patch Size km2", "CCCMA Spruce Total Area Burned km2", "CCCMA Tundra Total Area Burned km2")
	}else{
		colnames(output) <- c("ECHAM5 Spruce Number of Fires", "ECHAM5 Tundra Number of Fires","ECHAM5 Spruce Avg Patch Size km2","ECHAM5 Tundra Avg Patch Size km2", "ECHAM5 Spruce Total Area Burned km2", "ECHAM5 Tundra Total Area Burned km2")
	}
	rownames(output) <- paste(years-1,sep="")
	print(alf_name)
	
	# base path
	in_path_future <- paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/",alf_name,"_Maps2009to2100/Maps/",sep="")
	in_path_past <- "/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CalibrationResults_Maps1950to2009/Maps/"

	for(j in 1:length(years)){
		year <- years[j]
		print(year)
		
		if(year < 2009){
			age <- raster(paste(in_path_past,"Age_",median_rep,"_",year,".tif",sep=""))
			veg <- raster(paste(in_path_past,"Veg_",median_rep,"_",year-1,".tif",sep=""))
		}else{
			if(year == 2009){
				age <- raster(paste(in_path_future,"Age_",median_rep,"_",year,".tif",sep=""))
				veg <- raster(paste(in_path_past,"Veg_",median_rep,"_",year-1,".tif",sep=""))	
			}else{
				age <- raster(paste(in_path_future,"Age_",median_rep,"_",year,".tif",sep=""))
				veg <- raster(paste(in_path_future,"Veg_",median_rep,"_",year-1,".tif",sep=""))
			}
		}

		# subset to the 2 regions we are interested in, within the winter ranges of the herds
		spruce.age <- age[which((values(full.range) == 1) & (values(veg) == 2 | values(veg) == 3) & (values(age) == 0)), drop=F]
		tundra.age <- age[which(values(veg) == 1 & values(full.range) == 1), drop=F]
		
		# here we remove all of the values that are not zero
		# values(spruce.age)[which(values(spruce.age) != 0)] <- NA
		values(spruce.age)[which(values(spruce.age) == 0)] <- 1

		# values(tundra.age)[which(values(tundra.age) != 0)] <- NA
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


# now lets do some aggregation of that data into decadal averages
output2 <- cbind(output,paste(substring(output[,1],1,3),"0s",sep=""))
colnames(output2) <- c(colnames(output),"decadal_agg")
aggData <- aggregate(output2[,2:7], by=list(output2[,ncol(output2)]))

write.csv(aggData,file="new filename here.csv ")








