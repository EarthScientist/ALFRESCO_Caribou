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
alf_names <- c("HISTORICAL","CGCM31", "ECHAM")

# representative reps
median_reps <- c(72,49) #format c(cccma, echam5)
# echam5.rep <- 49
# cccma.rep <- 72

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

for(i in 1:length(alf_names)){
	alf_name <- alf_names[i]
	median_rep <- median_reps[i]

	# here is the years selector based on the inputs data
	if(alf_name == "CGCM31" | alf_name == "ECHAM"){
		years <- 2009:2100
	}else{
		years <- 1950:2009
	}

	# create the output matrix for each of the 3 table groups 
	output <- matrix(NA, nrow=length(years),ncol=3)
	colnames(output) <- c(paste(alf_name," Number of Fires",sep=""),paste(alf_name," Avg Fire Size",sep=""),paste(alf_name," Total Area Burned km2",sep=""))

	rownames(output) <- paste(years,sep="")
	print(alf_name)
	
	for(j in 1:length(years)){
		year <- years[j]
		print(year)
		
		if(alf_name == "CGCM31" | alf_name == "ECHAM"){
			in_path <- paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/",alf_name,"_Maps2009to2100/Maps/",sep="")
			age <- raster(paste(in_path,"Age_",median_rep,"_",year,".tif",sep=""))
		}else{
			age <- raster(paste("/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Fire/","ALF_AK_CAN_FireHistory_",year,".tif",sep=""))
		}		
			
		# subset to the 2 regions we are interested in, within the winter ranges of the herds
		output.age <- age[which((values(full.range) == 1) & (values(age) == 0)), drop=F]
	
		# here we remove all of the values that are not zero
		if(length(output.age) != 0){
			values(output.age)[which(values(output.age) == 0)] <- 1
			# lets clump them to count the nunmber of patches
			output.patch <- clump(output.age, directions=8)
			
			# error trapping
			if(all(is.na(values(output.patch))) == TRUE){
				output[j,1] <- 0
				output[j,3]	<- 0
				output[j,5] <- 0
			}else{
				output[j,1] <- length(unique(output.patch))
				output[j,3] <- mean(table(values(output.patch)))
				output[j,5] <- sum(table(values(output.patch)))
			}
		}
	}
	
	write.csv(output, file=paste(out_path,"Table04_",alf_name,".csv",sep=""))
	# now lets do some aggregation of that data into decadal averages
	output2 <- cbind(as.data.frame(output),paste(substring(rownames(output),1,3),"0s",sep=""))
	colnames(output2) <- c(colnames(output),"decadal_agg")
	aggData <- aggregate(output2[,2:7], by=list(output2[,ncol(output2)]), FUN=mean)
	write.csv(output2, file=paste(out_path,"Table04_",alf_name,"_decadalAgg.csv",sep=""))
}


# # read back in the output tables
# cccma <- read.csv("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/DerivedTables/Table03/Table03_CGCM31.csv")
# echam <- read.csv("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/DerivedTables/Table03/Table03_ECHAM.csv")

# # now lets do some aggregation of that data into decadal averages
# output2 <- cbind(as.data.frame(output),as.data.frame(paste(substring(output[,1],1,3),"0s",sep="")))
# colnames(output2) <- c(colnames(output),"decadal_agg")
# aggData <- aggregate(output2[,2:7], by=list(output2[,ncol(output2)]), FUN=mean)

# write.csv(aggData,file="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/DerivedTables/Table03/Table03_CCCMA_decadalAgg.csv")








