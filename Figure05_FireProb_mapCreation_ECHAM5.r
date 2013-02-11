# REASSESSMENT OF THE ALFRESCO CARIBOU FIRE RELATIONSHIP

# bring in some packages we need:
library(raster)
library(sp)
library(maptools)

setwd("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Maps/")


# we need to create a new matrix that has the columns of each of the reps outputs of the previous code
# how many reps are there?
nreps <- 90 # remember that the rep counts begin at ZERO
maps_path <- "/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps/"
years <- 2009:2100
incells <- 570115 # the number of cells in the raster layer

# create a list of the reps from [0:(reps-1)] IT IS ZERO ANCHORED!
repsList <- (1:nreps)-1

rep.mat.fireCount <- matrix(NA, nrow=incells, ncol=nreps)

for(reps in repsList){
	print(reps)

	# list the files we need
	# due to an asinine naming convention I am forced to do this the hard way
	# create an empty list object that is to be populated below
	l <- character()

	for(y in years){
		# make the desired output name
		outFile <- paste(maps_path,"Age_",reps,"_",y,".tif", sep="")

		# append that new filename to the list you instantiated above
		l <- append(l, outFile ,after=length(l))
	}

	# turn the list into a stack
	age0 <- stack(l)

	# now we will turn that stack into a matrix
	age0.mat <- getValues(age0)

	# now we will do the calculation
	a <- apply(age0.mat, 1, min)

	ind1 <- which(a > 0 | a < 0)#length(which(x == 0))
	ind2 <- which(a == 0)

	a[ind1] <- 0
	a[ind2] <- 1   

	rep.mat.fireCount[,reps+1] <- a
}

# give some more descript column names to describe the cols of the matrix
colnames(rep.mat.fireCount) <- paste("fireCount_rep",repsList, sep="")

# apply the function to the data that counts the fires 
finalSum <- rowSums(rep.mat.fireCount)

probBurn <- trunc((finalSum/nreps)*100,0) # where 90 is the total number of reps

outputRaster <- raster(matrix(probBurn, nrow=nrow(age0), byrow=TRUE),xmn=xmin(age0), xmx=xmax(age0), ymn=ymin(age0), ymx=ymax(age0), crs=projection(age0))

# lets change the values of the rock/ice class to something we can use later in display
alf2005 <- raster("/workspace/UA/malindgren/projects/NALCMS_Veg_reClass/August2012_FINALversion/Outputs/ALFRESCO_LandCover_2005_1km_gs6_5_FINALOUT3.tif")# bring in the input vegetation map
# crop and reset the extent to match the outputRAster with the alfresco veg map
alf2005.new <- crop(alf2005,intersect(alf2005,outputRaster))
extent(alf2005.new) <- extent(outputRaster)
alf2005.mask <- mask(alf2005.new, outputRaster)

values(outputRaster)[which(values(alf2005.new) == 0)] <- 110
values(outputRaster)[which(values(alf2005.new) == 255)] <- NA

writeRaster(outputRaster, filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Maps/fire_probability_allreps_allYears_cmip3_ECHAM5.tif", overwrite=T)

# some old code
# # normalize the data here?
# std=function(x){if(length(which(is.na(x)))==0) (x-mean(x))/sd(x) else (x-mean(x,na.rm=T))/sd(x,na.rm=T) } 
# # probBurn.scale <- scale(probBurn,center=TRUE)
# probBurn.scale <- std(probBurn)
# # now lets write out the new burn prob map for the series examined
# writeRaster(raster(matrix(probBurn.scale, nrow=nrow(age0), byrow=TRUE),xmn=xmin(age0), xmx=xmax(age0), ymn=ymin(age0), ymx=ymax(age0), crs=projection(age0)), filename="fire_probability_allreps_allYears_cmip3_ECHAM5_scale.tif", overwrite=T)

# # now lets add in a cut command to break it by quantiles and by sd
# output_map <- raster(matrix(probBurn.scale[,1], nrow=nrow(age0), byrow=TRUE),xmn=xmin(age0), xmx=xmax(age0), ymn=ymin(age0), ymx=ymax(age0), crs=projection(age0))

# #qmap <- quantile(output_map,c(0,.20,.40,.60,.80,1))

# quantile.cut <- cut(output_map,quantile(output_map))

# writeRaster(quantile.cut, filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/Figure_05_map/relative_flammability_echam5_allYearsReps_quantile.tif",overwrite=T)


# #sd
# sd.cut.vals <- c(mean(na.omit(getValues(output_map)))-sd(na.omit(getValues(output_map)))*2,
# (mean(na.omit(getValues(output_map))))-sd(na.omit(getValues(output_map))),
# mean(na.omit(getValues(output_map))),
# (mean(na.omit(getValues(output_map))))+sd(na.omit(getValues(output_map))),
# (mean(na.omit(getValues(output_map)))+sd(na.omit(getValues(output_map)))*2))

# sd.cut <- cut(output_map, sd.cut.vals)

# writeRaster(sd.cut, filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/Figure_05_map/relative_flammability_echam5_allYearsReps_stdev.tif",overwrite=T)


