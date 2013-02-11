# REASSESSMENT OF THE ALFRESCO CARIBOU FIRE RELATIONSHIP
# This is for the creation of figure 1
# 

# bring in some packages we need:
library(raster)
library(sp)
library(maptools)
library(ggplot2)

setwd("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/working")

alf_names <- c("CGCM31","ECHAM")

median_reps <- c(72,49) #format c(cccma, echam5)

# set an output path
out_path <- "/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Figures/fig03_04/Fig03_04_Tables/"

# read in the 2009 data
alf.veg <- raster("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/alf_veg_2005/NA_LandCover_2005_PRISM_extent_AKAlbers_1km_ALFRESCO_FINAL.tif")
# alf.veg <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CGCM31_Maps2009to2100/Maps/Veg_0_2009.tif")
# echam5.2009 <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps/Veg_0_2009.tif")

# read in the polygons needed for the analysis
pch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/PCH_winter_90kernal.shp")
cch <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/CaribouGIS/CAH_winter_expert.shp")

template <- raster("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/ECHAM_Maps2009to2100/Maps/Age_0_2009.tif")

# rasterize the input polygons to the raster extent and scale of the output maps
pch.ras <- rasterize(pch, template)
cch.ras <- rasterize(cch, template)

total.pch <- length(which(values(pch.ras) == 1))
total.cch <- length(which(values(cch.ras) == 1))

# this is the range of years
years <- 1950:2100

# read in the data that we want
for(i in 1:length(alf_names)){
	alf_name <- alf_names[i]
	median_rep <- median_reps[i]
	
	output <- matrix(nrow=length(years), ncol=2)
	if(alf_name == "CGCM31"){
		colnames(output) <- c("CCCMA Spruce PCH >60","CCCMA Spruce CAH >60") #
	}else{
		colnames(output) <- c("ECHAM5 Spruce PCH >60","ECHAM5 Spruce CAH >60")
	}
	print(alf_name)

	for(j in 1:length(years)){
		year <- years[j]
		print(year)
		if(year < 2009){
			in_data_path <- "/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CalibrationResults_Maps1950to2009/Maps/" # 1950-2008
		}else {
			in_data_path <- paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/",alf_name,"_Maps2009to2100/Maps/",sep="")
		}

		# else if(year == 2009){
		# 	age <- raster(paste(in_data_path <- paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/",alf_name,"_Maps2009to2100/Maps/",sep=""),"Age_",median_rep,"_",year,".tif",sep=""))
		# 	veg <- raster(paste("/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CalibrationResults_Maps1950to2009/Maps/","Veg_",median_rep,"_",year-1,".tif",sep=""))
		# }
		
		# # bring in the 2 maps for the current time step
		age <- raster(paste(in_data_path,"Age_",median_rep,"_",year,".tif",sep=""))
		veg <- raster(paste(in_data_path,"Veg_",median_rep,"_",year,".tif",sep=""))

		output[j,1] <- length(which((values(veg) == 2 | values(veg) == 3) & values(age) > 60 & values(pch.ras) == 1))/total.pch
		output[j,2] <- length(which((values(veg) == 2 | values(veg) == 3) & values(age) > 60 & values(cch.ras) == 1))/total.cch
	}
	rownames(output) <- years
	write.csv(output,file=paste(out_path,"Figure_03_04_",alf_name,"_pct.csv",sep=""))
}


# this option makes it easier to work with the data since it keeps bringing is as factors
options(stringsAsFactors=FALSE)

# now lets ggplot it FOR Spruce GROUP
echam <- as.matrix(read.csv("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Figures/fig03_04/Fig03_04_Tables/Figure_03_04_ECHAM_pct.csv", stringsAsFactors=FALSE))
cccma <- as.matrix(read.csv("/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Figures/fig03_04/Fig03_04_Tables/Figure_03_04_CGCM31_pct.csv",stringsAsFactors=FALSE))

rownames(echam) <- c()
colnames(echam) <- c()
rownames(cccma) <- c()
colnames(cccma) <- c()


# here we rework the data to fit the needs of the work
pch <- as.data.frame(rbind(cbind(echam[,1:2],"mpi_echam5"),cbind(cccma[,1:2],"cccma_cgcm31")))

# # convert the character to numeric
pch[,2] <- as.numeric(pch[,2])

# add a new column to aggregate the data on
pch$aggData <- paste(substring(pch[,1],1,3),"0s.",pch[,3],sep="")

# now we need to aggregate that data based on a new column
pch.agg <- aggregate(pch[,2], by=list(pch[,4]), FUN=mean)

tmp <- strsplit(pch.agg[,1],".", fixed=TRUE)

# now we will return that data to a data.frame that will be used in graphing
pch.df <- as.data.frame(matrix(NA,nrow=nrow(pch.agg),ncol=ncol(pch.agg)+1)) # new empty data.frame

for(j in 1:length(tmp)){
	pch.df[j,1] <- as.numeric(substring(tmp[[j]][1],1,4))
	pch.df[j,3] <- tmp[[j]][2]
}
pch.df[,2] <- pch.agg[,2]


# now re-work the cah stuff
cah <- as.data.frame(rbind(cbind(echam[,c(1,3)],"mpi_echam5"),cbind(cccma[,c(1,3)],"cccma_cgcm31")))

# # convert the character to numeric
cah[,2] <- as.numeric(cah[,2])

# add a new column to aggregate the data on
cah$aggData <- paste(substring(cah[,1],1,3),"0s.",cah[,3],sep="")

# now we need to aggregate that data based on a new column
cah.agg <- aggregate(cah[,2], by=list(cah[,4]), FUN=mean)

tmp <- strsplit(cah.agg[,1],".", fixed=TRUE)

# now we will return that data to a data.frame that will be used in graphing
cah.df <- as.data.frame(matrix(NA,nrow=nrow(cah.agg),ncol=ncol(cah.agg)+1)) # new empty data.frame

for(j in 1:length(tmp)){
	cah.df[j,1] <- as.numeric(substring(tmp[[j]][1],1,4))
	cah.df[j,3] <- tmp[[j]][2]
}
cah.df[,2] <- cah.agg[,2]

# now lets get the full range of values across both data groups
# ranges <- range(c(range(pch.df[,2]),range(cah.df[,2])))

# change the column names of the data.frame for use in ggplot2
colnames(pch.df) <- c("years","greater60","model")
colnames(cah.df) <- c("years","greater60","model")

# instantiate a ggplot2 instance with pch
pp <- ggplot(pch.df, aes(x=years, y=greater60, group=model, colour=model))

# make a png graphics device
png(filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Figures/fig03_04/Age_greater60_1950_2100_spruce_pch_012913_pct.png",width=800, height=800) 
pp + geom_line(size=1) + ggtitle("Spruce Cover with Age > 60 years \nPorcupine Herd (winter range)") + scale_y_continuous("Age > 60 years (% cover)") + scale_x_continuous("time (years)", breaks=seq(1950,2100,20)) + scale_color_manual(values=c("black", "grey60"))+ opts(panel.background = theme_rect(fill="grey85", colour="grey85"))
dev.off()

# instantiate a ggplot2 instance with cah
pc <- ggplot(cah.df, aes(x=years, y=greater60, group=model, colour=model))

# make a png graphics device
png(filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/FINAL_Jan2012/Figures/fig03_04/Age_greater60_1950_2100_spruce_cah_012913_pct.png",width=800, height=800) 
pc + geom_line(size=1) + ggtitle("Spruce Cover with Age > 60 years \nCentral Arctic Herd (winter range)") + scale_y_continuous("Age > 60 years (% cover)") + scale_x_continuous("time (years)", breaks=seq(1950,2100,20)) + scale_color_manual(values=c("black", "grey60"))+ opts(panel.background = theme_rect(fill="grey85", colour="grey85"))
dev.off()





# rownames(echam) <- c()
# colnames(echam) <- c()
# rownames(cccma) <- c()
# colnames(cccma) <- c()


# # here we rework the data to fit the needs of the work
# pch <- as.data.frame(rbind(cbind(echam[,1:2],"mpi_echam5"),cbind(cccma[,1:2],"cccma_cgcm31")))

# cah <- as.data.frame(rbind(cbind(echam[,c(1,3)],"mpi_echam5"),cbind(cccma[,c(1,3)],"cccma_cgcm31")))


# ### ABOVE IS WORKING


# # for the pch herd FIGURE 1
# # plotTable <-rbind(cbind(echam[,1],echam[,2],"echam5"),cbind(cccma[,1],cccma[,2],"cccma"))
# plotTable <- pch
# plotTable[1:nrow(echam),1:3] <- echam
# plotTable[nrow(echam)+1:nrow(cccma),1:3] <- cccma

# colnames(plotTable) <- c("years","greater60","model")

# p <- ggplot(as.data.frame(plotTable[,1:2]), aes(x=years, y=greater60, group=model, colour=model))

# png(filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/Age_greater60_1950_2100_Spruce_pch_2013.png")

# # p + geom_line(size=1) + scale_x_discrete(breaks=c(2000,2010,2020,2030,2040,2050,2060,2070,2080,2090), labels=c("2000","2010","2020","2030","2040","2050","2060","2070","2080","2090")) + scale_y_discrete(breaks=c(55,60,65,70,75), labels=c("55","60","65","70","75"))
# p + geom_line(size=1) + opts(title="Percent Cover Spruce >60 years - Porcupine Herd (winter range)") + scale_x_discrete(breaks=c(1950,2000,2050,2100)) + scale_y_discrete(breaks=c(min(as.integer(plotTable[,2])),median(as.integer(plotTable[,2])),max(as.integer(plotTable[,2])))) #breaks=quantile(as.numeric(plotTable[,1]))
# dev.off()

# # for the cah herd FIGURE 2
# plotTable <-rbind(cbind(echam[,1],echam[,3],"echam5"),cbind(cccma[,1],cccma[,3],"cccma"))

# colnames(plotTable) <- c("years","greater60","model")

# p <- ggplot(as.data.frame(plotTable), aes(x=years, y=greater60, group=model, colour=model))

# png(filename="/workspace/UA/malindgren/projects/ALFRESCO/Caribou_Reanalysis/Output/Age_greater60_1950_2100_Spruce_cah_2013.png")

# # p + geom_line(size=1) + scale_x_discrete(breaks=c(2000,2010,2020,2030,2040,2050,2060,2070,2080,2090), labels=c("2000","2010","2020","2030","2040","2050","2060","2070","2080","2090")) + scale_y_discrete(breaks=c(55,60,65,70,75), labels=c("55","60","65","70","75"))
# p + geom_line(size=1) + opts(title="Percent Cover Spruce >60 years - Central Arctic Herd (winter range)") + scale_x_discrete(breaks=c(1950,2000,2050,2100)) + scale_y_discrete(breaks=c(min(as.integer(plotTable[,2])),median(as.integer(plotTable[,2])),max(as.integer(plotTable[,2]))))
# dev.off()





