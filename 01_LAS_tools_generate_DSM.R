# Script to process lidar point clouds into DSM .tifs for use in snow depth analysis
# Original script by Phillip Harder January  27, 2023
# Updated by Maddie Harasyn 2023

# variables ###########################################################

#name of shapefile to clip ROI
shp_name<-"fortress_extent"

#resolution of output DEMs
RES_STEP = "1"

#list point cloud .las files
files<-list.files("F:/PointClouds/Fortress/Intensity", pattern="*.las",full.names = FALSE)  ######### CHANGE THIS LINE based on location of point clouds to be processed
files<-tools::file_path_sans_ext(files)

# LAS TOOLS ##########################################################

#LAStools processing produces a DEM
#processes are organised in LAStools_process.bat file.
#update lastools .bat script
txt<-readLines('LAStools_process.bat') 
#update list to process
txt[[13]]<-paste(c("set list=",files),collapse=" ")
#update working directory
txt[[14]]<-paste(c("set local_path=",gsub("/","\\\\",getwd())),collapse=" ")
#update clipping area with name of correct .shp file
txt[[15]]<-paste(c("set shp_name=", shp_name),collapse="")
#update resolution of DEMs
txt[[16]]<-paste(c("set STEP=", RES_STEP),collapse="")
writeLines(txt, con = "LAStools_process.bat")
#run LASTools code from R
shell('LAStools_process.bat') 
