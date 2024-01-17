# Apply field measurements of average density within HRUs to calculate SWE on days where regression was not strong
# Compare in situ snow depth and lidar snow depth using Hs .tifs and field survey data
# Original script by Alex Cebulski 2023
# Modified July 13, 2023 by Maddie Harasyn
# Note - if some in situ survey poitns are missing, this is because the rover was not on RTK lock while the GPS location was taken

# load libraries
library(tidyverse)
library(modelr)
library(ggpubr)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(fasterize)

#import list of tifs and indices for tifs for average density from previous code 
all_tifs <- scan("variables/all_tifs.txt", what = "")
avgdens_tifs <- scan("variables/avgdensity_tifs.txt", what = "")
average_indices <- scan("variables/avgdensity_indices.txt")

tifs_id <- substr(avgdens_tifs, 1, 6) #make a list of id values being used to filter survey data

#load HRU to partition snow depth tif, replace landcover names with survey names
HRU_shp <- st_read(file.path('hru/', "Fortress_HRU.shp"))

#change HRU classes to match snow survey names
colnames(HRU_shp)[2] <- 'Site'
HRU_shp[[2]][[1]] <- 'Powerline' #Conifer Forest HRU
HRU_shp[[2]][[2]] <- 'Canadian Ridge' #Slope forest north facing HRU
HRU_shp[[2]][[3]] <- 'Bonsai' #Valley clearing/water HRU
HRU_shp[[2]][[4]] <- 'Fortress Ridge' #Shrubland/ridge HRU
HRU_shp[[2]][[7]] <- 'Fortress Ridge South' #Slope forest south facing HRU


#plot hru shapefile
par(mar=c(0,0,0,0))
plot(st_geometry(HRU_shp), col = sf.colors(7, categorical = TRUE), lwd=0.25, border='black')


#location of field data 
survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points.csv')%>% 
  filter(Identifier %in% tifs_id) #only include days using average density 

# data table conversions ###########################################################
#convert snow depth to mm in table
survey$Depth <- survey$Hs_insitu *1000

# plot depth vs density to see spread of densities #########
survey |>
  ggplot(aes(x = Hs_insitu, y = Density))+
  geom_point(aes(colour = Site)) +
  facet_wrap(~as.Date(datetime, format = "%m/%d/%Y")) +
  theme_bw()

#make a nest to store the average densities for each date + site
nest <- survey %>%
  group_by(Site, Identifier)%>% 
  nest () %>%
  mutate(mean_density = map_dbl(data, ~ mean(.x$Density, na.rm=TRUE)),
         stdv_density = map_dbl(data, ~ sd(.x$Density, na.rm=TRUE)))%>% 
  group_by(Identifier) %>%
  nest()


#load in snow depth rasters for each date, apply average density value from nest to corresponding HRU
#multiply snow depth raster by HRU avg density layer to output SWE
#figure out how to overlay HRU from Logan on snow depth rasters, apply avg density to cells within HRU
output_statement <- c()
for (i in 1:length(tifs_id)){
  raster_Hs <- raster(paste("data/Hs/", avgdens_tifs[i], sep=""))
  names(raster_Hs) <- 'Hs_insitu'
  HRU_temp <- HRU_shp
  
  HRU_factor <- merge(HRU_temp, nest[[2]][[i]], by=c('Site') ,all=T) #merge shapefile and average densities from date (i)
  #add line here to filter/add value for sites with NaN avg density? 
  #Logan says he does not use areas where no data is collected, keep them NaN
  
  #create HRU_dens that only includes relevant info to be multiplied by lidar snow depth
  HRU_dens <- HRU_factor[, c(4, 6)]
  
  #convert HRU sp layer into a raster, multiply HRU raster by lidar snow depth raster
  HRU_rast <- fasterize(HRU_dens, raster_Hs, HRU_dens$mean_density)
  raster_swe <- (raster_Hs * HRU_rast) 
  
  writeRaster(raster_swe, paste0('outputs/swe/', avgdens_tifs[[i]]), overwrite = TRUE) #output .tif of swe map
  output_statement <- append(output_statement, avgdens_tifs[i])
}


#print output statement to tell user which method of SWE calc was applied to which tif
print(paste("SWE regression applied to: ", output_statement))

#output text file stating which files used avg density SWE
cat(sprintf(output_statement), "\n", file = "outputs/avgdensity_swe_tifs.txt")
