# Apply in situ depth/density linear regression by day to lidar snow depth rasters
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

# variables ###########################################################

#R2 value cutoff to determine if regression will be used or not
regression_cutoff <- 0.5

#cutoff for minimum number of survey points used for regression (i.e. days with less than 15 snow density measurements will use avg density instead)
survey_cutoff <- 10

#location of field data 
survey <- read.csv('Z:/lidar-processing-basin/data/survey_data/survey_points.csv')%>% 
  filter(!Identifier %in% c('22_047')) ##22_047 is a bad data date!! 

# data table conversions ###########################################################
#convert snow depth to mm in table
survey$Depth <- survey$Hs_insitu *1000


# plot depth vs density to check relationships #########
survey |>
  filter(is.na(SWE) == F) |>
  ggplot(aes(x = Hs_insitu, y = Density))+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(aes(colour = Site)) +
  facet_wrap(~as.Date(datetime, format = "%m/%d/%Y")) +
  stat_cor(aes(label = paste(after_stat(rr.label))), label.x.npc = 0.65, label.y.npc = 0.2, size = 3) +
  stat_cor(aes(label = paste(after_stat(p.label))), label.x.npc = 0.65, label.y.npc = 0.05, size = 3) +
  theme_bw()

# calculate the linear regression for each survey date followed process from: ####
# https://r4ds.had.co.nz/many-models.html#many-models

lm_model <- function(df) {
  lm(Density ~ Hs_insitu, data = df)
}


#filter out survey data by dates not meeting regression filters, create nest model 
nest <- survey |> 
  filter(is.na(Density) == F) |> 
  group_by(datetime) |> 
  filter(n()> survey_cutoff) |> #remove dates with less than 10 survey measurements (not adequate regression)
  nest() |> 
  mutate(model = map(data, lm_model),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions))

#create a list of dates that are filtered out because they do not have enough survey data
insuf_survey <- survey |> 
  filter(is.na(Density) == F) |> 
  group_by(datetime) |> 
  filter(n()< survey_cutoff)
insuf_survey <- unique(insuf_survey$Identifier)
  

#filter out dates with low R2 value
regression_indices <- c()
average_indices <- c()
all_tifs <- c()
average_density_tifs <- c()

for (i in 1:nrow(nest)){
  day_regression <- summary(nest[[3]][[i]])$adj.r.squared
  all_tifs <- append(all_tifs, paste(nest[[2]][[i]][[1, 1]], '.tif', sep=""))
  if (day_regression > regression_cutoff){
    regression_indices <- append(regression_indices, i) #create a list of indices for images to apply regression to
  }else{
    average_indices <- append(average_indices, i) #create a list of indices for images to apply avg density to
    average_density_tifs <- append(average_density_tifs, all_tifs[i])
  }
}

#apply regression SWE calculation to rasters 
output_statement <- c()
for (i in regression_indices){
  model_day <- nest$model[[i]] #need to filter if R2 less than 0.5
  raster_Hs <- raster(paste("data/Hs/", all_tifs[i], sep=""))
  names(raster_Hs) <- 'Hs_insitu'
  raster_density <- predict(raster_Hs, model_day)
  writeRaster(raster_density, paste0('outputs/density/', all_tifs[[i]]), overwrite = TRUE) #output .tif of density map
  
  raster_swe <- (raster_Hs * raster_density)
  writeRaster(raster_swe, paste0('outputs/swe/', all_tifs[[i]]), overwrite = TRUE) #output .tif of swe map
  
  output_statement <- append(output_statement, all_tifs[i])
}

#output all_tifs list and average_indices list for next code (05_swe_basin_map_avg)
all_tifs_out <- paste(unlist(all_tifs),collapse=" ")
average_indices_out <- paste(unlist(average_indices),collapse=" ")

cat(sprintf(all_tifs_out), "\n", file = "variables/all_tifs.txt")
cat(sprintf(average_density_tifs), "\n", file = "variables/avgdensity_tifs.txt") #CHANGED THIS LINE TO EXPORT ONLY AVG DENSITY TIFS LIST, CLEAN UP CODE AFTER 05 STEP COMPLETE
cat(sprintf(average_indices_out), "\n", file = "variables/avgdensity_indices.txt")

#print output statement to tell user which method of SWE calc was applied to which tif
print(paste("Date removed due to insufficient survey:", insuf_survey))
print(paste("SWE regression applied to: ", output_statement))

#output text file stating which files used regression SWE
cat(sprintf(output_statement), "\n", file = "variables/regression_swe_tifs.txt")

