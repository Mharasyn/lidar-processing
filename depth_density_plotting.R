#Plotting depth/density curves by site @ Fortress
#Maddie Harasyn
#Started June 27, 2023
rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)

# variables ###########################################################
#location of field data (output from rover_snow_processing.R) 
survey<-read.csv('Z:/lidar-processing-basin/data/survey_data/2022_2023_Drone_Survey_points.csv') |>
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y", tz = 'Etc/GMT+6')) |> 
  mutate(yy_ddd = paste0(format(Date, "%y"), "_", format(Date, "%j")))
drone_data<-read.csv('Z:/lidar-processing-basin/data/error_summary/Fortress_survey_data.csv')

drone_select <- drone_data %>% 
  filter(!Site %in% c('Powerline')) %>% 
  filter(!Identifier %in% c('22_047')) ##22_047 is a bad data date!!

surv_select <- survey %>% 
  filter(!Density %in% c(NA)) 

####Plot all points of one site to see if IDs are wacky####
site_select <- drone_select %>% 
  filter(!Hs_lidar %in% c(NA)) %>% 
  filter(Site %in% c('Canadian Ridge'))

ggplot(site_select, aes(x = Surv_ID)) +
  geom_line(aes(x = Surv_ID, y = Hs_insitu), colour = 'brown', linewidth = 1.5) +
  geom_line(aes(x = Surv_ID, y = Hs_lidar), colour = 'deepskyblue', linewidth = 1.5) +
  theme_bw() +
  labs(x = "Transect Point ID", y= "Snow depth (m)") +
  facet_wrap(~as.Date(datetime, format = "%m/%d/%Y"))

ggsave(paste0("Z:/lidar-processing-basin/figures/fortress_insitu_vs_lidar_CR.png"))

####Plot each day in a separate graph, coloured by site####
ggplot(surv_select, aes(x = Depth, y= Density, colour = Site)) +
  geom_point() +
  theme_bw() +
  scale_colour_viridis_d(option = "D",  name="Site") +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(x = "Depth (m)", y= "Density (kg/m3)") +
  facet_wrap(~as.Date(Date, format = "%m/%d/%Y"))

# ggsave(paste0("Z:/lidar-processing-basin/figures/fortress_depth_density_FRS.png"))

####Plot each site in a separate graph, coloured by day####
ggplot(surv_select, aes(x = Depth, y= Density, colour = yy_ddd)) +
  geom_point() +
  theme_bw() +
  scale_colour_viridis_d(option = "D",  name="Sampling\nDate") +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(x = "Depth (m)", y= "Density (kg/m3)") +
  facet_wrap(vars(Site), ncol=3)

# ggsave(paste0("Z:/lidar-processing-basin/figures/fortress_depth_density_curve.png"))

####Plot drone snow depth vs in situ depth####
ggplot(drone_select, aes(x = Hs_insitu, y= Hs_lidar, colour = Identifier)) +
  geom_point() +
  theme_bw() +
  scale_colour_viridis_d(option = "D",  name="Sampling\nDate") +
  geom_abline(slope=1, intercept = 0) + #ADD 1:1 LINE
  labs(x = "in situ snow depth (m)", y= "lidar snow depth (m)") +
  facet_wrap(vars(Site))

# ggsave(paste0("Z:/lidar-processing-basin/figures/fortress_insitu_vs_lidar_bysite.png"))

####Plot drone snow depth vs in situ depth average for each day####
avg_Hs <- drone_select |>
  group_by(Identifier, Site) |>
  summarise(avg_insitu = mean(Hs_insitu))

avg_Hs$avg_lidar <- drone_select |>
  group_by(Identifier, Site) |>
  summarise(avg_lidar = mean(Hs_lidar))

ggplot(avg_Hs, aes(x = avg_insitu, y= avg_lidar$avg_lidar, colour = Identifier)) +
  geom_point() +
  theme_bw() +
  scale_colour_viridis_d(option = "D",  name="Sampling\nDate") +
  geom_abline(slope=1, intercept = 0) + #ADD 1:1 LINE
  labs(x = "in situ snow depth (m)", y= "lidar snow depth (m)") +
  facet_wrap(vars(Site))

# ggsave(paste0("Z:/lidar-processing-basin/figures/fortress_insitu_vs_lidar_snowdepth_avg.png"))
