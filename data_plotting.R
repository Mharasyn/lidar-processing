library(tidyverse)
library(dplyr)
library(stringr)
library(ggpmisc)
library(scales)

Hs_data <- read.csv("Z:/CGU_analysis/R_outputs/FT_survey_data_01.csv")
point_error_data <- read.csv("Z:/CGU_analysis/R_outputs/FT_point_error_01.csv")
error_summary <- read.csv("Z:/CGU_analysis/Hs_errors_canopy_01.csv")
met_data <- read.csv("Z:/FieldData/flight_met_summary_table.csv")

### FUNCTIONS ####
dropLeadingZero <- function(l){
  str_replace(l, '0(?=.)', '')
}

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

#####
surv_select <- Hs_data %>% 
  # filter(canopy %in% c('O'))%>% 
  filter(!Identifier %in% c('22_140'))
max_bias <- max(surv_select$lidar_minus_insiut)
min_bias <- min(surv_select$lidar_minus_insiut)

##IN SITU DEPTHS VERIFICATION #####################################
#create plot of in situ snow depths for specific sampling site over all days 
surv_select <- Hs_data %>% 
  filter(survid == 3, canopy %in% c('M', 'E', 'O'), transect == 'T2')%>% 
  filter(!Identifier %in% c('22_140'))

ggplot(surv_select, aes(x = Identifier, y = Hs_insitu, colour= canopy)) +
  geom_point() #+
# facet_grid(~canopy) # change to transect later 




##LIDAR BIAS FOR ALL SURVEY POINTS##################################
#plot lidar bias for all survey points, coloured by survey date
surv_select <- Hs_data %>% 
  filter(canopy %in% c('M', 'O'))

surv_select %>% 
  ggplot(aes(x = surveyIndx, y = insitu_minus_lidar)) +
  geom_point(aes(colour = Identifier)) +        
  theme_bw() +
  scale_colour_viridis_d(option = "D",  name="Sampling\nDate") +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm',se = FALSE) +
  labs(x = "Survey Point # (T2 1 = 30)", y= "in situ - lidar (m)") 

#command to make scatterplot interactive 
plotly::ggplotly()
  

##1:1 PLOT OF LIDAR HS VERSUS IN SITU HS ##########################
#create plot 1:1 of lidar Hs/in situ Hs
surv_select <- Hs_data %>% 
  filter(canopy %in% c('M', 'O'))%>% 
  filter(!Identifier %in% c('22_140'))

surv_select %>% 
  ggplot(aes(x = Hs_insitu, y = Hs_lidar))+
  geom_point(aes(colour = canopy), size = 2) +
  
  scale_color_manual(values = c("M" = "#002D70", "O" = "#E1A02B"), name="Sampling\nLocation", labels=c("Tree Well", "Between Trees")) +
  
  geom_abline(slope=1, intercept = 0) + #ADD 1:1 LINE
  
  theme_bw() +                         #CHANGE ASETHETICS OF GRAPHS
  theme(panel.grid = element_blank(), strip.text = element_text(size = 9))+ 
  
  labs(x = expression(paste(italic("in situ"), " snow depth (m)")), y= "Lidar snow depth (m)") + #DEFINE AXIS LABELS
  scale_y_continuous(labels = dropLeadingZero)+ #REMOVE LEADING ZEROS FROM AXIS LABELS
  scale_x_continuous(labels = dropLeadingZero)+
  
  
  facet_wrap(~as.Date(datetime, format = "%m/%d/%Y"), ncol=3) #separates plots based on datetime

ggsave(paste0("Z:/CGU_analysis/Figures/insitu_lidar_snowdepth2.png"))

#command to make scatterplot interactive 
plotly::ggplotly()


##MET DATA PLOTTING ##########################
#compare met data to Hs bias
surv_select <- dplyr::left_join(error_summary, 
                                met_data, 
                                by = c("Identifier" = "flight_id"),
                                multiple = "all")
surv_select <- surv_select %>% 
  filter(canopy %in% c('A')) %>% 
  filter(!Identifier %in% c('22_140'))

surv_select %>% 
  ggplot(aes(y = lidar_insitu_Hs_RMSE, x = Avg..Wind.Speed..m.s.))+
  geom_point(colour="#002D70", size = 4) +
  # geom_point(aes(y = lidar_insitu_Hs_Bias), colour = "#002D70", shape = 17, size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  ylim(-0.2, 0.2)+
  geom_smooth(method = lm, se = FALSE, colour="#E1A02B") +
  labs(x = 'Wind speed (m/s)', y= "") 

ggsave(paste0("Z:/CGU_analysis/Figures/windspeed_rmse.png"))

plotly::ggplotly()  
  
# , shape = 17
# , linetype = 'dashed'

##SCATTERPLOT COLOURED BY DATE #########################################
#create scatterplot coloured by date

surv_select <- Hs_data %>% 
  filter(canopy %in% c('M', 'O'))%>% 
  filter(!Identifier %in% c('22_140'))

surv_select %>% 
  ggplot(aes(x = Hs_insitu, y = Hs_lidar))+
  geom_point(aes(colour = Identifier)) +
  scale_colour_viridis_d(option = "A") +
  
  
  geom_abline(slope=1, intercept = 0) +
  theme_bw() +
  labs(x = "in situ snow depth (m)", y= "Lidar snow depth (m)") 

ggsave(paste0("Z:/CGU_analysis/Figures/insitu_lidar_snowdepth_bydate.png"))

#command to make scatterplot interactive 
plotly::ggplotly()



##1:1 PLOT OF LIDAR HS VERSUS IN SITU HS BY DEM RESOLUTION ##################
#create plot 1:1 of lidar Hs/in situ Hs
error_summary_001 <- read.csv("Z:/lidar-processing/data/error_summary/res_001/FT_survey_data_001.csv")
  error_summary_001$res<-"0.01 m"
error_summary_01 <- read.csv("Z:/lidar-processing/data/error_summary/res_01/FT_survey_data_01.csv")
  error_summary_01$res<-"0.1 m"
error_summary_1 <- read.csv("Z:/lidar-processing/data/error_summary/res_1/FT_survey_data_1.csv")
  error_summary_1$res<-"1 m"
error_summary_005 <- read.csv("Z:/lidar-processing/data/error_summary/res_005/FT_survey_data_005.csv")
  error_summary_005$res<-"0.05 m"
error_summary_05 <- read.csv("Z:/lidar-processing/data/error_summary/res_05/FT_survey_data_05.csv")
  error_summary_05$res<-"0.5 m"

error_summary_full <- rbind(error_summary_001, error_summary_01, error_summary_1, error_summary_005,error_summary_05)

surv_select <- error_summary_full %>% 
  filter(canopy %in% c('M', 'O'))%>% 
  filter(!Identifier %in% c('22_068', '22_140', '23_059', '23_072', '23_073'))

surv_select %>% 
  ggplot(aes(x = Hs_insitu, y = Hs_lidar))+
  geom_point(aes(colour = canopy), size = 2) +
  scale_color_manual(values = c("M" = "#002D70", "O" = "#E1A02B")) +
  
  geom_abline(slope=1, intercept = 0) + #ADD 1:1 LINE
  
  theme_bw() +                         #CHANGE ASETHETICS OF GRAPHS
  theme(panel.grid = element_blank(), strip.text = element_text(size = 9))+ 
  
  labs(x = expression(paste(italic("in situ"), "snow depth (m)")), y= "Lidar snow depth (m)", color = "DEM\nResolution") + #DEFINE AXIS LABELS

  scale_y_continuous(labels = dropLeadingZero)+ #REMOVE LEADING ZEROS FROM AXIS LABELS
  scale_x_continuous(labels = dropLeadingZero) +
  
  facet_wrap(~res, ncol=1) #separates plots based on datetime

ggsave(paste0("Z:/CGU_analysis/Figures/insitu_lidar_resolution.png"))

#command to make scatterplot interactive 
plotly::ggplotly()





##DEM RESOLUTION VERSUS LIDAR BIAS ##################
#create plot 1:1 of lidar Hs/in situ Hs
error_summary_canopy <- read.csv("Z:/CGU_analysis/Hs_error_summary_canopy.csv")


error_summary_canopy %>% 
  ggplot(aes(x = RES, y = bias)) +
  # geom_point(aes(y = bias, colour = CANOPY), shape = 17, size = 3) + #add this line to plot both rsme and bias on same plot
  geom_point(aes(colour = CANOPY),shape = 17, size = 4) +

  scale_color_manual(values = c("M" = "#002D70", "O" = "#E1A02B"), name="Sampling\nLocation", labels=c("Tree Well", "Between Trees")) +

  theme_bw() +                         #CHANGE ASETHETICS OF GRAPHS
  theme(strip.text = element_text(size = 9))+
  geom_hline(yintercept = 0) +
  # ylim(-0.1, 0)+
  labs(x = "Snow depth DEM resolution (m)", y= "Lidar Bias (m)", color = "DEM\nResolution") #DEFINE AXIS LABELS


ggsave(paste0("Z:/CGU_analysis/Figures/insitu_lidar_avgbias.png"))

#command to make scatterplot interactive 
plotly::ggplotly()


##POINT CLOUD THINNING VERSUS LIDAR BIAS ###########
thinning_error <- read.csv("Z:/CGU_analysis/R_outputs/lidar_thinning_error.csv")

thinning_error %>% 
  ggplot(aes(x = point_res, y = bias)) +
  geom_point(aes(y = RMSE, colour = canopy), shape = 19, size = 3) + #add this line to plot both rsme and bias on same plot
  geom_point(aes(colour = canopy),shape = 17, size = 3) +

  scale_color_manual(values = c("M" = "#002D70", "O" = "#E1A02B"), name="Sampling\nLocation", labels=c("Tree Well", "Between Trees")) +

  scale_x_continuous(trans=reverselog_trans(10))+
  theme_bw() +                         #CHANGE ASETHETICS OF GRAPHS
  theme(strip.text = element_text(size = 9))+
  geom_hline(yintercept = 0) +
  labs(x = "Point cloud resolution (pt/m2)", y= "Lidar Bias/RMSE (m)", color = "DEM\nResolution") #DEFINE AXIS LABELS


ggsave(paste0("Z:/CGU_analysis/Figures/pointcloudres_biasRMSE.png"))

