### plots and summary analysis

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

###########################################################################################
############                  bring in boundaries             ##############################
############################################################################################



Lameroo_Vf_area_hard_fence_bound <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_working/HF_Lameroo_rough_proj.shp")  # this is the hard fences
Lameroo_Vf_area_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/HF_Lameroo_rough_10_proj.shp")  # this is the 

Lameroo_Vf_area <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")
Lameroo_Vf_area_buffer_10 <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_Buffer10_proj.shp")
water_pt <-  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/water_pts.shp")





Lameroo_Vf_area_hard_fence_bound <-
  st_transform(Lameroo_Vf_area_hard_fence_bound, crs = 28354)

Lameroo_Vf_area_buffer_10 <-
  st_transform(Lameroo_Vf_area_buffer_10, crs = 28354)
Lameroo_Vf_area <-
  st_transform(Lameroo_Vf_area, crs = 28354)
water_pt <-
  st_transform(water_pt, crs = 28354)

Lameroo_Vf_area_hard_fence_bound_buff<-
  st_transform(Lameroo_Vf_area_hard_fence_bound_buff, crs = 28354)

###########################################################################################
############                  bring in step 1 2 and 3 df             ##############################
############################################################################################

step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                       coords = c("X", "Y"),
                       crs = 28354,
                       agr = "constant")





names(step1_2_3_sf)

check <-
  step1_2_3_sf %>% dplyr::select( "deviceName"  ,
                           "Audio_values"    ,
                           "Shock_values" , 
                           "local_time"          ,  
                           "Sheep_ID"  ,
                           training_period)


### UP TO HERE ####


## TASK Mean audio/pulse ratio AIM: Across all animals
names(step1_2_3_sf)

summary_audio_ratio <- step1_2_3_sf %>% 
  group_by(Sheep_ID, date, training_period) %>% 
  summarise(audio_sum = sum(Audio_values, na.rm = TRUE),
            pulse_sum = sum(Shock_values, na.rm = TRUE),
            ratio_sum1 = audio_sum/ (pulse_sum+audio_sum )*100,
            ratio_sum2 = pulse_sum/ (audio_sum )*100)
  
summary_audio_ratio <- summary_audio_ratio %>% 
  ungroup(summary_audio_ratio) %>% 
  dplyr::select(-geometry)
names(summary_audio_ratio)

summary_audio_ratio$ratio_sum1 [is.nan(summary_audio_ratio$ratio_sum1 )]<-NA
summary_audio_ratio$ratio_sum2 [is.nan(summary_audio_ratio$ratio_sum2 )]<-NA

names(summary_audio_ratio)
summary_audio_ratio %>%
  filter(training_period == "non_training") %>% 
  ggplot(aes(x = date , y = ratio_sum1)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~Sheep_ID)+
  theme(axis.text.x = element_text(angle = 90))+
  #geom_vline(xintercept = as.Date(vertical_lines), col = "blue")+
  labs(
    x = "Date",
    y = "ratio (Audio / pulse+audio)*100 ",
    title = paste("Animals fitted with VF collars")
  )




summary_audio_ratio_from_training <- summary_audio_ratio %>% 
  group_by(training_period) %>% 
  summarise(audio_av = mean(audio_sum, na.rm = TRUE),
            pulse_av = mean(pulse_sum, na.rm = TRUE),
            ratio_1_mean = mean(ratio_sum1, na.rm= TRUE),
            ratio_2_mean = mean(ratio_sum2, na.rm= TRUE))
            

summary_audio_ratio_from_training %>%
  ggplot(aes(x = training_period , y = audio_av)) +
  geom_col()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))+
  #geom_vline(xintercept = as.Date(vertical_lines), col = "blue")+
  labs(
    x = "Traing period",
    y = "Avearge Audio ",
    title = paste("Average Audio in training period and during trial")
  )


################################################################################
## per animal per day what is the total (sum) and then what is the ratio
names(Fence_all)

non_train_summary <-  Fence_all %>% 
  group_by(Sheep_ID, date) %>% 
  summarise(sum_aduio = sum(Audio_values, na.rm = TRUE),
            sum_pulse = sum(Shock_values, na.rm = TRUE),
            ratio = (sum_aduio/(sum_pulse+ sum_aduio))*100)
non_train_summary$ratio <-   round(non_train_summary$ratio ,2)

non_train_summary$ratio [is.nan(non_train_summary$ratio )]<-NA


write.csv(non_train_summary, 
          paste0(path_output_files,"/summary_nonTrain.csv"), 
          row.names=FALSE)
