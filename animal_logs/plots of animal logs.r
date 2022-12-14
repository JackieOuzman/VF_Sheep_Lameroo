
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


path_output_files <- "W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/"


Lameroo_Vf_area_hard_fence_bound <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_working/HF_Lameroo_rough_proj.shp")  # this is the hard fences
Lameroo_Vf_area_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/HF_Lameroo_rough_10_proj.shp")  # this is the 

Lameroo_Vf_area <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")
Lameroo_Vf_area_buffer_10 <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_Buffer10_proj.shp")
water_pt <-  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/water_pts.shp")




animal_logs <- read_csv(paste0(path_output_files,
                                   "animal_GPS_data_nonTrain_step1_2.csv"))
                                   



names(animal_logs)


#turn into spatial data
animal_logs_sf <-
  st_as_sf(animal_logs,
           coords = c("X", "Y"),
           crs = 28354,
           agr = "constant")





ggplot() +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = animal_logs_sf ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(. ~ date)





#### I would like to display the audio and pulse records as another coloured dots
str(animal_logs_sf)


audio_records_only <- animal_logs_sf %>% 
   filter(Audio_values > 0)
shock_records_only <- animal_logs_sf %>% 
  filter(Shock_values > 0)

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "grey", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "grey", fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  #geom_sf(data = audio_records_only ,color ="Red",alpha = 0.01) +
  geom_sf(data = audio_records_only ,color ="Pink") +
  geom_sf(data = animal_logs_sf ,alpha = 0.01) +
  theme_bw()+
  theme(#legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(. ~ date)+
  labs(
    title = "Lameroo VF 2020 aniamal logs (non training)",
    subtitle = "Dates are faceted. \nBalck dots are GPS pts and pink are GPS records with pulse cues only. \nGrey is 10m buffer"
    #y = "", x = ""
    )


### whats happening with sheep 2 is it ok? yip I think so


animal_logs_sf_sheep2 <- animal_logs_sf %>% 
  filter(Sheep_ID == 2)

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "grey", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "grey", fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  #geom_sf(data = audio_records_only ,color ="Red",alpha = 0.01) +
  geom_sf(data = audio_records_only ,color ="Pink") +
  geom_sf(data = animal_logs_sf_sheep2 ,alpha = 0.01) +
  theme_bw()+
  theme(#legend.position = "none",
    axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(. ~ date)+
  labs(
    title = "Lameroo VF 2020 aniamal logs (non training)",
    subtitle = "Dates are faceted. \nBalck dots are GPS pts and pink are GPS records with pulse cues only. \nGrey is 10m buffer"
    #y = "", x = ""
  )


summary_nonTraining <- read_csv(paste0(path_output_files,
                               "summary_nonTrain.csv"))
names(summary_nonTraining)

summary_nonTraining %>%
  ggplot(aes(x = date , y = ratio)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~ Sheep_ID)+
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x=element_blank())+
  labs(
    title = "Lameroo VF Sheep - cue data for non training period",
    x = "Dates",
    y = "ratio audio / (pulse + audio) * 100")
