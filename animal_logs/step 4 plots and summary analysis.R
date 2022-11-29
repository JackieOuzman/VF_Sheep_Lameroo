### plots and summary analysis

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

############################################################################################
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
  step1_2_3_sf %>% dplyr::select("deviceName",
                                 "Audio_values",
                                 "Shock_values",
                                 "local_time",
                                 "Sheep_ID")





## TASK Mean audio/pulse ratio AIM: Across all animals
names(step1_2_3_sf)
# step 1 summaries audio and pulse per animal per day also training period 
summary_audio_ratio <- step1_2_3_sf %>% 
  dplyr::group_by(Sheep_ID, date) %>% 
  dplyr::summarise(audio_sum = sum(Audio_values, na.rm = TRUE),
            pulse_sum = sum(Shock_values, na.rm = TRUE),
            ratio_sum1 = audio_sum/ (pulse_sum+audio_sum )*100,
            ratio_sum2 = pulse_sum/ (audio_sum )*100)
  
summary_audio_ratio <- ungroup(summary_audio_ratio)

summary_audio_ratio$ratio_sum1 [is.nan(summary_audio_ratio$ratio_sum1 )]<-NA
summary_audio_ratio$ratio_sum2 [is.nan(summary_audio_ratio$ratio_sum2 )]<-NA

names(summary_audio_ratio)
summary_audio_ratio %>%
  ggplot(aes(x = date , y = ratio_sum1)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~Sheep_ID)+
  theme(axis.text.x = element_text(angle = 90))+
  #geom_vline(xintercept = as.Date(vertical_lines), col = "blue")+
  labs(
    x = "Date",
    y = "ratio (Audio / pulse+audio)*100 ",
    title = "Animals fitted with VF collars",
    subtitle = "audio and pulse counts summed per day and animal and then ratio calulated")
  
summary_audio_ratio








# step 2 summaries audio and pulse per animal per day also training period 

summary_audio_ratio_all <- summary_audio_ratio %>% 
  dplyr::group_by() %>% 
  dplyr::summarise(audio_av = mean(audio_sum, na.rm = TRUE),
            pulse_av = mean(pulse_sum, na.rm = TRUE),
            
            std_dev_Av_Audio = sd(audio_sum, na.rm = TRUE),
            SE_Av_Audio = std_dev_Av_Audio / sqrt(n()),
            
            std_dev_Av_Pulse = sd(pulse_sum, na.rm = TRUE),
            SE_Av_Pulse = std_dev_Av_Pulse / sqrt(n()),
            
            ratio_1_mean = mean(ratio_sum1, na.rm= TRUE),
            ratio_2_mean = mean(ratio_sum2, na.rm= TRUE),
            
            std_dev_Av_Ratio_1 = sd(ratio_sum1, na.rm = TRUE),
            SE_Av_std_dev_Av_Ratio_1 = std_dev_Av_Ratio_1 / sqrt(n()),
            
            std_dev_Av_Ratio_2 = sd(ratio_sum2, na.rm = TRUE),
            SE_Av_std_dev_Av_Ratio_2 = std_dev_Av_Ratio_2 / sqrt(n())
            
            )
  




names(summary_audio_ratio_all)


summary_audio_ratio_all <- as.data.frame(summary_audio_ratio_all)

summary_audio_ratio_all <- summary_audio_ratio_all %>%dplyr::select(
                audio_av ,  
                pulse_av  ,   
                std_dev_Av_Audio ,
                SE_Av_Audio , 
                std_dev_Av_Pulse ,
                SE_Av_Pulse,  
                ratio_1_mean  ,
                ratio_2_mean , 
                std_dev_Av_Ratio_1  ,    
                SE_Av_std_dev_Av_Ratio_1 , 
                std_dev_Av_Ratio_2,
                SE_Av_std_dev_Av_Ratio_2)

str(summary_audio_ratio_all)


### lets make df long
summary_audio_ratio_all_long <- summary_audio_ratio_all %>% 
  dplyr::select(audio_av ,pulse_av, ratio_1_mean, ratio_2_mean) %>% 
  pivot_longer(cols=c('audio_av', 'pulse_av',"ratio_1_mean", "ratio_2_mean"),
                      names_to='cue',
                      values_to='value')

summary_audio_ratio_all_long


summary_audio_ratio_all_long %>%
  filter(cue == "audio_av" | cue == "pulse_av" ) %>% 
  ggplot(aes(x = cue , y = value)) +
  geom_col()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    y = "Avearge cue for animals ",
    title = "Average cue values for all animals over the trial period",
    subtitle = "Note Audio and pulse counts summed per day and animal and then ratio calulated 
    then average taken of all animals over trial period")


names(summary_audio_ratio_all)

summary_audio_ratio_all_long_v2 <- summary_audio_ratio_all %>% 
  pivot_longer(cols=c('audio_av':"SE_Av_std_dev_Av_Ratio_2"),
               names_to='cue',
               values_to='value')

summary_audio_ratio_all_long_v2

DT::datatable(summary_audio_ratio_all_long_v2 ,
              rownames = FALSE,  
              options = list(columnDefs =
                               list(list(className = 'dt-center',
                                         targets = "_all")))) %>%
  formatRound(c(2), 2) #this round clm number  to 2 decimal places




#### BACK TO PER DAY PER ANIMAL

names(summary_audio_ratio)
summary_audio_ratio <- as.data.frame(summary_audio_ratio)
summary_audio_ratio <- summary_audio_ratio %>% dplyr::select("Sheep_ID",
                                                             "date",
                                                             "audio_sum",
                                                             "pulse_sum",
                                                             "ratio_sum1",
                                                             "ratio_sum2",
                                                             ) 

summary_audio_ratio

## what is the average per animals?
summary_audio_ratio_per_animal <- summary_audio_ratio %>% 
  dplyr::group_by(Sheep_ID) %>% 
  dplyr::summarise(audio_av = mean(audio_sum, na.rm = TRUE),
                   pulse_av = mean(pulse_sum, na.rm = TRUE),
                   
                   std_dev_Av_Audio = sd(audio_sum, na.rm = TRUE),
                   SE_Av_Audio = std_dev_Av_Audio / sqrt(n()),
                   
                   std_dev_Av_Pulse = sd(pulse_sum, na.rm = TRUE),
                   SE_Av_Pulse = std_dev_Av_Pulse / sqrt(n()),
                   
                   ratio_1_mean = mean(ratio_sum1, na.rm= TRUE),
                   ratio_2_mean = mean(ratio_sum2, na.rm= TRUE),
                   
                   std_dev_Av_Ratio_1 = sd(ratio_sum1, na.rm = TRUE),
                   SE_Av_std_dev_Av_Ratio_1 = std_dev_Av_Ratio_1 / sqrt(n()),
                   
                   std_dev_Av_Ratio_2 = sd(ratio_sum2, na.rm = TRUE),
                   SE_Av_std_dev_Av_Ratio_2 = std_dev_Av_Ratio_2 / sqrt(n())
                   
  )
summary_audio_ratio_per_animal


summary_audio_ratio_per_animal_long <- summary_audio_ratio_per_animal %>% 
  pivot_longer(cols=c('audio_av':"SE_Av_std_dev_Av_Ratio_2"),
               names_to='cue',
               values_to='value')
summary_audio_ratio_per_animal_long

summary_audio_ratio_per_animal_long %>%
  filter(cue == "audio_av" | cue == "pulse_av" ) %>% 
  ggplot(aes(x = cue , y = value)) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~ Sheep_ID)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    y = "Avearge cue for animals ",
    title = "Average of cue values per animal over the trial period",
    subtitle = "Note Audio and pulse counts summed per day and animal and then ratio calulated 
    then average taken per animal over trial period")






## GPS Plots

#AIM: Daily plots are probably sufficient. 




step1_2_3_sf %>%  distinct(DOY)

step1_2_3_sf <- step1_2_3_sf %>% 
  dplyr::mutate(
    Day_of_Trial = case_when(
      DOY == 290 ~ "Day 1",
      DOY == 291 ~ "Day 2",
      DOY == 292 ~ "Day 3",
      DOY == 293 ~ "Day 4",
      DOY == 294 ~ "Day 5"))
step1_2_3_sf_trial_only <- step1_2_3_sf %>% 
  filter(training_period == "non_training")
plot1 <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA, linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_trial_only ,alpha = 0.2) +
  facet_wrap(.~ Day_of_Trial)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
# labs(title = "Animal logs",
#              subtitle = "log when animals were yarded removed, and clipped to 10 meter buffer")
plot1



ggsave(plot1,
       device = "png",
       filename = paste0("plot1.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)
