### plots and summary analysis

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library("ggspatial")
library("ggsn")

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
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA,  linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black", fill = NA, linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_trial_only ,alpha = 0.2) +
  facet_wrap(.~ Day_of_Trial)+
  theme_bw()+
  
  annotation_scale(pad_x = unit(3.5, "cm"),
                   pad_y = unit(0.2, "cm"),
                   width_hint = 0.3,
                   height = unit(0.09, "cm")) +
  
  
  annotation_north_arrow( pad_x = unit(5.0, "cm"),
                          pad_y = unit(0.5, "cm"),
                          height = unit(0.5, "cm"),
                          width = unit(0.5, "cm"),
                          which_north = "true",
                          style = north_arrow_orienteering( text_size = 8))+
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

## nb I also saved using the export button on the viewer pane.



################################################################################
#### rapid movement on day 4 between midnight and 1 am

str(step1_2_3_sf_trial_only)
step1_2_3_sf_trial_only <- step1_2_3_sf_trial_only %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))
step1_2_3_sf_trial_only <- step1_2_3_sf_trial_only %>% 
  mutate(hours_format1 = case_when(
    hour ==  1 ~ "1 am", 
    hour ==  2 ~ "2 am", 
    hour ==  3 ~ "3 am", 
    hour ==  4 ~ "4 am",
    hour ==  5 ~ "5 am", 
    hour ==  6 ~ "6 am", 
    hour ==  7 ~ "7 am", 
    hour ==  8 ~ "8 am", 
    hour ==  9 ~ "9 am", 
    hour ==  10 ~ "10 am", 
    hour ==  11 ~ "11 am", 
    hour ==  12 ~ "Midday", 
    hour ==  13 ~ "1 pm", 
    hour ==  14 ~ "2 pm", 
    hour ==  15 ~ "3 pm", 
    hour ==  16 ~ "4 pm", 
    hour ==  17 ~ "5 pm", 
    hour ==  18 ~ "6 pm", 
    hour ==  19 ~ "7 pm", 
    hour ==  20 ~ "8 pm", 
    hour ==  21 ~ "9 pm", 
    hour ==  22 ~ "10 pm", 
    hour ==  23 ~ "11 pm",
    hour ==  0 ~ "Midnight" )
  )


step1_2_3_sf_trial_only$hours_format1 <-
  factor(
    step1_2_3_sf_trial_only$hours_format1,
    levels = c(
      "Midnight",
      "1 am",
      "2 am",
      "3 am",
      "4 am",
      "5 am",
      "6 am",
      "7 am",
      "8 am",
      "9 am",
      "10 am",
      "11 am",
      "Midday",
      "1 pm",
      "2 pm",
      "3 pm",
      "4 pm",
      "5 pm",
      "6 pm",
      "7 pm",
      "8 pm",
      "9 pm",
      "10 pm",
      "11 pm"
      
    )
  )



day_4_scare <- step1_2_3_sf_trial_only %>% filter( Day_of_Trial == "Day 4")
 

day_4_scare_time <- day_4_scare %>%  
  filter(local_time >= ymd_hms("2022-10-20 00:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-20 02:00:00", tz= "Australia/Adelaide"))


str(day_4_scare_time)

day_4_scare_time$minute <- as.factor(day_4_scare_time$minute)
unique(day_4_scare_time$minute)



day_4_scare_time <- day_4_scare_time %>% 
  mutate(min_format1 = case_when(
    minute ==  1 ~   "00:00", 
    minute ==  11 ~  "10:00", 
    minute ==  21 ~  "20:00", 
    minute ==  31 ~  "30:00",
    minute ==  41 ~  "40:00", 
    minute ==  51 ~  "50:00" 
     )
  )


str(day_4_scare)


day_4_scare_plot <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA, linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = day_4_scare_time ,alpha = 0.2) +
  facet_wrap( hours_format1 ~ min_format1  ,  nrow = 2)+
  
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
day_4_scare_plot


ggsave(day_4_scare_plot,
       device = "png",
       filename = paste0("day_4_scare_plot.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)


####################################################
## cues by animal
str(step1_2_3_sf)


summary_audio_ratio <- step1_2_3_sf %>% 
  dplyr::group_by(Sheep_ID, date, training_period) %>% 
  dplyr::summarise(audio = sum(Audio_values, na.rm = TRUE),
                   pulse = sum(Shock_values, na.rm = TRUE),
                   ratio_sum1 = audio/ (pulse+audio )*100,
                   ratio_sum2 = pulse/ (audio )*100)


summary_audio_ratio <- ungroup(summary_audio_ratio)
summary_audio_ratio <- as.data.frame(summary_audio_ratio)
summary_audio_ratio <- summary_audio_ratio %>% select(Sheep_ID, date,audio,  pulse, training_period)


summary_audio_pluse_long <- summary_audio_ratio %>% 
  pivot_longer(
  cols = c(audio , pulse),
  names_to = "cue",
  values_to = "value")


summary_audio_pluse_long <- summary_audio_pluse_long %>% arrange(training_period, date)
str(summary_audio_pluse_long)



summary_audio_pluse_long$cue <-
  factor(summary_audio_pluse_long$cue,
         levels = c("pulse","audio"  ))

### option 1
total_cues_per_day_animal_plot_v1 <- summary_audio_pluse_long %>%
  filter(training_period != "training") %>% 
  ggplot(aes(x = date, y = value, fill = cue)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("grey55",
                             "grey80"))+
  theme_classic() +
  facet_wrap(.~ Sheep_ID)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank())+
  
  labs(
    y = "Count of cue per day for animals ",
    title = "",
    subtitle = "")
total_cues_per_day_animal_plot_v1
### option 2

summary_audio_pluse_long$Sheep_ID <- as.character(summary_audio_pluse_long$Sheep_ID)

summary_audio_pluse_long$Sheep_ID <-
  factor(summary_audio_pluse_long$Sheep_ID,
         levels = c("1","2" , "3", "4", "5", "6", "7", "8", "9", "10" ))

summary_audio_pluse_long <- summary_audio_pluse_long %>% 
  mutate(Date_with_training = 
           case_when(
     date ==  "2022-10-17" & training_period == "training" ~ "Day 1 training",
     date ==  "2022-10-17" & training_period == "non_training" ~ "Day 1 trial",
     date ==  "2022-10-18" & training_period == "non_training" ~ "Day 2 trial",
     date ==  "2022-10-19" & training_period == "non_training" ~ "Day 3 trial",
     date ==  "2022-10-20" & training_period == "non_training" ~ "Day 4 trial",
     date ==  "2022-10-21" & training_period == "non_training" ~ "Day 5 trial",
    TRUE ~ "error"
  ))


total_cues_per_day_animal_plot_v2 <- summary_audio_pluse_long %>%
  ggplot(aes(x = Sheep_ID, y = value, fill = cue)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("grey55",
                             "grey80"))+
  theme_classic() +
  facet_wrap(.~ Date_with_training)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank())+
  
  labs(
    y = "Cue per animal per day",
    title = "",
    subtitle = "")
total_cues_per_day_animal_plot_v2



ggsave(total_cues_per_day_animal_plot_v1,
       device = "png",
       filename = paste0("total_cues_per_day_animal_plot_v1.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)
ggsave(total_cues_per_day_animal_plot_v2,
       device = "png",
       filename = paste0("total_cues_per_day_animal_plot_v2.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)

###############################################################################

summary_audio_ratio <- step1_2_3_sf %>% 
  dplyr::group_by(Sheep_ID, date, training_period) %>% 
  dplyr::summarise(audio = sum(Audio_values, na.rm = TRUE),
                   pulse = sum(Shock_values, na.rm = TRUE),
                   ratio_sum1 = audio/ (pulse+audio )*100,  na.rm = TRUE)


summary_audio_ratio <- ungroup(summary_audio_ratio)
summary_audio_ratio <- as.data.frame(summary_audio_ratio)
summary_audio_ratio <- summary_audio_ratio %>% select(Sheep_ID, date,ratio_sum1, training_period)

summary_audio_ratio$Sheep_ID <- as.character(summary_audio_ratio$Sheep_ID)

summary_audio_ratio$Sheep_ID <-
  factor(summary_audio_ratio$Sheep_ID,
         levels = c("1","2" , "3", "4", "5", "6", "7", "8", "9", "10" ))

summary_audio_ratio <- summary_audio_ratio %>% 
  mutate(Date_with_training = 
           case_when(
             date ==  "2022-10-17" & training_period == "training" ~ "Day 1 training",
             date ==  "2022-10-17" & training_period == "non_training" ~ "Day 1 trial",
             date ==  "2022-10-18" & training_period == "non_training" ~ "Day 2 trial",
             date ==  "2022-10-19" & training_period == "non_training" ~ "Day 3 trial",
             date ==  "2022-10-20" & training_period == "non_training" ~ "Day 4 trial",
             date ==  "2022-10-21" & training_period == "non_training" ~ "Day 5 trial",
             TRUE ~ "error"
           ))

str(summary_audio_ratio)
ratio_per_day_animal_plot_v2 <- summary_audio_ratio %>%
  ggplot(aes(x = Sheep_ID, y = ratio_sum1 )) +
  geom_col()+
  theme_classic() +
  facet_wrap(.~ Date_with_training)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank())+
  
  labs(
    y = "Ratio per animal per day",
    title = "Audio / (pulse+audio)*100",
    subtitle = "")

summary_audio_ratio <- summary_audio_ratio %>%  arrange(Date_with_training, Sheep_ID)

ggsave(ratio_per_day_animal_plot_v2,
       device = "png",
       filename = paste0("ratio_per_day_animal_plot_v2.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)
