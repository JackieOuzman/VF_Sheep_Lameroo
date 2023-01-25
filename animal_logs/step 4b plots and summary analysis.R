### run step 4 plots and summary first so you have all the files in your environment

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)


library(rgdal)
library(sf)

#install.packages("plotKML")
library(plotKML)
library(knitr)
library(png)

library(readxl)


library(ggmap)
library(maps)
library(mapdata)

library(raster)
library(agricolae)
library(FSA) 
library(agricolae)
library(multcomp)
library(lsmeans)
#library(hms) Add these before I use them?
#library(plyr)

library(readxl)
library(tidyverse)
library(dplyr)
library(FSA) 
library(agricolae)
library(multcomp)
library(multcomp)
library(lsmeans)
library(multcompView)
library(Rmisc)

library(ggplot2)
library(car)
library(DescTools)



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




step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                           coords = c("X", "Y"),
                           crs = 28354,
                           agr = "constant")

### comments from Caroline 29/11/2022

step1_2_3_sf_day1 <- step1_2_3_sf %>% filter(date=="2022-10-17")

step1_2_3_sf_day1 <- step1_2_3_sf_day1 %>% 
  mutate( hour = hour(local_time),
     minute = minute(local_time))

step1_2_3_sf_day1 %>%  distinct(hour)

step1_2_3_sf_day1 <- step1_2_3_sf_day1 %>% 
  mutate(hours_format1 = case_when(
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
    hour ==  23 ~ "11 pm" )
  )


step1_2_3_sf_day1$hours_format1 <- factor(step1_2_3_sf_day1$hours_format1,
                                            levels = c("11 am", 
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
                                                       ))

str(step1_2_3_sf_day1)

step1_2_3_sf_day1$training_period <- factor(step1_2_3_sf_day1$training_period,
                                            levels = c("training", "non_training")) 




step1_2_3_sf_day1_training <-step1_2_3_sf_day1 %>%  filter(training_period == "training")
step1_2_3_sf_day1_Non_training <-step1_2_3_sf_day1 %>%  filter(training_period == "non_training")

day1 <- ggplot() +
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA, linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.5, fill = NA) +
  
  
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = step1_2_3_sf_day1 ,alpha = 0.08) +
  
  geom_sf(data = step1_2_3_sf_day1_training ,alpha = 0.09, color = "red") +
  geom_sf(data = step1_2_3_sf_day1_Non_training ,alpha = 0.09, color = "black") +
  
  theme_bw()+
  annotation_scale(pad_x = unit(1.8, "cm"),
                   pad_y = unit(0.2, "cm"),
                   width_hint = 0.3,
                   height = unit(0.09, "cm")) +
  
  
  annotation_north_arrow( pad_x = unit(2.8, "cm"),
                          pad_y = unit(0.5, "cm"),
                          height = unit(0.5, "cm"),
                          width = unit(0.5, "cm"),
                          which_north = "true",
                          style = north_arrow_orienteering( text_size = 8))+
  facet_wrap(. ~ hours_format1, nrow=2)+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
#labs(title = "Animal logs on first day")
day1


ggsave(day1,
       device = "png",
       filename = paste0("Lameroo_sheep_day1_diff_colour_buff_bound_no_title.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)
       
       
### DAY 4 only - hours as a facet wrap

step1_2_3_sf_day4 <- step1_2_3_sf %>% filter(date=="2022-10-20")

step1_2_3_sf_day4 <- step1_2_3_sf_day4 %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))

step1_2_3_sf_day4 %>%  distinct(hour)
test<- as.data.frame(step1_2_3_sf_day4) %>% 
  dplyr::select(local_time, date ,training_period, hour,  hours_format1)
str(test)


step1_2_3_sf_day4 <- step1_2_3_sf_day4 %>% 
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


step1_2_3_sf_day4$hours_format1 <-
  factor(
    step1_2_3_sf_day4$hours_format1,
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




day4 <-ggplot() +
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA,  linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.8, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_day4 ,alpha = 0.08) +
  theme_bw()+
  annotation_scale(pad_x = unit(1.3, "cm"),
                   pad_y = unit(0.2, "cm"),
                   width_hint = 0.3,
                   height = unit(0.09, "cm")) +
  
  
  annotation_north_arrow( pad_x = unit(2.4, "cm"),
                          pad_y = unit(0.5, "cm"),
                          height = unit(0.5, "cm"),
                          width = unit(0.5, "cm"),
                          which_north = "true",
                          style = north_arrow_orienteering( text_size = 8))+
  facet_wrap(.~ hours_format1,  nrow = 3)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
  #labs(title = "Animal logs on 4th day")

day4

ggsave(day4,
       device = "png",
       filename = paste0("Lameroo_sheep_day4_no_title_buff_bound.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)
       
  



###########################################################################



## Cue data again - 



step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                           coords = c("X", "Y"),
                           crs = 28354,
                           agr = "constant")

# step 1 summaries audio and pulse per animal per day also training period 
summary_audio_ratio <- step1_2_3_sf %>% 
  dplyr::group_by(Sheep_ID, date, training_period) %>% 
  dplyr::summarise(audio_sum = sum(Audio_values, na.rm = TRUE),
                   pulse_sum = sum(Shock_values, na.rm = TRUE),
                   ratio_sum1 = audio_sum/ (pulse_sum+audio_sum )*100,
                   ratio_sum2 = pulse_sum/ (audio_sum )*100)



summary_audio_ratio$ratio_sum1 [is.nan(summary_audio_ratio$ratio_sum1 )]<-NA
summary_audio_ratio$ratio_sum2 [is.nan(summary_audio_ratio$ratio_sum2 )]<-NA

names(summary_audio_ratio)


summary_audio_ratio <- ungroup(summary_audio_ratio)

summary_audio_ratio
# step 2 summaries audio and pulse per animal per day also training period 

summary_audio_ratio_from_training_date <- summary_audio_ratio %>% 
  dplyr::group_by(training_period, date) %>% 
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



summary_audio_ratio_from_training_date$training_period <- factor(summary_audio_ratio_from_training_date$training_period,
                                                            levels = c("training", "non_training"))


summary_audio_ratio_from_training_date <- as.data.frame(summary_audio_ratio_from_training_date)

str(summary_audio_ratio_from_training_date)

summary_audio_ratio_from_training_date <-
  summary_audio_ratio_from_training_date %>% dplyr::select(
    training_period ,
    date,
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
    SE_Av_std_dev_Av_Ratio_2
  )



table_per_day_cues_pulses <-summary_audio_ratio_from_training_date %>% dplyr::select(
  date,
  training_period ,
  audio_av ,
  std_dev_Av_Audio ,
  pulse_av  ,
  std_dev_Av_Pulse 
  ) %>% 
  arrange(date,training_period )

table_per_day_cues_pulses$std_dev_Av_Audio <- round(table_per_day_cues_pulses$std_dev_Av_Audio,2)
table_per_day_cues_pulses$std_dev_Av_Pulse <- round(table_per_day_cues_pulses$std_dev_Av_Pulse,2)  

write.csv(table_per_day_cues_pulses, "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/table_per_day_cues_pulses.csv",row.names = FALSE)



       