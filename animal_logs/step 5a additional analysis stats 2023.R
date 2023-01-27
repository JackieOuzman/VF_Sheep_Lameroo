### the team now want the day 4 logs split into two groups before and after spooking

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

#


step1_2_3 <- step1_2_3 %>% 
  dplyr::mutate(spooking = 
                  case_when(
                    
                    local_time > "2022-10-20 01:00:00" & local_time < "2022-10-20 09:00:00" ~ "spooking",
                    TRUE                      ~ "regular"
                  ))


step1_2_3
str(step1_2_3)


###############################################################################

summary_audio_ratio_regular_only <- step1_2_3 %>% 
  dplyr::filter(spooking  == "regular") %>%           
  dplyr::group_by(Sheep_ID, date, training_period) %>% 
  dplyr::summarise(audio_sum = sum(Audio_values, na.rm = TRUE),
                   pulse_sum = sum(Shock_values, na.rm = TRUE),
                   ratio_sum_D = ((audio_sum- pulse_sum)/audio_sum)*100)



summary_audio_ratio_regular_only


##############################################################################################
### t.test for for sheep 1?
# The data analysis should include more statistics 
# e.g. significant differences between individuals and between training and non-training period. 
# I would also like to see some results showing the development in number of audio cues and electric pulses through time.

unique(summary_audio_ratio_regular_only$Sheep_ID)
str(summary_audio_ratio_regular_only)
summary_audio_ratio_regular_only <- ungroup(summary_audio_ratio_regular_only)

sheep_1 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "1") %>% 
  dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
str(sheep_1)

sheep_1_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_1, var.equal = TRUE)
sheep_1_t.test
sheep_1_t.test$p.value
sheep_1_t.test$estimate[1]
sheep_1_t.test$estimate[2]


sheep_2 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "2") %>% 
  dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )

sheep_2_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_2, var.equal = TRUE)
sheep_2_t.test


sheep_3 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "3") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_3_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_3, var.equal = TRUE)
sheep_3_t.test

sheep_4 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "4") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_4_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_4, var.equal = TRUE)
sheep_4_t.test

sheep_5 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "5") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_5_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_5, var.equal = TRUE)
sheep_5_t.test

sheep_6 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "6") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_6_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_6, var.equal = TRUE)

sheep_7 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "7") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_7_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_7, var.equal = TRUE)

sheep_8 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "8") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_8_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_8, var.equal = TRUE)

sheep_9 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "9") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_9_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_9, var.equal = TRUE)

sheep_10 <- summary_audio_ratio_regular_only %>%  filter(Sheep_ID == "10") %>%   dplyr::select( Sheep_ID , ratio_sum_D ,training_period, date )
sheep_10_t.test <- t.test(ratio_sum_D     ~ training_period, data = sheep_10, var.equal = TRUE)
sheep_10_t.test

df_results_t.test <- data.frame(
  sheep_ID=c(1,2,3,4,5,6,7,8,9,10),
  p.value=c(sheep_1_t.test$p.value, 
            sheep_2_t.test$p.value,
            sheep_3_t.test$p.value,
            sheep_4_t.test$p.value,
            sheep_5_t.test$p.value,
            sheep_6_t.test$p.value,
            sheep_7_t.test$p.value,
            sheep_8_t.test$p.value,
            sheep_9_t.test$p.value,
            sheep_10_t.test$p.value),


  ratio_non_training=c(sheep_1_t.test$estimate[1],
                       sheep_2_t.test$estimate[1],
                       sheep_3_t.test$estimate[1],
                       sheep_4_t.test$estimate[1],
                       sheep_5_t.test$estimate[1],
                       sheep_6_t.test$estimate[1],
                       sheep_7_t.test$estimate[1],
                       sheep_8_t.test$estimate[1],
                       sheep_9_t.test$estimate[1],
                       sheep_10_t.test$estimate[1]),
  

  ratio_training=c(sheep_1_t.test$estimate[2],
                   sheep_2_t.test$estimate[2],
                   sheep_3_t.test$estimate[2],
                   sheep_4_t.test$estimate[2],
                   sheep_5_t.test$estimate[2],
                   sheep_6_t.test$estimate[2],
                   sheep_7_t.test$estimate[2],
                   sheep_8_t.test$estimate[2],
                   sheep_9_t.test$estimate[2],
                   sheep_10_t.test$estimate[2]
                   )
)

df_results_t.test$p.value <- round(df_results_t.test$p.value, 4)
df_results_t.test$ratio_non_training  <- round(df_results_t.test$ratio_non_training , 2)
df_results_t.test$ratio_training <- round(df_results_t.test$ratio_training, 2)


df_results_t.test
write.csv(df_results_t.test, "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/Two_Sample_t_test_training_vs_trial_sheep.csv")

##############################################################################################
### t.test for for day 1?
# The data analysis should include more statistics 
# e.g. significant differences between individuals and between training and non-training period. 
# I would also like to see some results showing the development in number of audio cues and electric pulses through time.

day_1 <- step1_2_3 %>%  filter(date == "2022-10-17") %>% 
  dplyr::select( Sheep_ID , Audio_values,Shock_values, training_period, spooking, date )
str(day_1)

day_1_t.test <- t.test(Audio_values ~ training_period, data = day_1, var.equal = TRUE)
day_1_t.test


##############################################################################################
### t.test for for sheep 1?
# The data analysis should include more statistics 
# e.g. significant differences between individuals and between training and non-training period. 
# I would also like to see some results showing the development in number of audio cues and electric pulses through time.

unique(step1_2_3$Sheep_ID)

sheep_1 <- step1_2_3 %>%  filter(Sheep_ID == "1") %>% 
  dplyr::select( Sheep_ID , Audio_values,Shock_values, training_period, spooking, date )
str(sheep_1)

sheep_1_t.test <- t.test(Audio_values ~ training_period, data = sheep_1, var.equal = TRUE)
sheep_1_t.test
sheep_1_t.test$p.value
sheep_1_t.test$estimate
