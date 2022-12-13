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


step1_2_3_sf$local_time <- ymd_hms(step1_2_3_sf$local_time)


str(step1_2_3_sf)



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

step1_2_3_sf_trial_only <- step1_2_3_sf_trial_only %>% 
  mutate( hour = hour(local_time),
          minute = minute(local_time))


################################################################################
## Rick day night plots
str(step1_2_3_sf_trial_only)

#use local time to make day night clm
#d %>%  filter(between(as_date(Date), as_date("2019-11-02"), as_date("2019-11-02")))

step1_2_3_sf_trial_only <- step1_2_3_sf_trial_only  %>% 
  dplyr::mutate(
    day_night = case_when(
      
      ymd_hms(local_time) > ymd_hms("2022-10-17 09:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-17 19:30:00")      
      ~ "Day 1",
      
      ymd_hms(local_time) > ymd_hms("2022-10-18 09:30:00") &
        ymd_hms(local_time) < ymd_hms("2022-10-18 19:30:00")
      ~ "Day",

      ymd_hms(local_time) > ymd_hms("2022-10-19 09:30:00") &
        ymd_hms(local_time) < ymd_hms("2022-10-19 19:30:00")
      ~ "Day",


      ymd_hms(local_time) > ymd_hms("2022-10-20 09:30:00") &
        ymd_hms(local_time) < ymd_hms("2022-10-20 19:30:00")
      ~ "Day",

      ymd_hms(local_time) > ymd_hms("2022-10-21 09:30:00") &
        ymd_hms(local_time) < ymd_hms("2022-10-21 19:30:00")
      ~ "Day",

      TRUE                      ~ "Night"
      
    ))

str(step1_2_3_sf_trial_only)




day_night_plot <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA, linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_trial_only ,alpha = 0.2) +
  facet_wrap(Day_of_Trial~ day_night,  nrow = 2)+
  
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
# labs(title = "Animal logs",
#              subtitle = "log when animals were yarded removed, and clipped to 10 meter buffer")
day_night_plot


# ggsave(day_night_plot,
#        device = "png",
#        filename = paste0("day_night_plot_DAY_0930_to_1930.png"),
#        path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
#        width=8.62,
#        height = 6.28,
#        dpi=600
# )



##################################################################################
### check of the plots they are confusing.
#################################################################################
check_min_max_time_day1_2 <- step1_2_3_sf_trial_only %>% filter(date=="2022-10-17" | date =="2022-10-18")


step1_2_3_sf_trial_only <- step1_2_3_sf_trial_only  %>% 
  dplyr::mutate(
    day_night_detail = case_when(
      
      ymd_hms(local_time) > ymd_hms("2022-10-17 09:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-17 19:30:00")      
      ~ "Day 1",
      
      ymd_hms(local_time) > ymd_hms("2022-10-17 19:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-17 23:55:00")      
      ~ "Night 1 19:30 to midnight",
      
      ymd_hms(local_time) > ymd_hms("2022-10-17 00:00:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-17 09:30:00")      
      ~ "Night 1 midnight to 09:30",
      
      
      
      
      ymd_hms(local_time) > ymd_hms("2022-10-18 09:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-18 19:30:00")      
      ~ "Day 2",
      
      ymd_hms(local_time) > ymd_hms("2022-10-18 19:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-18 23:55:00")      
      ~ "Night 2 19:30 to midnight",
      
      ymd_hms(local_time) > ymd_hms("2022-10-18 00:00:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-18 09:30:00")      
      ~ "Night 2 midnight to 09:30",
      
      
      
      
      ymd_hms(local_time) > ymd_hms("2022-10-19 09:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-19 19:30:00")      
      ~ "Day 3",
      
      ymd_hms(local_time) > ymd_hms("2022-10-19 19:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-19 23:55:00")      
      ~ "Night 3 19:30 to midnight",
      
      ymd_hms(local_time) > ymd_hms("2022-10-19 00:00:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-19 09:30:00")      
      ~ "Night 3 midnight to 09:30",
      
      
      
      ymd_hms(local_time) > ymd_hms("2022-10-20 09:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-20 19:30:00")      
      ~ "Day 4",
      
      ymd_hms(local_time) > ymd_hms("2022-10-20 19:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-20 23:55:00")      
      ~ "Night 4 19:30 to midnight",
      
      ymd_hms(local_time) > ymd_hms("2022-10-20 00:00:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-20 09:30:00")      
      ~ "Night 4 midnight to 09:30",
      
      
     
      
       ymd_hms(local_time) > ymd_hms("2022-10-21 09:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-21 19:30:00")      
      ~ "Day 5",
      
      ymd_hms(local_time) > ymd_hms("2022-10-21 19:30:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-21 23:55:00")      
      ~ "Night 5 19:30 to midnight",
      
      ymd_hms(local_time) > ymd_hms("2022-10-21 00:00:00") & 
        ymd_hms(local_time) < ymd_hms("2022-10-21 09:30:00")      
      ~ "Night 5 midnight to 09:30",
      
       
       TRUE                      ~ "other"
      
    ))



unique(step1_2_3_sf_trial_only$day_night_detail)

step1_2_3_sf_trial_only$day_night_detail <- as.factor()


step1_2_3_sf_trial_only$day_night_detail <- factor(
  step1_2_3_sf_trial_only$day_night_detail,
  levels = c(
"Day 1",
"Night 1 19:30 to midnight",

"Night 2 midnight to 09:30",
"Day 2",
"Night 2 19:30 to midnight",

"Night 3 midnight to 09:30",
"Day 3",
"Night 3 19:30 to midnight" ,

"Night 4 midnight to 09:30",
"Day 4",                    
"Night 4 19:30 to midnight",

"Night 5 midnight to 09:30" ,
 "Day 5"))                    


Day_night_plots_explained <- ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA, linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_trial_only ,alpha = 0.2) +
  
  facet_wrap(. ~day_night_detail,
             labeller = labeller(day_night_detail = label_wrap_gen(width = 10)),  nrow = 2)+
  
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+
Day_night_plots_explained

ggsave(Day_night_plots_explained,
       device = "png",
       filename = paste0("Day_night_plots_explained_0930_to_1930.png"),
       path= "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
       width=8.62,
       height = 6.28,
       dpi=600
)


