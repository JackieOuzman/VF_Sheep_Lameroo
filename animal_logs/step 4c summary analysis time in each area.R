

### Summary analysis how much time in VF and non VF parts of the paddock

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


step1_2_3 <- read_csv("W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                           coords = c("X", "Y"),
                           crs = 28355,
                           agr = "constant")


str(step1_2_3)
total_time <- step1_2_3 %>% 
  dplyr::select(local_time,
                date,
                DOY,
                Sheep_ID)
str(total_time)
total_time_trial <- total_time 

rm(step1_2_3, step1_2_3_sf)

#### ---------------- what is the max and min time for the whole trial ----------------#### 

start <- min(total_time_trial$local_time, na.rm = TRUE) #"2022-06-28 09:56:30 AEST"
end <-   max(total_time_trial$local_time, na.rm = TRUE) # "2022-07-02 10:06:32 AEST"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "346202s (~4.01 days)"




#### ---------------- how many seconds were the animals in the yard not part of the trial? ----------------#### 
# Times sheep were brought in each day for the VF Chiswick trial;
# 28/6- sheep out 9:50 This is the start of the trail no need to do anything with this
# 29/6 11:21- 12:21
# 30/6 10:34- 11:36
# 1/7- 10:37- 11:20
# 2/7- Brought in at 10:10
#### each day the animals were yarded so i need to remove this data


yard_start_29 <- ymd_hms("2022-06-29 11:21:00", tz = "Australia/Sydney")
yard_end_29 <-   ymd_hms("2022-06-29 12:21:00", tz = "Australia/Sydney")

yarded_interval_29 <- yard_start_29 %--% yard_end_29
yarded_duration_29 <- as.duration(yarded_interval_29)
yarded_duration_29 #"3600s (~1 hours)"


yard_start_30 <- ymd_hms("2022-06-30 10:34:00", tz = "Australia/Sydney")
yard_end_30 <-   ymd_hms("2022-06-30 11:36:00", tz = "Australia/Sydney")

yarded_interval_30 <- yard_start_30 %--% yard_end_30
yarded_duration_30 <- as.duration(yarded_interval_30)
yarded_duration_30 #"3720s (~1.03 hours)"


yard_start_01 <- ymd_hms("2022-07-01 10:37:00", tz = "Australia/Sydney")
yard_end_01 <-   ymd_hms("2022-07-01 11:20:00", tz = "Australia/Sydney")

yarded_interval_01 <- yard_start_01 %--% yard_end_01
yarded_duration_01 <- as.duration(yarded_interval_01)
yarded_duration_01 #"2580s (~43 minutes)"

rm(yard_start_29, yard_end_29, yarded_interval_29,
   yard_start_30, yard_end_30, yarded_interval_30,
   yard_start_01, yard_end_01, yarded_interval_01)

### sum of yarded time 

sum_yard_time <- sum(yarded_duration_29, yarded_duration_30, yarded_duration_01)
sum_yard_time_duration <- as.duration(sum_yard_time)
sum_yard_time_duration #"9900s (~2.75 hours)"


#### ---------------- remove the yarded time from the lenght of the trail  ----------------#### 
time.duration
sum_yard_time_duration

lenght_of_trail <- time.duration - sum_yard_time_duration
lenght_of_trail #"336302s (~3.89 days)"
#### don't forget to times this by all the animals
lenght_of_trail <- lenght_of_trail*6

lenght_of_trail #"2017812s (~3.34 weeks)"

##
rm(total_time, total_time_trial)
rm(end, start, time.duration, time.interval, 
   yarded_duration_01, yarded_duration_29, yarded_duration_30)










#### ---------------- for each sheep how much time did they spend outside the VF  ----------------#### 
### maybe I can make a new variable in exclusion zone.




############################################################################################
############                  bring in boundaries             ##############################
############################################################################################




Chiswick_hard_fence_bound <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final.shp")  # this is the hard fences

Chiswick_hard_fence_bound <-
  st_transform(Chiswick_hard_fence_bound, crs = 28355)


Chiswick_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final_buff10.shp")  # this is the 

Chiswick_hard_fence_bound_buff <-
  st_transform(Chiswick_hard_fence_bound_buff, crs = 28355)


VF_paddock <-   st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/VF_paddock.shp")

VF_paddock <-  st_transform(VF_paddock, crs = 28355)

water_pt <-  st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/water_pt.shp")

Exclusion_zone  <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Exclusion_zone.shp")
Exclusion_zone <-  st_transform(Exclusion_zone, crs = 28355)




################################################################
### Clip to the Exclusion_zone  #########
################################################################
step1_2_3 <- read_csv("W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")



step1_2_3_trial <- step1_2_3 %>% 
  mutate( hour = hour(local_time),
          minute = minute(local_time))

str(step1_2_3_trial)


step1_2_3_trial <- step1_2_3_trial %>%  dplyr::select(Sheep_ID, date, local_time,hour,  minute, X, Y )
#turn into spatial data
step1_2_3_trial_sf <-   st_as_sf(step1_2_3_trial,
                         coords = c("X", "Y"),
                         crs = 28355,
                         agr = "constant")



#To the VF area
step1_2_3_trial_sf_clip_Excl <-
  st_intersection(step1_2_3_trial_sf, Exclusion_zone)

str(step1_2_3_trial_sf_clip_Excl)

plotVF <- ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA,linetype = "dashed", size = 0.5) +
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_trial_sf_clip_Excl ,alpha = 0.2) +
  facet_wrap(.~ date                )+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())#+

plotVF


### ----- when did an animal cross over to the exlusion zone and how long did it last?
step1_2_3_trial_sf_clip_Excl %>%  distinct(date)
str(step1_2_3_trial_sf_clip_Excl)

### ----- I can assume every unique time entry is about 10 min block of time----#####


count_entries_per_sheep <- step1_2_3_trial_sf_clip_Excl %>%  
  group_by(Sheep_ID, date) %>% 
  distinct(local_time, .keep_all = TRUE ) %>% 
  tally()
  
count_entries_per_sheep <- count_entries_per_sheep %>% 
  mutate(Time_in_exclusion_zone_19 = 10*n, #logged every 10mins
         D_Time_in_exclusion_zone_19 = duration(num = Time_in_exclusion_zone_19, units = "minutes"))

count_entries_per_sheep <- ungroup(count_entries_per_sheep)
count_entries_per_sheep <- count_entries_per_sheep %>%  
  dplyr::select(Sheep_ID, date, n, Time_in_exclusion_zone_19, D_Time_in_exclusion_zone_19)  
count_entries_per_sheep
##############################################################
sum_clm <- count_entries_per_sheep %>% summarize_if(is.numeric, sum, na.rm=TRUE)
sum_clm



lenght_of_trail
exclusion <- sum_clm$D_Time_in_exclusion_zone_19
exclusion #3600

(3600/2017812)*100
