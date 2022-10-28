library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all

animal_GPS_data <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/raw_data/db_trial_csiro_lemaroo_mob_287_filtered.csv")
#format time and date clm from character to time
animal_GPS_data <-
  animal_GPS_data %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))


animal_GPS_data <- animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

animal_GPS_data <- animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))

## Add a clm for ID_jaxs
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())


### what are the fences callled in this dataset?
unique(animal_GPS_data$fencesID) # we only have 2 14594 and NULL


## reorder the clms
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::select(ID_jaxs,deviceUIDHex:local_time)



animal_GPS_data <- animal_GPS_data %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


#############################################################################################
####    Assign collar to sheep names #####
unique(animal_GPS_data$deviceName)

animal_GPS_data <- animal_GPS_data %>% 
  mutate(Sheep_ID = case_when(
    deviceName == 1390743 ~ "10",#was called blank
    deviceName == 1390581 ~ "1",
    deviceName == 1390826 ~ "2", #before 18/10 I assume at 10:30
    deviceName == 1391505 ~ "2", #after 18/10 I assume at 10:30
    deviceName == 0490705 ~ "3",
    deviceName == 1390577 ~ "4",
    deviceName == 1390737 ~ "5",
    deviceName == 1390749 ~ "6",
    deviceName == 1390456 ~ "7",
    deviceName == 1390182 ~ "8",
    deviceName == 1390736 ~ "9",
    deviceName == 1390189 ~ "water_pt",
    TRUE                      ~ "other"
    
  ))

#remove sheep 2 records
animal_GPS_data_no2 <- animal_GPS_data %>% 
  filter(Sheep_ID != "2" )

#subset data keep only sheep 2 records
animal_2 <- animal_GPS_data %>% 
  filter(Sheep_ID == "2" ) 

#filter out time sheep 2 used each device
animal_1390826 <- animal_2 %>% 
  filter( deviceName == "1390826" ) %>% 
  filter(local_time <=  ymd_hms("2022-10-18 10:30:00", tz= "Australia/Adelaide")) 

animal_1391505 <- animal_2 %>% 
  filter( deviceName == "1391505" ) %>% 
  filter(local_time >=  ymd_hms("2022-10-18 10:30:00", tz= "Australia/Adelaide")) 


animal_2 <- rbind(animal_1390826,animal_1391505 )

animal_GPS_data <- rbind (animal_GPS_data_no2 , animal_2 )
rm(animal_2, animal_1390826,animal_1391505, animal_GPS_data_no2 )

############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
#str(animal_GPS_data)

## remove null values in coodinates
animal_GPS_data <- animal_GPS_data %>% 
  filter(!is.na(gpsData.lng))

#turn into spatial data
animal_GPS_data_sf <-
  st_as_sf(animal_GPS_data,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

animal_GPS_data_sf_trans <-
  st_transform(animal_GPS_data_sf, crs = 28354)


rm(animal_GPS_data,animal_GPS_data_sf )





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


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs with a buffer of 10m")



ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
facet_wrap(. ~ date)+
  labs(title = "all animal logs, dates as facet")


###### Need to work out what the start time 
#Rick said there was a time the animals moved back in the VF area after they were in the south of the paddock and this is the satrt time

GPS_data_sf_trans_17 <- animal_GPS_data_sf_trans %>% filter(date=="2022-10-17")

GPS_data_sf_trans_17 <- GPS_data_sf_trans_17 %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  facet_wrap(.~ hour)+
  labs(title = "Day 1, hours as facet")

#so 11am to 14 looks like the window make 3 df for each of these hrs

GPS_data_sf_trans_17_Hr_11 <- GPS_data_sf_trans_17 %>% 
  filter(local_time >= ymd_hms("2022-10-17 11:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-17 12:00:00", tz= "Australia/Adelaide"))

GPS_data_sf_trans_17_Hr_12 <- GPS_data_sf_trans_17 %>% 
  filter(local_time >= ymd_hms("2022-10-17 12:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-17 13:00:00", tz= "Australia/Adelaide")) 

GPS_data_sf_trans_17_Hr_13 <- GPS_data_sf_trans_17 %>% 
  filter(local_time >= ymd_hms("2022-10-17 13:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
         local_time <=  ymd_hms("2022-10-17 14:00:00", tz= "Australia/Adelaide"))  

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_11 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 11am minute as facet")+
  facet_wrap(.~ minute)
  
ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_12 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 12am minute as facet")+
  facet_wrap(.~ minute)

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_12 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 12am minute as facet")+
  facet_wrap(.~ minute)


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_13 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 1pm minute as facet")+
  facet_wrap(.~ minute)

### so it look like the VF started at 13:10 prior to this the animals were training
## the data will need a new clm for training and VF trial

rm(GPS_data_sf_trans_17, GPS_data_sf_trans_17_Hr_11, GPS_data_sf_trans_17_Hr_12, GPS_data_sf_trans_17_Hr_13)





################################################################################
#### filtering out data based on times ####

# start of trial and training period (according to sue) - keep everything after  17th 11:35

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
  local_time >=  ymd_hms("2022-10-17 11:35:00", tz= "Australia/Adelaide"))



### start of training period 

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  mutate(training_period = case_when(
    local_time <= ymd_hms("2022-10-17 13:10:00", tz= "Australia/Adelaide")~ "training",
    TRUE                      ~ "non_training"
    
  ))


### filter data between two dates start and end of trial

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>%
  filter(
    local_time <=  ymd_hms("2022-10-21 11:50:00", tz = "Australia/Adelaide")
  )


#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_17 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-17")
day_18 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-18")
day_19 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-19")
day_20 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-20")
day_21 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-10-21")

# keep everything after before yarding and after yarding

day_18_before_yarding <- day_18 %>%
  filter(local_time <=  ymd_hms("2022-10-18 09:40:00", tz = "Australia/Adelaide"))
day_18_after_yarding <- day_18 %>%
  filter(local_time >=  ymd_hms("2022-10-18 10:30:00", tz = "Australia/Adelaide"))
                  
day_18_clean <- rbind(day_18_before_yarding, day_18_after_yarding)
rm(day_18_before_yarding, day_18_after_yarding, day_18)


day_19_before_yarding <- day_19 %>%
  filter(local_time <=  ymd_hms("2022-10-19 09:10:00", tz = "Australia/Adelaide"))
day_19_after_yarding <- day_19 %>%
  filter(local_time >=  ymd_hms("2022-10-19 10:18:00", tz = "Australia/Adelaide"))

day_19_clean <- rbind(day_19_before_yarding, day_19_after_yarding)
rm(day_19_before_yarding, day_19_after_yarding, day_19)



day_20_before_yarding <- day_20 %>%
  filter(local_time <=  ymd_hms("2022-10-20 08:58:00", tz = "Australia/Adelaide"))
day_20_after_yarding <- day_20 %>%
  filter(local_time >=  ymd_hms("2022-10-20 10:19:00", tz = "Australia/Adelaide"))

day_20_clean <- rbind(day_20_before_yarding, day_20_after_yarding)
rm(day_20_before_yarding, day_20_after_yarding, day_20)


### put it back togther 

animals_GPS_trim_time <- rbind(day_17, day_18_clean, day_19_clean, day_19_clean, day_20_clean, day_21)

rm(day_17, day_18_clean, day_19_clean, day_19_clean, day_21, day_20_clean, animal_GPS_data_sf_trans)

########################################################################################





### remove the water and other animals logs

unique(animals_GPS_trim_time_non_train$Sheep_ID)

animals_GPS_trim_time <- animals_GPS_trim_time %>% 
  filter(Sheep_ID !=  "other") %>% 
  filter(Sheep_ID !=  "water_pt")


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_10, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = animals_GPS_trim_time ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "All animal logs between 17th at 10:30 and 21st at 11:50",
  subtitle = "log when animals were yarded removed")









################################################################
### Clip to the VF hard fences  with 10 meter buffer   #########
################################################################


#To the large block boundary
animals_GPS_trim_time_clip <-
  st_intersection(animals_GPS_trim_time, Lameroo_Vf_area_hard_fence_bound_buff)


## remove all the rows that don't have fence ID
#unique(animal_GPS_data_sf_trans_clip$fencesID)
animals_GPS_trim_time_clip <-animals_GPS_trim_time_clip %>%
  filter(!is.na(fencesID) ) %>%
  filter(fencesID !=  "NULL")



########################################################################################################




#Write out files "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"

output_path <- "W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working"


animals_log <- animals_GPS_trim_time_clip
animals_log_training <- animals_log %>% filter(training_period == "training")
animals_log_NON_training <- animals_log %>% filter(training_period != "training") 

rm(animals_GPS_trim_time, animals_GPS_trim_time_clip)
############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(animals_log_NON_training))
animals_log_NON_training <- as.data.frame(animals_log_NON_training)

animals_log_NON_training <- animals_log_NON_training %>% 
  dplyr::select(-"geometry")


animals_log <-   cbind(animals_log_NON_training,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


animals_log$local_time <-   format(animals_log$local_time, usetz=TRUE)
animals_log$GMT        <-   format(animals_log$GMT, usetz=TRUE)
animals_log$start_fence <-  format(animals_log$start_fence, usetz=TRUE)
animals_log$end_fence    <- format(animals_log$end_fence, usetz=TRUE)
animals_log$start_trial    <- format(animals_log$start_trial, usetz=TRUE)

write.csv(animals_log, 
          paste0(output_path,"/animals_log_non_training.csv"), 
          row.names=FALSE)
#############################################################

coordinates <-as.data.frame( st_coordinates(animals_log_training))
animals_log_training <- as.data.frame(animals_log_training)

animals_log_training <- animals_log_training %>% 
  dplyr::select(-"geometry")


animals_log <-   cbind(animals_log_training,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


animals_log$local_time <-   format(animals_log$local_time, usetz=TRUE)
animals_log$GMT        <-   format(animals_log$GMT, usetz=TRUE)
animals_log$start_fence <-  format(animals_log$start_fence, usetz=TRUE)
animals_log$end_fence    <- format(animals_log$end_fence, usetz=TRUE)
animals_log$start_trial    <- format(animals_log$start_trial, usetz=TRUE)

write.csv(animals_log, 
          paste0(output_path,"/animals_log_training.csv"), 
          row.names=FALSE)

