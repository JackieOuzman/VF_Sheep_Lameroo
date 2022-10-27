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

Lameroo_Vf_area <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_proj.shp")
Lameroo_Vf_area_buffer_5 <-                  st_read("W:/VF/Sheep_Lameroo_2022/spatial_boundary/VF_Buffer_proj.shp")


plot(Lameroo_Vf_area_hard_fence_bound)
plot(Lameroo_paddock_area)
plot(Lameroo_Vf_area)

Lameroo_Vf_area_hard_fence_bound <-
  st_transform(Lameroo_Vf_area_hard_fence_bound, crs = 28354)

Lameroo_Vf_area_buffer_5 <-
  st_transform(Lameroo_Vf_area_buffer_5, crs = 28354)
Lameroo_Vf_area <-
  st_transform(Lameroo_Vf_area, crs = 28354)



ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs")



ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
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
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
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
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
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
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
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
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
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
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17_Hr_13 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "Day 1 at 1pm minute as facet")+
  facet_wrap(.~ minute)

### so it look like the VF started at 13:10 prior to this the animals were training
## the data will need a new clm for training and VF trial


animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  mutate(training_period = case_when(
    local_time <= ymd_hms("2022-10-17 13:10:00", tz= "Australia/Adelaide")~ "training",
    TRUE                      ~ "non_training"
    
  ))


### filter data between two dates start and end of trial

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>%  filter(local_time >= ymd_hms("2022-10-17 10:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
                                               local_time <=  ymd_hms("2022-10-21 11:50:00", tz= "Australia/Adelaide"))

rm(GPS_data_sf_trans_17_Hr_13, GPS_data_sf_trans_17_Hr_12, GPS_data_sf_trans_17_Hr_11, GPS_data_sf_trans_17)



ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs between 17th at 10:30 and 21st at 11:50")




animal_GPS_non_training <- animal_GPS_data_sf_trans %>% 
  filter(training_period == "non_training")


ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_non_training ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs between 17th at 13:10 and 21st at 11:50",
       subtitle = "non training")


## what is the water point?
list_of_deviceName <- unique(animal_GPS_non_training$deviceName)

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_non_training ,alpha = 0.05) +
  facet_wrap(.~ deviceName)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs between 17th at 13:10 and 21st at 11:50",
       subtitle = "non training")


#------------------UP TO HERE ------------------------------------##
### I have asked Sue and Dana about the data 27/10/2022 waiting for resposne.


################################################################
###               Clip to the VF hard fences           #########
################################################################

# plot(pinnaroo_Vf_area_hard_fence_bound)
# 
# #To the large block boundary
# animal_GPS_data_sf_trans_clip <-
#   st_intersection(animal_GPS_data_sf_trans, pinnaroo_Vf_area_hard_fence_bound)
# 
# 
# ## remove all the rows that don't have fence ID
# #unique(animal_GPS_data_sf_trans_clip$fencesID)
# animal_GPS_data_sf_trans_clip <-animal_GPS_data_sf_trans_clip %>% 
#   filter(!is.na(fencesID) ) %>% 
#   filter(fencesID !=  "NULL")
# str(animal_GPS_data_sf_trans_clip)
# 
# 
# rm(animal_GPS_data, animal_GPS_data_sf, animal_GPS_data_sf_trans)


########################################################################################################



pinnaroo_Vf_area <- pinnaroo_Vf_area %>% 
  rename("Jax_fence_ID"  = "Jax_Fence_")

#Write out files "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"

output_path <- "W:/VF/Pinnaroo 2022/animal_log/jax_working_outputs"
str(pinnaroo_paddock_area)
str(pinnaroo_Vf_area)
str(animal_GPS_data_sf_trans_clip)


#write_sf(pinnaroo_paddock_area)
st_write(pinnaroo_paddock_area, 
         dsn = output_path, 
         layer = "pinnaroo_paddock_area.shp", 
         driver = "ESRI Shapefile")

st_write(pinnaroo_Vf_area, 
         dsn = output_path, 
         layer = "pinnaroo_Vf_area.shp", 
         driver = "ESRI Shapefile")


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(animal_GPS_data_sf_trans_clip))
animal_GPS_data_sf_trans_clip <- as.data.frame(animal_GPS_data_sf_trans_clip)

animal_GPS_data_sf_trans_clip <- animal_GPS_data_sf_trans_clip %>% 
  dplyr::select(-"geometry")


animal_GPS_data_sf_trans_clip <-   cbind(animal_GPS_data_sf_trans_clip,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


animal_GPS_data_sf_trans_clip$local_time <-   format(animal_GPS_data_sf_trans_clip$local_time, usetz=TRUE)
animal_GPS_data_sf_trans_clip$GMT        <-   format(animal_GPS_data_sf_trans_clip$GMT, usetz=TRUE)
animal_GPS_data_sf_trans_clip$start_fence <-  format(animal_GPS_data_sf_trans_clip$start_fence, usetz=TRUE)
animal_GPS_data_sf_trans_clip$end_fence    <- format(animal_GPS_data_sf_trans_clip$end_fence, usetz=TRUE)
animal_GPS_data_sf_trans_clip$start_trial    <- format(animal_GPS_data_sf_trans_clip$start_trial, usetz=TRUE)

write.csv(animal_GPS_data_sf_trans_clip, 
          paste0(output_path,"/animal_GPS_data_sf_trans_clip.csv"), 
          row.names=FALSE)




