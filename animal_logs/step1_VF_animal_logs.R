library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



### bring in animal logs for VF all


#path_spatial <- "W:/VF/Pinnaroo 2022/Spatial/VF/VF_display_modified/"

animal_GPS_data <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/raw_data/db_trial_csiro_lemaroo_mob_287_filtered.csv")
#animal_GPS_data <- read_csv("W:/VF/Pinnaroo 2022/animal_log/raw_data_supplied/trial_csiro_pinnaroo_mob_273_angus_heifers_filtered.csv")


#format time and date clm from character to time
animal_GPS_data <-
  animal_GPS_data %>%
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", format = "%d/%m/%Y %H:%M"))






animal_GPS_data <- animal_GPS_data %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

animal_GPS_data <- animal_GPS_data %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))

str(animal_GPS_data)

names(animal_GPS_data)

## Add a clm for ID_jaxs
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::mutate( ID_jaxs = row_number())


### what are the fences callled in this dataset?
unique(animal_GPS_data$fencesID) # we only have 2 14594 and NULL


## reorder the clms
animal_GPS_data <- animal_GPS_data %>% 
  dplyr::select(ID_jaxs,deviceUIDHex:local_time)


### ----- this will need to be updated I have end time 12:00 on the 21st

### filter data between two dates start and end of trial

animal_GPS_data <- animal_GPS_data %>%  filter(local_time >= ymd_hms("2022-10-17 08:00:00", tz= "Australia/Adelaide"), #yyy-mm-dd hh:mm:ss
                                               local_time <=  ymd_hms("2022-10-21 11:50:00", tz= "Australia/Adelaide"))

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
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
#facet_wrap(. ~ Jax_fence_ID)





names(animal_GPS_data_sf_trans)




ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
facet_wrap(. ~ date)

unique(animal_GPS_data_sf_trans$date)
GPS_data_sf_trans_17 <- animal_GPS_data_sf_trans %>% filter(date=="2022-10-17")


#### ----- Up to HERE ----- ###### Need to work out what the start time is need to create new variable with time

ggplot() +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_buffer_5, color = "black", fill = NA) +
  #geom_sf(data = water_pts_sf ,color ="Blue") +
  geom_sf(data = GPS_data_sf_trans_17 ,alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


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




