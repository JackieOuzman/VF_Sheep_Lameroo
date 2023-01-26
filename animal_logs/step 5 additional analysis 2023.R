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

check
################################################################################
#### work out when the spooking occurred #######################################
################################################################################

### DAY 4 only - hours as a facet wrap

step1_2_3_sf_day4 <- step1_2_3_sf %>% filter(date=="2022-10-20")

step1_2_3_sf_day4 <- step1_2_3_sf_day4 %>% 
  mutate(hour = hour(local_time),
         minute = minute(local_time))

step1_2_3_sf_day4 %>%  distinct(hour)


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


### the animals moved over the VF at about 1am - when was the actual time the last animal moved over?
str(step1_2_3_sf_day4)
unique(step1_2_3_sf_day4$hour)


step1_2_3_sf_day4_0100 <- step1_2_3_sf_day4 %>% filter(hour==1)
str(step1_2_3_sf_day4_0100)
step1_2_3_sf_day4_0100 <-  step1_2_3_sf_day4_0100 %>% 
  dplyr::mutate(minute_format1 = minute )
step1_2_3_sf_day4_0100$minute_format1 <- as.factor(step1_2_3_sf_day4_0100$minute_format1)

day4_0100 <-ggplot() +
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA,  linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.8, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_day4_0100 ,alpha = 0.08) +
  theme_bw()+
  facet_wrap(.~ minute_format1,  nrow = 3)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


day4_0100

# This is saying all of the animals were over the VF after 01:20

### the animals moved back the VF at about 10am - when was the actual time the last animal moved over?
unique(step1_2_3_sf_day4$hour)

step1_2_3_sf_day4_0800_0900_10000 <- step1_2_3_sf_day4 %>% filter(hour=="8" |hour=="9" |  hour=="10") 
str(step1_2_3_sf_day4_0800_0900_10000)
unique(step1_2_3_sf_day4_0800_0900_10000$hour)


step1_2_3_sf_day4_0800_0900_10000 <-  step1_2_3_sf_day4_0800_0900_10000 %>% 
  dplyr::mutate(minute_format1 = minute )
step1_2_3_sf_day4_0800_0900_10000$minute_format1 <- as.factor(step1_2_3_sf_day4_0800_0900_10000$minute_format1)
str(step1_2_3_sf_day4_0800_0900_10000)


day4_0800_0900_1000 <-ggplot() +
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA,  linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.8, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_day4_0800_0900_10000 ,alpha = 0.08) +
  theme_bw()+
  facet_wrap(hours_format1~ minute_format1,  nrow = 3)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


day4_0800_0900_1000

# This is saying all of the animals were moved back over the VF after 09:00


################################################################################
#### from the above plots we can say spooking occurred ##########################
####        after 01:20 and before 09:00               #########################
################################################################################


rm(day4_0800_0900_1000,
   day4_0100,
   day4_0800_0900_1000,
   day4,
   step1_2_3_sf_day4,
   step1_2_3_sf_day4_0100,
   step1_2_3_sf_day4_0800_0900_10000,
   step1_2_3_sf_day4_0900_10000)


###############################################################################
#### create a new clm for spooking  ##########################
####        after 01:20 and before 09:00               #########################
################################################################################
str(step1_2_3_sf)

step1_2_3_sf <- step1_2_3_sf %>% 
  dplyr::mutate(spooking = 
                  case_when(
                    
                    local_time > "2022-10-20 01:20:00" & local_time < "2022-10-20 09:00:00" ~ "spooking",
                    TRUE                      ~ "regular"
                  ))
  


spooking <-ggplot() +
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA,  linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.8, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf ,alpha = 0.08) +
  theme_bw()+
  facet_wrap(date ~ spooking,  nrow = 3)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


spooking


################################################################################
### new clm with training and spooking together

names(step1_2_3_sf)
unique(step1_2_3_sf$training_period)
unique(step1_2_3_sf$spooking)

step1_2_3_sf <- step1_2_3_sf %>% 
  dplyr::mutate(period = 
                  case_when(
                    spooking == "spooking" & training_period == "non_training" ~  "Trial when spooked",
                    spooking == "spooking" & training_period == "training" ~      "Training when spooked",
                    
                    spooking == "regular" & training_period == "non_training" ~   "Trial",
                    spooking == "regular" & training_period == "training" ~       "Training",
                    TRUE                      ~ "check"
                  ))



period <-ggplot() +
  geom_sf(data = Lameroo_Vf_area, color = "red", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA,  linewidth = 0.75) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.8, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf ,alpha = 0.08) +
  theme_bw()+
  facet_wrap(date ~ period,  nrow = 3)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


period

