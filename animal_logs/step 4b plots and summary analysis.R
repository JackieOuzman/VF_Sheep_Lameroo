### run step 4 plots and summary first so you have all the files in your environment

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
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.5, fill = NA) +
  
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = step1_2_3_sf_day1 ,alpha = 0.08) +
  
  geom_sf(data = step1_2_3_sf_day1_training ,alpha = 0.08, color = "red") +
  geom_sf(data = step1_2_3_sf_day1_Non_training ,alpha = 0.08, color = "black") +
  
  theme_bw()+
  facet_wrap(. ~ hours_format1)+
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
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area, color = "black", fill = NA) +
  geom_sf(data = Lameroo_Vf_area_hard_fence_bound_buff, color = "black",linetype = "dotted", size = 0.5, fill = NA) +
  
  geom_sf(data = water_pt ,color ="Blue") +
  
  geom_sf(data = step1_2_3_sf_day4 ,alpha = 0.08) +
  theme_bw()+
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
       
       
       