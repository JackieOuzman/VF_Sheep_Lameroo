
step1_2_3 <- read_csv("W:/VF/Sheep_Lameroo_2022/animal_logs/jax_working/animal_GPS_data_step1_2_3.csv")
#turn into spatial data
step1_2_3_sf <-   st_as_sf(step1_2_3,
                           coords = c("X", "Y"),
                           crs = 28354,
                           agr = "constant")

### comments from Caroline 29/11/2022
####################################################
## cues by animal
str(step1_2_3_sf)


summary_audio_ratio <-
  step1_2_3_sf %>%
  dplyr::group_by(Sheep_ID, date, training_period) %>%
  dplyr::summarise(
    audio = sum(Audio_values, na.rm = TRUE),
    pulse = sum(Shock_values, na.rm = TRUE),
    ratio_sum1 = audio / (pulse +
                            audio) * 100,
    ratio_sum2 = pulse / (audio) *
      100
  )


summary_audio_ratio <-  ungroup(summary_audio_ratio)
summary_audio_ratio <-  as.data.frame(summary_audio_ratio)
str(summary_audio_ratio)
summary_audio_ratio <-  summary_audio_ratio %>%  dplyr::select(-geometry)

summary_audio_ratio <-  summary_audio_ratio %>% dplyr::select(Sheep_ID, date, audio,  pulse, training_period)


summary_audio_pluse_long <-
  summary_audio_ratio %>%
  pivot_longer(
    cols = c(audio , pulse),
    names_to = "cue",
    values_to = "value"
  )


summary_audio_pluse_long <-
  summary_audio_pluse_long %>% arrange(training_period, date)
str(summary_audio_pluse_long)



summary_audio_pluse_long$cue <-
  factor(summary_audio_pluse_long$cue,
         levels = c("pulse", "audio"))
                                    
### option 1
total_cues_per_day_animal_plot_v1 <-
  summary_audio_pluse_long %>%
  filter(training_period != "training") %>%
  ggplot(aes(x = date, y = value, fill = cue)) +
  geom_bar(position = "stack", stat =
             "identity") +
  scale_fill_manual(values = c("grey55",
                               "grey80")) +
  theme_classic() +
  facet_wrap(. ~ Sheep_ID) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  
  labs(y = "Count of cue per day for animals ",
       title = "",
       subtitle = "")
total_cues_per_day_animal_plot_v1
### option 2

summary_audio_pluse_long$Sheep_ID <-
  as.character(summary_audio_pluse_long$Sheep_ID)

summary_audio_pluse_long$Sheep_ID <-
  factor(
    summary_audio_pluse_long$Sheep_ID,
    levels = c("1", "2" , "3", "4", "5", "6", "7", "8", "9", "10")
  )

summary_audio_pluse_long <-
  summary_audio_pluse_long %>%
  mutate(
    Date_with_training =
      case_when(
        date ==  "2022-10-17" &
          training_period == "training" ~ "Day 1 training",
        date ==  "2022-10-17" &
          training_period == "non_training" ~ "Day 1 trial",
        date ==  "2022-10-18" &
          training_period == "non_training" ~ "Day 2 trial",
        date ==  "2022-10-19" &
          training_period == "non_training" ~ "Day 3 trial",
        date ==  "2022-10-20" &
          training_period == "non_training" ~ "Day 4 trial",
        date ==  "2022-10-21" &
          training_period == "non_training" ~ "Day 5 trial",
        TRUE ~ "error"
      )
  )
                                    
                                    
total_cues_per_day_animal_plot_v2 <- summary_audio_pluse_long %>%
  ggplot(aes(x = Sheep_ID, y = value, fill = cue)) +
  geom_bar(position = "stack", stat =
             "identity") +
  scale_fill_manual(values = c("grey40",
                               "grey60")) +
  theme_classic() +
  facet_wrap(. ~ Date_with_training) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  
  labs(y = "Cue per animal per day",
       title = "",
       subtitle = "")
total_cues_per_day_animal_plot_v2



ggsave(
  total_cues_per_day_animal_plot_v1,
  device = "png",
  filename = paste0("total_cues_per_day_animal_plot_v1.png"),
  path = "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
  width = 8.62,
  height = 6.28,
  dpi = 600
)
ggsave(
  total_cues_per_day_animal_plot_v2,
  device = "png",
  filename = paste0("total_cues_per_day_animal_plot_v2.png"),
  path = "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
  width = 8.62,
  height = 6.28,
  dpi = 600
)

###############################################################################

summary_audio_ratio <-
  step1_2_3_sf %>%
  dplyr::group_by(Sheep_ID, date, training_period) %>%
  dplyr::summarise(
    audio = sum(Audio_values, na.rm = TRUE),
    pulse = sum(Shock_values, na.rm = TRUE),
    ratio_sum1 = audio / (pulse +
                            audio) * 100,
    na.rm = TRUE
  )


summary_audio_ratio <-
  ungroup(summary_audio_ratio)
summary_audio_ratio <-
  as.data.frame(summary_audio_ratio)
summary_audio_ratio <-
  summary_audio_ratio %>% select(Sheep_ID, date, ratio_sum1, training_period)

summary_audio_ratio$Sheep_ID <-
  as.character(summary_audio_ratio$Sheep_ID)

summary_audio_ratio$Sheep_ID <-
  factor(summary_audio_ratio$Sheep_ID,
         levels = c("1", "2" , "3", "4", "5", "6", "7", "8", "9", "10"))

summary_audio_ratio <-
  summary_audio_ratio %>%
  mutate(
    Date_with_training =
      case_when(
        date ==  "2022-10-17" &
          training_period == "training" ~ "Day 1 training",
        date ==  "2022-10-17" &
          training_period == "non_training" ~ "Day 1 trial",
        date ==  "2022-10-18" &
          training_period == "non_training" ~ "Day 2 trial",
        date ==  "2022-10-19" &
          training_period == "non_training" ~ "Day 3 trial",
        date ==  "2022-10-20" &
          training_period == "non_training" ~ "Day 4 trial",
        date ==  "2022-10-21" &
          training_period == "non_training" ~ "Day 5 trial",
        TRUE ~ "error"
      )
  )

str(summary_audio_ratio)
ratio_per_day_animal_plot_v2 <-
  summary_audio_ratio %>%
  ggplot(aes(x = Sheep_ID, y = ratio_sum1)) +
  geom_col() +
  theme_classic() +
  facet_wrap(. ~ Date_with_training) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  
  labs(y = "Ratio per animal per day",
       title = "Audio / (pulse+audio)*100",
       subtitle = "")

summary_audio_ratio <-
  summary_audio_ratio %>%  arrange(Date_with_training, Sheep_ID)

ggsave(
  ratio_per_day_animal_plot_v2,
  device = "png",
  filename = paste0("ratio_per_day_animal_plot_v2.png"),
  path = "W:/VF/Sheep_Lameroo_2022/R_scripts/plots/",
  width = 8.62,
  height = 6.28,
  dpi = 600
)