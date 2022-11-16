library(tidyverse)
library(readxl)

data <- read_excel("data/processed/weight_development.xlsx") %>% 
  select(sample_id:cage, bodyweight_birthday_group_average, bodyweight_birthday_plus_24H_group_average, bodyweight_birthday_plus_48H_group_average, bodyweight_baseline, bodyweight_baseline_plus_24h, bodyweight_baseline_plus_48h) %>% 
  rename(day_0 = "bodyweight_birthday_group_average",
         day_1 = "bodyweight_birthday_plus_24H_group_average", 
         day_2 = "bodyweight_birthday_plus_48H_group_average",
         day_3 = bodyweight_baseline, 
         day_4 = `bodyweight_baseline_plus_24h`, 
         day_5 = `bodyweight_baseline_plus_48h`,) %>% 
  pivot_longer(., cols = c(day_0:day_5),
               names_to = "time",
               values_to = "bodyweight") %>% 
  mutate(received_antibiotics = recode_factor(received_antibiotics, 
                                              yes = "MAT",
                                              no = "CON"))
startweight <- data %>% 
  filter(exp_number %in% c("1","3","4")) %>% 
  filter(time %in% c("day_3", "day_5")) %>%
  drop_na(bodyweight) %>% 
  group_by(exp_number, type_of_feed, time) %>% 
  summarise(startweight = mean(bodyweight),
            sd = sd(bodyweight))

birthweight <- data %>% 
  filter(exp_number %in% "2") %>% 
  filter(time %in% c("day_0", "day_1", "day_2")) %>%
  drop_na(bodyweight) %>% 
  group_by(exp_number, received_antibiotics, time) %>% 
  summarise(birthweight = mean(bodyweight),
            sd = sd(bodyweight))

birthweight %>% 
  filter(time == "day_0") %>% 
  knitr::kable()
  

