### Gender ####

gender <- cces %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>% 
  group_by(gender_10) %>% 
  mean_ci(change, wt = weight) %>% 
  mutate(gender = frcode(gender_10 == 1 ~ "Male",
                         gender_10 == 2 ~ "Female")) %>% 
  mutate(group = "CCES Panel")

gender2 <- vp %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>%
  group_by(gender_baseline) %>% 
  mean_ci(change, wt = weight_2017) %>% 
  mutate(gender = frcode(gender_baseline == 1 ~ "Male",
                         gender_baseline == 2 ~ "Female"))  %>% 
  mutate(group = "VP Panel")

graph <- bind_df("gender")

graph %>% 
  mutate(mean = round(mean, 3)) %>% 
  ggplot(., aes(x = gender, y = mean, fill = gender)) +
  geom_col(color = "black") +
  facet_grid(~ group) +
  theme_gg("Quicksand") +
  scale_fill_manual(values = c("forestgreen", "darkorange3")) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = .05, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  labs(x = "Gender", y = "", title = "Born-Again Status Change by Gender") +
  ggsave("D://born_again/images/gender_change.png")


### Education #####

ed1 <- cces %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>% 
  group_by(educ_14) %>% 
  mean_ci(change, wt = weight) %>% 
  mutate(educ = frcode(educ_14 == 1 ~ "No HS",
                       educ_14 == 2 ~ "HS Grad",
                       educ_14 == 3 ~ "Some College", 
                       educ_14 == 4 ~ "2 Year",
                       educ_14 == 5 ~ "4 Year",
                       educ_14 == 6 ~ "Post-grad",
                       TRUE ~ "REMOVE")) %>% 
  mutate(group = "CCES Panel")

ed2 <- vp %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>%
  group_by(educ_2017) %>% 
  mean_ci(change, wt = weight_2017) %>% 
  mutate(educ = frcode(educ_2017 == 1 ~ "No HS",
                       educ_2017 == 2 ~ "HS Grad",
                       educ_2017 == 3 ~ "Some College", 
                       educ_2017 == 4 ~ "2 Year",
                       educ_2017 == 5 ~ "4 Year",
                       educ_2017 == 6 ~ "Post-grad",
                       TRUE ~ "REMOVE")) %>% 
  mutate(group = "VP Panel")

graph <- bind_rows(ed1, ed2)

graph %>% 
  filter(educ != "REMOVE") %>% 
  mutate(mean = round(mean, 3)) %>% 
  ggplot(., aes(x = educ, y = mean, fill = educ)) +
  geom_col(color = "black") +
  facet_grid(~ group) +
  theme_gg("Quicksand") +
  scale_fill_d3() +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = .025, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 24)) +
  labs(x = "Education", y = "", title = "Born-Again Status Change by Education") +
  ggsave("D://born_again/images/ed_change.png", width = 9)

#### Age ####

age1 <- cces %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>%
  mutate(age = 2014 - birthyr_14) %>% 
  mutate(age2 = car::recode(age, "18:35 =1; 36:44=2; 45:54=3; 55:64=4; 65:100 =5")) %>% 
  group_by(age2) %>% 
  mean_ci(change, wt = weight) %>% 
  mutate(age = frcode(age2 == 1 ~ "18 to 35",
                      age2 == 2 ~ "36 to 44",
                      age2 == 3 ~ "45 to 54", 
                      age2 == 4 ~ "55 to 64",
                      age2 == 5 ~ "65 and Older",
                      TRUE ~ "REMOVE")) %>% 
  mutate(group = "CCES Panel")


age2 <- vp %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>%
  mutate(age = 2012 - birthyr_baseline) %>% 
  mutate(age2 = car::recode(age, "18:35 =1; 36:44=2; 45:54=3; 55:64=4; 65:100 =5")) %>% 
  group_by(age2) %>% 
  mean_ci(change, wt = weight_2017) %>% 
  mutate(age = frcode(age2 == 1 ~ "18 to 35",
                      age2 == 2 ~ "36 to 44",
                      age2 == 3 ~ "45 to 54", 
                      age2 == 4 ~ "55 to 64",
                      age2 == 5 ~ "65 and Older",
                      TRUE ~ "REMOVE")) %>% 
  mutate(group = "VP Panel")

graph <- bind_rows(age1, age2)

graph %>% 
  filter(age2 != "REMOVE") %>% 
  mutate(mean = round(mean, 3)) %>% 
  ggplot(., aes(x = age, y = mean, fill = age)) +
  geom_col(color = "black") +
  facet_grid(~ group) +
  theme_gg("Quicksand") +
  scale_fill_d3() +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = .025, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 24)) +
  labs(x = "Age Group", y = "", title = "Born-Again Status Change by Age Group") +
  ggsave("D://born_again/images/age_change.png", width = 9)


## Race ####

race1 <- cces %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>%
  group_by(race_10) %>% 
  mean_ci(change, wt = weight) %>% 
  mutate(race = frcode(race_10 == 1 ~ "White",
                      race_10 == 2 ~ "Black",
                      race_10 == 3 ~ "Hispanic", 
                      race_10 == 4 ~ "Asian",
                      race_10 == 5 ~ "Native American",
                      race_10 == 6 ~ "Mixed",
                      race_10 == 7 ~ "Other",
                      race_10 == 8 ~ "Middle Eastern",
                      TRUE ~ "REMOVE")) %>% 
  mutate(group = "CCES Panel")


race2 <- vp %>% 
  mutate(change = car::recode(reborn, "1:2=1; 0=0; 3=0; else = NA")) %>%
  group_by(race_2017) %>% 
  mean_ci(change, wt = weight_2017) %>% 
  mutate(race = frcode(race_2017 == 1 ~ "White",
                      race_2017 == 2 ~ "Black",
                      race_2017 == 3 ~ "Hispanic", 
                      race_2017 == 4 ~ "Asian",
                      race_2017 == 5 ~ "Native American",
                      race_2017 == 6 ~ "Mixed",
                      race_2017 == 7 ~ "Other",
                      race_2017 == 8 ~ "Middle Eastern",
                      TRUE ~ "REMOVE")) %>% 
  mutate(group = "VP Panel")


graph <- bind_rows(race1, race2) %>% 
  select(race, mean, sd, n, se, lower, upper, group)

graph %>% 
  filter(race != "REMOVE") %>% 
  mutate(mean = round(mean, 3)) %>% 
  ggplot(., aes(x = race, y = mean, fill = race)) +
  geom_col(color = "black") +
  facet_grid(~ group) +
  theme_gg("Quicksand") +
  scale_fill_d3() +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = .025, label = paste0(mean*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 24)) +
  labs(x = "Age Group", y = "", title = "Born-Again Status Change by Racial Group") +
  ggsave("D://born_again/images/race_change.png", width = 11)