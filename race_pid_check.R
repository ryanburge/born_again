cces %>% 
  filter(reborn10 == 1) %>%
  filter(reborn12 == 0) %>% 
  filter(pid7_10 <= 7) %>% 
  filter(pid7_12 <= 7) %>% 
  filter(race_10 ==2) %>% 
  mutate(pid = pid7_12 - pid7_10) %>% 
  count(pid)


cces %>% 
  filter(reborn12 == 1) %>%
  filter(reborn14 == 0) %>% 
  filter(pid7_10 <= 7) %>% 
  filter(pid7_12 <= 7) %>% 
  filter(race_10 ==2) %>% 
  mutate(pid = pid7_12 - pid7_10) %>% 
  count(pid)


cces %>% 
  filter(reborn10 == 0) %>%
  filter(reborn12 == 1) %>% 
  filter(pid7_10 <= 7) %>% 
  filter(pid7_12 <= 7) %>% 
  filter(race_10 ==2) %>% 
  mutate(pid = pid7_12 - pid7_10) %>% 
  count(pid)


cces %>% 
  filter(reborn12 == 0) %>%
  filter(reborn14 == 1) %>% 
  filter(pid7_10 <= 7) %>% 
  filter(pid7_12 <= 7) %>% 
  filter(race_10 ==2) %>% 
  mutate(pid = pid7_12 - pid7_10) %>% 
  count(pid)


### 36 of 43 AAs who went to BA didn't change their PID.
### 44 of 53 AAs who went to not BA didn't change their PID. 