## Checking AA in the CCES

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


## Checking AA in the VP


vp %>% 
  filter(reborn12 == 0) %>%
  filter(reborn16 == 1) %>% 
  filter(pid7_baseline <= 7) %>% 
  filter(pid7_2016 <= 7) %>% 
  filter(race_baseline ==2) %>% 
  mutate(pid = pid7_2016 - pid7_baseline) %>% 
  ct(pid)

vp %>% 
  filter(reborn16 == 0) %>%
  filter(reborn17 == 1) %>% 
  filter(pid7_2016 <= 7) %>% 
  filter(pid7_2017 <= 7) %>% 
  filter(race_baseline ==2) %>% 
  mutate(pid = pid7_2017 - pid7_2016) %>% 
  ct(pid)

## 50 total - 3 more Demcorat - 8 more Republican

vp %>% 
  filter(reborn12 == 1) %>%
  filter(reborn16 == 0) %>% 
  filter(pid7_baseline <= 7) %>% 
  filter(pid7_2016 <= 7) %>% 
  filter(race_baseline ==2) %>% 
  mutate(pid = pid7_2016 - pid7_baseline) %>% 
  ct(pid)

vp %>% 
  filter(reborn16 == 1) %>%
  filter(reborn17 == 0) %>% 
  filter(pid7_2016 <= 7) %>% 
  filter(pid7_2017 <= 7) %>% 
  filter(race_baseline ==2) %>% 
  mutate(pid = pid7_2017 - pid7_2016) %>% 
  ct(pid)

## 48 total - 6 more Democrat - 5 more Republican


