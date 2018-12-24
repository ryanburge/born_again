library(labelled)

## This is CCES Data checks 

cces <- cces %>% 
  mutate(reborn10 = car::recode(pew_bornagain_10, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn12 = car::recode(pew_bornagain_12, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn14 = car::recode(pew_bornagain_14, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn = reborn10 + reborn12 + reborn14) %>% 
  mutate(reborn1012 = reborn10 + reborn12) %>% 
  mutate(reborn1214 = reborn12 + reborn14)

## This is people who go from BA to Not BA ####

count1 <- cces %>% 
  filter(reborn10 == 1) %>%
  filter(reborn12 == 0) %>% 
  filter(pid7_10 <= 7) %>% 
  filter(pid7_12 <= 7) %>% 
  mutate(pid = pid7_12 - pid7_10) %>% 
  count(pid)
  
count2 <- cces %>% 
  filter(reborn12 == 1) %>%
  filter(reborn14 == 0) %>% 
  filter(pid7_12 <= 7) %>% 
  filter(pid7_14 <= 7) %>% 
  mutate(pid = pid7_14 - pid7_12) %>% 
  count(pid) %>% 
  rename(n2 = n)

not_ba <- left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(pid, total) %>% 
  mutate(pct = total/513) %>% 
  mutate(pct = round(pct,3))

not_ba  %>% 
  summarise(sum = sum(total))

g1 <- not_ba %>% 
  mutate(pid = as.factor(pid)) %>% 
  ggplot(., aes(x = pid, y = pct, fill = pid)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#7e1314", "#891a1b", "#9a1e1f", "#a53133", "#bf4143", "azure3", "#9999ff", "#6666ff", "#3232ff", "#0000ff")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Becoming More Republican : Becoming More Democrat -->", y = "", title = "Moving to a Not Born-Again Status", caption = "Data: CCES Panel (2010-2014)") +
  annotate("text", x=3.5, y = .5, label = "More Republican", size =12, family = "font") +
  annotate("text", x=3.5, y = .45, label = "13.5%", size =12, family = "font") +
  annotate("text", x=8, y = .5, label = "More Democrat", size =12, family = "font") +
  annotate("text", x=8, y = .45, label = "11.5%", size =12, family = "font") +
  ggsave("D://born_again/cces_nba_pid.png")



## This is people who go from Not BA to BA ####

count1 <- cces %>% 
  filter(reborn10 == 0) %>%
  filter(reborn12 == 1) %>% 
  filter(pid7_10 <= 7) %>% 
  filter(pid7_12 <= 7) %>% 
  mutate(pid = pid7_12 - pid7_10) %>% 
  count(pid)

count2 <- cces %>% 
  filter(reborn12 == 0) %>%
  filter(reborn14 == 1) %>% 
  filter(pid7_12 <= 7) %>% 
  filter(pid7_14 <= 7) %>% 
  mutate(pid = pid7_14 - pid7_12) %>% 
  count(pid) %>% 
  rename(n2 = n)

ba <- left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(pid, total) %>% 
  mutate(pct = total/509) %>% 
  mutate(pct = round(pct,3))


g2 <- ba %>% 
  mutate(pid = as.factor(pid)) %>% 
  ggplot(., aes(x = pid, y = pct, fill = pid)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#891a1b", "#9a1e1f", "#a53133", "#bf4143", "azure3", "#9999ff", "#6666ff", "#3232ff", "#0000ff", "#000066")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Becoming More Republican : Becoming More Democrat -->", y = "", title = "Moving to a Born-Again Status", caption = "Data: CCES Panel (2010-2014)") +
  annotate("text", x=2.5, y = .5, label = "More Republican", size =12, family = "font") +
  annotate("text", x=2.5, y = .45, label = "11.4%", size =12, family = "font") +
  annotate("text", x=7.5, y = .5, label = "More Democrat", size =12, family = "font") +
  annotate("text", x=7.5, y = .45, label = "12.6%", size =12, family = "font") +
  ggsave("D://born_again/cces_ba_pid.png")


both <- g1 + g2

ggsave("D://born_again/pid_cces_patch.png", both, width = 10)


## This is the Voter Project Data

vp <- vp %>% 
  mutate(reborn12 = car::recode(pew_bornagain_baseline, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn16 = car::recode(pew_bornagain_2016, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn17 = car::recode(pew_bornagain_2017, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn = reborn12 + reborn16 + reborn17) 
## This is not BA

count1 <- vp %>% 
  filter(reborn12 ==1) %>% 
  filter(reborn16 ==0) %>% 
  filter(pid7_baseline <= 7) %>% 
  filter(pid7_2016 <= 7) %>% 
  mutate(pid = pid7_2016 - pid7_baseline) %>% 
  count(pid)
  
count2 <- vp %>% 
  filter(reborn16 ==1) %>% 
  filter(reborn17 ==0) %>% 
  filter(pid7_baseline <= 7) %>% 
  filter(pid7_2016 <= 7) %>% 
  mutate(pid = pid7_2017 - pid7_2016) %>% 
  count(pid) %>% 
  rename(n2 = n)

not_ba <- left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(pid, total) %>% 
  mutate(pct = total/279) %>% 
  mutate(pct = round(pct,3))

g3 <- not_ba %>% 
  mutate(pid = as.factor(pid)) %>% 
  ggplot(., aes(x = pid, y = pct, fill = pid)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#7e1314", "#891a1b", "#a53133", "#bf4143", "azure3", "#9999ff", "#6666ff", "#4c4cff", "#3232ff", "#1919ff",  "#0000ff")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Becoming More Republican : Becoming More Democrat -->", y = "", title = "Moving to a Not Born-Again Status", caption = "Data: Voter Study Group (2012-2017)") +
  annotate("text", x=2.5, y = .5, label = "More Republican", size =12, family = "font") +
  annotate("text", x=2.5, y = .45, label = "16.2%", size =12, family = "font") +
  annotate("text", x=8.75, y = .5, label = "More Democrat", size =12, family = "font") +
  annotate("text", x=8.75, y = .45, label = "12%", size =12, family = "font") +
  ggsave("D://born_again/vp_nba_pid.png")

### This is moving to BA

count1 <- vp %>% 
  filter(reborn12 ==0) %>% 
  filter(reborn16 ==1) %>% 
  filter(pid7_baseline <= 7) %>% 
  filter(pid7_2016 <= 7) %>% 
  mutate(pid = pid7_2016 - pid7_baseline) %>% 
  count(pid)

count2 <- vp %>% 
  filter(reborn16 ==0) %>% 
  filter(reborn17 ==1) %>% 
  filter(pid7_baseline <= 7) %>% 
  filter(pid7_2016 <= 7) %>% 
  mutate(pid = pid7_2017 - pid7_2016) %>% 
  count(pid) %>% 
  rename(n2 = n)

ba <- left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(pid, total) %>% 
  mutate(pct = total/365) %>% 
  mutate(pct = round(pct,3))

g4 <- ba %>% 
  mutate(pid = as.factor(pid)) %>% 
  ggplot(., aes(x = pid, y = pct, fill = pid)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#7e1314", "#891a1b", "#9a1e1f", "#a53133", "#bf4143", "azure3", "#9999ff", "#6666ff", "#4c4cff", "#3232ff", "#1919ff",  "#0000ff")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Becoming More Republican : Becoming More Democrat -->", y = "", title = "Moving to a Born-Again Status", caption = "Data: Voter Study Group (2012-2017)") +
  annotate("text", x=2.5, y = .5, label = "More Republican", size =12, family = "font") +
  annotate("text", x=2.5, y = .45, label = "13.2%", size =12, family = "font") +
  annotate("text", x=9.25, y = .5, label = "More Democrat", size =12, family = "font") +
  annotate("text", x=9.25, y = .45, label = "21.9%", size =12, family = "font") +
  ggsave("D://born_again/vp_ba_pid.png")


both2 <- g3 + g4 


ggsave("D://born_again/pid_vp_patch.png", both2, width = 10)


