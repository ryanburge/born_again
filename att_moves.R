
cces <- cces %>% 
  mutate(reborn10 = car::recode(pew_bornagain_10, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn12 = car::recode(pew_bornagain_12, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn14 = car::recode(pew_bornagain_14, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn = reborn10 + reborn12 + reborn14) 

cces <- cces %>% 
  mutate(att10 = car::recode(pew_churatd_10, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att12 = car::recode(pew_churatd_12, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att14 = car::recode(pew_churatd_14, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) 


## Going Not BA -- CCES ####

count1 <- cces %>% 
  filter(reborn10 == 1) %>%
  filter(reborn12 == 0) %>% 
  mutate(att = att12 - att10) %>% 
  count(att)

count2 <- cces %>% 
  filter(reborn10 == 1) %>%
  filter(reborn12 == 0) %>% 
  mutate(att = att14 - att12) %>% 
  count(att) %>% 
  rename(n2 = n)

not_ba <-  left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(att, total) %>% 
  mutate(pct = total/533) %>% 
  mutate(pct = round(pct,3)) %>% 
  na.omit()


g1 <- not_ba %>% 
  mutate(att = as.factor(att)) %>% 
  ggplot(., aes(x = att, y = pct, fill = att)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#800080", "#8c198c", "#993299", "#bf7fbf", "azure3", "#7fbf7f", "#66b266", "#198c19", "#008000")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Attending Less : Attending More -->", y = "", title = "Moving to a Not Born-Again Status", caption = "Data: CCES Panel (2010-2014)") +
  annotate("text", x=2.5, y = .5, label = "Less Attendance", size =12, family = "font") +
  annotate("text", x=2.5, y = .45, label = "26.9%", size =12, family = "font") +
  annotate("text", x=8, y = .5, label = "More Attendance", size =12, family = "font") +
  annotate("text", x=8, y = .45, label = "14.6%", size =12, family = "font") +
  ggsave("D://born_again/cces_nba_att.png")

### Becoming BA CCES ####

count1 <- cces %>% 
  filter(reborn10 == 0) %>%
  filter(reborn12 == 1) %>% 
  mutate(att = att12 - att10) %>% 
  count(att)

count2 <- cces %>% 
  filter(reborn10 == 0) %>%
  filter(reborn12 == 1) %>% 
  mutate(att = att14 - att12) %>% 
  count(att) %>% 
  rename(n2 = n)

ba <- left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(att, total) %>% 
  mutate(pct = total/586) %>% 
  mutate(pct = round(pct,3)) %>% 
  na.omit()


g2 <- ba %>% 
  mutate(att = as.factor(att)) %>% 
  ggplot(., aes(x = att, y = pct, fill = att)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#800080", "#8c198c", "#bf7fbf", "azure3", "#7fbf7f", "#66b266", "#329932","#198c19", "#008000")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Attending Less : Attending More -->", y = "", title = "Moving to a Born-Again Status", caption = "Data: CCES Panel (2010-2014)") +
  annotate("text", x=2, y = .5, label = "Less Attendance", size =12, family = "font") +
  annotate("text", x=2, y = .45, label = "17%", size =12, family = "font") +
  annotate("text", x=7, y = .5, label = "More Attendance", size =12, family = "font") +
  annotate("text", x=7, y = .45, label = "19.6%", size =12, family = "font") +
  ggsave("D://born_again/cces_ba_att.png")


both <- g1 + g2

ggsave("D://born_again/att_cces_patch.png", both, width = 10)


## This is VP data ####


vp <- vp %>% 
  mutate(reborn12 = car::recode(pew_bornagain_baseline, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn16 = car::recode(pew_bornagain_2016, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn17 = car::recode(pew_bornagain_2017, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn = reborn12 + reborn16 + reborn17) 

vp <- vp %>% 
  mutate(att12 = car::recode(pew_churatd_baseline, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att16 = car::recode(pew_churatd_2016, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att17 = car::recode(pew_churatd_2017, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA"))


### This is going to Not Ba ##


count1 <- vp %>% 
  filter(reborn12 == 1) %>%
  filter(reborn16 == 0) %>% 
  mutate(att = att16 - att12) %>% 
  count(att)

count2 <- vp %>% 
  filter(reborn16 == 1) %>%
  filter(reborn17 == 0) %>% 
  mutate(att = att17 - att16) %>% 
  count(att) %>% 
  rename(n2 = n)

not_ba <-  left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(att, total) %>% 
  na.omit() %>% 
  mutate(pct = total/287) %>% 
  mutate(pct = round(pct,3)) 


g3 <- not_ba %>% 
  mutate(att = as.factor(att)) %>% 
  ggplot(., aes(x = att, y = pct, fill = att)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#800080", "#730073", "#8c198c", "#993299", "#bf7fbf", "azure3", "#7fbf7f", "#66b266", "#4ca64c", "#198c19", "#008000")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Attending Less : Attending More -->", y = "", title = "Moving to a Not Born-Again Status", caption = "Data: Voter Study Group (2012-2017)") +
  annotate("text", x=3, y = .5, label = "Less Attendance", size =12, family = "font") +
  annotate("text", x=3, y = .45, label = "35.4%", size =12, family = "font") +
  annotate("text", x=8, y = .5, label = "More Attendance", size =12, family = "font") +
  annotate("text", x=8, y = .45, label = "11.5%", size =12, family = "font") +
  ggsave("D://born_again/vp_nba_att.png")

## This going to BA


count1 <- vp %>% 
  filter(reborn12 == 0) %>%
  filter(reborn16 == 1) %>% 
  mutate(att = att16 - att12) %>% 
  count(att)

count2 <- vp %>% 
  filter(reborn16 == 0) %>%
  filter(reborn17 == 1) %>% 
  mutate(att = att17 - att16) %>% 
  count(att) %>% 
  rename(n2 = n)

ba <-  left_join(count1, count2) %>% 
  na_zero(n2) %>% 
  mutate(total = n + n2) %>% 
  select(att, total) %>% 
  na.omit() %>% 
  mutate(pct = total/365) %>% 
  mutate(pct = round(pct,3)) 


g4 <- ba %>% 
  mutate(att = as.factor(att)) %>% 
  ggplot(., aes(x = att, y = pct, fill = att)) + 
  geom_col(color = "black") +
  scale_fill_manual(values = c("#800080", "#730073", "#8c198c", "#993299", "#bf7fbf", "azure3", "#7fbf7f", "#66b266", "#4ca64c", "#198c19", "#008000")) +
  theme_gg("Quicksand") +
  scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 38)) +
  theme(axis.title.x =  element_text(size = 32)) +
  geom_text(aes(y = pct + .015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "font") +
  labs(x = "<-- Attending Less : Attending More -->", y = "", title = "Moving to a Born-Again Status", caption = "Data: Voter Study Group (2012-2017)") +
  annotate("text", x=3, y = .5, label = "Less Attendance", size =12, family = "font") +
  annotate("text", x=3, y = .45, label = "15.9%", size =12, family = "font") +
  annotate("text", x=9, y = .5, label = "More Attendance", size =12, family = "font") +
  annotate("text", x=9, y = .45, label = "27.3%", size =12, family = "font") +
  ggsave("D://born_again/vp_ba_att.png")


both <- g3 + g4

ggsave("D://born_again/att_vp_patch.png", both, width = 10)


