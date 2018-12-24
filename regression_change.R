rb <- cces %>% 
  filter(reborn == 1 | reborn == 2) %>% 
  mutate(change = 1)

rb1 <- cces %>% 
  filter(reborn == 0 | reborn == 3) %>% 
  mutate(change = 0)

cces <- bind_rows(rb, rb1)

cces <- cces %>% 
  mutate(white = recode(race_10, "1=1; else = 0")) %>% 
  mutate(black = recode(race_10, "2=1; else = 0")) %>% 
  mutate(age = 2010 - birthyr_10) %>% 
  mutate(age2 = age/91) %>% 
  mutate(male = car::recode(gender_10, "1=1;2=0"))

cces <- cces %>% 
  mutate(att10 = car::recode(pew_churatd_10, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att12 = car::recode(pew_churatd_12, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att14 = car::recode(pew_churatd_14, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att_change = att14- att10) %>% 
  mutate(att_reg = abs(att_change)) %>% 
  mutate(att_reg = att_reg/5)
  
cces <- cces %>% 
  mutate(pid10 = car::recode(pid7_10, "8:99 = NA")) %>% 
  mutate(pid14 = car::recode(pid7_14, "8:99 = NA")) %>% 
  mutate(pid_change = pid14 - pid10) %>% 
  mutate(pid_reg = abs(pid_change)) %>% 
  mutate(pid_reg = pid_reg/7)


reg1 <- glm(change ~ pid_reg + att_reg + white + black + age2 + male, data = cces, family = "binomial")
summary(reg1)

dwplot(reg1, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% 
  relabel_predictors(c(pid_reg = "Change in PID",
                       att_reg = "Change in Attendance",
                       white = "White", 
                       black = "Black",
                       age2 = "Age", 
                       male = "Male")) +
  theme_gg("Quicksand") +
  theme(plot.title = element_text(size = 40)) +
  labs(x = "Coefficient Estimate", y = "", title = "Predicting a Born-Again Status Change", caption = "Data: CCES Panel (2010-2014)") +
  ggsave("D://born_again/images/regress.png") 





