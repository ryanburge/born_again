library(dotwhisker)

## Predicting a Change in Born-Again Status ###

## CCES Data ####
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
  mutate(age = age - 18) %>% 
  mutate(age2 = age/73) %>% 
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
  ggsave("D://born_again/images/regress_cces.png") 

## VP Data ####

rb <- vp %>% 
  filter(reborn == 1 | reborn == 2) %>% 
  mutate(change = 1)

rb1 <- vp %>% 
  filter(reborn == 0 | reborn == 3) %>% 
  mutate(change = 0)

vp <- bind_rows(rb, rb1)

vp <- vp %>% 
  mutate(white = recode(race_baseline, "1=1; else = 0")) %>% 
  mutate(black = recode(race_baseline, "2=1; else = 0")) %>% 
  mutate(age = 2012 - birthyr_baseline) %>% 
  mutate(age = age - 18) %>% 
  mutate(age2 = age/73) %>% 
  mutate(male = car::recode(gender_baseline, "1=1;2=0"))

vp <- vp %>% 
  mutate(att12 = car::recode(pew_churatd_baseline, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att16 = car::recode(pew_churatd_2016, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att17 = car::recode(pew_churatd_2017, "6=1; 5=2; 4=3; 3=4; 2=5; 1=6; else = NA")) %>% 
  mutate(att_change = att17- att12) %>% 
  mutate(att_reg = abs(att_change)) %>% 
  mutate(att_reg = att_reg/5)

vp <- vp %>% 
  mutate(pid12 = car::recode(pid7_baseline, "8 = NA")) %>% 
  mutate(pid17 = car::recode(pid7_2017, "8 = NA")) %>% 
  mutate(pid_change = pid17 - pid12) %>% 
  mutate(pid_reg = abs(pid_change)) %>% 
  mutate(pid_reg = pid_reg/7)

reg1 <- glm(change ~ pid_reg + att_reg + white + black + age2 + male, data = vp, family = "binomial")
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
  labs(x = "Coefficient Estimate", y = "", title = "Predicting a Born-Again Status Change", caption = "Data: Voter Study Group (2012-2017)") +
  ggsave("D://born_again/images/regress_vp.png") 


## Two Models in One Graph #####

reg1 <- glm(change ~ pid_reg + att_reg + white + black + age2 + male, data = cces, family = "binomial")
reg2 <- glm(change ~ pid_reg + att_reg + white + black + age2 + male, data = vp, family = "binomial")


reg1_t <- tidy(reg1) %>% mutate(model = "CCES")
reg2_t <- tidy(reg2) %>% mutate(model = "Voter Project")

two <- bind_rows(reg1_t, reg2_t)

dwplot(two, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% 
  relabel_predictors(c(pid_reg = "Change in PID",
                       att_reg = "Change in Attendance",
                       white = "White", 
                       black = "Black",
                       age2 = "Age",
                       # age2 = "Age Squared", 
                       male = "Male")) +
  theme_gg("Quicksand") +
  theme(plot.title = element_text(size = 40)) +
  theme(legend.position = c(.85,.1)) +
  scale_color_manual(values = c("cornflowerblue", "darkorchid")) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(x = "Coefficient Estimate", y = "", title = "Predicting a Born-Again Status Change", caption = "") +
  ggsave("D://born_again/images/regress_both.png") 
