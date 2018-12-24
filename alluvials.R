library(reshape2)
library(ggalluvial)

## Do This With CCES Data

cces <- cces %>% 
  mutate(reborn10 = car::recode(pew_bornagain_10, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn12 = car::recode(pew_bornagain_12, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn14 = car::recode(pew_bornagain_14, "1=1; 2 =0; else = NA")) %>% 
  mutate(reborn = reborn10 + reborn12 + reborn14)
         
test1 <- cces %>% 
  filter(reborn == 3 | reborn == 0) %>% 
  select(caseid)


small <- anti_join(cces, test1)

small <- small %>% 
  select(caseid, reborn10, reborn12, reborn14)

df <- melt(small, id = c("caseid")) %>% arrange(caseid) 

df <- df %>% 
  mutate(variable = car::recode(variable, "'reborn10'= '2010'; 'reborn12'= '2012'; 'reborn14'= '2014'"))

df <- df %>% 
  mutate(value = car::recode(value, " 1= 'Born Again'; 0= 'Not Born Again'")) %>% 
  mutate(value = fct_relevel(value, "Born Again", "Not Born Again")) 

all1 <- ggplot(df, aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_flow(stat = "alluvium", lode.guidance = "leftright") +
  geom_stratum() +
  theme_gg("Quicksand") +
  geom_label(fill = "white", stat = "stratum", size = 10, colour = "black", family = "font") +
  scale_fill_manual(values = c("cornflowerblue", "goldenrod")) +
  scale_color_manual(values = c("cornflowerblue", "goldenrod")) +
  theme(plot.title = element_text(size = 64)) +
  labs(x = "Year", y = "Number of Respondents", title = "How Do People Change Their Born Again Status?", caption = "Data: CCES Panel (2010-2014)") +
  ggsave("D://born_again/cces_alluvial_full.png") 

## Voter Project ####

vp <- vp %>% 
  mutate(reborn12 = car::recode(pew_bornagain_baseline, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn16 = car::recode(pew_bornagain_2016, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn17 = car::recode(pew_bornagain_2017, "1=1; 2=0; else = NA")) %>% 
  mutate(reborn = reborn12 + reborn16 + reborn17) %>% 
  rename(caseid = case_identifier)

test1 <- vp %>% 
  filter(reborn == 3 | reborn == 0) 

vp1 <- vp %>% 
  filter(reborn != "NA")

small <- anti_join(vp1, test1) 

small <- small %>% 
  filter(reborn != "NA")

small <- small %>% 
  select(caseid, reborn12, reborn16, reborn17)


df <- melt(small, id = c("caseid")) %>% arrange(caseid) 


df <- df %>% 
  mutate(variable = car::recode(variable, "'reborn12'= '2012'; 'reborn16'= '2016'; 'reborn17'= '2017'"))

df <- df %>% 
  mutate(value = car::recode(value, " 1= 'Born Again'; 0= 'Not Born Again'")) %>% 
  mutate(value = fct_relevel(value, "Born Again", "Not Born Again"))


all2 <- ggplot(df, aes(x = variable, stratum = value, alluvium = caseid, fill = value, label = value)) +
  geom_flow(stat = "alluvium", lode.guidance = "leftright") +
  geom_stratum() +
  theme_gg("Quicksand") +
  geom_label(fill = "white", stat = "stratum", size = 10, colour = "black", family = "font") +
  scale_fill_manual(values = c("cornflowerblue", "goldenrod")) +
  scale_color_manual(values = c("cornflowerblue", "goldenrod")) +
  theme(plot.title = element_text(size = 44)) +
  labs(x = "Year", y = "Number of Respondents", title = "", caption = "Data: Voter Study Group (2012-2017)") +
  ggsave("D://born_again/vp_alluvial_full.png") 


plot <- all1 + all2

ggsave("D://born_again/patch_all_full.png", plot, width =10)
