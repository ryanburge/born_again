library(socsci)
library(car)
library(haven)

vp <- read_dta("voter.dta")
cces <- read_dta("cces_panel.dta")

cces %>% 
  filter(pew_bornagain_10 ==1) %>% 
  filter(pew_bornagain_12 ==1) %>% 
  filter(pew_bornagain_14 ==1) %>% 
  count(wt = weight) #2835

cces %>% 
  filter(pew_bornagain_10 ==1 & pew_bornagain_12 ==1 & pew_bornagain_14 ==1) %>% 
  count(wt = weight)

cces %>% 
  filter(pew_bornagain_10 !=1) %>% 
  filter(pew_bornagain_12 !=1) %>% 
  filter(pew_bornagain_14 !=1) %>% 
  count(wt = weight) #5660

cces %>% 
  filter(pew_bornagain_10 !=1) %>% 
  filter(pew_bornagain_12 ==1) %>% 
  filter(pew_bornagain_14 ==1) %>% 
  count(wt = weight) #231

cces %>% 
  filter(pew_bornagain_10 !=1) %>% 
  filter(pew_bornagain_12 !=1) %>% 
  filter(pew_bornagain_14 ==1) %>% 
  count(wt = weight) #217

cces %>% 
  filter(pew_bornagain_10 ==1) %>% 
  filter(pew_bornagain_12 ==1) %>% 
  filter(pew_bornagain_14 !=1) %>% 
  count(wt = weight) #105

cces %>% 
  filter(pew_bornagain_10 ==1) %>% 
  filter(pew_bornagain_12 !=1) %>% 
  filter(pew_bornagain_14 !=1) %>% 
  count(wt = weight) #228

cces %>% 
  filter(pew_bornagain_10 ==1) %>% 
  filter(pew_bornagain_12 !=1) %>% 
  filter(pew_bornagain_14 ==1) %>% 
  count(wt = weight) #137

cces %>% 
  filter(pew_bornagain_10 !=1) %>% 
  filter(pew_bornagain_12 ==1) %>% 
  filter(pew_bornagain_14 !=1) %>% 
  count(wt = weight) #86


### Voter Project ####

vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(pew_bornagain_2016 ==1) %>% 
  filter(pew_bornagain_2017 ==1) %>% 
  count(wt = weight_2017) # 1113

vp %>% 
  filter(pew_bornagain_baseline !=1) %>% 
  filter(pew_bornagain_2016 !=1) %>% 
  filter(pew_bornagain_2017 !=1) %>% 
  count(wt = weight_2017) # 3329


vp %>% 
  filter(pew_bornagain_baseline !=1) %>% 
  filter(pew_bornagain_2016 ==1) %>% 
  filter(pew_bornagain_2017 ==1) %>% 
  count(wt = weight_2017) # 173


vp %>% 
  filter(pew_bornagain_baseline !=1) %>% 
  filter(pew_bornagain_2016 !=1) %>% 
  filter(pew_bornagain_2017 ==1) %>% 
  count(wt = weight_2017) # 60


vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(pew_bornagain_2016 ==1) %>% 
  filter(pew_bornagain_2017 !=1) %>% 
  count(wt = weight_2017) # 29

vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(pew_bornagain_2016 !=1) %>% 
  filter(pew_bornagain_2017 !=1) %>% 
  count(wt = weight_2017) # 176

vp %>% 
  filter(pew_bornagain_baseline ==1) %>% 
  filter(pew_bornagain_2016 !=1) %>% 
  filter(pew_bornagain_2017 ==1) %>% 
  count(wt = weight_2017) # 29

vp %>% 
  filter(pew_bornagain_baseline !=1) %>% 
  filter(pew_bornagain_2016 ==1) %>% 
  filter(pew_bornagain_2017 !=1) %>% 
  count(wt = weight_2017) # 66

