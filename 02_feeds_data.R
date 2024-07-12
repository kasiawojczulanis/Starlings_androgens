# Libs and funs ----

library(lubridate)
library(tidyverse) 
library(tidyr)
`%notin%` <- Negate(`%in%`)

# Basics reads and filtering -----

# read data
df01 <- readRDS("01_preprocessed_data.rds")

# focus on parents only 
df02 <- df01 %>% 
  filter(!is.na(role))


# filtering out TROVAN errors
df02 <- df02 %>% 
  group_by(file_name, birdID) %>% 
  mutate(lag_date_time = lag(date_time),
         interv_diff = as.numeric(difftime(date_time, lag_date_time, units = "secs"))) %>% 
  filter(interv_diff > 8) # removes all reads <8 secs

saveRDS(df02, "02_feeds_data.rds")


# Read data and analysis ----

df02 <- readRDS("02_feeds_data.rds")

df03 <- df02 %>% 
  filter(year %in% c(2012, 2013)) %>% 
  filter(min_age >= 5 & min_age <= 14) %>% 
  filter(fm_treatment_pooled %in% c("androgen_control", "control_androgen", "control_control"))

nfeeds <- df03 %>% 
  group_by(year, fm_treatment_pooled, file_name, wave, nest, min_age, p6, birdID, sex, role, hr, hr_rec) %>% 
  summarise(nfeeds = n()) %>% 
  mutate(nfeeds_p6 = nfeeds/as.numeric(p6))

# EXP FEMALES FOCUS ----
# First breeding of treated females and their parters behavior 
# (2012: andro_ctrl vs ctrl_ctrl; 1 and 1.5 wave for simplicity)

nfeeds_sel_females <- nfeeds %>% 
  filter(fm_treatment_pooled %in% c("androgen_control", "control_control"),
         wave %in% c(1, 1.5),
         year == 2012)

model <- lm(nfeeds ~ role * fm_treatment_pooled + p6 + hr, data = nfeeds_sel_females)
summary(model)

ggplot(nfeeds_sel_females, aes(x = hr, y = nfeeds, col = fm_treatment_pooled)) + 
  geom_point() + facet_grid( year ~ role) + 
  geom_smooth()
