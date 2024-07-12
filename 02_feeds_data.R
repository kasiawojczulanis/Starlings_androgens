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
  mutate(nfeeds_p6 = nfeeds/as.numeric(p6)) %>% 
  mutate(plot_treatment = case_when(
    fm_treatment_pooled == "androgen_control" ~ "androgen",
    fm_treatment_pooled == "control_control" ~ "control",
    fm_treatment_pooled == "control_androgen" ~ "androgen",
    .default = as.character(fm_treatment_pooled)
  )) %>% 
  mutate(plot_sex = case_when(
    sex == "female" ~ "Females",
    sex == "male" ~ "Males",
    .default = as.character(sex)
  )) 


# EXP FEMALES FOCUS ----
# First breeding of treated females and their parters behavior 
# (2012: andro_ctrl vs ctrl_ctrl; 1 and 1.5 wave for simplicity)

nfeeds_sel_f_effect <- nfeeds %>% 
  filter(fm_treatment_pooled %in% c("androgen_control", "control_control"),
         wave %in% c(1, 1.5),
         year == 2012)


# Stats (sample size)
nfeeds_sel_f_effect %>% 
  group_by(file_name, fm_treatment_pooled, wave) %>% 
  summarise(n = n()) %>% 
  group_by(fm_treatment_pooled, wave) %>% 
  summarise(n = n())


# Models
nfeeds_sel_f_effect_females <- nfeeds_sel_f_effect  %>% 
  filter(sex == "female")

nfeeds_sel_f_effect_males <- nfeeds_sel_f_effect  %>% 
  filter(sex == "male")

# frequenciest
library(lmerTest)

# females
model_glmer_females <- glmer(nfeeds ~ fm_treatment_pooled + p6 + wave + as.factor(hr) + (1 | birdID), 
            data = nfeeds_sel_f_effect_females,
            family = poisson(link = "log"))
summary(model_glmer_females)

# male partners
model_glmer_males <- glmer(nfeeds ~ fm_treatment_pooled + p6 + wave + as.factor(hr) + (1 | birdID), 
                             data = nfeeds_sel_f_effect_males,
                             family = poisson(link = "log"))
summary(model_glmer_males)

# bayesian
library(brms)

model_brms_females <- brm(nfeeds ~ fm_treatment_pooled + p6 + wave + hr + (1 | birdID), 
    data = nfeeds_sel_f_effect_females, family = poisson)
summary(model_brms_females)

# male partners
model_brms_males <- brm(nfeeds ~ fm_treatment_pooled + p6 + hr + wave + (1 | birdID), 
                          data = nfeeds_sel_f_effect_males, family = poisson)
summary(model_brms_males)


# Plot 
ggplot(nfeeds_sel_f_effect, aes(x = hr, y = nfeeds_p6, col = plot_treatment)) + 
  geom_jitter(shape = 4, width = 0.2) + 
  facet_grid(wave ~ plot_sex) + 
  geom_smooth() + 
  theme_bw() +
  labs(x = "Hours of the day",
       y = "Brood size standardized number of nest visits",
       col = "Experimental treatment",
       title = "Nest attendance of FEMALES being androgen-treated or controls \n (at their first breeding attempt) and of their partners") +

  scale_x_continuous(breaks = seq(6,21,2)) +
  scale_y_continuous(breaks = seq(0,18,2)) +
  scale_color_manual(values = c("sienna3", "olivedrab4"))



s# EXP MALES FOCUS ----
# First breeding of treated males and their partners behavior 
# (2013: andro_ctrl vs ctrl_ctrl; 1 and 1.5 wave for simplicity)

nfeeds_sel_m_effect <- nfeeds %>% 
  filter(fm_treatment_pooled %in% c("control_androgen", "control_control"),
         wave %in% c(1, 1.5),
         year == 2013) %>% 
  filter(!is.na(sex))


# Stats (sample size)
nfeeds_sel_m_effect %>% 
  group_by(file_name, fm_treatment_pooled, wave) %>% 
  summarise(n = n()) %>% 
  group_by(fm_treatment_pooled, wave) %>% 
  summarise(n = n())


# Models
nfeeds_sel_m_effect_females <- nfeeds_sel_m_effect  %>% 
  filter(sex == "female")

nfeeds_sel_m_effect_males <- nfeeds_sel_m_effect  %>% 
  filter(sex == "male")

# frequenciest
library(lmerTest)

# males
model_glmer_males <- glmer(nfeeds ~ fm_treatment_pooled + p6 + wave + as.factor(hr) + (1 | birdID), 
                           data = nfeeds_sel_m_effect_males,
                           family = poisson(link = "log"))
summary(model_glmer_males)

# female partners
model_glmer_females <- glmer(nfeeds ~ fm_treatment_pooled + p6 + wave + as.factor(hr) + (1 | birdID), 
                             data = nfeeds_sel_m_effect_females,
                             family = poisson(link = "log"))
summary(model_glmer_females)


# bayesian
library(brms)

# males 
model_brms_males <- brm(nfeeds ~ fm_treatment_pooled + p6 + wave + hr + (1 | birdID), 
                        data = nfeeds_sel_m_effect_males, family = poisson)
summary(model_brms_males)

# female partners 
model_brms_females <- brm(nfeeds ~ fm_treatment_pooled + p6 + wave + hr + (1 | birdID), 
                          data = nfeeds_sel_m_effect_females, family = poisson)
summary(model_brms_females)



# Plot 
ggplot(nfeeds_sel_m_effect, aes(x = hr, y = nfeeds_p6, col = plot_treatment)) + 
  geom_jitter(shape = 4, width = 0.2) + 
  facet_grid(wave ~ plot_sex) + 
  geom_smooth() + 
  theme_bw() +
  labs(x = "Hours of the day",
       y = "Brood size standardized number of nest visits",
       col = "Experimental treatment",
       title = "Nest attendance of MALES being androgen-treated or controls \n (at their first breeding attempt) and of their partners") +
  
  scale_x_continuous(breaks = seq(6,21,2)) +
  scale_y_continuous(breaks = seq(0,18,2)) +
  scale_color_manual(values = c("sienna3", "olivedrab4"))

