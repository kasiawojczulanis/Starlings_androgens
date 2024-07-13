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
  
  mutate(plot_wave = case_when(
    wave == 1 ~ "first",
    wave == 1.5 ~ "intermediate",
    wave == 2 ~ "second",
    .default = as.character(wave)
  )) %>% 
  
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
  )) %>% 
  
  ungroup()


# EXP FEMALES FOCUS ----
# First breeding of treated females and their parters behavior 
# (2012: andro_ctrl vs ctrl_ctrl; 1 and 1.5 wave for simplicity)

nfeeds_sel_f_effect <- nfeeds %>% 
  filter(fm_treatment_pooled %in% c("androgen_control", "control_control"),
         wave %in% c(1, 1.5),
         year == 2012)

saveRDS(nfeeds_sel_f_effect, "02_nfeeds_sel_f_effect.rds")

# Stats (sample size)
nfeeds_sel_f_effect %>% 
  group_by(file_name, fm_treatment_pooled, wave, p6) %>% 
  summarise(n = n()) %>% 
  group_by(fm_treatment_pooled, wave, p6) %>% 
  summarise(n = n())


# Models
nfeeds_sel_f_effect_females <- nfeeds_sel_f_effect  %>% 
  filter(sex == "female")

nfeeds_sel_f_effect_males <- nfeeds_sel_f_effect  %>% 
  filter(sex == "male")

# frequenciest
library(lmerTest)


# females
model_glmer_females1 <- glmer(nfeeds ~ fm_treatment_pooled + p6 + as.factor(wave) + as.factor(hr) + (1 | birdID), 
                              data = nfeeds_sel_f_effect_females,
                              family = poisson(link = "log"))
summary(model_glmer_females1)


# male partners
model_glmer_males1 <- glmer(nfeeds ~ fm_treatment_pooled + p6 + as.factor(wave) + as.factor(hr) + (1 | birdID), 
                             data = nfeeds_sel_f_effect_males,
                             family = poisson(link = "log"))
summary(model_glmer_males1)

# # bayesian
# library(brms)
# 
# # pr <- prior (prior(normal(0, 1), class = 'b'))
# 
# model_brms_females1 <- brm(nfeeds ~ fm_treatment_pooled + p6 + wave + as.factor(hr) + (1 | birdID), 
#     data = nfeeds_sel_f_effect_females, 
#     family = poisson,
#     # prior = pr,
#     cores = 4)
# 
# summary(model_brms_females1)
# 
# # male partners
# model_brms_males1 <- brm(nfeeds ~ fm_treatment_pooled + p6 + as.factor(hr) + wave + (1 | birdID),
#                         data = nfeeds_sel_f_effect_males, 
#                         family = poisson,
#                         # prior = pr,
#                         cores = 4)
# summary(model_brms_males1)
 

# Plot - females

nfeeds_sel_f_effect %>% 
  filter(sex == "female") %>% 
  
  ggplot(aes(x = hr, y = nfeeds, col = plot_treatment)) + 
  geom_jitter(shape = 4, width = 0.1) + 
  facet_grid(plot_wave ~ p6) + 
  geom_smooth() + 
  theme_bw() +
  labs(x = "Hours of the day",
       y = "Number of trovan reads (proxy for nest attendance)",
       col = "Experimental treatment",
       title = "Nest attendance of FEMALES being androgen-treated or controls \n (at their first breeding attempt)") +
  
  scale_x_continuous(breaks = seq(6,21,2)) +
  scale_color_manual(values = c("sienna3", "olivedrab4"))

ggsave("females_effect_females.jpg", plot = last_plot())



# Plot - male partners

nfeeds_sel_f_effect %>% 
  filter(sex == "male") %>% 
  
  ggplot(aes(x = hr, y = nfeeds, col = plot_treatment)) + 
  geom_jitter(shape = 4, width = 0.1) + 
  facet_grid(plot_wave ~ p6) + 
  geom_smooth() + 
  theme_bw() +
  labs(x = "Hours of the day",
       y = "Number of trovan reads (proxy for nest attendance)",
       col = "Experimental treatment",
       title = "Nest attendance of MALE PARTNERS of females being androgen-treated or controls \n (at the females first breeding attempt)") +
  
  scale_x_continuous(breaks = seq(6,21,2)) +
  scale_color_manual(values = c("sienna3", "olivedrab4"))

ggsave("females_effect_male_partners.jpg", plot = last_plot())


# EXP MALES FOCUS ----
# First breeding of treated males and their partners behavior 
# (2013: andro_ctrl vs ctrl_ctrl; 1 and 1.5 wave for simplicity)

nfeeds_sel_m_effect <- nfeeds %>% 
  filter(fm_treatment_pooled %in% c("control_androgen", "control_control"),
         wave %in% c(1, 1.5),
         year == 2013) %>% 
  filter(!is.na(sex))

saveRDS(nfeeds_sel_m_effect, "02_nfeeds_sel_m_effect.rds")

# Stats (sample size)
nfeeds_sel_m_effect %>% 
  group_by(file_name, fm_treatment_pooled, wave, p6) %>% 
  summarise(n = n()) %>% 
  group_by(fm_treatment_pooled, wave, p6) %>% 
  summarise(n = n())


# Models
nfeeds_sel_m_effect_females <- nfeeds_sel_m_effect  %>% 
  filter(sex == "female")

nfeeds_sel_m_effect_males <- nfeeds_sel_m_effect  %>% 
  filter(sex == "male")

# frequenciest
library(lmerTest)

# males
model_glmer_males2 <- glmer(nfeeds ~ fm_treatment_pooled + p6 + as.factor(wave) + as.factor(hr) + (1 | birdID), 
                           data = nfeeds_sel_m_effect_males,
                           family = poisson(link = "log"))
summary(model_glmer_males2)


# female partners
model_glmer_females2 <- glmer(nfeeds ~ fm_treatment_pooled + p6 + as.factor(wave) + as.factor(hr) + (1 | birdID), 
                             data = nfeeds_sel_m_effect_females,
                             family = poisson(link = "log"))
summary(model_glmer_females2)


# # bayesian
# library(brms)
# 
# # pr <- prior(normal(0, 1), class = 'b')
# 
# # males 
# model_brms_males2 <- brm(nfeeds ~ fm_treatment_pooled + p6 + wave + hr + (1 | birdID), 
#                         data = nfeeds_sel_m_effect_males, 
#                         family = poisson,
#                         # prior = pr,
#                         cores = 4)
# 
# summary(model_brms_males2)
# 
# # female partners 
# model_brms_females2 <- brm(nfeeds ~ fm_treatment_pooled + p6 + wave + hr + (1 | birdID), 
#                           data = nfeeds_sel_m_effect_females, 
#                           family = poisson,
#                           # prior = pr,
#                           cores = 4)
# summary(model_brms_females2)



# Plot - males
nfeeds_sel_m_effect %>% 
  filter(sex == "male") %>% 
  
  ggplot(aes(x = hr, y = nfeeds, col = plot_treatment)) + 
  geom_jitter(shape = 4, width = 0.1) + 
  facet_grid(plot_wave ~ p6) + 
  geom_smooth() + 
  theme_bw() +
  labs(x = "Hours of the day",
       y = "Brood size standardized number of nest visits",
       col = "Experimental treatment",
       title = "Nest attendance of MALES being androgen-treated or controls \n (at their first breeding attempt)") +
  
  scale_x_continuous(breaks = seq(6,21,2)) +
  scale_color_manual(values = c("sienna3", "olivedrab4"))

ggsave("males_effect_males.jpg", plot = last_plot())


# Plot - female partners
nfeeds_sel_m_effect %>% 
  filter(sex == "female") %>% 
  
  ggplot(aes(x = hr, y = nfeeds, col = plot_treatment)) + 
  geom_jitter(shape = 4, width = 0.1) + 
  facet_grid(plot_wave ~ p6) + 
  geom_smooth() + 
  theme_bw() +
  labs(x = "Hours of the day",
       y = "Brood size standardized number of nest visits",
       col = "Experimental treatment",
       title = "Nest attendance of FEMALE PARTNERS of males being androgen-treated or controls \n (at their first breeding attempt)") +
  
  scale_x_continuous(breaks = seq(6,21,2)) +
  scale_color_manual(values = c("sienna3", "olivedrab4"))
  
ggsave("males_effect_female_partner.jpg", plot = last_plot())



