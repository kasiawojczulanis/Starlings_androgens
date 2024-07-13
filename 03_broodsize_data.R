# Libs and funs ----

library(lubridate)
library(tidyverse) 
library(tidyr)
`%notin%` <- Negate(`%in%`)


# Read data
nfeeds_sel_f_effect <- readRDS("02_nfeeds_sel_f_effect.rds")
nfeeds_sel_m_effect <- readRDS("02_nfeeds_sel_m_effect.rds")

# Transform data
cont_table <- nfeeds_sel_f_effect %>% 
  group_by(file_name, plot_treatment, plot_wave, p6) %>% 
  summarise(n = n()) %>% 
  select(-n) %>% 
  mutate(p6 = as.character(p6))

cont_table_chi2 <- nfeeds_sel_f_effect %>%
group_by(file_name, plot_treatment, wave, p6) %>%
summarise(n = n()) %>%
group_by(wave, plot_treatment, p6) %>%
summarise(n = n()) %>%
pivot_wider(names_from = plot_treatment, values_from = n) %>%
mutate(androgen = if_else(is.na(androgen), 0, androgen)) %>%
mutate(control = if_else(is.na(control), 0, control)) %>%
ungroup()


w1_ct <- cont_table_chi2 %>% 
  filter(wave == 1) %>% 
  select(-wave) %>% 
  mutate(total = androgen + control,
         p6 = as.character(p6)) 

w1.5_ct <- cont_table_chi2 %>% 
  filter(wave == 1.5)  %>% 
  select(-wave) %>% 
  mutate(total = androgen + control)

chisq_1 <- chisq.test(w1_ct)
chisq_1.5 <- chisq.test(w1.5_ct)

library(ggmosaic)
ggplot(data = cont_table) +
  geom_mosaic(aes(x = product(plot_treatment, p6), fill = plot_treatment)) + 
  facet_wrap(~ plot_wave) +
  scale_fill_manual(values = c("sienna3", "olivedrab4")) +
  labs(x = "Brood size (n chicks)",
       y = "",
      fill = "Treatment") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank())

# Modelling  
model <- brms::brm(
  bf(androgen | trials(total) ~ p6),
  data = w1_ct,
  family = binomial(link = "logit"))


# https://www.andrewheiss.com/blog/2023/05/15/fancy-bayes-diffs-props/#the-questions