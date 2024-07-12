rm(list = ls())

# Libs and funs ----
library(lubridate)
library(tidyverse) 
library(tidyr)
`%notin%` <- Negate(`%in%`)

hours_blocks <- function(x) {
  nhours <- length(rle(x)$values)
  each_nhours <- rle(x)$lengths
  rep(seq(1, nhours ,1), each_nhours)  
} # requires already somehow grouped data; gives consecutive numbers for the distinguishable blocks


# Read meta data data ----
birdsID_df <- readxl::read_excel("C:/Users/User/Dropbox/VIP experiment/Files for VIP coordination.xlsx")

# get sex-treatment data
birdsID_dt_females <- birdsID_df %>% 
  mutate(sex = "female",
         birdID = str_sub(female, 3, nchar(female))) %>% 
  rename(treatment = fem_trt) %>% 
  select(birdID, sex, treatment) %>% 
  distinct()

birdsID_dt_males <- birdsID_df %>% 
  mutate(sex = "male",
         birdID = str_sub(male, 3, nchar(male))) %>% 
  rename(treatment = male_trt) %>% 
  select(birdID, sex, treatment) %>% 
  distinct()

birdsID_dt <- bind_rows(birdsID_dt_females, birdsID_dt_males)

# get nest data

nest_dt_females <- birdsID_df %>% 
  mutate(role = "female_parent",
         birdID = str_sub(female, 3, nchar(female))) %>% 
  select(nest, year, hatch_date, p6, p14, wave, role, birdID, file_name, min_age) %>% 
  distinct()

nest_dt_males <- birdsID_df %>% 
  mutate(role = "male_parent",
         birdID = str_sub(male, 3, nchar(male))) %>% 
  select(nest, year, hatch_date, wave, role, birdID, file_name, min_age) %>% 
  distinct()

nest_dt <- bind_rows(nest_dt_females, nest_dt_males)


# Read all RFID data ----

folders_list <- list.files("C:/Users/User/Dropbox/VIP experiment/rfid_data")

folder_files_df <- list()

for(j in 1: length(folders_list)) {
  
  folder_path <- paste0("C:/Users/User/Dropbox/VIP experiment/rfid_data/", folders_list[j], sep = "")
  
  files_in_folder <- list.files(path = folder_path)
  
  # inner loop - start (add read each file from year-folder and add the file ID)
  files_df <- list()
  
  for(i in 1:length(files_in_folder)) {
    file_path <- paste0(folder_path, "/", files_in_folder[i], sep = "")
    files_df[[i]] <- readr::read_csv2(file_path, id = "file_id")
  }
  
  
  folder_files_df_temp <- plyr::ldply(files_df, data.frame)
  # inner loop - end
  
  folder_files_df[[j]] <- folder_files_df_temp
  
}

rfid_alldt <- plyr::ldply(folder_files_df, data.frame)



# Data checking/tuning ----

rfid_alldt_temp_df <- rfid_alldt %>% 
  mutate(date = mdy(Date.),
         date_time = ymd_hms(paste0(date, " ", as.character(Time.), sep = "")),
         year = as.integer(str_sub(file_id, 48,51)),
         updated_date_time = update(date_time, year = year),
         birdID = str_sub(Transponder.Code., 4, nchar(Transponder.Code.)),
         file_name = str_sub(file_id, 53, nchar(file_id)-4),
         rownms = row_number())

# in some files dates don't parse (dmy instead mdy as in most of the files) 
badrecords1 <- rfid_alldt_temp_df %>% # bad records - list
  filter(is.na(date)) %>% 
  group_by(rownms, Date.) %>% 
  summarise(n = n()) 

rfid_alldt_temp_df_bad1 <- rfid_alldt_temp_df %>% # bad records - fixing
  filter(rownms %in% unique(badrecords1$rownms)) %>% 
  mutate(date = dmy(Date.),
         date_time = ymd_hms(paste0(date, " ", as.character(Time.), sep = "")),
         updated_date_time = update(date_time, year = year))

rfid_alldt_temp_df_good1 <- rfid_alldt_temp_df %>% # good records only
  filter(rownms %notin% unique(badrecords1$rownms))


rfid_alldt_temp_df2 <- bind_rows(rfid_alldt_temp_df_good1, rfid_alldt_temp_df_bad1) # good and bad records

# still in  some files dates don't parse but those are simply missing date - to remove
badrecords2 <- rfid_alldt_temp_df2 %>% # bad records - check
  filter(is.na(date_time))

rfid_alldt_temp_df3 <- rfid_alldt_temp_df2 %>% 
  filter(!is.na(updated_date_time)) %>% 
  select(rownms, file_name, year, updated_date_time, birdID)


# add sex and treatment data

rfid_alldt_temp_df4 <- left_join(rfid_alldt_temp_df3, birdsID_dt, by = "birdID")

# fixing some artefacts
rfid_alldt_temp_df4 <- rfid_alldt_temp_df4 %>% 
  mutate(birdID = if_else(birdID == "3308", "7143308", birdID))

rfid_alldt_temp_df4 <- rfid_alldt_temp_df4 %>% 
  mutate(birdID = if_else(birdID == "1499", "7141499", birdID))

rfid_alldt_temp_df4 <- rfid_alldt_temp_df4 %>% 
  mutate(birdID = if_else(birdID == "1719", "7141719", birdID))

rfid_alldt_temp_df4 <- rfid_alldt_temp_df4 %>% 
  mutate(birdID = if_else(birdID == "1996", "7141996", birdID))



# add parent, nest, hatching date/wave info

rfid_alldt_temp_df5 <- left_join(rfid_alldt_temp_df4, nest_dt, by = c("year", "birdID", "file_name"))


# experiment/nest grouping

rfid_alldt_temp_df5 <- rfid_alldt_temp_df5 %>% 
  mutate(sx_treat = paste0(sex, "_", treatment, sep = ""))

rfid_alldt_temp_df5_temp <- rfid_alldt_temp_df5 %>% 
  filter(!is.na(role))
nest_groups <- rfid_alldt_temp_df5_temp %>% 
  select(file_name, sex, treatment) %>% 
  distinct() %>% 
  pivot_wider(values_from = treatment, names_from = sex) %>% 
  mutate(fm_treatmet = paste0(female, "_", male, sep = "")) %>% 
  select(file_name, fm_treatmet) 

# summary table for exp groups
nest_groups %>% 
  group_by(fm_treatmet) %>% 
  summarise(n = n())

rfid_alldt_temp_df6 <- left_join(rfid_alldt_temp_df5, nest_groups, by = "file_name")

rfid_alldt_temp_df6 <- rfid_alldt_temp_df6  %>% 
  mutate(fm_treatment_pooled = case_when(
    fm_treatmet == "control_NA" ~ "control_control",
    fm_treatmet == "androgen_NA" ~ "androgen_control",
    fm_treatmet == "NA_control" ~ "control_control",
    fm_treatmet == "NA_androgen" ~ "control_androgen",
    fm_treatmet == "NA_NA" ~ "control_control",
    .default = as.character(fm_treatmet)
  ))



# add hours/rec
rfid_alldt_temp_df6 <- rfid_alldt_temp_df6 %>% 
  mutate(hr = hour(updated_date_time)) %>% 
  group_by(file_name) %>% 
  mutate(hr_rec = hours_blocks(hr))

# tuning up 
rfid_alldt_temp_df6 <- rfid_alldt_temp_df6 %>% 
  rename(date_time = updated_date_time,
         ind_treatment = treatment) %>% 
  select(-sx_treat)

# fix chicks age
rfid_alldt_temp_df6 <- rfid_alldt_temp_df6 %>% 
  group_by(year, file_name) %>% 
  fill(p6, .direction = "down")


saveRDS(rfid_alldt_temp_df6, "01_preprocessed_data.rds")

