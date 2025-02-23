

rm(list=ls())

library(tidyverse)
library(readxl)
library(ggpubr)
library(purrr)
library(tidyr)

# source("Scripts/get_data.R")

#Get rid of annoying dates


filerename <-function(x){

 #file.rename(x, gsub("\\d{4}[-_]\\d{2}[-_]\\d{2}", "", x))
  file.rename(x, gsub("__", "_", x))
  return(x)
}

purrr::map(list.files("data/", full.names = TRUE, recursive = T), filerename)

###### Readin all covariate files  ##### 



read_cov <- function(x) {
  
  # Function to categorize device based on user agent string
  extract_device_category <- function(ua_string) {
    if (grepl("Windows", ua_string, ignore.case = TRUE)) {
      return("Laptop/Desktop")
    } else if (grepl("Macintosh|Mac OS X", ua_string, ignore.case = TRUE)) {
      return("Laptop/Desktop")
    } else if (grepl("CrOS", ua_string, ignore.case = TRUE)) {
      return("Laptop/Desktop")  # Chrome OS devices are typically laptops
    } else if (grepl("Android", ua_string, ignore.case = TRUE)) {
      if (grepl("Mobile", ua_string, ignore.case = TRUE)) {
        return("Mobile Phone")
      } else {
        return("Tablet")
      }
    } else if (grepl("iPhone", ua_string, ignore.case = TRUE)) {
      return("Mobile Phone")
    } else if (grepl("iPad", ua_string, ignore.case = TRUE)) {
      return("Tablet")
    } else {
      return("Other/Unknown")  # Default category for unidentified devices
    }
  }
  
  # Function to extract specific device type
  extract_device_type <- function(ua_string) {
    if (grepl("Windows", ua_string, ignore.case = TRUE)) {
      return("Windows")
    } else if (grepl("Macintosh|Mac OS X", ua_string, ignore.case = TRUE)) {
      return("Mac")
    } else if (grepl("Linux", ua_string, ignore.case = TRUE)) {
      return("Linux")
    } else if (grepl("Android", ua_string, ignore.case = TRUE)) {
      return("Android")
    } else if (grepl("iPhone", ua_string, ignore.case = TRUE)) {
      return("iPhone")
    } else if (grepl("iPad", ua_string, ignore.case = TRUE)) {
      return("iPad")
    } else if (grepl("CrOS", ua_string, ignore.case = TRUE)) {
      return("Chrome OS")
    } else {
      return("Other")  # Default category for unidentified operating systems
    }
  }
  
  # Identify DCE files in the same directory and read them
  dcepath <- list.files(dirname(x), full.names = TRUE, pattern = "DCE")
  
  # Read and merge DCE data
  dcedata <- dcepath %>%
    purrr::set_names(gsub("\\.xlsx", "", basename(.))) %>%
    map(read_excel) %>%
    bind_rows(.id = "dce_source") %>%
    mutate(pref1 = ifelse(grepl("swap", dce_source), 3 - pref1, pref1))  # Adjust pref1 based on dce_source
  
  # Read main dataset and process columns
  raw_data <- read_excel(x) %>%
    rename(lat = latlng_wood_SQ_1_1, lon = latlng_wood_SQ_1_2,
           lat_tc = latlng_wood_SQ2_1_1, lon_tc = latlng_wood_SQ2_1_2) %>%
    mutate(
      RID = as.numeric(RID),
      survey_round = gsub("_covariates.xlsx", "", basename(x)),

      
      # Recoding STATUS variable
      STATUS_recoded = case_when(
        STATUS == 1 ~ "New respondent",
        STATUS == 2 ~ "Invalid entry",
        STATUS == 3 ~ "Pending",
        STATUS == 4 ~ "Over quota on Segment Assignment",
        STATUS == 5 ~ "Rejected",
        STATUS == 6 ~ "Started",
        STATUS == 7 ~ "Complete",
        STATUS == 8 ~ "Screened out",
        STATUS == 9 ~ "User initiated timeout",
        STATUS == 10 ~ "System initiated timeout",
        STATUS == 11 ~ "Bad parameters",
        STATUS == 12 ~ "Survey closed",
        STATUS == 13 ~ "System error",
        STATUS == 14 ~ "Reentrant",
        STATUS == 15 ~ "Active",
        STATUS == 16 ~ "Over Quota at Start",
        STATUS == 17 ~ "Manually Screened Out",
        TRUE ~ NA_character_
      ),
      
      # Recoding gender
      gender_chr = case_when(
        gender == 1 ~ "male",
        gender == 2 ~ "female",
        gender == 3 ~ "diverse",
        gender == 4 ~ "na"
      ),
      
      # Convert percentage-based numeric variables
      sq_hnv_share = as.numeric(gsub("%", "", sq_hnv_share)),
      sq_pa_share = as.numeric(gsub("%", "", sq_pa_share)),
      cv = as.numeric(cv),
      birthyear = as.numeric(birthyralt_other),
      
      # Recode satisfaction, health, and income variables
      lifesat_recode = coalesce(lifesat, lifesat_mobile) - 1,
      healthphys_recode = coalesce(healthphys, healthphys_mobile) - 1,
      healthpsych_recode = coalesce(healthpsych, healthpsych_mobile) - 1,
      
      # Recode urban vs rural category
      urban_rural = case_when(
        q2_1 < 3 ~ "Village",
        q2_1 < 5 ~ "Small City",
        q2_1 < 7 ~ "Large City",
        TRUE ~ NA_character_
      ),
      
      # Compute time spent in minutes
      hours_spend = replace_na(as.numeric(hours_spend), 0),
      minutes_spend = replace_na(as.numeric(minutes_spend), 0),
      time_spend_tc = hours_spend * 60 + minutes_spend,
      
      # Device-related classifications
      device_type = sapply(respondent_ua, extract_device_type),
      device_category = sapply(respondent_ua, extract_device_category)
    ) %>%
    
    # Merge timestamps and DCE data
    left_join(read_excel(gsub("covariates", "timestamps", x)), by = "RID") %>%
    left_join(dcedata, by = "RID") %>%
    
    # Compute total preference scores and categorize protester types
    group_by(RID) %>%
    mutate(
      total_pref1 = sum(pref1, na.rm = TRUE),
      protester = case_when(
        total_pref1 == 20 ~ 0,
        total_pref1 > 13 ~ 1,
        total_pref1 > 10 & total_pref1 <= 13 ~ 2,
        total_pref1 == 10 ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    
    # Create dummy variables for specific conditions
    mutate(
      Dummy_pa_half = case_when(a2_x2 == 2 ~ 1, TRUE ~ 0),
      Dummy_pa_full = case_when(a2_x2 == 3 ~ 1, TRUE ~ 0),
      Dummy_hnv_visible = case_when(a2_x4 == 2 ~ 1, TRUE ~ 0),
      
      # Compute hnv_att and pa_att based on experiment type and response
      hnv_att = case_when(
        grepl("exp$|swap$", dce_source) & a2_x3 == 1 ~ sq_hnv_area + 100,
        grepl("exp_2$|swap_2$", dce_source) & a2_x3 == 1 ~ sq_hnv_area + 200
      ),
      pa_att = case_when(
        grepl("exp$|swap$", dce_source) & a2_x1 == 1 ~ sq_pa_area + 100,
        grepl("exp_2$|swap_2$", dce_source) & a2_x1 == 1 ~ sq_pa_area + 200
      ),
      
      # Assign cost based on response levels
      cost_att = case_when(
        a2_x5 == 1 ~ 5, a2_x5 == 2 ~ 10, a2_x5 == 3 ~ 20,
        a2_x5 == 4 ~ 40, a2_x5 == 5 ~ 60, a2_x5 == 6 ~ 80,
        a2_x5 == 7 ~ 120, a2_x5 == 8 ~ 150, a2_x5 == 9 ~ 200, a2_x5 == 10 ~ 250
      ),
      
      # Convert character variables to appropriate types
      across(where(is.character), ~ type.convert(.x, as.is = TRUE)),
      across(any_of(c("lat","lon","birthyralt_other","natvisit_company","forest_size", "natvisit_next12m", "false_zip")), as.numeric),
    )
  
  return(raw_data)
}


# Read all files into a list of data frames
raw_data <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
  purrr::set_names(gsub("_covariates.xlsx","",basename(.))) %>% 
  map(read_cov)





all_data <- bind_rows(raw_data, .id = "survey_round")

survey_round_map <- all_data %>%
  distinct(survey_round) %>%
  mutate(prefix = row_number() * 10000) %>%  # Assign a unique 10,000s prefix
  deframe()

# Generate `RID_unique`
all_data <-all_data %>%
  mutate(RID_unique = survey_round_map[survey_round] + RID)


database <- all_data %>% 
  filter(STATUS_recoded == "Complete")




         



















