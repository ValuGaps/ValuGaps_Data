

rm(list=ls())

library(tidyverse)
library(readxl)
library(ggpubr)
library(purrr)

# source("Scripts/get_data.R")

# Get rid of annoying dates
# filerename <-function(x){
# 
#  file.rename(x, gsub("__", "_", x))
#   return(x)
# }
# 
# purrr::map(list.files("data/", full.names = TRUE, recursive = T), filerename)

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
      
      # Convert character variables to appropriate types
      across(where(is.character), ~ type.convert(.x, as.is = TRUE)),
      
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
      )
    )
  
  return(raw_data)
}


# Read all files into a list of data frames
raw_data <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
  purrr::set_names(gsub("_covariates.xlsx","",basename(.))) %>% 
  map(read_cov)

















         



##Question 1
labels <- c(
  "1" = "less than 10%",
  "2" = "around 25 %",
  "3" = "appr. 50%",
  "4" = "around 75 %",
  "5" = "more than 90 %"
)
data$q10 <- factor(data$q10, levels = c(1, 2, 3, 4, 5))



##Question 2
#
calculate_share <- function(data, miss_col, corr_col, label) {
  data %>%
    summarise(
      count_miss = sum(!is.na(get(miss_col))),
      count_corr = sum(!is.na(get(corr_col))),
      total_count = count_miss + count_corr,
      share_corr = count_corr / total_count * 100
    ) %>%
    mutate(
      Issue = label
    )
}

hnv1_share <- calculate_share(data, "hnv1_miss", "hnv1_corr", "Rich Small Water Bodies and Ditches")
hnv2_share <- calculate_share(data, "hnv2_miss", "hnv2_corr", "Flower Strips")
hnv3_share <- calculate_share(data, "hnv3_miss", "hnv3_corr", "Hedges")
hnv4_share <- calculate_share(data, "hnv4_miss", "hnv4_corr", "Field Shrubs")
hnv5_share <- calculate_share(data, "hnv5_miss", "hnv5_corr", "Straightened Rivers")
hnv6_share <- calculate_share(data, "hnv6_miss", "hnv6_corr", "Wide Forest Roads")

# Combine all shares into one data frame
share_data <- bind_rows(hnv1_share, hnv2_share, hnv3_share, hnv4_share, hnv5_share, hnv6_share)





##Question 3
labels <- c(
  "1" = "In scattered small areas",
  "2" = "Grouped together",
  "3" = "Both scattered and grouped"
)
data$q14 <- factor(data$q14, levels = c(1, 2, 3))






educ_summary <- data %>%
  count(educ) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(n))















# Step 2: Calculate the share of people who zoomed for each getZoom column
zoom_summary_mobile <- database %>%
  group_by(is_mobile) %>%
  summarise(across(starts_with("zoom_getZoom"), mean, na.rm = TRUE))

# Step 3: Reshape the data to long format for plotting
zoom_summary_long_mobile <- zoom_summary_mobile %>%
  pivot_longer(cols = starts_with("zoom_getZoom"),
               names_to = "Zoom_Variable",
               values_to = "Zoom_Share") %>%
  mutate(Zoom_Variable = factor(Zoom_Variable, 
                                levels = paste0("zoom_getZoom", 1:10)),
         is_mobile = ifelse(is_mobile == "true", "Mobile", "Non-Mobile"))  # Rename for plot

# Step 4: Plot with lines and points, separating mobile and non-mobile
zoom_cc_card <- ggplot(zoom_summary_long_mobile, aes(x = Zoom_Variable, y = Zoom_Share, group = is_mobile, color = is_mobile)) +
  geom_line(size = 1) +   # Line connecting the points
  geom_point(size = 3) +  # Points on the line
  labs(x = "Zoom Variable", y = "Share of People Who Zoomed", color = "Device Type", 
       title = "Share of People Who Zoomed per Choice Card") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# Apply the function to create a new column for the device category in the dataframe
data <- data %>%
  mutate(device_category = sapply(respondent_ua, extract_device_category))



raw_data <- raw_data %>% 
  mutate(device_type = sapply(respondent_ua, extract_device_type)) %>% 
  mutate(device_category = sapply(respondent_ua, extract_device_category))


##### Merge with pretest data ####


source("Scripts/add_pretest_data.R")

source("Scripts/add_main1_data.R")


fullchoice_all <- bind_rows(fullchoice, fullchoice_main1, fullchoice_pre) 

fullchoice_raw_pre$RID <- as.numeric(fullchoice_raw_pre$RID)

raw_data_pre$RID <- as.numeric(raw_data_pre$RID)

fullchoice_all_raw <- bind_rows(fullchoice_raw, fullchoice_raw_main1, fullchoice_raw_pre)

raw_data_all <- bind_rows(raw_data, raw_data_main1, raw_data_pre)

time_stamps_all <- bind_rows(time_stamps_raw, time_stamps_main1, time_stamps_pre)

data_pre <- data_pre %>% mutate(q10 = as.factor(q10),
                                q14 = as.factor(q14))  


data_all <- bind_rows(data, data_main1, data_pre)
