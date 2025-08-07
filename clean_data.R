



library(tidyverse)
library(readxl)
library(purrr)
library(tidylog)
library(sf)

# source("Scripts/get_data.R")

#Get rid of annoying dates

# 
# filerename <-function(x){
# 
#  file.rename(x, gsub("_\\d{4}[-_]\\d{2}[-_]\\d{2}", "", x))
# #file.rename(x, gsub("VALUGAPs_Main_3_Round_5", "Main_7", x))
# 
# 
# 
#   return(x)
# }
# 
# filerename <- function(x) {
#   # Generate new names
#   new_names <- stringr::str_replace_all(x, "(DCE)|([ECTS])", function(z) {
#     if (!is.na(z[1]) && z[1] == "DCE") {
#       return("DCE")
#     } else {
#       return(tolower(z))
#     }
#   })
# 
#   # Perform the renaming
#   file.rename(x, new_names)
# 
#   # Show what changed
#   result <- data.frame(
#     original = x,
#     renamed  = new_names,
#     stringsAsFactors = FALSE
#   )
# 
#   print(result)
#   invisible(result)
# }
# 
# 
# 
# 
# purrr::map(list.files("data/main_study/Main_7/", full.names = TRUE, recursive = T), filerename)

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
    mutate(RID=as.numeric(RID), pref1 = ifelse(grepl("swap", dce_source), 3 - as.numeric(pref1), as.numeric(pref1)))  # Adjust pref1 based on dce_source

  # Read main dataset and process columns
  raw_data <- read_excel(x) %>%
    rename(lat = latlng_wood_SQ_1_1, 
           lon = latlng_wood_SQ_1_2,
           lat_tc = latlng_wood_SQ2_1_1, 
           lon_tc = latlng_wood_SQ2_1_2,
           
           # Renaming the specified columns
           participation_consent = q1,
           res_settlement_type = q2_1,
           knowledge_hnv_share = q10,
           knowledge_pa_effectiveness = q14,
           estimated_hnv_near_residence = q16,
           estimated_pa_near_residence = q18,
           envisioned_levy_distribution = q27_2,

           # Renaming the q27_1_1 to q27_1_11 columns for clarity
           eval_time_taken = q27_1_1,
           eval_survey_meaningful = q27_1_2,
           eval_response_consistency = q27_1_3,
           eval_conscientious_reading = q27_1_4,
           eval_attention_check = q27_1_5,
           eval_understanding_difficulty = q27_1_6,
           eval_alt_distinction_difficulty = q27_1_7,
           eval_cost_realism = q27_1_8,
           eval_referendum_realism = q27_1_9,
           eval_measures_effectiveness_belief = q27_1_10,
           eval_policy_relevance_assumption = q27_1_11
    ) %>%
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
      gender_male = case_when(gender==1 ~ 1, TRUE~0),
      
      # Convert percentage-based numeric variables
      sq_hnv_share = as.numeric(gsub("%", "", sq_hnv_share)),
      sq_pa_share = as.numeric(gsub("%", "", sq_pa_share)),
      cv = as.numeric(cv),
      birthyear_uncleaned = as.numeric(birthyralt_other),
      birthyear = ifelse(birthyear_uncleaned < 1900 | birthyear_uncleaned > 3000, NA, birthyear_uncleaned),
      
      
      # Recode satisfaction, health, and income variables
      lifesat_recode = coalesce(lifesat, lifesat_mobile) - 1,
      healthphys_recode = coalesce(healthphys, healthphys_mobile) - 1,
      healthpsych_recode = coalesce(healthpsych, healthpsych_mobile) - 1,
      
      
      # recode househole income
            hhnetinc_recode = factor(hhnetinc, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'),
                               labels = c('less than 500 Euro', '500 - 999 Euro', '1000 - 1499 Euro',
                                          '1500 - 1999 Euro', '2000 - 2499 Euro', '2500 - 2999 Euro',
                                          '3000 - 3499 Euro', '3500 - 3999 Euro', '4000 - 4999 Euro',
                                          'more than 5000 Euro', 'k.A.')),
      hhnetinc_numeric = case_when(
        hhnetinc == '1' ~ 250,
        hhnetinc == '2' ~ 750,
        hhnetinc == '3' ~ 1250,
        hhnetinc == '4' ~ 1750,
        hhnetinc == '5' ~ 2250,
        hhnetinc == '6' ~ 2750,
        hhnetinc == '7' ~ 3250,
        hhnetinc == '8' ~ 3750,
        hhnetinc == '9' ~ 4500,
        hhnetinc == '10' ~ 5500,
        TRUE ~ NA_real_  # Exclude 'k.A.'
      ),
      
      # Recode voting and protest variables
      voting = factor(pol_btw, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                      labels = c("CDU/CSU", "SPD", "BSW", "AfD", "Die Linke", "Die Grünen", "FDP", "Keine Angabe", "Sonstige")),
      protest_1_recode = factor(protest_1, levels = c("1", "2", "3", "4", "5", "6"),
                                labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
      protest_2_recode = factor(protest_2, levels = c("1", "2", "3", "4", "5", "6"),
                                labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
      protest_3_recode = factor(protest_3, levels = c("1", "2", "3", "4", "5", "6"),
                                labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
      protest_4_recode = factor(protest_4, levels = c("1", "2", "3", "4", "5", "6"),
                                labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
      protest_5_recode = factor(protest_5, levels = c("1", "2", "3", "4", "5", "6"),
                                labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
      
      # Recode urban vs rural category
      urban_rural = case_when(
        res_settlement_type < 3 ~ "Small City",
        res_settlement_type < 5 ~ "Medium-size City",
        res_settlement_type < 7 ~ "Large City",
        TRUE ~ NA_character_
      ),
      dogowner = case_when(
        dog %in% c(1, 2) ~ 1,  # Assign 1 if dog is 1 or 2
        dog == 3 ~ 2,          # Assign 2 if dog is 3
        TRUE ~ NA_real_        # Assign NA for all other cases
      ),
      
      # Compute time spent in minutes
      hours_spend = replace_na(as.numeric(hours_spend), 0),
      minutes_spend = replace_na(as.numeric(minutes_spend), 0),
      time_spend_tc = hours_spend * 60 + minutes_spend,
      zoom_first_cc = case_when(getZoom1 > 0 ~ 1, TRUE~0),
      payment_distribution = case_when(envisioned_levy_distribution == 1 ~ "Progressive", envisioned_levy_distribution == 2 ~ "Nobody pays", envisioned_levy_distribution == 3 ~ "Equal", envisioned_levy_distribution == 4 ~"Not thought about it"),
      payment_vision = if ("q1171" %in% names(.)) {
        case_when(
          q1171 == 1 ~ "Every household the same",
          q1171 == 2 ~ "Relative to their income tax",
          q1171 == 3 ~ "Do not care about distribution",
          q1171 == 4 ~ "Other"
        )
      } else {
        NA_character_  # Return NA if q1171 does not exist
      },
      
      # Device-related classifications
      device_type = sapply(respondent_ua, extract_device_type),
      device_category = sapply(respondent_ua, extract_device_category)
    ) %>%
    
    # Merge timestamps and calculate time spend on page
    left_join(read_excel(gsub("covariates", "timestamps", x) , col_types = "numeric") %>% mutate(RID=as.numeric(RID)), by = "RID") %>%
    mutate(across(starts_with("PAGE_SUBMIT"), ~ . - get(sub("SUBMIT", "DISPLAY", cur_column())), .names = "time_spent_{col}")) %>% 
    

    
    # Merge DCE data
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
      Dummy_pa_no = case_when(a2_x2 == 1 ~ 1, TRUE ~ 0),
      Dummy_hnv_no = case_when(a2_x4 == 1 ~ 1, TRUE ~ 0),
      # Compute hnv_att and pa_att based on experiment type and response
      hnv_att = case_when(
        dce_version %in% c(1, 2) & a2_x3 == 1 ~ sq_hnv_area + 100,
        dce_version %in% c(1, 2) & a2_x3 == 2 ~ sq_hnv_area + 200,
        dce_version %in% c(1, 2) & a2_x3 == 3 ~ sq_hnv_area + 300,
        dce_version %in% c(1, 2) & a2_x3 == 4 ~ sq_hnv_area + 500,
        dce_version %in% c(1, 2) & a2_x3 == 5 ~ sq_hnv_area + 800,
        
        dce_version %in% c(3, 4) & a2_x3 == 1 ~ sq_hnv_area + 200,
        dce_version %in% c(3, 4) & a2_x3 == 2 ~ sq_hnv_area + 400,
        dce_version %in% c(3, 4) & a2_x3 == 3 ~ sq_hnv_area + 600,
        dce_version %in% c(3, 4) & a2_x3 == 4 ~ sq_hnv_area + 1000,
        dce_version %in% c(3, 4) & a2_x3 == 5 ~ sq_hnv_area + 1600
      ),
      pa_att = case_when(
        dce_version %in% c(1, 2) & a2_x1 == 1 ~ sq_pa_area + 100,
        dce_version %in% c(1, 2) & a2_x1 == 2 ~ sq_pa_area + 200,
        dce_version %in% c(1, 2) & a2_x1 == 3 ~ sq_pa_area + 300,
        dce_version %in% c(1, 2) & a2_x1 == 4 ~ sq_pa_area + 500,
        dce_version %in% c(1, 2) & a2_x1 == 5 ~ sq_pa_area + 800,
        
        dce_version %in% c(3, 4) & a2_x1 == 1 ~ sq_pa_area + 200,
        dce_version %in% c(3, 4) & a2_x1 == 2 ~ sq_pa_area + 400,
        dce_version %in% c(3, 4) & a2_x1 == 3 ~ sq_pa_area + 600,
        dce_version %in% c(3, 4) & a2_x1 == 4 ~ sq_pa_area + 1000,
        dce_version %in% c(3, 4) & a2_x1 == 5 ~ sq_pa_area + 1600),
      
      # Assign cost based on response levels
      cost_att = case_when(
        grepl("pilot", survey_round) ~ case_when(
          a2_x5 == 1 ~ 5, 
          a2_x5 == 2 ~ 10, 
          a2_x5 == 3 ~ 40,
          a2_x5 == 4 ~ 80, 
          a2_x5 == 5 ~ 120, 
          a2_x5 == 6 ~ 150,
          a2_x5 == 7 ~ 200, 
          a2_x5 == 8 ~ 250,
          TRUE ~ NA_real_
        ),
        grepl("Main", survey_round) ~ case_when(
          a2_x5 == 1 ~ 5, 
          a2_x5 == 2 ~ 10, 
          a2_x5 == 3 ~ 20,
          a2_x5 == 4 ~ 40, 
          a2_x5 == 5 ~ 60, 
          a2_x5 == 6 ~ 80,
          a2_x5 == 7 ~ 120, 
          a2_x5 == 8 ~ 150, 
          a2_x5 == 9 ~ 200, 
          a2_x5 == 10 ~ 250,
          TRUE ~ NA_real_
        ),
        TRUE ~ NA_real_  # Default case for unexpected values
      )
      ,
      
      scope_dce = case_when(dce_version %in% c(1,2)~"low", TRUE~"high"),
      
      survey_round_pooled = case_when(grepl("pilot", survey_round) ~ "Pilot", TRUE ~ "Main"),
      device = case_when(
        str_detect(respondent_ua, "iPhone") ~ "iPhone",
        str_detect(respondent_ua, "iPad") ~ "iPad",
        str_detect(respondent_ua, "Android") ~ "Android",
        TRUE ~ "Laptop/Other"
      ),
      NR_score = ifelse(uhh != 3 & nr6_1 != 6 & nr6_2 != 6 & nr6_3 != 6 &
                                 nr6_4 != 6 & nr6_5 != 6 & nr6_6 != 6,
                               (nr6_1 + nr6_2 + nr6_3 + nr6_4 + nr6_5 + nr6_6) / 6, 
                               NA),
      
      # Convert character variables to appropriate types
      across(where(is.character), ~ type.convert(.x, as.is = TRUE)),
      across(any_of(c("lat","lon","birthyralt_other","natvisit_company","forest_size", "natvisit_next12m", "false_zip", "allocationa_0630", "allocationb_0630", "slope_0630", "to_a_max_0630",
                      "to_b_max_0630", "allocationa_0820", "allocationb_0820", "slope_0820" ,"to_a_max_0820", "to_b_max_0820", "hhsize", "postcode", "pref1")), as.numeric),
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
  mutate(prefix = row_number() * 100000) %>%  # Assign a unique 10,000s prefix
  deframe()

# Generate `RID_unique`
all_data <-all_data %>%
  mutate(RID_unique = survey_round_map[survey_round] + RID) %>% 
  arrange(RID_unique) %>% 
  rename(RID_sample = RID, RID=RID_unique,
         preferred_levy_distribution = q1171,
                  visited_nature_last12m = tc1,
                  natvisit_last12m_estimation_basis = nv_2a,
                  natvisit_fav_estimation_basis = nv_4a)

all_data_complete <- all_data %>% 
  filter(STATUS_recoded == "Complete")
 
median_dur <- median(all_data_complete$DURATION)

database <- all_data %>% 
  filter(STATUS_recoded == "Complete") %>% filter(DURATION >= 1/3*median_dur,
                                                  eval_attention_check == 4)

all_data <- all_data %>%   distinct(RID,.keep_all=TRUE)

complete_data <- database %>% 
  distinct(RID,.keep_all=TRUE) %>% 
  filter(STATUS_recoded == "Complete") %>% filter(DURATION >= 1/3*median_dur,
                                                  eval_attention_check == 4)

rm(read_cov, survey_round_map, all_data_complete)





# Assign administrative names by spatial join - based on lat,lon
germany_admin <- read_sf("secondary_data/germany_shapefiles/", "gadm41_DEU_4") %>%
  select(NAME_1, NAME_2, NAME_3, NAME_4, CC_4)

admin_assignments <- complete_data %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(germany_admin, left = FALSE) %>%
  rename(
    federal_state = NAME_1,
    county_name = NAME_2,
    municipality_name = NAME_3,
    town_name = NAME_4,
    ARS = CC_4
  ) %>%
  st_drop_geometry() %>%
  select(RID, federal_state, county_name, municipality_name, town_name, ARS)

complete_data <- complete_data %>% left_join(admin_assignments, by = "RID")

# 20 observations without assignment: Border cases -> keep and find next closest?
# Handle missing town_name by nearest polygon fallback
missing_admin <- complete_data %>%
  filter(is.na(town_name) & !is.na(lat) & !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

if (nrow(missing_admin) > 0) {
  nearest_idx <- st_nearest_feature(missing_admin, germany_admin)
  nearest_fallback <- tibble(
    RID = missing_admin$RID,
    federal_state_nearest = germany_admin$NAME_1[nearest_idx],
    county_name_nearest = germany_admin$NAME_2[nearest_idx],
    municipality_name_nearest = germany_admin$NAME_3[nearest_idx],
    town_name_nearest = germany_admin$NAME_4[nearest_idx],
    ARS_nearest = germany_admin$CC_4[nearest_idx]
  ) %>%
    group_by(RID) %>%
    slice(1) %>%
    ungroup()
  
  complete_data <- complete_data %>%
    left_join(nearest_fallback, by = "RID") %>%
    mutate(
      federal_state = coalesce(federal_state, federal_state_nearest),
      county_name = coalesce(county_name, county_name_nearest),
      municipality_name = coalesce(municipality_name, municipality_name_nearest),
      town_name = coalesce(town_name, town_name_nearest),
      ARS = coalesce(ARS, ARS_nearest)
    ) %>%
    select(-ends_with("_nearest"))
}

rm(admin_assignments, missing_admin, nearest_fallback, germany_admin)

# # List of renamed variables (Aug 06) - lookup
# participation_consent = q1,
# res_settlement_type = q2_1,
# knowledge_hnv_share = q10,
# knowledge_pa_effectiveness = q14,
# estimated_hnv_near_residence = q16,
# estimated_pa_near_residence = q18,
# envisioned_levy_distribution = q27_2,
# preferred_levy_distribution = q1171,
# visited_nature_last12m = tc1,
# natvisit_last12m_estimation_basis = nv_2a,
# natvisit_fav_estimation_basis = nv_4a,
# most_visited_nature_type = q91test_group,
# 
# eval_time_taken = q27_1_1,
# eval_survey_meaningful = q27_1_2,
# eval_response_consistency = q27_1_3,
# eval_conscientious_reading = q27_1_4,
# eval_attention_check = q27_1_5,
# eval_understanding_difficulty = q27_1_6,
# eval_alt_distinction_difficulty = q27_1_7,
# eval_cost_realism = q27_1_8,
# eval_referendum_realism = q27_1_9,
# eval_measures_effectiveness_belief = q27_1_10,
# eval_policy_relevance_assumption = q27_1_11