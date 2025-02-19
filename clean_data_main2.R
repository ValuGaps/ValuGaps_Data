library(tidyverse)
library(purrr)
# Function to determine the most preferred type
choose_best_type <- function(types) {
  if ("double" %in% types) {
    return("as.numeric")  # Prioritize numeric
  } else if ("integer" %in% types) {
    return("as.numeric")  # Convert integer to numeric for consistency
  } else {
    return("as.character")  # If no numeric types, default to character
  }
}

# Read all files into a list of data frames
df_list <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
  map(read_cov)

# Ensure all datasets contain the 'samplename' column
df_list <- map(df_list, ~ .x %>% mutate(samplename = as.character(samplename)))

# Extract column types for each dataset
all_col_types_df <- map_dfr(df_list, function(df) {
  tibble(
    samplename = unique(df$samplename),
    column = colnames(df),
    type = sapply(df, class)
  )
})

# Reshape to wide format for easier comparison
all_col_types_wide <- all_col_types_df %>%
  pivot_wider(names_from = samplename, values_from = type)

# Determine the best type for each column
best_types <- all_col_types_df %>%
  group_by(column) %>%
  summarise(best_type = choose_best_type(unique(type)), .groups = "drop") %>%
  deframe()

# Function to enforce correct types in each dataframe
standardize_types <- function(df, best_types) {
  # Add missing columns with the correct NA type
  for (col in names(best_types)) {
    if (!col %in% colnames(df)) {
      df[[col]] <- if (best_types[[col]] == "as.numeric") NA_real_ else NA_character_
    }
  }
  
  # Apply type conversion only to existing columns
  df <- df %>%
    mutate(across(all_of(intersect(names(df), names(best_types))), 
                  ~ match.fun(best_types[[cur_column()]])(.x)))  # Removed .names to keep original names
  
  return(df)  # Ensure the modified df is returned
}

# Apply standardization to all data frames
df_list <- map(df_list, ~ standardize_types(.x, best_types))

# Bind rows with corrected types
covariates <- bind_rows(df_list) %>%
  select(RID, samplename, everything())

# Get unique samplenames
samplenames <- unique(covariates$samplename)

# Assign numeric prefixes
samplename_map <- setNames(seq_along(samplenames), samplenames)

# Apply transformation to create unique numeric IDs
covariates <- covariates %>% 
                 mutate(
                   unique_RID = (paste0(samplename,"_",  RID))
                 ) %>%
                 select(unique_RID, everything()) # Move unique_RID to the front


addID <- 222 # id marker for new survey round to get unique ids



raw_data <- read_excel(data_path) %>%
  mutate(RID = as.numeric(paste0(RID, addID)),  
         birthyralt_other = as.numeric(birthyralt_other),
         natvisit_next12m = as.character(natvisit_next12m),
         survey_round = "main2") 


time_stamps <- read_excel("data/Main_2/VALUGAPS_Main_2__Timestamps.xlsx") %>% 
  mutate(survey_round ="main2",  RID = paste0(RID, addID), RID = as.numeric(RID))

choice_exp_1 <- read_xlsx("data/Main_2/VALUGAPS_Main_2_DCE_Exp.xlsx") 
choice_exp_swap_1 <- read_xlsx("data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap.xlsx")
choice_exp_2 <- read_xlsx("data/Main_2/VALUGAPS_Main_2__DCE_Exp_2.xlsx")
choice_exp_swap_2 <- read_xlsx("data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap_2.xlsx")

choicedata_low <- read_xlsx("data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap.xlsx") %>% 
  mutate(pref1= case_when(
    pref1 == 1 ~ 2,
    pref1 == 2 ~ 1,
    TRUE ~ NA)
  ) %>% 
  bind_rows(read_xlsx("data/Main_2/VALUGAPS_Main_2_DCE_Exp.xlsx"))


choicedata_high <- read_xlsx("data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap_2.xlsx") %>% 
  mutate(pref1= case_when(
    pref1 == 1 ~ 2,
    pref1 == 2 ~ 1,
    TRUE ~ NA)
  ) %>% 
  bind_rows(read_xlsx("data/Main_2/VALUGAPS_Main_2__DCE_Exp_2.xlsx"))


##### Combine data #####



choicedata <- bind_rows(choicedata_low, choicedata_high) %>% mutate(RID = paste0(RID, addID), RID = as.numeric(RID))


raw_data <- raw_data %>%
  mutate(
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
  TRUE ~ NA_character_  # Assign NA if the STATUS doesn't match any of the conditions
  ),
     gender_chr = case_when( gender == 1 ~ "male",
                             gender == 2 ~ "female",
                             gender == 3 ~ "diverse",
                             gender == 4 ~ "na",)) %>% 
  rename(lat = latlng_wood_SQ_1_1, lon = latlng_wood_SQ_1_2) %>% 
  mutate(         lon = as.numeric(lon),
                  lat = as.numeric(lat)) %>% 
  rename(lat_tc = latlng_wood_SQ2_1_1, lon_tc = latlng_wood_SQ2_1_2) %>% 
  mutate(sq_hnv_share = as.numeric(gsub("%", "", sq_hnv_share)),
         sq_pa_share = as.numeric(gsub("%", "", sq_pa_share)),
         cv = as.numeric(cv),
         birthyear = as.numeric(birthyralt_other))






data <- raw_data %>% 
  filter(STATUS == 7) %>%
  mutate(lifesat_recode = coalesce(lifesat, lifesat_mobile)-1,
         healthphys_recode = coalesce(healthphys, healthphys_mobile)-1,
         healthpsych_recode = coalesce(healthpsych, healthpsych_mobile)-1,
         hhnetinc_recode = factor(hhnetinc, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'),
                                   labels = c('weniger als 500 Euro', '500 - 999 Euro', '1000 - 1499 Euro',
                                              '1500 - 1999 Euro', '2000 - 2499 Euro', '2500 - 2999 Euro',
                                              '3000 - 3499 Euro', '3500 - 3999 Euro', '4000 - 4999 Euro',
                                              'mehr als 5000 Euro', 'k.A.')),
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
         urban_rural = case_when(q2_1 < 3 ~ "Village", q2_1 < 5 ~ "Small City", q2_1 < 7 ~ "Large City", TRUE~NA_character_),
         hours_spend = as.numeric(hours_spend),
         minutes_spend = as.numeric(minutes_spend),
         hours_spend = replace_na(hours_spend, 0),
         minutes_spend = replace_na(minutes_spend, 0),
         time_spend_tc = hours_spend * 60  + minutes_spend,
         zoom_first_cc = case_when(getZoom1 > 0 ~ 1, TRUE~0), 
         payment_distribution = case_when(q27_2 == 1 ~ "Progressive", q27_2 == 2 ~ "Nobody pays", q27_2 == 3 ~ "Equal", q27_2 == 4 ~"Not thought about it"),
         payment_vision = case_when(q1171 == 1 ~ "Every household the same", q1171 == 2 ~ "Relative to their income tax", q1171 == 3 ~ "Do not care about distribution", q1171 == 4 ~"Other"),

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
         corr_all = if_else(
           q10 == 3 & q14 == 3 &
             is.na(hnv1_miss) & is.na(hnv2_miss) &
             is.na(hnv3_miss) & is.na(hnv4_miss) &
             is.na(hnv5_miss) & is.na(hnv6_miss),
           1, 
           0
         ),
         dogowner = case_when(
           dog %in% c(1, 2) ~ 1,  # Assign 1 if dog is 1 or 2
           dog == 3 ~ 2,          # Assign 2 if dog is 3
           TRUE ~ NA_real_        # Assign NA for all other cases
         ),
         educ = factor(educ, levels = 1:8, labels = c(
           "Derzeit Schüler*in in einer allgemeinbildenden Schule",
           "Von der Schule abgegangen ohne Schulabschluss",
           "Hauptschulabschluss",
           "Mittlere Reife (Realschulabschluss)",
           "Abitur",
           "Hochschulabschluss (Universität, FH)",
           "Will ich nicht beantworten",
           "Sonstiges"
         )),
         
  )

# Merge choice data

fullchoice <- choicedata %>% 
  left_join(data,by = "RID") %>% 
  filter(STATUS == 7)

fullchoice_raw <- choicedata %>% 
  left_join(raw_data,by = "RID")


         



voting_order <- c("CDU/CSU", "SPD", "BSW", "AfD", "Die Linke", "Die Grünen", "FDP", "Sonstige")

poll_data_0701 <- data.frame(
  voting = voting_order,
  poll = c(31.3, 16.2, 5.1, 19.1, 3.0, 13.5, 3.6, 8.2)
)

data_voting_diff <- data %>%
  filter(voting != "Keine Angabe") %>%
  group_by(voting) %>%
  summarise(count = n()) %>%
  mutate(share = round(count / sum(count) * 100, 1)) %>%
  left_join(poll_data_0701, by = "voting") %>%
  mutate(diff = share - poll)

data_voting_diff$voting <- factor(data_voting_diff$voting, levels = voting_order)

voting_pl_diff <- ggplot(data=data_voting_diff) +
  geom_bar(aes(x=voting, y=diff, fill=as.factor(voting)), stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Party", title="Difference between sample and national poll data (%pts)") +
  xlab("") +
  ylab("Share (%)") +
  scale_fill_manual(values = c("black", "red", "mediumvioletred", "blue", "deeppink", "green3", "yellow", "grey"))














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

total_pref1_scores <- fullchoice %>%
  group_by(RID) %>%
  summarise(total_pref1 = sum(pref1, na.rm = TRUE))

total_pref1_scores <- total_pref1_scores %>%
  mutate(protester = case_when(
    total_pref1 == 20 ~ 0, # Absolute supporter
    total_pref1 > 13 ~ 1,  # No protester/No absolute supporter
    total_pref1 > 10 & total_pref1 <= 13 ~ 2,  # Moderate Protester
    total_pref1 == 10 ~ 3,  # Absolute protester
    TRUE ~ NA_real_  # Handle cases where total_pref1 is NA or below threshold
  ))
#table(total_pref1_scores$protester)

data <- data %>%
  left_join(total_pref1_scores %>% dplyr::select(RID, total_pref1, protester), by = "RID")





















##### Create & Check Database ####

database <- fullchoice %>% 
  mutate(Dummy_pa_half = case_when(a2_x2 == 2 ~1, TRUE~0),
         Dummy_pa_full = case_when(a2_x2 == 3 ~1, TRUE~0),
         Dummy_hnv_visible = case_when(a2_x4 == 2 ~1, TRUE~0),
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
           dce_version %in% c(3, 4) & a2_x1 == 5 ~ sq_pa_area + 1600
         ),
         cost_att = case_when(a2_x5 == 1 ~ 5, a2_x5 == 2 ~ 10,
                              a2_x5 == 3 ~ 20, a2_x5 == 4 ~ 40,
                              a2_x5 == 5 ~ 60, a2_x5 == 6 ~ 80,
                              a2_x5 == 7 ~ 120, a2_x5 == 8 ~ 150,
                              a2_x5 == 9 ~ 200, a2_x5 == 10 ~ 250))





#### Calculate share of people who zoom on each cc card 
database <- database %>%
  mutate(across(c(getZoom1, getZoom2, getZoom3, getZoom4, getZoom5, getZoom6, getZoom7, getZoom8, getZoom9, getZoom10),
                ~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)), 
                .names = "zoom_{col}"))

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
    return("Other")
  }
}

# Apply the function to create a new column in the dataframe
data <- data %>%
  mutate(device_type = sapply(respondent_ua, extract_device_type))


extract_device_category <- function(ua_string) {
  
  # Check for Windows or Mac which are typically laptops or desktops
  if (grepl("Windows", ua_string, ignore.case = TRUE)) {
    return("Laptop/Desktop")
  } else if (grepl("Macintosh|Mac OS X", ua_string, ignore.case = TRUE)) {
    return("Laptop/Desktop")
  } else if (grepl("CrOS", ua_string, ignore.case = TRUE)) { # Chrome OS devices are typically laptops
    return("Laptop/Desktop")
    
    # Check for Android devices - check if they are mobile or tablets
  } else if (grepl("Android", ua_string, ignore.case = TRUE)) {
    if (grepl("Mobile", ua_string, ignore.case = TRUE)) {
      return("Mobile Phone")
    } else {
      return("Tablet")
    }
    
    # Check for iPhones and iPads
  } else if (grepl("iPhone", ua_string, ignore.case = TRUE)) {
    return("Mobile Phone")
  } else if (grepl("iPad", ua_string, ignore.case = TRUE)) {
    return("Tablet")
    
    # Any other device type
  } else {
    return("Other/Unknown")
  }
}

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
