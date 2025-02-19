rm(list=ls())

library(tidyverse)
library(readxl)
library(ggpubr)

# source("Scripts/get_data.R")


### 
data_path <- "Data_and_Output/Data/Main_2/VALUGAPS_Main_2_Covariates_2024-12-24.xlsx" 


addID <- 222 # id marker for new survey round to get unique ids

raw_data <- read_excel(data_path) %>%
  mutate(RID = paste0(RID, addID), RID = as.numeric(RID), 
         birthyralt_other = as.numeric(birthyralt_other),
         natvisit_next12m = as.character(natvisit_next12m),
         survey_round = "main2") 


time_stamps <- read_excel("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__Timestamps_2024-12-24.xlsx") %>% 
  mutate(survey_round ="main2",  RID = paste0(RID, addID), RID = as.numeric(RID))

choice_exp_1 <- read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2_DCE_Exp_2024-12-24.xlsx") 
choice_exp_swap_1 <- read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap_2024-12-24.xlsx")
choice_exp_2 <- read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__DCE_Exp_2_2024-12-24.xlsx")
choice_exp_swap_2 <- read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap_2_2024-12-24.xlsx")

table(choice_exp_1$pref1)
table(choice_exp_swap_1$pref1)
table(choice_exp_2$pref1)
table(choice_exp_swap_2$pref1)

choicedata_low <- read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap_2024-12-24.xlsx") %>% 
  mutate(pref1= case_when(
    pref1 == 1 ~ 2,
    pref1 == 2 ~ 1,
    TRUE ~ NA)
  ) %>% 
  bind_rows(read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2_DCE_Exp_2024-12-24.xlsx"))


choicedata_high <- read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__DCE_Exp_Swap_2_2024-12-24.xlsx") %>% 
  mutate(pref1= case_when(
    pref1 == 1 ~ 2,
    pref1 == 2 ~ 1,
    TRUE ~ NA)
  ) %>% 
  bind_rows(read_xlsx("Data_and_Output/Data/Main_2/VALUGAPS_Main_2__DCE_Exp_2_2024-12-24.xlsx"))


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





status_pl <- ggplot(data = raw_data) +
  geom_histogram(aes(x=STATUS_recoded, fill=STATUS_recoded), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Status")


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


gender <- ggplot(data = data) +
  geom_histogram(aes(x=gender, fill=as.factor(gender_chr)), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Gender") +
  xlab("")

# Check and change if linke and fdp get selected

data_voting <- data %>%
  group_by(voting) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

data_voting_clean <- data %>% filter(pol_btw != 8) %>% 
  group_by(voting) %>%
  summarise(count = n()) %>%
  mutate(share = count / sum(count) * 100)

         
voting_pl <- ggplot(data=data_voting) +
  geom_bar(aes(x=voting, y=share, fill=as.factor(voting)), stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Party", title="Which party would you vote for if there would be elections next Sunday?") +
  xlab("") +
  ylab("Share (%)") +
  scale_fill_manual(values = c("black", "red", "mediumvioletred", "blue", "deeppink", "green3", "yellow", "white", "grey"))


voting_pl_clean <- ggplot(data=data_voting_clean) +
  geom_bar(aes(x=voting, y=share, fill=as.factor(voting)), stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Party", title="Which party would you vote for if there would be elections next Sunday?") +
  xlab("") +
  ylab("Share (%)") +
  scale_fill_manual(values = c("black", "red", "mediumvioletred", "blue", "deeppink", "green3", "yellow", "grey"))



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

hh_size <- ggplot(data = data) +
  geom_boxplot(aes(x="HH Size", y=hhsize), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 10)) +  # Adjust the limits based on your expected household sizes
  geom_boxplot(aes(x="HH Size", y = hhsize)) +
  ylab("Household Size") +
  xlab("")
  
vg_hist <- function(d = data, var , xlabel = "Groups" , legendtit = "category"){

  ggplot(d, aes(x = {{ var }}, fill = as.factor({{ var }}))) +
    geom_histogram(stat = "count") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(fill = legendtit) +
    xlab(xlabel) 
}

uhh_deck <- vg_hist(var = uhh, legendtit = "Random groups")

dce_v <- vg_hist(var = dce_version, legendtit = "DCE groups")

block <- vg_hist(var =block)

tc_1 <- vg_hist(var=tc1,legendtit = "Visited place for Recreation last 12 month?" )
tc_map_pl <- vg_hist(var=tc_map,legendtit = "Random groups TC map" )
tc_timeline_pl <- vg_hist(var=tc_timeline,legendtit = "Random groups TC time" )
tc_calc_pl <- vg_hist(var=tc_calc,legendtit = "Random groups TC calc" )
tc_avg_pl <- vg_hist(var=tc_avg, legendtit = "Random groups Tc avg")

tc_dog <- vg_hist(var = dog) + 
  scale_fill_discrete(
    labels = c("Yes with my dog", "No, but I own a dog", "No, I don't own a dog", "Other"),
    name = "Answer"
  )

tc_dogowner <- vg_hist(var = dogowner) + 
  scale_fill_discrete(
    labels = c("Dogowner", "No dog", "N/A"),
    name = "Answer"
  )

equity <- vg_hist(var = equity, legendtit = "Equity Sample") + 
  scale_x_continuous(breaks = c(1, 2), labels = c("All same", "income tax"))


paymentyears <- vg_hist(var = arm, legendtit = "Random groups: Years of payment") +
  scale_x_continuous(breaks = c(1, 2, 3, 4),
                     labels = c("5 years", "10 years", "20 years", "indefinitely"))

block

radius <- ggplot(data = data) +
  geom_histogram(aes(x=radius, fill=as.factor(radius)), stat = "count") +
  labs(fill = "Radius") +
  xlab("Groups")

##cross-randomizations
variables <- c('uhh', 'dce_version', 'equity', 'arm', 'block', 'radius', 'order')

# Initialize an empty matrix to store p-values
p_value_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables))
rownames(p_value_matrix) <- variables
colnames(p_value_matrix) <- variables

# Function to run chi-squared test and extract p-value
run_chisq_test <- function(var1, var2) {
  table_data <- table(data[[var1]], data[[var2]])
  test_result <- chisq.test(table_data)
  return(test_result$p.value)
}

# Loop through each combination of variables and run the chi-squared test
for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    if (i != j) {
      p_value_matrix[i, j] <- run_chisq_test(variables[i], variables[j])
    }
  }
}

# Display the p-value matrix
cat("Chi-Squared Test P-Value Matrix:\n")
print(p_value_matrix)



age <- ggplot(data=data) +
  geom_boxplot(aes(x=gender, y=birthyear, group =gender, fill=as.factor(gender_chr))) +
  labs(fill="Gender")

age_jitter <- ggplot(data = data) +
  geom_jitter(aes(x = gender, y = birthyear, color = as.factor(gender_chr), group = gender), width = 0.2) +
  labs(color = "Gender")

ggplot(data=data) +
  geom_boxplot(aes(x=gender, y=lifesat_recode, group =gender, fill=as.factor(gender_chr))) +
  labs(fill = "Gender") 

lifesat_etc <- ggplot(data = data) +
  geom_boxplot(aes(x = factor("Life Sat"), y = lifesat_recode), fill = "lightcoral") +
  geom_boxplot(aes(x = factor("Health Psych"), y = healthpsych_recode), fill = "lightblue") +
  geom_boxplot(aes(x = factor("Health Phys"), y = healthphys_recode), fill = "lightgreen") +
  xlab("") +
  ylab("Value") +
  ggtitle("Boxplots of Health and Life Satisfaction Variables")

##Question 1
labels <- c(
  "1" = "less than 10%",
  "2" = "around 25 %",
  "3" = "appr. 50%",
  "4" = "around 75 %",
  "5" = "more than 90 %"
)
data$q10 <- factor(data$q10, levels = c(1, 2, 3, 4, 5))

testqu1 <- ggplot(data, aes(x = q10, fill = q10)) +
  geom_bar(color = "black") +
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = c(
    "1" = "lightblue",
    "2" = "lightblue",
    "3" = "darkgreen",
    "4" = "lightblue",
    "5" = "lightblue"
  )) +
  labs(
    title = "Q: How much of Germany's total area is agricultural land?",
    x = "Answers",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Hide the legend if not needed
  )

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

# Plot the combined shares with updated English labels and colors
testqu2 <- ggplot(share_data, aes(x=Issue, y=share_corr, fill=Issue)) +
  geom_bar(stat="identity", show.legend=FALSE) +
  scale_fill_manual(values=c(
    "Rich Small Water Bodies and Ditches"="forestgreen",
    "Flower Strips"="forestgreen",
    "Hedges"="forestgreen",
    "Field Shrubs"="forestgreen",
    "Straightened Rivers"="red",
    "Wide Forest Roads"="red"
  )) +
  labs(
    title = "Q: Which of these landscape elements are typically found on HNV farmland?",
    x = "Landscape Element",
    y = "Share of Correct Answers (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


##Question 3
labels <- c(
  "1" = "In scattered small areas",
  "2" = "Grouped together",
  "3" = "Both scattered and grouped"
)
data$q14 <- factor(data$q14, levels = c(1, 2, 3))

testqu3 <- ggplot(data, aes(x = q14, fill = q14)) +
  geom_bar(color = "black") +
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = c(
    "1" = "lightblue",
    "2" = "lightblue",
    "3" = "darkgreen"
  )) +
  labs(
    title = "Q: How do PAs fulfil their protective function for biodiversity best?",
    x = "Answers",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Hide the legend if not needed
  )


hhnetinc_histogram <- ggplot(data, aes(x = hhnetinc_numeric)) +
  geom_histogram(binwidth = 500, fill = "coral", color = "black", alpha = 0.7) +
  scale_x_continuous(
    breaks = c(250, 750, 1250, 1750, 2250, 2750, 3250, 3750, 4500, 5500),
    labels = c(
      'less than 500 Euro',
      '500 - 999 Euro',
      '1000 - 1499 Euro',
      '1500 - 1999 Euro',
      '2000 - 2499 Euro',
      '2500 - 2999 Euro',
      '3000 - 3499 Euro',
      '3500 - 3999 Euro',
      '4000 - 4999 Euro',
      'more than 5000 Euro'
    )
  ) +
  labs(
    title = "Histogram of Household Net Income",
    x = "Household Net Income Bracket",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  # Removes vertical grid lines
    panel.grid.minor.x = element_blank(),  # Removes minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90")  # Light horizontal grid lines
  )

hhnetinc_histogram_normal <- ggplot(data, aes(x = hhnetinc_numeric/hhsize)) +
  geom_histogram(binwidth = 500, fill = "coral3", color = "black", alpha = 0.7) +
  scale_x_continuous(
    breaks = c(250, 750, 1250, 1750, 2250, 2750, 3250, 3750, 4500, 5500),
    labels = c(
      'less than 500 Euro',
      '500 - 999 Euro',
      '1000 - 1499 Euro',
      '1500 - 1999 Euro',
      '2000 - 2499 Euro',
      '2500 - 2999 Euro',
      '3000 - 3499 Euro',
      '3500 - 3999 Euro',
      '4000 - 4999 Euro',
      'more than 5000 Euro'
    )
  ) +
  labs(
    title = "Histogram of Household Net Income considering Household Size",
    x = "Household Net Income Bracket",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  # Removes vertical grid lines
    panel.grid.minor.x = element_blank(),  # Removes minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90")  # Light horizontal grid lines
  )

hhnetinc_boxplot <- ggplot(data = data, aes(x = factor(1), y = hhnetinc_numeric)) +
  geom_boxplot() +
  xlab("") +
  ylab("Household Net Income (Euro)") +
  ggtitle("Boxplot of Household Net Income")

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

frequency_table <- table(data$dce_version, data$protester)
#print(frequency_table)


data_prot <- data %>% filter(!is.na(protest_1))

prot1_pl <- vg_hist(d=data_prot, var =protest_1_recode) + labs(title="Ich möchte kein Geld für den Schutz der Natur ausgeben.")
prot2_pl <- vg_hist(d=data_prot, var =protest_2_recode) + labs(title="Jemand anderes sollte für den Schutz der Natur bezahlen.")
prot3_pl <- vg_hist(d=data_prot, var =protest_3_recode) + labs(title="Ich war nicht ausreichend informiert.")
prot4_pl <- vg_hist(d=data_prot, var =protest_4_recode) + labs(title="Ich habe Einwände gegen die Art und Weise, wie die Fragen gestellt wurden.")
prot5_pl <- vg_hist(d=data_prot, var =protest_5_recode) + labs(title="Die angegebene Zahlungsmethode ist für mich unpassend.")


prot1_pl_new <- ggplot(data_prot, aes(x = factor(protest_1_recode), fill = factor(protester > 1))) + geom_bar(position = position_dodge(preserve = "single")) + labs(title = "Ich möchte kein Geld für den Schutz der Natur ausgeben.", fill = "Protester Group", x = "Protest Question 1", y = "Count") + scale_fill_manual(values = c("#ff6c91", "#00c1a9"), labels = c("No Protester", "Protester")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
prot2_pl_new <- ggplot(data_prot, aes(x = factor(protest_2_recode), fill = factor(protester > 1))) + geom_bar(position = position_dodge(preserve = "single")) + labs(title="Jemand anderes sollte für den Schutz der Natur bezahlen.", fill = "Protester Group", x = "Protest Question 1", y = "Count") + scale_fill_manual(values = c("#ff6c91", "#00c1a9"), labels = c("No Protester", "Protester")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
prot3_pl_new <- ggplot(data_prot, aes(x = factor(protest_3_recode), fill = factor(protester > 1))) + geom_bar(position = position_dodge(preserve = "single")) + labs(title = "Ich war nicht ausreichend informiert.", fill = "Protester Group", x = "Protest Question 1", y = "Count") + scale_fill_manual(values = c("#ff6c91", "#00c1a9"), labels = c("No Protester", "Protester")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
prot4_pl_new <- ggplot(data_prot, aes(x = factor(protest_4_recode), fill = factor(protester > 1))) + geom_bar(position = position_dodge(preserve = "single")) + labs(title = "Ich habe Einwände gegen die Art und Weise, wie die Fragen gestellt wurden.", fill = "Protester Group", x = "Protest Question 1", y = "Count") + scale_fill_manual(values = c("#ff6c91", "#00c1a9"), labels = c("No Protester", "Protester")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
prot5_pl_new <- ggplot(data_prot, aes(x = factor(protest_5_recode), fill = factor(protester > 1))) + geom_bar(position = position_dodge(preserve = "single")) + labs(title = "Die angegebene Zahlungsmethode ist für mich unpassend.", fill = "Protester Group", x = "Protest Question 1", y = "Count") + scale_fill_manual(values = c("#ff6c91", "#00c1a9"), labels = c("No Protester", "Protester")) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


data_voting_supp <- data %>%
  filter(protester < 2) %>%
  group_by(voting) %>%
  summarise(count = n()) %>%
  mutate(share_supp = round(count / sum(count) * 100, 1))

data_voting_prot <- data %>%
  filter(protester > 1) %>%
  group_by(voting) %>%
  summarise(count = n()) %>%
  mutate(share_prot = round(count / sum(count) * 100, 1)) %>%
  left_join(data_voting %>% dplyr::select(voting, share), by = "voting") %>%  # Join data_voting to add the share column
  left_join(data_voting_supp %>% dplyr::select(voting, share_supp), by = "voting") %>%  # Join data_voting_supp to add share_supp
  mutate(diff_prot = share_prot - share_supp)

data_voting_prot$voting <- factor(data_voting_prot$voting, levels = voting_order)

voting_pl_prot <- ggplot(data=data_voting_prot) +
  geom_bar(aes(x=voting, y=share_prot, fill=as.factor(voting)), stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Party", title="Protesters subsample: Which party would you vote for if there would be elections next Sunday?") +
  xlab("") +
  ylab("Share (%)") +
  scale_fill_manual(values = c("black", "red", "mediumvioletred", "blue", "green3", "grey", "white"))

voting_pl_prot_diff <- ggplot(data=data_voting_prot) +
  geom_bar(aes(x=voting, y=diff_prot, fill=as.factor(voting)), stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(fill = "Party", title="Difference between protesters and remainder (%pts)") +
  xlab("") +
  ylab("Share (%)") +
  scale_fill_manual(values = c("black", "red", "mediumvioletred", "blue", "green3", "grey", "white"))

#### Analyse time ####

rids_complete <- unique(data$RID)

time_stamps_raw <- time_stamps

# Step 1: Calculate time spent on each page
time_stamps <- time_stamps %>%
  filter(RID %in% rids_complete) %>%
  mutate(across(starts_with("PAGE_SUBMIT"), ~ . - get(sub("SUBMIT", "DISPLAY", cur_column())), .names = "time_spent_{col}"))


# Step 2: Reshape to long format
time_long <- time_stamps %>%
  pivot_longer(cols = starts_with("time_spent_"),
               names_to = "Page",
               values_to = "Time_Spent") %>%
  mutate(Page = as.numeric(gsub("time_spent_PAGE_SUBMIT_", "", Page)))


# Step 3: Calculate the average time per page
average_time_per_page <- time_long %>%
  group_by(Page) %>%
  summarize(Average_Time_Spent = mean(Time_Spent, na.rm = TRUE),
            Median_Time_Spent = median(Time_Spent, na.rm = TRUE)) %>%
  mutate(Color_Group = case_when(Page >= 31 & Page <= 40 ~ "DCE",
                                 Page >= 43 & Page <= 51 ~ "NR, CV, TC",
                                 Page >= 54 & Page <= 62 ~ "UHH", TRUE~"Other"))



# Step 4: Plot with highlighted pages
page_times_mean <- ggplot(average_time_per_page, aes(x = Page, y = Average_Time_Spent, fill = Color_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("DCE" = "red", "NR, CV, TC"="forestgreen", "UHH" = "steelblue", "Other" = "grey")) +
  labs(title = "Average Time Spent on Each Page by Respondents",
       x = "Page Number",
       y = "Average Time Spent (seconds)",
       fill = "Page Type") +
  theme_minimal()

page_times_median <- ggplot(average_time_per_page, aes(x = Page, y = Median_Time_Spent, fill = Color_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("DCE" = "red", "NR, CV, TC"="forestgreen", "UHH" = "steelblue", "Other" = "grey")) +
  labs(title = "Median Time Spent on Each Page by Respondents",
       x = "Page Number",
       y = "Median Time Spent (seconds)",
       fill = "Page Type") +
  theme_minimal()

#


cv_hhinc <- ggplot(data = data %>%
                     mutate(income_group = cut(hhnetinc_numeric/hhsize,
                                               breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
                                               labels = c('less than 500 Euro','500 - 999 Euro','1000 - 1499 Euro','1500 - 1999 Euro','2000 - 2499 Euro','more than 2500 Euro'),include.lowest = TRUE))) +
  geom_boxplot(aes(x = income_group,
                   y = cv, 
                   group = income_group,
                   fill = income_group),
               outlier.shape = NA) +
  ylab("Payment per year in €") +
  xlab("Household Net Income Bracket") +
  scale_fill_manual(values = c("darkgreen", "green", "yellow", "orange", "red", "gray"), 
                    labels = c(
                      'less than 500 Euro',
                      '500 - 999 Euro',
                      '1000 - 1499 Euro',
                      '1500 - 1999 Euro',
                      '2000 - 2499 Euro',
                      'more than 2500 Euro'),
                    name = "Income Bracket") +
  coord_cartesian(ylim = c(0, 200)) +  # Zoom into the y-axis range
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90")
  )



payment_distribution_hhinc <- ggplot(data) +
  geom_histogram(aes(x = cut(hhnetinc_numeric/hhsize,
                             breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
                             labels = c('less than 500 Euro','500 - 999 Euro','1000 - 1499 Euro','1500 - 1999 Euro','2000 - 2499 Euro','more than 2500 Euro'),include.lowest = TRUE),
                     fill = as.factor(payment_distribution)), 
                 stat = "count", position = "dodge") +
  xlab("Household Net Income Bracket") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "How did you envisage the distribution of the increased taxes among households when you voted on the proposed policy measures?",
       fill = "Payment Distribution") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

payment_vision_hhinc <- ggplot(data) +
  geom_histogram(aes(x = cut(hhnetinc_numeric/hhsize,
                             breaks = c(0, 500, 1000, 1500, 2000, 2500, Inf),
                             labels = c('less than 500 Euro','500 - 999 Euro','1000 - 1499 Euro','1500 - 1999 Euro','2000 - 2499 Euro','more than 2500 Euro'),include.lowest = TRUE),
                     fill = as.factor(payment_vision)), 
                 stat = "count", position = "dodge") +
  xlab("Household Net Income Bracket") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "How do you prefer the levy increases to be distributed?",
       fill = "Payment Vision") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

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


table((database$hnv_att - database$sq_hnv_area), database$dce_version)
table((database$cost_att), database$dce_version)
table(database$Dummy_hnv_visible, database$dce_version)
table(database$Dummy_pa_full, database$dce_version)
table(database$Dummy_pa_half, database$dce_version)
table((database$pref1), database$dce_version)


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
