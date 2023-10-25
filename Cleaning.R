#Title: Cleaning Data for ISSN (International Study of Strategy and Needs)
#R Version (4.2.3)


# -------------------------------
# Set up
# -------------------------------


## Remove all objects from the current workspace 
rm(list = ls())

#Set working directory (ISSN = International Study of Strategy and Needs)
setwd("C:/Users/jack_/OneDrive/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

# Load required libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(stringr)

# Read the survey data from raw data (csv)
data <- read.csv(file = "Raw data 20102023.csv")

#Check data

glimpse(data)

# -------------------------------
# Data cleaning
# -------------------------------

## Remove duplicate header rows 
data <- data[-c(1,2), ]

## Clean column names
data <- clean_names(data)

## Remove trailing & leading white spaces
data <- data %>% 
  mutate_if(is.character, str_trim)

## Remove participants who did not consent
table(data$consent, exclude = FALSE)

data <- data %>% 
  filter(consent != "I DO NOT consent (this will eject you from the survey)") #removes 0 respondents

##

# 1. Exclude survey responses that meet exclusion criteria  (do not work with Farmed land animals or Farmed aquatic animals)



# Check manually through "Other" options to determine whether they include farmed animals

data %>%
  filter(!grepl("Farmed land animals|Farmed aquatic animals", animal_type)) %>%
  select(animal_type_8_text) %>%
  na.omit() %>%
  unique()

# Create a vector of target animal types
target_animal_types <- c("Farmed land animals", "Farmed aquatic animals")
exclusion_types <- c("sick ,injured abandoned animals", "feral cats specifically, but also domesticated cats and dogs")

# Filter data to keep only rows with target animal types and without exclusion types
data <- data %>% 
  filter(grepl("Farmed land animals|Farmed aquatic animals", animal_type) & 
           !animal_type_8_text %in% exclusion_types)

# Count the number of completed and incomplete surveys based on 'participant_role' (final question that was required)

completed_count <- nrow(filter(data, participant_role != ""))
incomplete_count <- nrow(filter(data, participant_role == ""))

# Display the counts ("Number of completed surveys:  176"; "Number of incomplete surveys:  40")
#
print(paste("Number of completed surveys: ", completed_count))
print(paste("Number of incomplete surveys: ", incomplete_count))

#Exclude those who responded too quickly

data <- data %>%
  mutate(duration_in_seconds = as.numeric(duration_in_seconds)) %>%
  mutate(speed_limit = median(duration_in_seconds) / 3,
         flag_speed = duration_in_seconds < speed_limit) 

#Check proportion of those who were removed

table(data$flag_speed)

##data <-data %>%
  #filter(flag_speed != "TRUE") 

#Check for duplicates

duplicates <- data %>%
  group_by(response_id) %>%
  summarise(dupe_id = n()) %>%
  arrange(desc(dupe_id))%>%
  filter(dupe_id > 1) ###None found

# 3. Code all empty fields as NA

data <- data %>%
  mutate_all(~ifelse(is.na(.), "NA", .))

##Variables

# For countries working on a national basis, copy org_country to country_focus, so that all respondents have a country_focus 

data <- data %>%
  mutate(country_focus = ifelse(org_geographic_lvl == "At the national level, operating almost exclusively within a single country",
                                org_country, country_focus))

#Remove unnecessary variables (note, keeping response ID for translation)

clean_data <- data %>%
  select(-c("start_date", "end_date", "status", "ip_address", "progress", "finished", "recorded_date",
            "recipient_last_name", "recipient_first_name", "recipient_email",
            "external_reference", "location_latitude", "location_longitude",
            "distribution_channel", "pid"))
## 

## Obtain final sample sizes 
samplesize <- clean_data %>% summarise(n = n())
print(samplesize)

# Extracting text for translation

data_for_translation <- data %>%
  filter(q_language != 'EN') %>%
  select(response_id, q_language, animal_type_8_text, org_budget_currency_5_text, org_advocacy_6_text, org_focus_6_text, 
         advocacy_diet_4_text, advocacy_corporate_4_text, advocacy_policy_5_text, advocacy_inst_5_text, 
         interest_diet_r2, interest_corp_r1, interest_corp_r2, interest_policy_r1, interest_policy_r2, 
         interest_direct_r1, interest_direct_r2, interest_inst_r1, interest_inst_r2, 
         corporate_dissatisfy, policy_dissatisfy, insti_dissatisfy, direct_dissatisfy, other_dissatisfy, 
         participant_role_9_text)

#Manually verified that they all contain foreign language text, with the exceptions of the ones below

exclude_ids <- c("R_31tNOBC6h7aajmB", "R_pbn7EdCKVi7jSV3", "R_3KJ7bFlV3EvtEwV", "R_2xW43kI5YQJ1RPk", "R_2as3oIhfeiDyYju", "R_2PgOmUjVkduOId4", "R_OkasPyleXi8UNDb")

data_for_translation <- data_for_translation %>%
  filter(!(response_id %in% exclude_ids))

# Write the filtered data to a CSV file
write.csv(data_for_translation, "data_for_translation.csv")

# verifying no translations needed in excluded files

data_for_excluded <- data %>%
  filter(q_language != 'EN') %>%  # Including this to maintain the original filter
  filter(response_id %in% exclude_ids)
write.csv(data_for_excluded, "data_for_excluded.csv")


# -------------------------------
# De-identifying 
# -------------------------------
clean_data <- clean_data %>%
  select(-c("org_name", "participant_email", "interview_1"))

# -------------------------------
# Save final cleaned dataset 
# -------------------------------
write.csv(clean_data, "data_cleaned.csv")
save(clean_data, file='data_cleaned.Rdata')

