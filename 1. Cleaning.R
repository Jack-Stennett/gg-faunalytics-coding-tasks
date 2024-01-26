#Title: Cleaning Data for ISSN (International Study of Strategy and Needs)
#R Version (4.2.3)


# -------------------------------
# Set up
# -------------------------------


## Remove all objects from the current workspace 
rm(list = ls())

#Set working directory
setwd("C:/Users/jack_/Desktop/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

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

## Exclude survey responses that meet exclusion criteria  (do not work with Farmed land animals, Farmed aquatic animals or Dogs/ Cats raised for meat)# (Pre-registered: [https://osf.io/76dw4])

# Create a vector of target animal types

target_animal_types <- c("Farmed land animals", "Farmed aquatic animals", "Dogs/cats used for meat", "Other animals (please specify)")

# Verify that "other animal" responses could include farmed animals 

other_animal_types_data <- data %>%
  filter(!grepl("Farmed land animals|Farmed aquatic animals|Dogs/cats used for meat", animal_type)) %>%
  select(animal_type_8_text) %>%
  na.omit() %>%
  unique()

view(other_animal_types_data)

# Create a vector for "other" animal types to exclude based on manual check (1 example that does not include farmed animals)

exclusion_types <- c("feral cats specifically, but also domesticated cats and dogs")

# Calculate how many responses were excluded based on animal type 

animal_excluded <- data %>%
  filter(!(grepl("Farmed land animals|Farmed aquatic animals|Dogs/cats used for meat|Other animals", animal_type) &
             !animal_type_8_text %in% exclusion_types))

# Filter data
data <- data %>%
  filter(grepl("Farmed land animals|Farmed aquatic animals|Dogs/cats used for meat|Other animals", animal_type) & 
           !animal_type_8_text %in% exclusion_types)

# Count and print excluded and included rows
excluded_count <- nrow(animal_excluded)
included_count <- nrow(data)

print(paste("Excluded:", excluded_count)) ##"Excluded: 52 because they didn't meet the inclusion criteria (animal_type)"
print(paste("Included:", included_count)) ##"Included: 226"

## Exclude those who responded too quickly
# Identify responses that exceed speed limit

data <- data %>%
  mutate(duration_in_seconds = as.numeric(duration_in_seconds)) %>%
  mutate(speed_limit = median(duration_in_seconds) / 3,
         flag_speed = duration_in_seconds < speed_limit) 

# Store and view excluded data based on speed limit

speed_excluded <- data %>%
  filter(flag_speed == TRUE)

#view(speed_excluded)### 14 excluded, currently retaining "R_1obeZxneDwoVnfN", which misses the speed limit, but answered fewer questions due to the nature of early responses

data <- data %>%
  filter(flag_speed != TRUE | response_id == "R_1obeZxneDwoVnfN")

# Check for duplicates

duplicates <- data %>%
  group_by(response_id) %>%
  summarise(dupe_id = n()) %>%
  arrange(desc(dupe_id))%>%
  filter(dupe_id > 1) ###None found

# Manually look for org name duplicates, note that language is still not recoded

#view(data[, c("org_name", "org_country", "country_focus", "q_language")])

#Duplicate org names: Anonymous for the voiceless (1 in HK, 1 in Taiwan), Impactful Animal Advocacy (1 online only, 1 in UK), 
#Mercy For Animals (Philippines, Multiple, Malaysia), Princípio Animal (Both Brazil), Shrimp Welfare Project (1 in India, 1 in Vietnam)
#We think that answers are sufficiently different that we should include all organisation duplicates, but this should be noted in the supplementary materials.

##Removing unnecessary variables (note, keeping response ID for translation)

clean_data <- data %>%
  select(-c("start_date", "end_date", "status", "ip_address", "progress", "finished", "recorded_date", 
            "recipient_last_name", "recipient_first_name", "recipient_email", 
            "external_reference", "location_latitude", "location_longitude",
            "distribution_channel", "pid", "interview_1", "consent", "speed_limit", "flag_speed", "duration_in_seconds"))

## Obtain final sample sizes 

samplesize <- clean_data %>% summarise(n = n())
print(samplesize) #Final sample size: 212

# Count the number of completed and incomplete surveys based on 'participant_role' (final question that was required)
# ("Number of completed surveys:  186"; "Number of incomplete surveys: 26")

completed_count <- nrow(filter(data, participant_role != ""))
incomplete_count <- nrow(filter(data, participant_role == ""))
print(paste("Number of completed surveys: ", completed_count))
print(paste("Number of incomplete surveys: ", incomplete_count))

# Extracting text for translation (leaving only response_id, language, and open ended responses)

data_for_translation <- data %>%
  filter(q_language != 'EN') %>%
  select(response_id, q_language, animal_type_8_text, org_budget_currency_5_text, org_advocacy_6_text, org_focus_6_text, 
         advocacy_diet_4_text, advocacy_corporate_4_text, advocacy_policy_5_text, advocacy_inst_5_text, 
         interest_diet_r2, interest_corp_r1, interest_corp_r2, interest_policy_r1, interest_policy_r2, 
         interest_direct_r1, interest_direct_r2, interest_inst_r1, interest_inst_r2, 
         corporate_dissatisfy, policy_dissatisfy, insti_dissatisfy, direct_dissatisfy, other_dissatisfy, 
         participant_role_9_text)

#view(data_for_translation)

#Manually verified that they all contain foreign language text, with the exceptions of the responses below, which responded in English

exclude_ids <- c("R_31tNOBC6h7aajmB", "R_pbn7EdCKVi7jSV3", "R_3KJ7bFlV3EvtEwV", "R_2xW43kI5YQJ1RPk", "R_2as3oIhfeiDyYju", "R_2PgOmUjVkduOId4", "R_OkasPyleXi8UNDb", "R_2SCaTJf7UIS1BDn")

#Verify excluded samples - one example is mixed EN/ID, there are only 4 words in Indonesian, so excluding (Semua hewan= all animals; Tidak dihitung = not calculated)

verif_exclude_ids <- data_for_translation %>%
  filter((response_id %in% exclude_ids))

#view(verif_exclude_ids)

data_for_translation <- data_for_translation %>%
  filter(!(response_id %in% exclude_ids))

#view(data_for_translation)

# Write the filtered data to a CSV file
write.csv(data_for_translation, "data_for_translation.csv")

# verifying no translations needed in excluded files

data_for_excluded <- data %>%
  filter(q_language != 'EN') %>%  # Including this to maintain the original filter
  filter(response_id %in% exclude_ids)

#view(data_for_excluded)

# -------------------------------
# De-identifying 
# -------------------------------
clean_data <- clean_data %>%
  select(-c("org_name", "participant_email"))

# -------------------------------
# Save final cleaned dataset 
# -------------------------------
write.csv(clean_data, "data_cleaned.csv")
save(clean_data, file='data_cleaned.Rdata')

