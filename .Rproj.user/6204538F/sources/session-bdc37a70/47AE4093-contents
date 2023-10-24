

# Load required libraries
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(tidyverse)
library(janitor)
library(stringr)

#Set working directory (ISSN = International Study of Strategy and Needs)
setwd("C:/Users/jack_/OneDrive/GitHub/ISSN")

# Read the survey data from raw data csv (currently test data- needs to be updated with raw data)
data <- read.csv(file = "Test Data ISSN.csv")

glimpse(data)
# Data cleaning


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
  filter(consent != "I DO NOT consent (this will eject you from the survey)") #seems to remove 0 respondents

##

# 1. Exclude survey responses that meet exclusion criteria  (do not work with Farmed land animals or Farmed aquatic animals)

# Create a vector of target animal types
target_animal_types <- c("Farmed land animals", "Farmed aquatic animals")

# Count the number of participants for each type of animal (to confirm, "dogs and cats for meat")

table(data$animal_type, exclude = target_animal_types) %>%
  summary(n(n))

# Filter data to keep only rows with target animal types (note - need to add "separate "Other animals")
data <- data %>% 
  filter(grepl("Farmed land animals|Farmed aquatic animals", animal_type)) 

#Remove unfinished answers (final question that was required) - Instead, we need to report how many people gave full responses 

table(data$participant_role, exclude = FALSE)

data <- data %>% filter(participant_role != "")

#Exclude those who responded too quickly

data <- data %>%
  mutate(duration_in_seconds = as.numeric(duration_in_seconds)) %>%
  mutate(speed_limit = median(duration_in_seconds) / 3,
         flag_speed = duration_in_seconds < speed_limit) 

table(data$flag_speed, exclude = FALSE)

data <-data %>%
  filter(flag_speed != "TRUE") 

duplicates <- data %>%
  group_by(response_id) %>%
  summarise(dupe_id = n()) %>%
  arrange(desc(dupe_id))

###Duplicate IDs (to add manually)

duplicate_ids <- c("")

duplicates <- data %>%
  filter(response_id %in% duplicate_ids) #checking which responses to omit 

responses_exclude <- c()

data <- data %>% filter(!response_id %in% responses_exclude)

# 3. Code all empty fields as NA

data <- data %>%
  mutate_all(~ifelse(is.na(.), "NA", .))

##Variables

# For countries working on a national basis, copy org_country to country_focus, so that all respondents have a country_focus 

data <- data %>%
  mutate(country_focus = ifelse(org_geographic_lvl == "At the national level, operating almost exclusively within a single country",
                                org_country, country_focus))

# 6. Budget needs to be converted to USD (new variable)

data$org_budget_converted <- data$org_budget

# Exchange rates as of 6/10/2023
# Conversion rate for Euro
conversion_rate_from_eur <- 1.06 

# Conversion rate for GBP
conversion_rate_from_gbp <- 1.23 


# Conversion rate for Canadian Dollar
conversion_rate_from_cad <- 0.74 

# Step 1: Check for any non-numeric values in the org_budget column where the currency is Euro, GBP, or CAD.

data %>%
  filter(org_budget_currency %in% c("Euro", "GBP (British Pound)", "Canadian Dollar")) %>%
  filter(!is.numeric(as.numeric(org_budget)))

# Step 2: Convert the entire org_budget column to numeric values to ensure consistency in data type.
# This is to make sure that any numeric value is a true numeric value.

data$org_budget <- as.numeric(as.character(data$org_budget))

# Step 3: Now, compute the converted budgets based on their currency. 
# If the currency is one of the three specified (Euro, GBP, CAD), the conversion rate is applied. 
# Otherwise, the original budget is retained.

data <- data %>%
  mutate(
    org_budget_converted = ifelse(
      !is.na(org_budget) & is.numeric(org_budget) & org_budget_currency == "Euro",
      org_budget * conversion_rate_from_eur,
      ifelse(!is.na(org_budget) & is.numeric(org_budget) &
               org_budget_currency == "GBP (British Pound)",
             org_budget * conversion_rate_from_gbp,
             ifelse(!is.na(org_budget) & is.numeric(org_budget) &
                      org_budget_currency == "Canadian Dollar",
                    org_budget * conversion_rate_from_cad,
                    org_budget
             )
      )
    )
  )

#Recode countries 

data <- data %>% 
  mutate(
    morphed_country = case_when(
      q_language == "PT_BR" ~ str_replace_all(org_country, "Afghanistan", "Brazil"),
      q_language == "PT_BR" ~ str_replace_all(org_country, "Albania", "Portugal"),
      q_language == "JA" ~ str_replace_all(org_country, "Afghanistan", "Japan"),
      q_language == "TGL" ~ str_replace_all(org_country, "Afghanistan", "Philippines"),
      q_language == "ZH-S" ~ str_replace_all(org_country, "Afghanistan", "China"),
      q_language == "ID" ~ str_replace_all(org_country, "Afghanistan", "Indonesia"),
      q_language == "TH" ~ str_replace_all(org_country, "Afghanistan", "Thailand"),
      q_language == "NE" ~ str_replace_all(org_country, "Afghanistan", "Nepal"),
      q_language == "VI" ~ str_replace_all(org_country, "Afghanistan", "Vietnam"),
      q_language == "ZH-T" ~ str_replace_all(org_country, "Afghanistan", "Taiwan") %>%
        str_replace_all("Albania", "Hong Kong") %>%
        str_replace_all("Algeria", "Macau"),
      TRUE ~ org_country
    )
  )


# Create a dummy variable for Western vs. non-Western vs. Mixed

western_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia",
  "Slovenia", "Sweden", "Spain", "United Kingdom", "Canada", "United States of America",
  "Australia", "New Zealand"
)

non_western_countries <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", 
  "Armenia", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belize", 
  "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", 
  "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Central African Republic", 
  "Chad", "Chile", "China", "Colombia", "Comoros", "Congo, Republic of the...", "Costa Rica", 
  "Côte d'Ivoire", "Cuba", "Democratic Republic of the Congo", "Djibouti", "Dominica", "Dominican Republic", 
  "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Fiji", 
  "Gabon", "Gambia", "Georgia", "Ghana", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", 
  "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Israel", "Jamaica", 
  "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
  "Latvia", "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya", "Liechtenstein", "Lithuania", 
  "Luxembourg", "Macau (S.A.R.)", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", 
  "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia, Federated States of", 
  "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", 
  "Nepal", "Nicaragua", "Niger", "Nigeria", "North Korea", "Norway", "Oman", "Pakistan", 
  "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", 
  "Poland", "Qatar", "Republic of Moldova", "Romania", "Russian Federation", "Rwanda", 
  "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", 
  "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", 
  "Singapore", "Somalia", "South Africa", "South Korea", "South Sudan", "Sri Lanka", "Sudan", 
  "Suriname", "Switzerland", "Syrian Arab Republic", "Taiwan", "Tajikistan", "The former Yugoslav Republic of Macedonia", 
  "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
  "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Republic of Tanzania", "Uruguay", 
  "Uzbekistan", "Vanuatu", "Venezuela, Bolivarian Republic of", "Viet Nam", "Yemen", "Zambia", 
  "Zimbabwe", "Online only", "Democratic Republic of the Congo"
)

data <- data %>%
  mutate(western_vs_nonwestern = case_when(
    country_focus %in% western_countries ~ "Western",
    country_focus %in% non_western_countries ~ "Non-Western",
    TRUE ~ "Mixed"
  ))

#Remove unnecessary variables

clean_data <- data %>%
  select(-c("status", "ip_address", "progress", "finished", "response_id",
            "recipient_last_name", "recipient_first_name", "recipient_email",
            "external_reference", "location_latitude", "location_longitude",
            "distribution_channel", "pid"))

## Obtain final sample sizes 
samplesize <- clean_data %>% summarise(n = n())

# Summary statistics

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
