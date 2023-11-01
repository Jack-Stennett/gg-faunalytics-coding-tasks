
#Title: Recoding data
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

## Remove all objects from the current workspace 
rm(list = ls())

install.packages("tidyverse") 
install.packages("janitor")
install.packages("stringr")
install.packages("naniar")

# Load required libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(stringr)
library (naniar)

#Set working directory (ISSN = International Study of Strategy and Needs)
setwd("C:/Users/jack_/OneDrive/GitHub/ISSN")

# Read the cleaned data from C:/Users/jack_/OneDrive/GitHub/ISSN/cleaning.R raw data csv (currently test data- needs to be updated with raw data)
data <- read.csv(file = "data_cleaned.csv")

glimpse(data)


#view(data)

#-----------------------------------------------------------
# Removing NAs 
#-----------------------------------------------------------
## convert all empty strings to NA
data <- data %>%
  replace_with_na_all(condition = ~.x == "") %>%
  glimpse
#-----------------------------------------------------------
# Renaming  
#-----------------------------------------------------------

# Placeholder for any necessary variable renaming

# -------------------------------
# Recoding variables
# -------------------------------

# Storing original data for validation purposes 

original_data <- data

#Recode countries; Background: in order to help respondents, we shifted the choices around so that, for non-English language
#respondents, the country where this language is spoken is the 1st-3rd choice. 

data <- data %>% 
  mutate(
    morphed_country = case_when(
      q_language == "PT-BR" ~ str_replace_all(org_country, c("Afghanistan" = "Brazil", "Albania" = "Portugal")),
      q_language == "JA" ~ str_replace_all(org_country, "Afghanistan", "Japan"),
      q_language == "TGL" ~ str_replace_all(org_country, "Afghanistan", "Philippines"),
      q_language == "ZH-S" ~ str_replace_all(org_country, "Afghanistan", "China"),
      q_language == "ID" ~ str_replace_all(org_country, "Afghanistan", "Indonesia"),
      q_language == "TH" ~ str_replace_all(org_country, "Afghanistan", "Thailand"),
      q_language == "NE" ~ str_replace_all(org_country, "Afghanistan", "Nepal"),
      q_language == "VI" ~ str_replace_all(org_country, "Afghanistan", "Vietnam"),
      q_language == "HI" ~ str_replace_all(org_country, "Afghanistan", "India"),
      q_language == "ZH-T" ~ str_replace_all(org_country, c("Afghanistan" = "Taiwan", "Albania" = "Hong Kong (S.A.R.)", "Algeria" = "Macau")),
      TRUE ~ org_country
    )
  )


data <- data %>% 
  mutate(
    morphed_country_focus = case_when(
      q_language == "PT-BR" ~ str_replace_all(country_focus, c("Afghanistan" = "Brazil", "Albania" = "Portugal")),
      q_language == "JA" ~ str_replace_all(country_focus, "Afghanistan", "Japan"),
      q_language == "TGL" ~ str_replace_all(country_focus, "Afghanistan", "Philippines"),
      q_language == "ZH-S" ~ str_replace_all(country_focus, "Afghanistan", "China"),
      q_language == "ID" ~ str_replace_all(country_focus, "Afghanistan", "Indonesia"),
      q_language == "TH" ~ str_replace_all(country_focus, "Afghanistan", "Thailand"),
      q_language == "NE" ~ str_replace_all(country_focus, "Afghanistan", "Nepal"),
      q_language == "VI" ~ str_replace_all(country_focus, "Afghanistan", "Vietnam"),
      q_language == "HI" ~ str_replace_all(country_focus, "Afghanistan", "India"),
      q_language == "ZH-T" ~ str_replace_all(country_focus, c("Afghanistan" = "Taiwan", "Albania" = "Hong Kong (S.A.R.)", "Algeria" = "Macau")),
      TRUE ~ country_focus
    )
  )

# Code to verify  
# View(data[, c("org_country", "morphed_country", "country_focus", "morphed_country_focus", "q_language")])
# filtered_data <- data %>% filter(q_language != "EN" & q_language != "ES-ES")
# View(filtered_data[, c("org_country", "morphed_country", "country_focus", "morphed_country_focus", "q_language")])

# Make changes to initial variables, remove new variables

data <- data %>% mutate(
  org_country = morphed_country,
  country_focus = morphed_country_focus
)

data <- data %>% select(-morphed_country, -morphed_country_focus)

# We have two variables 1) org_country and 2) country_focus, only for those who had multiple for org_country
# This code copies org_country to country_focus if there is only a single country selected for org_country, so that all respondents have a country_focus 

data <- data %>%
  mutate(country_focus = ifelse(nchar(org_country) - nchar(gsub(",", "", org_country)) == 0,
                                org_country, country_focus))

#Verifying that it copies all relevant countries - some respondents unfinished
#View(data[, c("org_country", "country_focus")]) 

sum(is.na(data$org_country)) ## 11 non-responses (unfinished)
sum(is.na(data$country_focus)) ## 15 non-responses  (unfinished)
sum(!is.na(data$country_focus)) ## 201 responses that get past this question

###
# Fixing Budget Variables
###

#Budget needs to be converted to USD, creating a new variable

data$org_budget_usd <- data$org_budget

# Check for any non-numeric values in the org_budget column where the currency is Euro, GBP, or CAD.

view(data[, c("org_budget", "org_budget_currency", "org_budget_currency_5_text")])

# Convert the entire org_budget column to numeric values to ensure consistency in data type.

data$org_budget <- as.numeric(as.character(data$org_budget))

# Three answers were answered incorrectly, placing the currency in the wrong box, 1) I assume that 8000.000 XAF refers to 
# 8 million. 2) one crore is the Indian term for 10 million, 3) the other example didn't specify currency, so unable to compute

data <- data %>%
  mutate(
    org_budget = case_when(
      org_budget_currency_5_text == "8000.000XAF" ~ 8000000,
      TRUE ~ as.numeric(org_budget)  
    )
  )

data <- data %>%
  mutate(
    org_budget = case_when(
      org_budget_currency_5_text == "Rs. One Crore" ~ 10000000,
      TRUE ~ as.numeric(org_budget) 
    )
  )

# Compute the converted budgets based on their currency. 
# If the currency is one other than USD, the conversion rate is applied

# Exchange rates as of 27/10/2023
# Conversion rate for Euro
conversion_rate_from_eur <- 1.06 

# Conversion rate for GBP
conversion_rate_from_gbp <- 1.23 


# Conversion rate for Canadian Dollar
conversion_rate_from_cad <- 0.74 

# Conversion rate for JPY (Japanese Yen)
conversion_rate_from_jpy <- 0.0067  # 27/10/2023

# Conversion rate for IDR (Indonesian Rupiah)
conversion_rate_from_idr <- 0.000063  # 27/10/2023

# Conversion rate for INR (Indian Rupee)
conversion_rate_from_inr <- 0.012  # 27/10/2023

# Conversion rate for BRL (Brazilian Real)
conversion_rate_from_brl <- 0.20  # 27/10/2023

# Conversion rate for MXN (Mexican Peso)
conversion_rate_from_mxn <- 0.055  # 27/10/2023

# Conversion rate for MYR (Malaysian Ringgit)
conversion_rate_from_myr <- 0.21  # 27/10/2023

# Conversion rate for XAF (Central African CFA franc)
conversion_rate_from_xaf <- 0.0016  # 27/10/2023

# Conversion rate for COP (Colombian Peso)
conversion_rate_from_cop <- 0.00024  # 27/10/2023

# Conversion rate for AUD (Australian Dollar)
conversion_rate_from_aud <- 0.64  # 27/10/2023

#Converting currency text to their currency code

data <- data %>%
  mutate(
    org_currency_other = case_when(
      org_budget_currency_5_text == "日本円" ~ "JPY",
      org_budget_currency_5_text == "円" ~ "JPY",
      org_budget_currency_5_text == "人民币" ~ "RMB",
      org_budget_currency_5_text == "Yen" ~ "JPY",
      org_budget_currency_5_text == "Rupiah" ~ "IDR",
      org_budget_currency_5_text == "rupiah" ~ "IDR",
      org_budget_currency_5_text == "Rs. One Crore" ~ "INR (crore)",
      org_budget_currency_5_text == "Real (R$) - Brasil" ~ "BRL",
      org_budget_currency_5_text == "Real (Brazil)" ~ "BRL",
      org_budget_currency_5_text == "Real - Brazil" ~ "BRL",
      org_budget_currency_5_text == "Real" ~ "BRL",
      org_budget_currency_5_text == "Pesos mexicanos" ~ "MXN",
      org_budget_currency_5_text == "Pesos" ~ "MXN",
      org_budget_currency_5_text == "Peso mexicano" ~ "MXN",
      org_budget_currency_5_text == "Myr" ~ "MYR",
      org_budget_currency_5_text == "MXN" ~ "MXN",
      org_budget_currency_5_text == "JPY" ~ "JPY",
      org_budget_currency_5_text == "INR" ~ "INR",
      org_budget_currency_5_text == "francs CFA" ~ "XAF",
      org_budget_currency_5_text == "Colombian pesos" ~ "COP",
      org_budget_currency_5_text == "BRL" ~ "BRL",
      org_budget_currency_5_text == "AUD" ~ "AUD",
      org_budget_currency_5_text == "8000.000XAF" ~ "XAF",
      org_budget_currency_5_text == "50000" ~ NA_character_,
      TRUE ~ org_budget_currency_5_text
    )
  )

# Compute org_budget_usd
data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_currency_other == "JPY" ~ org_budget * conversion_rate_from_jpy,
      org_currency_other == "IDR" ~ org_budget * conversion_rate_from_idr,
      org_currency_other == "INR" ~ org_budget * conversion_rate_from_inr,
      org_currency_other == "BRL" ~ org_budget * conversion_rate_from_brl,
      org_currency_other == "MXN" ~ org_budget * conversion_rate_from_mxn,
      org_currency_other == "MYR" ~ org_budget * conversion_rate_from_myr,
      org_currency_other == "XAF" ~ org_budget * conversion_rate_from_xaf,
      org_currency_other == "COP" ~ org_budget * conversion_rate_from_cop,
      org_currency_other == "AUD" ~ org_budget * conversion_rate_from_aud,
      org_budget_currency == "Euro" ~ org_budget * conversion_rate_from_eur,
      org_budget_currency == "GBP (British Pound)" ~ org_budget * conversion_rate_from_gbp,
      org_budget_currency == "Canadian Dollar" ~ org_budget * conversion_rate_from_cad,
      TRUE ~ org_budget 
    )
  )

# Preview data
view(data[, c("response_id", "org_budget", "org_budget_usd", "org_budget_currency", "org_currency_other")])
#Excluding a response from org_budget_usd that doesn't specify currency (Canadian, but works in three countries, so uncertain)

data$org_budget_usd[data$response_id == "R_2xWArVSC4E8aHNT"] <- NA

#Note: there are some particularly low responses from Brazil, Indonesia and India (below $10 USD annual budget)

# Found some errors while dealing with "Hong Kong (S.A.R.)", so converting to "Hong Kong SAR"
# Find indices of strings containing "Hong Kong"
hk_indices <- grep("Hong Kong", data$country_focus)

# Replace those entries with "Hong Kong SAR"
data$country_focus[hk_indices] <- "Hong Kong SAR"

# Create a dummy variable for Western vs. non-Western vs. Mixed

western_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia",
  "Slovenia", "Sweden", "Spain", "United Kingdom", "Canada", "United States of America", "Switzerland",
  "Australia", "New Zealand", "The former Yugoslav Republic of Macedonia", "Bosnia and Herzegovina", "Monaco", 
  "Ukraine", "Norway", "Poland", "Romania", "Albania", "Andorra", "San Marino"
)

non_western_countries <- c(
  "Afghanistan", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", 
  "Armenia", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belize", 
  "Benin", "Bhutan", "Bolivia", "Botswana", "Brazil", "Brunei Darussalam", 
  "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Central African Republic", 
  "Chad", "Chile", "China", "Colombia", "Comoros", "Congo, Republic of the...", "Costa Rica", 
  "Côte d'Ivoire", "Cuba", "Democratic Republic of the Congo", "Djibouti", "Dominica", "Dominican Republic", 
  "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Fiji", 
  "Gabon", "Gambia", "Georgia", "Ghana", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", 
  "Guyana", "Haiti", "Hong Kong SAR", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Israel", "Jamaica", "Japan",
  "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
  "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya",
  "Macau (S.A.R.)", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", 
  "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia, Federated States of", 
  "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", 
  "Nepal", "Nicaragua", "Niger", "Nigeria", "North Korea", "Oman", "Pakistan", 
  "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", 
  "Qatar", "Republic of Moldova",  "Russian Federation", "Rwanda", 
  "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", 
  "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", 
  "Singapore", "Somalia", "South Africa", "South Korea", "South Sudan", "Sri Lanka", "Sudan", 
  "Suriname",  "Syrian Arab Republic", "Taiwan", "Tajikistan", "Thailand",  
  "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
  "Tuvalu", "Uganda", "United Arab Emirates", "United Republic of Tanzania", "Uruguay", 
  "Uzbekistan", "Vanuatu", "Venezuela, Bolivarian Republic of", "Vietnam", "Viet Nam", "Yemen", "Zambia", 
  "Zimbabwe", "Democratic Republic of the Congo"
)

western_pattern <- paste(western_countries, collapse = "|")
non_western_pattern <- paste(non_western_countries, collapse = "|")

data <- data %>%
  mutate(western_vs_nonwestern = case_when(
    str_detect(country_focus, western_pattern) & str_detect(country_focus, non_western_pattern) ~ "Mixed",
    str_detect(country_focus, western_pattern) ~ "Western",
    str_detect(country_focus, non_western_pattern) ~ "Non-Western",
    str_detect(country_focus, "Online only") ~ "Online only",
    TRUE ~ "NA"
  ))

# Verifying

#view(data[, c("western_vs_nonwestern", "country_focus")])

#print(table(data$western_vs_nonwestern))

#     Mixed Non-Western       NA (Incomplete)     Western     Online only
#     12         127          15                  60          2

# Converting importance, obstacles, interested, satisfied and resources into factors

# Variables list
factors_importance_cols <- grep('factors_importance_', names(data), value = TRUE)
obstacles_cols <- grep('obstacles_', names(data), value = TRUE)
resources_cols <- grep('resources_', names(data), value = TRUE)

# Convert factors_importance columns
for (column in factors_importance_cols) {data[[column]] <- factor(data[[column]], 
                                                                  levels <- c("Not at all important", "Somewhat important", "Very important"), ordered=TRUE)
}

# Convert obstacles columns
for (column in obstacles_cols) {
  data[[column]] <- factor(data[[column]], levels=c("Not at all an obstacle", "Somewhat of an obstacle", "Very much an obstacle"), ordered=TRUE)
}

# Convert resources columns
for (column in resources_cols) {
  data[[column]] <- factor(data[[column]], levels=c("Not at all useful", "Somewhat useful", "Very useful"), ordered=TRUE)
}

# Convert the 'interested' variables into ordered factors
for (column in columns_to_convert_interested) {
  data[[column]] <- factor(data[[column]], 
                           levels=c("Very uninterested", "Somewhat uninterested", "Neutral", "Somewhat interested", "Very interested"), 
                           ordered=TRUE)
}

# Convert the 'satisfied' variables into ordered factors
for (column in columns_to_convert_satisfied) {
  data[[column]] <- factor(data[[column]], 
                           levels=c("Very dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", "Somewhat satisfied", "Very satisfied"), 
                           ordered=TRUE)
}

#simplify so we have both the three point and the five point scale; 

view(data)

columns_to_convert_interested <- c('interest_diet', 'interest_corp', 'interest_policy', 'interest_inst', 'interest_direct')
columns_to_convert_satisfied <- c('advocacy_satisfy_1', 'advocacy_satisfy_2', 'advocacy_satisfy_3', 'advocacy_satisfy_4', 'advocacy_satisfy_5')

# Helper function to convert levels
convert_to_3p <- function(x, type = "interest") {
  
  if (type == "interest") {
    recode_vector <- c("Very uninterested" = "Uninterested",
                       "Somewhat uninterested" = "Uninterested",
                       "Neutral" = "Neutral",
                       "Somewhat interested" = "Interested",
                       "Very interested" = "Interested")
  } else if (type == "satisfaction") {
    recode_vector <- c("Very dissatisfied" = "Dissatisfied",
                       "Somewhat dissatisfied" = "Dissatisfied",
                       "Neither satisfied nor dissatisfied" = "Neutral",
                       "Somewhat satisfied" = "Satisfied",
                       "Very satisfied" = "Satisfied")
  }
  
  return(as.factor(recode_vector[x]))
}

# Convert 'interest' columns
for (column in columns_to_convert_interested) {
  new_col <- sapply(data[[column]], convert_to_3p, type = "interest")
  data[paste(column, "3p", sep = "_")] <- factor(new_col, levels = c("Uninterested", "Neutral", "Interested"), ordered = TRUE)
}

# Convert 'satisfaction' columns
for (column in columns_to_convert_satisfied) {
  new_col <- sapply(data[[column]], convert_to_3p, type = "satisfaction")
  data[paste(column, "3p", sep = "_")] <- factor(new_col, levels = c("Dissatisfied", "Neutral", "Satisfied"), ordered = TRUE)
}

unique(data$interest_diet)
unique(data$interest_diet_3p)

view(data$interest_corp)
view(data$interest_corp_3p)


#Verification

# Check transformed columns of interest_diet for an example, numbers match ()
print("Summary for interest_diet:")
print(summary(data$interest_diet))
print("Summary for interest_diet_3p:")
print(summary(data$interest_diet_3p))

# Verify changes for factors_importance columns
for (column in factors_importance_cols) {
  cat("\nSummary for", column, ":\n")
  print(summary(data[[column]]))
}

# Verify changes for obstacles columns
for (column in obstacles_cols) {
  cat("\nSummary for", column, ":\n")
  print(summary(data[[column]]))
}

# Verify changes for resources columns
for (column in resources_cols) {
  cat("\nSummary for", column, ":\n")
  print(summary(data[[column]]))
}

# Recoding responses that may fit into other categories: I think I want to leave them as "other": most of them also selected an non-other option that is close to their "other" option:

view(data[, c("org_advocacy", "org_advocacy_6_text")])

#
#Factorising all variables
#

# Columns to factorize
cols_to_factorize <- c(
  "animal_type", "org_years", "org_geographic_lvl", "org_country", "org_size", "org_budget_currency", 
  "org_mission",  "country_focus", "org_advocacy", "org_focus", "advocacy_diet", "advocacy_corporate","advocacy_policy", "advocacy_inst", 
  "participant_role", "q_language", "western_vs_nonwestern"
)

# Factorizing the columns
data[cols_to_factorize] <- lapply(data[cols_to_factorize], factor)

# Check the classes after factorization
sapply(data[cols_to_factorize], class)

# -------------------------------
# Save recoded dataset 
# -------------------------------
write.csv(recoded_data, "data_recoded.csv")
save(recoded_data, file='data_recoded.Rdata')
