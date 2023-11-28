
#Title: Recoding data
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

## Remove all objects from the current workspace 
rm(list = ls())

# Load required libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(stringr)
library (naniar)

#Set working directory:
setwd("C:/Users/jack_/OneDrive/Documents/Github/International-Study-Of-Strategies-And-Needs")

# Read the cleaned data
load("data_cleaned.RData")

data <- clean_data

glimpse(data)

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

data <- data %>%
  rename(animal_type_text = animal_type_8_text,
         diet_disinterested_motivation = interest_diet_r1,
         diet_interested_not_funding_motivation = interest_diet_r2,
         corporate_disinterested_motivation = interest_corp_r1,
         corporate_interested_not_funding_motivation = interest_corp_r2,
         policy_disinterested_motivation = interest_policy_r1,
         policy_interested_not_funding_motivation = interest_policy_r2,
         institutional_disinterested_motivation = interest_inst_r1,
         institutional_interested_not_funding_motivation = interest_inst_r2,
         direct_disinterested_motivation = interest_direct_r1,
         direct_interested_not_funding_motivation = interest_direct_r2,
         satisfaction_diet_advocacy = advocacy_satisfy_1,
         satisfaction_corp_advocacy = advocacy_satisfy_2,
         satisfaction_policy_advocacy = advocacy_satisfy_3,
         satisfaction_institutional_advocacy = advocacy_satisfy_4,
         satisfaction_direct_advocacy = advocacy_satisfy_5,
         satisfaction_other_advocacy = advocacy_satisfy_6,
         satisfaction_other_advocacy_text = advocacy_satisfy_6_text,
         diet_dissatisfied_motivation = diet_dissatisfy,
         corporate_dissatisfied_motivation = corporate_dissatisfy,
         policy_dissatisfied_motivation = policy_dissatisfy,
         institutional_dissatisfied_motivation = insti_dissatisfy,
         direct_dissatisfied_motivation = direct_dissatisfy,
         other_dissatisfied_motivation = other_dissatisfy,
         importance_funding_availability = factors_importance_1,
         importance_talent_availability = factors_importance_2,
         importance_context_appropriateness = factors_importance_3,
         importance_impact_cost_effectiveness = factors_importance_4,
         importance_mission_alignment = factors_importance_5,
         obstacles_legal_regulatory_barriers = obstacles_1,
         obstacles_political_legal_influence = obstacles_2,
         obstacles_public_awareness_support = obstacles_3,
         obstacles_lack_of_training_skills = obstacles_4,
         obstacles_lack_of_staff = obstacles_5,
         obstacles_lack_of_funding = obstacles_6,
         resources_usefulness_financial = resources_1,
         resources_usefulness_grant_applications = resources_2,
         resources_usefulness_professional_development = resources_3,
         resources_usefulness_staff_well_being = resources_4,
         resources_usefulness_finding_talent = resources_5,
         resources_usefulness_research_data_access = resources_6,
         resources_usefulness_collaboration_networking = resources_7,
         resources_usefulness_professional_mentorship = resources_8)

# -------------------------------
# Recoding country variables
# -------------------------------

# Storing original data for validation purposes 

original_data <- data

# Found some errors while dealing with "Hong Kong (S.A.R.)" and "Congo, Republic of the...", so converting to "Hong Kong SAR" and "Republic of the Congo" to remove punctuation that could influence the code

data$country_focus <- gsub("Hong Kong \\(S\\.A\\.R\\.\\)", "Hong Kong SAR", data$country_focus)
data$country_focus <- gsub("Congo, Republic of the\\.{3}", "Republic of the Congo", data$country_focus)

data$org_country <- gsub("Hong Kong \\(S\\.A\\.R\\.\\)", "Hong Kong SAR", data$org_country)
data$org_country <- gsub("Congo, Republic of the\\.{3}", "Republic of the Congo", data$org_country)

# Background: We had an issue with Qualtrics where we were unable to move certain countries around for respondents in different languages without disabling the multiple choice selection, 
# and this would make it difficult for respondents to find their given country. The solution we found was to translate the countries Afghanistan, Albania, and Algeria into the more likely options for a given translated non-English option, 
# so that when they respond, the country where this language is spoken is the 1st-3rd choice. When someone chooses the first option (which is read as 'Japan', if the language is JP),
# it is automatically stored in the data as "Afghanistan". This can be the case with multiple options, for example, for  ZH-T/ traditional Chinese we replaced Afghanistan with Taiwan, Albania with Hong Kong, and Algeria with Macau.)

data$uncorrected_org_country = data$org_country
data$uncorrected_country_focus = data$country_focus 

# When combined with a given Q_language variable, we can recode to the desired choice. 

data <- data %>% 
  mutate(
    org_country = case_when(
      q_language == "PT-BR" ~ str_replace_all(org_country, c("Afghanistan" = "Brazil", "Albania" = "Portugal")),
      q_language == "JA" ~ str_replace_all(org_country, "Afghanistan", "Japan"),
      q_language == "TGL" ~ str_replace_all(org_country, "Afghanistan", "Philippines"),
      q_language == "ZH-S" ~ str_replace_all(org_country, "Afghanistan", "China"),
      q_language == "ID" ~ str_replace_all(org_country, "Afghanistan", "Indonesia"),
      q_language == "TH" ~ str_replace_all(org_country, "Afghanistan", "Thailand"),
      q_language == "NE" ~ str_replace_all(org_country, "Afghanistan", "Nepal"),
      q_language == "VI" ~ str_replace_all(org_country, "Afghanistan", "Viet Nam"),
      q_language == "HI" ~ str_replace_all(org_country, "Afghanistan", "India"),
      q_language == "ZH-T" ~ str_replace_all(org_country, c("Afghanistan" = "Taiwan", "Albania" = "Hong Kong SAR", "Algeria" = "Macau")),
      TRUE ~ org_country
    )
  )


data <- data %>% 
  mutate(
    country_focus = case_when(
      q_language == "PT-BR" ~ str_replace_all(country_focus, c("Afghanistan" = "Brazil", "Albania" = "Portugal")),
      q_language == "JA" ~ str_replace_all(country_focus, "Afghanistan", "Japan"),
      q_language == "TGL" ~ str_replace_all(country_focus, "Afghanistan", "Philippines"),
      q_language == "ZH-S" ~ str_replace_all(country_focus, "Afghanistan", "China"),
      q_language == "ID" ~ str_replace_all(country_focus, "Afghanistan", "Indonesia"),
      q_language == "TH" ~ str_replace_all(country_focus, "Afghanistan", "Thailand"),
      q_language == "NE" ~ str_replace_all(country_focus, "Afghanistan", "Nepal"),
      q_language == "VI" ~ str_replace_all(country_focus, "Afghanistan", "Viet Nam"),
      q_language == "HI" ~ str_replace_all(country_focus, "Afghanistan", "India"),
      q_language == "ZH-T" ~ str_replace_all(country_focus, c("Afghanistan" = "Taiwan", "Albania" = "Hong Kong SAR", "Algeria" = "Macau")),
      TRUE ~ country_focus
    )
  )

# Code to verify, note that we left countries in the same order for Spanish. 

filtered_data <- data %>% filter(q_language != "EN" & q_language != "ES-ES")
view(filtered_data[, c("uncorrected_org_country", "org_country", "uncorrected_country_focus", "country_focus", "q_language")])

# We have two variables 1) org_country (all countries they worked in), and 2) country_focus (only for those who gave multiple responses for org_country)
# Those who chose a single response for org_country did not answer country_focus, therefore this code copies org_country to country_focus 

data <- data %>%
  mutate(country_focus = ifelse(nchar(org_country) - nchar(gsub(",", "", org_country)) == 0,
                                org_country, country_focus))

#Verifying that it copies all relevant countries - some respondents unfinished

View(data[, c("org_country", "country_focus")]) 

sum(is.na(data$org_country)) ## 2 non-responses (unfinished)
sum(is.na(data$country_focus)) ## 6 non-responses  (unfinished)
sum(!is.na(data$country_focus)) ## 206 responses with valid responses

# -------------------------------
# Recoding budget variables
# -------------------------------

#Budget needs to be converted to USD, creating a new numeric variable, also correcting the responses that use 1.000 to refer to 1000

data$org_budget_usd <- as.numeric(gsub("\\.000", "000", data$org_budget))

# Check for any incorrect or non-numeric values in the org_budget column.

view(data[, c("org_budget", "org_budget_usd", "org_budget_currency", "org_budget_currency_5_text")])

# Five answers were answered incorrectly, placing the currency in the wrong box. Notes: 1) I assume that 8000.000 XAF refers to 8 million Francs (using a decimal point in this way is common in Francophone areas)
# 2) one crore is the Indian term for 10 million, Lakh is the term for 100,000; 3) one other example didn't specify currency, so unable to compute
# For these examples I manually edit the number here, and the currency below:

data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_budget_currency_5_text == "8000.000XAF" ~ 8000000,
      TRUE ~ as.numeric(org_budget_usd)  
    )
  )


data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_budget_currency_5_text == "NRs7200000" ~ 7200000,
      TRUE ~ as.numeric(org_budget_usd)  
    )
  )

data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_budget_currency_5_text == "Rs. One Crore" ~ 10000000,
      TRUE ~ as.numeric(org_budget_usd) 
    )
  )

data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_budget_currency_5_text == "Rs 5 lakhs" ~ 500000,
      TRUE ~ as.numeric(org_budget_usd) 
    )
  )

data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_budget_currency_5_text == "लगभग 1,00,00,000 रुपए" ~ 10000000,
      TRUE ~ as.numeric(org_budget_usd) 
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

# Conversion rate for NRS (Nepalese Rupee)
conversion_rate_from_nrs <- 0.0075  # 07/11/2023

# Conversion rate for RMB (Chinese Yuan Renminbi)
conversion_rate_from_rmb <- 0.14  # 07/11/2023

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
      org_budget_currency_5_text == "Rs. One Crore" ~ "INR",
      org_budget_currency_5_text == "Rs 5 lakhs" ~ "INR",
      org_budget_currency_5_text == "लगभग 1,00,00,000 रुपए" ~ "INR",
      org_budget_currency_5_text == "Rupees( Indian Currency)" ~ "INR",
      org_budget_currency_5_text == "Indian Rupees" ~ "INR",
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
      org_budget_currency_5_text == "NRs7200000" ~ "NRS",
      org_budget_currency_5_text == "50000" ~ NA_character_,
      TRUE ~ org_budget_currency_5_text
    )
  )

# Compute org_budget_usd
data <- data %>%
  mutate(
    org_budget_usd = case_when(
      org_currency_other == "JPY" ~ org_budget_usd * conversion_rate_from_jpy,
      org_currency_other == "IDR" ~ org_budget_usd * conversion_rate_from_idr,
      org_currency_other == "INR" ~ org_budget_usd * conversion_rate_from_inr,
      org_currency_other == "BRL" ~ org_budget_usd * conversion_rate_from_brl,
      org_currency_other == "MXN" ~ org_budget_usd * conversion_rate_from_mxn,
      org_currency_other == "MYR" ~ org_budget_usd * conversion_rate_from_myr,
      org_currency_other == "XAF" ~ org_budget_usd * conversion_rate_from_xaf,
      org_currency_other == "RMB" ~ org_budget_usd * conversion_rate_from_rmb,
      org_currency_other == "COP" ~ org_budget_usd * conversion_rate_from_cop,
      org_currency_other == "AUD" ~ org_budget_usd * conversion_rate_from_aud,
      org_currency_other == "NRS" ~ org_budget_usd * conversion_rate_from_nrs,
      org_budget_currency == "Euro" ~ org_budget_usd * conversion_rate_from_eur,
      org_budget_currency == "GBP (British Pound)" ~ org_budget_usd * conversion_rate_from_gbp,
      org_budget_currency == "Canadian Dollar" ~ org_budget_usd * conversion_rate_from_cad,
      TRUE ~ org_budget_usd 
    )
  )

# Preview data

view(data[, c("response_id", "org_budget", "org_budget_usd", "org_budget_currency", "org_currency_other")])

#Excluding a response from org_budget_usd that doesn't specify currency (Canadian, but works in three countries, so uncertain)
data$org_budget_usd[data$response_id == "R_2xWArVSC4E8aHNT"] <- NA

# -------------------------------
# Recoding Western/ Non-Western Country Dummy Variables 
# -------------------------------

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
  "Chad", "Chile", "China", "Colombia", "Comoros", "Republic of the Congo", "Costa Rica", 
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
  "Uzbekistan", "Vanuatu", "Venezuela, Bolivarian Republic of", "Viet Nam", "Yemen", "Zambia", 
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
    TRUE ~ NA
  ))

# Verifying

view(data[, c("western_vs_nonwestern", "country_focus", "org_country")])

print(table(data$western_vs_nonwestern))

#     Mixed Non-Western       NA (Incomplete)     Western     Online only
#     11         132          6                  61          2

# -------------------------------
# Recoding likert variables
# -------------------------------

# Variables list
factors_importance_cols <- grep('importance_', names(data), value = TRUE)
obstacles_cols <- grep('obstacles_', names(data), value = TRUE)
resources_cols <- grep('resources_', names(data), value = TRUE)
interested_cols <- grep('interest_', names(data), value = TRUE)
satisfied_cols <- grep('satisfaction_', names(data), value = TRUE)

#Convert all of the above into ordered factors

for (column in factors_importance_cols) {data[[column]] <- factor(data[[column]], 
                                                                  levels <- c("Not at all important", "Somewhat important", "Very important"), ordered=TRUE)
}

for (column in obstacles_cols) {
  data[[column]] <- factor(data[[column]], levels=c("Not at all an obstacle", "Somewhat of an obstacle", "Very much an obstacle"), ordered=TRUE)
}

for (column in resources_cols) {
  data[[column]] <- factor(data[[column]], levels=c("Not at all useful", "Somewhat useful", "Very useful"), ordered=TRUE)
}

for (column in interested_cols) {
  data[[column]] <- factor(data[[column]], 
                           levels=c("Very uninterested", "Somewhat uninterested", "Neutral", "Somewhat interested", "Very interested"), 
                           ordered=TRUE)
}

for (column in satisfied_cols) {
  data[[column]] <- factor(data[[column]], 
                           levels=c("Very dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", "Somewhat satisfied", "Very satisfied"), 
                           ordered=TRUE)
}

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

# Convert 'interest' and 'satisfaction' columns using convert_to_3p function
for (column in interested_cols) {
  new_col <- sapply(data[[column]], convert_to_3p, type = "interest")
  data[paste(column, "3p", sep = "_")] <- factor(new_col, levels = c("Uninterested", "Neutral", "Interested"), ordered = TRUE)
}

for (column in satisfied_cols) {
  new_col <- sapply(data[[column]], convert_to_3p, type = "satisfaction")
  data[paste(column, "3p", sep = "_")] <- factor(new_col, levels = c("Dissatisfied", "Neutral", "Satisfied"), ordered = TRUE)
}

#Confirm levels

unique(data$interest_diet)
unique(data$interest_diet_3p)

#Verification

view(data[, c("interest_corp", "interest_corp_3p")])

# Check transformed columns of interest_diet for an example, numbers match (13 Uninterested (9+4), 19 Neutral; 33 interested (19+14), 147 NAs)
print(summary(data$interest_diet))
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

# -------------------------------
# Factorising other variables
# -------------------------------

# Copies mutually exclusive variable for org_focus

data <- data %>%
  mutate(
    org_focus = ifelse(
      is.na(org_focus) & 
        (str_count(org_advocacy, "Other") + 
           str_count(org_advocacy, "Policy") + 
           str_count(org_advocacy, "Individual") + 
           str_count(org_advocacy, "Corporate") + 
           str_count(org_advocacy, "Direct") +
           str_count(org_advocacy, "Institutional") == 1), 
      org_advocacy, 
      org_focus
    )
  )

#Verifying
org_advocacy_focus <- data[c("org_focus", "org_advocacy")]
view(org_advocacy_focus)

# Check current levels (none currently factorised)
unique(data$animal_type)
unique(data$org_advocacy)
unique(data$org_size)
unique(data$org_years)

# Reorder 'org_size' levels
data$org_size <- factor(data$org_size, 
                        levels = c("Less than 1", "1-5", "6-10", "11-20", "21-40", "41-100", "101+"),
                        ordered = TRUE)

# Reorder 'org_years' levels
data$org_years <- factor(data$org_years, 
                         levels = c("Less than 1 year", "1-2 years", "3-5 years", "6-10 years", "More than 10 years"),
                         ordered = TRUE)

# Additional recoding for the farmed animal variable: 
# Create 8 binary variables for: Farmed land animals, farmed aquatic animals, dogs/ cats used for meat, companion animals, lab animals, wild animals, other captive animals, Other

data$animal_type_dogcat_meat = as.integer(grepl("Dogs/cats used for meat", data$animal_type))
data$animal_type_companion = as.integer(grepl("Companion animals", data$animal_type))
data$animal_type_other = as.integer(grepl("Other animals", data$animal_type))
data$animal_type_wild = as.integer(grepl("Wild animals", data$animal_type))
data$animal_type_lab = as.integer(grepl("Lab animals", data$animal_type))
data$animal_type_captive = as.integer(grepl("Other captive animals", data$animal_type))
data$animal_type_aquatic_farm = as.integer(grepl("Farmed aquatic animals", data$animal_type))
data$animal_type_land_farm = as.integer(grepl("Farmed land animals", data$animal_type))

# Recode other options to integrate into these categories

update_categories <- function(description, existing_categories) {
  categories <- existing_categories
  
  # Use grepl for regex matching and combine conditions (using (|)) - note: "遭走私的活體動物" means smuggled animals, "Resgate de animais em situação de Desastres" means rescuing disaster animals
  all_animals_pattern <- "All animals|Animals in distress|All animals being exploited by people|sick ,injured abandoned animals|Humanxs y todxs lxs animales y sus ecosistemas|I leave no stone unturned. Compassion versus exploitation and violence.|Semua hewan|Todos los animales|सभी प्रकार के जानवर जिनको मदद या इलाज की जरूरत है ।हम उनकी मदद करते हैं"
  if (grepl(all_animals_pattern, description)) {
    categories <- lapply(categories, function(x) 1)
  } else if (grepl("community dogs and cats", description)) {
    categories$animal_type_companion <- 1
  } else if (grepl("Cow and calf", description)) {
    categories$animal_type_land_farm <- 1
  } else if (grepl("farmed land and aquatic animals, along with wildlife", description)) {
    categories$animal_type_aquatic_farm <- 1
    categories$animal_type_land_farm <- 1
    categories$animal_type_wild <- 1
  } else if (grepl("Horses owned as property", description)) {
    categories$animal_type_captive <- 1
  } else if (grepl("Insects", description)) {
    categories$animal_type_other <- 1
  } else if (grepl("Resgate de animais em situação de Desastres", description)) {
    categories$animal_type_other <- 1
  } else if (grepl("since Nepal has introduced policy of wildanimal farming", description)) {
    categories$animal_type_wild <- 1
  } else if (grepl("we focus on diet, so all animals that can be slaughtered for food", description)) {
    categories$animal_type_aquatic_farm <- 1
    categories$animal_type_land_farm <- 1
    categories$animal_type_wild <- 1
  } else if (grepl("Wild aquatic animals caught for human consumption", description)) {
    categories$animal_type_wild <- 1
  } else if (grepl("Working animals like horse, donkey", description)) {
    categories$animal_type_captive <- 1
  } else if (grepl("遭走私的活體動物", description)) {
    categories$animal_type_other <- 1
  }
  
  return(categories)
}

# Using function to update categories

data <- data %>%
  rowwise() %>%
  mutate(categories = list(update_categories(
    animal_type_text, 
    list(
      animal_type_dogcat_meat = animal_type_dogcat_meat, 
      animal_type_companion = animal_type_companion,
      animal_type_other = animal_type_other,
      animal_type_wild = animal_type_wild,
      animal_type_lab = animal_type_lab,
      animal_type_captive = animal_type_captive,
      animal_type_aquatic_farm = animal_type_aquatic_farm,
      animal_type_land_farm = animal_type_land_farm
    )
  ))) 

# Adjusting the column selection for verifying

selected_data_animal_type <- data %>%
  select(animal_type, animal_type_text, animal_type_other, 
         animal_type_dogcat_meat, animal_type_companion, animal_type_other,
         animal_type_wild, animal_type_lab, animal_type_captive,
         animal_type_aquatic_farm, animal_type_land_farm)

view(selected_data_animal_type)

print(table(data$animal_type_aquatic_farm)) # 111 work on aquatic farmed animals
print(table(data$animal_type_land_farm))# 197 work on land farmed animals
print(table(data$animal_type_dogcat_meat))# 46 work on dog/cat meat

# -------------------------------
# Recoding org_advocacy variables
# -------------------------------

data$advocacy_type_corporate <- as.integer(grepl("Corporate campaigns", data$org_advocacy))
data$advocacy_type_policy <- as.integer(grepl("Policy campaigns", data$org_advocacy))
data$advocacy_type_institutional <- as.integer(grepl("Institutional campaigns", data$org_advocacy))
data$advocacy_type_direct_work <- as.integer(grepl("Direct work", data$org_advocacy))
data$advocacy_type_individual_diet <- as.integer(grepl("Individual diet outreach", data$org_advocacy))
data$advocacy_type_other <- as.integer(grepl("Other", data$org_advocacy))

data$advocacy_type_institutional[data$org_advocacy_6_text == "Educational Programs in Schools and Outreach Events"] <- 1
data$advocacy_type_individual_diet[data$org_advocacy_6_text == "Influence people about benefit from being Vegetarian & Vegan. Introduce the delicious food that not come from animals"] <- 1

selected_data_advocacy <- data %>%
  select(org_advocacy, org_advocacy_6_text,
         advocacy_type_corporate, advocacy_type_policy,
         advocacy_type_institutional, advocacy_type_direct_work,
         advocacy_type_individual_diet, advocacy_type_other)

view(selected_data_advocacy)

# Columns to factorize
cols_to_factorize <- c(
  "org_years", "org_geographic_lvl", "org_size", 
  "org_mission",  "org_advocacy", "org_focus", "advocacy_diet", "advocacy_corporate","advocacy_policy", "advocacy_inst", 
  "participant_role", "western_vs_nonwestern")

# Factorizing the columns
data[cols_to_factorize] <- lapply(data[cols_to_factorize], factor)

# Verifying levels
levels(data$org_years) # 5
levels(data$org_geographic_lvl) # 3
levels(data$org_size) # 7
levels(data$org_mission) # 4
levels(data$western_vs_nonwestern) # 4
levels(data$org_focus) # 6


# -------------------------------
# Recoding advocacy_diet variables
# -------------------------------

data$advocacy_diet_inperson <- as.integer(grepl("In-person", data$advocacy_diet))
data$advocacy_diet_online <- as.integer(grepl("Online", data$advocacy_diet))
data$advocacy_diet_community <- as.integer(grepl("Community", data$advocacy_diet))
data$advocacy_diet_other <- as.integer(grepl("Other", data$advocacy_diet))

data$advocacy_corporate_producers_cage_crate_free <- as.integer(grepl("Campaigning for producers", data$advocacy_corporate))
data$advocacy_corporate_business_vegan <- as.integer(grepl("Campaigns to increase", data$advocacy_corporate))
data$advocacy_corporate_business_ethical_other <- as.integer(grepl("Campaigns for businesses", data$advocacy_corporate))
data$advocacy_corporate_other <- as.integer(grepl("Other", data$advocacy_corporate))

data$advocacy_policy_cage_crate_free_legislation <- as.integer(grepl("Cage-free", data$advocacy_policy))
data$advocacy_policy_other_welfare_legislation <- as.integer(grepl("Other legislation or", data$advocacy_policy))
data$advocacy_policy_plantbased_label_regulation <- as.integer(grepl("related to plant-based", data$advocacy_policy))
data$advocacy_policy_meat_label_regulation <- as.integer(grepl("related to meat", data$advocacy_policy))
data$advocacy_policy_other <- as.integer(grepl("Please specify", data$advocacy_policy))

data$advocacy_inst_educating_students_teachers <- as.integer(grepl("Educating students", data$advocacy_inst))
data$advocacy_inst_vegan_vegetarian_educational_institutions <- as.integer(grepl("educational institutions", data$advocacy_inst))
data$advocacy_inst_vegan_vegetarian_prisons <- as.integer(grepl("prisons", data$advocacy_inst))
data$advocacy_inst_vegan_vegetarian_hospitals <- as.integer(grepl("hospitals", data$advocacy_inst))
data$advocacy_inst_other <- as.integer(grepl("Other", data$advocacy_inst))

# Create a subset with just the new variables
new_vars_diet <- data[, grepl("advocacy_diet", names(data))]
new_vars_corporate <- data[, grepl("advocacy_corporate", names(data))]
new_vars_policy <- data[, grepl("advocacy_policy", names(data))]
new_vars_institutional <- data[, grepl("advocacy_inst", names(data))]

#Confirming subsets

View(new_vars_diet)
View(new_vars_corporate)
View(new_vars_policy)
View(new_vars_institutional)

# -------------------------------
# Verifying classes and factors
# -------------------------------

# Check the classes after factorization
sapply(data[cols_to_factorize], class)

recoded_data <- data 

# -------------------------------
# Save recoded dataset 
# -------------------------------

save(recoded_data, file='data_recoded.Rdata')
