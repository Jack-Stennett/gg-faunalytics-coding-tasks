
#Title: Descriptive statistics
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

## Remove all objects from the current workspace 
rm(list = ls())

# Load required libraries
library(tidyverse)
library(knitr)
library(htmlwidgets)
library(plotly)

#Set working directory
setwd("C:/Users/jack_/OneDrive/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

# Create the "Exported Descriptive Tables" folder if it doesn't exist
if (!file.exists("Exported Descriptive Tables")) {
  dir.create("Exported Descriptive Tables")
}

# Read the recoded data
load("data_recoded.RData")

data <- recoded_data

glimpse(data)

# ------------------------------------
# Functions for creating summary tables,
# ------------------------------------

# Creating basic frequency tables

create_summary_table <- function(data, column_name, header_names) {
  # Check if the column exists in the dataframe
  if (!column_name %in% names(data)) {
    stop("Column not found in the data.")
  }
  
  # Filter out NA values and summarize the data
  summary <- data %>%
    filter(!is.na(.data[[column_name]])) %>%
    group_by(.data[[column_name]]) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(percentage = (count / sum(count)) * 100)
    print (summary)
  
  # Calculate the total count
  total_count <- sum(summary$count)
  total_percentage <- 100  
  
  # Add total row to the summary with matching column names
  total_row <- data.frame(`.data[[column_name]]` = "Total", count = total_count, percentage = total_percentage)
  names(total_row) <- names(summary)  # Ensure the column names match
  summary <- rbind(summary, total_row)
  
  # Bold the header titles and add "Percentage" column
  bold_headers <- sapply(c(header_names, "Percentage"), function(x) paste0("<b>", x, "</b>"))
  
  # Create a formatted table with plotly
  p <- plot_ly(summary, type = 'table',
          header = list(values = bold_headers),
          cells = list(values = rbind(as.character(summary[[column_name]]), summary$count, round(summary$percentage, 2))))
  
file_name <- paste0("Exported Descriptive Tables", "/", column_name, ".html")
  
# Save the plotly object as an HTML file
saveWidget(p, file = file_name, selfcontained = TRUE)
}

###
#Function for binary variables
###

create_binary_summary_table <- function(data, binary_variable_names) {
  # Create an empty data frame to store the results
  summary_df <- data.frame(Variable = character(),
                           Count_1 = integer(),
                           Percentage_1 = numeric())
  
  # Loop through each binary variable
  for (variable_name in binary_variable_names) {
    # Check if the variable exists in the dataframe
    if (!variable_name %in% names(data)) {
      stop(paste("Variable", variable_name, "not found in the data."))
    }
    
    # Calculate the count and percentage of 1's for the current variable
    total_count <- sum(!is.na(data[[variable_name]]))
    count_1 <- sum(data[[variable_name]] == 1, na.rm = TRUE)
    percentage_1 <- (count_1 / total_count) * 100
    
    # Bind the results into the summary dataframe
    summary_df <- rbind(summary_df, data.frame(Variable = variable_name,
                                               Count_1 = count_1,
                                               Percentage_1 = percentage_1))
    print(summary_df)
  }
  
  # Return the Plotly interactive table
  p <- plot_ly(summary_df, type = 'table',
                 header = list(values = c("Variable", "Count", "Percentage")),
                 cells = list(values = rbind(summary_df$Variable, summary_df$Count_1, round(summary_df$Percentage_1, 2))))
  
  file_name <- paste0("Exported Descriptive Tables", "/", variable_name, ".html")
  
  # Save the plotly object as an HTML file
  saveWidget(p, file = file_name, selfcontained = TRUE)
}

###
# Creating function for Numeric Stats Table
###

create_stats_table <- function(data, variable_name) {
  # Check if the variable exists in the dataframe
  if (!variable_name %in% names(data)) {
    stop("Variable not found in the data.")
  }
  
  # Extract the variable and remove NA values for accurate summary statistics
  num_variable <- na.omit(data[[variable_name]])
  
  # Calculate summary statistics
  summary_stats <- summary(num_variable)
  
  # Additional detailed statistics
  detailed_stats <- c(summary_stats, 
                      Mean = mean(num_variable), 
                      SD = sd(num_variable), 
                      Median = median(num_variable))
  
  print(detailed_stats)
  
  # Create a data frame from the detailed statistics for a neat table format
  stats_df <- data.frame(Statistic = names(detailed_stats), Value = detailed_stats)
  
  # Return the Plotly interactive table
  p <- plot_ly(stats_df, type = 'table',
                 header = list(values = c("Statistic", "Value")),
                 cells = list(values = rbind(stats_df$Statistic, stats_df$Value)))
  
  # Generate file name based on the variable name
  file_name <- paste0("Exported Descriptive Tables", "/", variable_name, ".html")
  saveWidget(p, file = file_name, selfcontained = TRUE)
}

###
#Descriptives Tables
###

# Budget Stats Table

create_stats_table(data, "org_budget_usd")

# Summary table for non-likert, factor variables

variables_to_summarize <- c("user_language", "org_years", "org_geographic_lvl",
                            "western_vs_nonwestern",
                            "org_size", "org_mission",
                            "org_focus")

summaries <- lapply(variables_to_summarize, function(var) {
  create_summary_table(data, var, c(var, "Count"))
})

print(summaries[]) # Replace 1 with the index of the variable you want to print

####
#Descriptives for Likert variables ()
####

create_summary_table(data, "satisfaction_diet_advocacy", c("satisfaction_diet_advocacy", "Count"))

interest_types <- c("interest_diet", "interest_corp", "interest_policy", "interest_inst", "interest_direct")
lapply(interest_types, function(var) create_summary_table(data, var, c(var, "Count")))

interest_types_3p <- c("interest_diet_3p", "interest_corp_3p", "interest_policy_3p", "interest_inst_3p", "interest_direct_3p")
lapply(interest_types_3p, function(var) create_summary_table(data, var, c(var, "Count")))

satisfaction_advocacy <- c("satisfaction_diet_advocacy", "satisfaction_corp_advocacy", "satisfaction_policy_advocacy",
                           "satisfaction_institutional_advocacy", "satisfaction_direct_advocacy", "satisfaction_other_advocacy")
lapply(satisfaction_advocacy, function(var) create_summary_table(data, var, c(var, "Count")))

satisfaction_advocacy_3p <- c("satisfaction_diet_advocacy_3p", "satisfaction_corp_advocacy_3p", "satisfaction_policy_advocacy_3p",
                              "satisfaction_institutional_advocacy_3p", "satisfaction_direct_advocacy_3p", "satisfaction_other_advocacy_3p")
lapply(satisfaction_advocacy_3p, function(var) create_summary_table(data, var, c(var, "Count")))

importance_factors <- c("importance_funding_availability", "importance_talent_availability", "importance_context_appropriateness",
                        "importance_impact_cost_effectiveness", "importance_mission_alignment")
lapply(importance_factors, function(var) create_summary_table(data, var, c(var, "Count")))

obstacles_impact <- c("obstacles_legal_regulatory_barriers", "obstacles_political_legal_influence", "obstacles_public_awareness_support",
                      "obstacles_lack_of_training_skills", "obstacles_lack_of_staff", "obstacles_lack_of_funding")
lapply(obstacles_impact, function(var) create_summary_table(data, var, c(var, "Count")))

resources_usefulness <- c("resources_usefulness_financial", "resources_usefulness_grant_applications",
                          "resources_usefulness_professional_development", "resources_usefulness_staff_well_being",
                          "resources_usefulness_finding_talent", "resources_usefulness_research_data_access",
                          "resources_usefulness_collaboration_networking", "resources_usefulness_professional_mentorship")
lapply(resources_usefulness, function(var) create_summary_table(data, var, c(var, "Count")))

###
#Descriptives for multiple binary variables
###

# Animal_binary_variable types

animal_binary_variables <- c("animal_type_aquatic_farm", "animal_type_captive", "animal_type_companion", 
                           "animal_type_dogcat_meat", "animal_type_lab", "animal_type_land_farm", 
                           "animal_type_other", "animal_type_wild") # Replace with actual variable names

# Org Advocacy Types
org_advocacy_types <- c("advocacy_type_corporate", "advocacy_type_policy", 
                        "advocacy_type_institutional", "advocacy_type_direct_work", 
                        "advocacy_type_individual_diet", "advocacy_type_other")

# Org Advocacy Sub Types - Diet
advocacy_diet <- c("advocacy_diet_inperson", "advocacy_diet_online", 
                   "advocacy_diet_community", "advocacy_diet_other")

# Org Advocacy Sub Types - Corporate
advocacy_corporate <- c("advocacy_corporate_producers_cage_crate_free", 
                        "advocacy_corporate_business_vegan", 
                        "advocacy_corporate_business_ethical_other", 
                        "advocacy_corporate_other")

# Org Advocacy Sub Types - Policy
advocacy_policy <- c("advocacy_policy_cage_crate_free_legislation", 
                     "advocacy_policy_other_welfare_legislation", 
                     "advocacy_policy_plantbased_label_regulation", 
                     "advocacy_policy_meat_label_regulation", 
                     "advocacy_policy_other")

# Org Advocacy Sub Types - Institutional
advocacy_institutional <- c("advocacy_inst_educating_students_teachers", 
                            "advocacy_inst_vegan_vegetarian_educational_institutions", 
                            "advocacy_inst_vegan_vegetarian_prisons", 
                            "advocacy_inst_vegan_vegetarian_hospitals", 
                            "advocacy_inst_other")

# Call the function for each category and sub-category
create_binary_summary_table(data, org_advocacy_types)
create_binary_summary_table(data, advocacy_diet)
create_binary_summary_table(data, advocacy_corporate)
create_binary_summary_table(data, advocacy_policy)
create_binary_summary_table(data, advocacy_institutional)
create_binary_summary_table(data, animal_binary_variables)

#######
# Additional variables - splitting org_country, country_focus and participant_role into their separate categories
#######

#Identify number of total non-NA responses

total_responses <- sum(!is.na(data$org_country))

# Split the 'org_country' column into individual countries and convert to a long format
org_country_data_long <- data %>%
  mutate(org_country_list = strsplit(as.character(org_country), ",\\s*")) %>%
  unnest(org_country_list)

# Count the occurrences of each country
org_country_summary <- org_country_data_long %>%
  group_by(org_country_list) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / total_responses) * 100)

org_country_summary <- org_country_summary %>%
  arrange(desc(count))

# View the summary in the console
print(org_country_summary)

# Create the Plotly interactive table
p <- plot_ly(org_country_summary, type = 'table',
             header = list(values = list("Country", "Count", "Percentage")),
             cells = list(values = list(org_country_summary$org_country_list, org_country_summary$count, round(org_country_summary$percentage, 2))))

# Define the filename with subfolder path + save plotly object as an HTML file
file_name <- "Exported Descriptive Tables/org_country_summary.html"
saveWidget(p, file = file_name, selfcontained = TRUE)


#Country Focus

total_responses <- sum(!is.na(data$country_focus))

# Split the 'org_country' column into individual countries and convert to a long format
country_focus_data_long <- data %>%
  mutate(country_focus_list = strsplit(as.character(country_focus), ",\\s*")) %>%
  unnest(country_focus_list)

# Count occurrences of each country
country_focus_summary <- country_focus_data_long %>%
  group_by(country_focus_list) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / total_responses) * 100)

country_focus_summary <- country_focus_summary %>%
  arrange(desc(count))

# View the summary
print(country_focus_summary)

# Create the Plotly interactive table
p <- plot_ly(country_focus_summary, type = 'table',
             header = list(values = list("Country", "Count", "Percentage")),
             cells = list(values = list(country_focus_summary$country_focus_list, country_focus_summary$count, round(country_focus_summary$percentage, 2))))

# Define the filename with subfolder path and save the plotly object as an HTML file
file_name <- "Exported Descriptive Tables/country_focus_summary.html"
saveWidget(p, file = file_name, selfcontained = TRUE)

# Participant Role

total_responses <- sum(!is.na(data$participant_role))

# Split the 'org_participant_role' column into individual countries and convert to a long format
participant_role_long <- data %>%
  mutate(participant_role_list = strsplit(as.character(participant_role), ",\\s*")) %>%
  unnest(participant_role_list)

# Now tally the occurrences of each participant_role
participant_role_summary <- participant_role_long %>%
  group_by(participant_role_list) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / total_responses) * 100)

# Sort by count
participant_role_summary <- participant_role_summary %>%
  arrange(desc(count))

# View the summary
print(participant_role_summary)

# Create the Plotly interactive table
p <- plot_ly(participant_role_summary, type = 'table',
             header = list(values = list("participant_role", "Count", "Percentage")),
             cells = list(values = list(participant_role_summary$participant_role_list, participant_role_summary$count, round(participant_role_summary$percentage, 2))))

# Define the filename with subfolder path
file_name <- "Exported Descriptive Tables/participant_role_summary.html"

# Save the plotly object as an HTML file
saveWidget(p, file = file_name, selfcontained = TRUE)