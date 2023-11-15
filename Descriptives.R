
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
library(plotly)

#Set working directory
setwd("C:/Users/jack_/OneDrive/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

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
  
  # Calculate the total count
  total_count <- sum(summary$count)
  total_percentage <- 100  # As it's the total, it's always 100%
  
  # Add total row to the summary with matching column names
  total_row <- data.frame(`.data[[column_name]]` = "Total", count = total_count, percentage = total_percentage)
  names(total_row) <- names(summary)  # Ensure the column names match
  summary <- rbind(summary, total_row)
  
  # Bold the header titles and add "Percentage" column
  bold_headers <- sapply(c(header_names, "Percentage"), function(x) paste0("<b>", x, "</b>"))
  
  # Create a formatted table with plotly
  plot_ly(summary, type = 'table',
          header = list(values = bold_headers),
          cells = list(values = rbind(as.character(summary[[column_name]]), summary$count, round(summary$percentage, 2))))
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
  }
  
  # Return the Plotly interactive table
  return(plot_ly(summary_df, type = 'table',
                 header = list(values = c("Variable", "Count", "Percentage")),
                 cells = list(values = rbind(summary_df$Variable, summary_df$Count_1, round(summary_df$Percentage_1, 2)))))
  
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
  
  # Create a data frame from the detailed statistics for a neat table format
  stats_df <- data.frame(Statistic = names(detailed_stats), Value = detailed_stats)
  
  # Return the Plotly interactive table
  return(plot_ly(stats_df, type = 'table',
                 header = list(values = c("Statistic", "Value")),
                 cells = list(values = rbind(stats_df$Statistic, stats_df$Value))))
}

###
#Descriptives Tables
###

# Budget Stats Table

create_stats_table(data, "org_budget_usd")

# Summary table for non-likert, factor variables

variables_to_summarize <- c("user_language", "org_years", "org_geographic_lvl",
                            "country_focus", "western_vs_nonwestern",
                            "org_size", "org_mission",
                            "org_focus")

# Assuming create_summary_table is defined as per your requirements
summaries <- lapply(variables_to_summarize, function(var) {
  create_summary_table(data, var, c(var, "Count"))
})

# To view the summary for a specific variable, you can print it like so:
print(summaries[])  # Replace 1 with the index of the variable you want to print

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
