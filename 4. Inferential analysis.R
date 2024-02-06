
#Title: Analysis
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

# Set the scipen option to avoid scientific notation
options(scipen = 19)

## Remove all objects from the current workspace 
rm(list = ls())

#Set working directory
setwd("C:/Users/jack_/Desktop/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

# Load required libraries
library(tidyverse)
library(fastDummies)
library(lmtest)
library(MASS)
library(wec)
library(dplyr)

# Read the recoded data
load("data_recoded.RData")

data <- recoded_data

# Some recoding responses for simplicity

data$org_geographic_lvl <- recode(data$org_geographic_lvl, 
                                  "At the international level, operating across multiple regions (e.g., working on projects across North America, Latin America and Southeast Asia)" = "International",
                                  "At the national level, operating almost exclusively within a single country" = "National",
                                  "At the regional level, operating in multiple countries within one region (e.g., within Asia, Africa, Europe, Latin America, Middle East and North Africa, North America, or Oceania)" = "Regional")

data$org_mission <- recode(data$org_mission,
                           "Animal rights: Advocates for the inherent rights of animals and seeks to end all forms of exploitation and abuse" = "Rights",
                           "Animal welfare: Supports the improvement of animal well-being, humane treatment, and the prevention of suffering" = "Welfare",
                           "Mixed: Combines ideas related to both welfare and rights" = "Mixed",
                           "Other (please specify)" = "Other")

data$org_focus <- recode(data$org_focus,
                         "Corporate campaigns (e.g., getting cage-free pledges from producers or working with restaurants)" = "Corporate Campaigns",
                         "Direct work (e.g., rescues, veterinary care, sanctuaries)" = "Direct Work",
                         "Individual diet outreach, vegan/vegetarian advocacy, and education" = "Diet Outreach",
                         "Institutional campaigns (e.g., working with schools, prisons, etc.)" = "Institutional Campaigns",
                         "Policy campaigns (e.g., lobbying national, state, or local government)" = "Policy Campaigns",
                         "Other (please specify)" = "Other")


# In the following section we reduce the smaller categories of org_size and org_years because the number of responses
# is lower than 10, so pooling is necessary for analysis

# Adjust 'org_size' to combine "21-40", "41-100" and "101+" into "21+"
data$org_size <- ifelse(data$org_size %in% c("21-40", "41-100", "101+"), "21+", as.character(data$org_size))
data$org_size <- factor(data$org_size, 
                        levels = c("Less than 1", "1-5", "6-10", "11-20", "21+"),
                        ordered = TRUE)

# Adjust 'org_years' to combine "Less than 1 year" and "1-2 years" into "2 years or under"
data$org_years <- ifelse(data$org_years %in% c("Less than 1 year", "1-2 years"), "2 years or under", as.character(data$org_years))
data$org_years <- factor(data$org_years, 
                         levels = c("2 years or under", "3-5 years", "6-10 years", "More than 10 years"),
                         ordered = TRUE)

data$org_budget_usd_standardized <- scale(data$org_budget_usd)

#Recode "online only" in western/ non-western as "Mixed" - this is recoded because there are only 2 responses that select "online only", therefore this is not valuable 
# for any analysis and is recoded as "mixed" because "online only" organisations are likely to work with global actors

data$western_vs_nonwestern <- recode(data$western_vs_nonwestern, 
                                         `Online only` = "Mixed")

# -------------------------------
# Chi-Squared Test for Western/ Non-Western Hypothesis 
# -------------------------------

# Description: To test the hypothesis that approaches taken by non-Western groups will differ significantly from 
#Western groups, our goal is to detect a medium effect size (w = 0.30) using chi-square tests of independence 
#with 80% power and alpha corrected for False Discovery Rate (FDR; 0.05/5 tests = 0.01 at the strict end).

# Summarize the total count for each advocacy type

table(data$advocacy_type_corporate) # 59 selected corporate
table(data$advocacy_type_direct_work) # 59 selected direct work
table(data$advocacy_type_institutional) # 85 selected institutional 
table(data$advocacy_type_policy) # 80 selected policy
table(data$advocacy_type_individual_diet) # 133 selected individual diet
table(data$advocacy_type_other) # 39 selected "other", but not part of pre-registration for inferentials

# Our preregistration required 130 respondents per advocacy approach, therefore we are under-sampled in all 
# but individual_diet

# Filter out 'Mixed' and 'Online only' categories
subset_filtered <- data %>%
  filter(western_vs_nonwestern != "Mixed", western_vs_nonwestern != "Online only")

# Drop unused factor levels and verify (levels)
subset_filtered$western_vs_nonwestern <- droplevels(subset_filtered$western_vs_nonwestern)
levels(subset_filtered$western_vs_nonwestern)

# Create a new contingency table for western/ non-western and advocacy types
contingency_table_corporate <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_corporate)
contingency_table_direct_work <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_direct_work)
contingency_table_policy <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_policy)
contingency_table_individual_diet <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_individual_diet)
contingency_table_institutional <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_institutional)

# Function to create a combined table with the desired format
create_combined_table <- function(data_column, advocacy_type_name) {
  # Change levels to 'No' and 'Yes'
  levels(data_column) <- c("No", "Yes")
  
  contingency_table <- table(subset_filtered$western_vs_nonwestern, data_column)
  proportion_table <- proportions(contingency_table, 1) * 100  # Convert to percentage
  proportion_table <- round(proportion_table, 2)  # Round to 2 decimal places
  
  combined_table <- cbind(contingency_table, proportion_table)
  colnames(combined_table) <- c("No", "Yes", "No (%)", "Yes (%)")  # Renaming columns
  
  # Add a title
  title <- paste("Advocacy Type:", advocacy_type_name)
  combined_table <- list(Title = title, Table = combined_table)
  
  return(combined_table)
}

# Apply this function to each advocacy type (excluding "other" as it wasn't listed in the prereg)

advocacy_types <- list(subset_filtered$advocacy_type_corporate, subset_filtered$advocacy_type_direct_work, 
                       subset_filtered$advocacy_type_institutional, subset_filtered$advocacy_type_policy, 
                       subset_filtered$advocacy_type_individual_diet)

# Define advocacy type names
advocacy_type_names <- c("Corporate", "Direct Work", "Institutional", "Policy", "Individual Diet")

# Apply the function to each advocacy type
combined_tables <- Map(create_combined_table, advocacy_types, advocacy_type_names)

# Output the result
print(combined_tables)

# Advocacy Type: Corporate: 27.27% (n = 36) of Non-western groups, and 27.9% of Western groups engage in corporate advocacy 
# Advocacy Type: Direct Work: 33.33% (n = 44) of Non-Western groups, and 19.67% (n = 12) of Western groups engage in direct work advocacy.
# Advocacy Type: Institutional: 46.97% (n = 62) of Non-Western groups, and 26.23% (n = 16) of Western groups engage in institutional advocacy.
# Advocacy Type: Policy: 38.64% (n = 51) of Non-Western groups, and 36.07% (n = 22) of Western groups engage in policy advocacy.
# Advocacy Type: Individual Diet: 64.39% (n = 85) of Non-Western groups, and 67.21% (n = 41) of Western groups engage in individual diet advocacy.

# Perform the chi-square test
chi_square_corporate <- chisq.test(contingency_table_corporate)
# X-squared = 0, p-value = 1
chi_square_direct <- chisq.test(contingency_table_direct_work)
#X-squared = 3.1462, p-value = 0.07611
chi_square_policy <- chisq.test(contingency_table_policy)
#X-squared = 0.03341, p-value = 0.855
chi_square_diet <- chisq.test(contingency_table_individual_diet)
# X-squared = 0.048354, p-value = 0.826
chi_square_institutional <- chisq.test(contingency_table_institutional)
# X-squared = 6.616, p-value = 0.01011

# Output the result
print(chi_square_corporate)
print(chi_square_direct)
print(chi_square_policy)
print(chi_square_diet)
print(chi_square_institutional)

# Gather all p-values from chi-square tests
p_values <- c(1, 0.07611, 0.855, 0.826, 0.01011)

# Apply the Benjamini-Hochberg correction
adjusted_p_values <- p.adjust(p_values, method = "BH")

# Output the adjusted p-values
print(adjusted_p_values)

# 1.000000 0.190275 1.000000 1.000000 0.050550
# After adjusting for multiple testing, no advocacy type differs significantly between Western and non-Western countries. 
# However, the difference in institutional advocacy is close to the 0.05 threshold after adjustment. This result is marginally
# significant (between p= 0.05 and p= 0.10).

# -------------------------------
# Two-tailed logistic multivariate regression for RQ2
# -------------------------------

# RQ2: Our objective is to determine which factors influence groups’ selection of approaches. Our desired sample
# size was 94 participants per approach. Only individual diet meets this criteria, therefore analysis of other 
# approaches will be underpowered.

# To understand which factors influence groups’ selection of approaches (RQ2),
# we will run a hierarchical binary logistic regression for each animal advocacy approach separately as the outcome variable.
# To do this, we must ensure variables are coded correctly (dummy coding/ effect coding) 
# We then fit the first set of models with the first set of predictors using the lm() function
# Add the second set of predictors.

# Importance variables need to be recoded so that "very important" is being compared to "somewhat important" and 
# "not at all important" 

relevel_importance_variables <- function(data, var_names) {
  for (var_name in var_names) {
    # Convert ordered factor to unordered factor
    data[[var_name]] <- factor(data[[var_name]], ordered = FALSE)
    
    # Set "very important" as the reference level
    data[[var_name]] <- relevel(data[[var_name]], ref = "Very important")
  }
  return(data)
}

# Variables to transform
vars_to_transform <- c("importance_funding_availability", "importance_talent_availability", 
                       "importance_context_appropriateness", "importance_impact_cost_effectiveness", 
                       "importance_mission_alignment")
# Function to relevel, reorder, and apply custom contrasts to importance variables
relevel_and_reorder_importance_variables <- function(data, variables) {
  
  # Define the custom contrast matrix for adjacent comparisons
  custom_contrasts <- matrix(c( 1,  0,  # Comparison 1: "Very important" vs "Somewhat important"
                                -1,  1,  # "Somewhat important" is involved in both comparisons
                                0, -1), # Comparison 2: "Somewhat important" vs "Not at all important"
                             nrow = 3, byrow = TRUE)
  
  # Name the rows and columns for clarity
  rownames(custom_contrasts) <- c("Very important", "Somewhat important", "Not at all important")
  colnames(custom_contrasts) <- c("Very vs Somewhat", "Somewhat vs Not")
  
  for (var in variables) {
    # Relevel and reorder the levels
    data[[var]] <- factor(data[[var]], 
                          levels = c("Very important", "Somewhat important", "Not at all important"),
                          ordered = TRUE)
    
    # Apply the custom contrasts to each variable
    contrasts(data[[var]]) <- custom_contrasts
  }
  
  return(data)
}

# Apply the function to the dataset
data <- relevel_and_reorder_importance_variables(data, vars_to_transform)

# Check the contrasts for a variable
contrasts(data$importance_funding_availability)

# Check the levels of the variables
sapply(data[vars_to_transform], levels)

# Apply weighted effect coding to org_size

data$org_size.wec <- factor(data$org_size)
data$org_size.wec_b <- factor(data$org_size) #duplicate variable to see X level vs. grand mean 

contrasts(data$org_size.wec) <- contr.wec(data$org_size.wec, "Less than 1") #Less than 1 is omitted 
contrasts(data$org_size.wec)

contrasts(data$org_size.wec_b) <- contr.wec(data$org_size.wec_b, "21+") #21+ is now omitted 
contrasts(data$org_size.wec_b)

# Apply weighted effect coding to org_years
data$org_years
data$org_years.wec <- factor(data$org_years)
data$org_years.wec_b <- factor(data$org_years) #duplicate variable to can look at X level vs. grand mean 

contrasts(data$org_years.wec) <- contr.wec(data$org_years.wec, "2 years or under") #"2 years or under" is omitted 
contrasts(data$org_years.wec)

contrasts(data$org_years.wec_b) <- contr.wec(data$org_years.wec_b, "More than 10 years") #More than 10 years is now omitted 
contrasts(data$org_years.wec_b)

# Apply weighted effect coding to org_mission

data$org_mission.wec <- factor(data$org_mission)
data$org_mission.wec_b <- factor(data$org_mission) #duplicate variable to see X level vs. grand mean 

contrasts(data$org_mission.wec) <- contr.wec(data$org_mission.wec, "Rights") #Rights is omitted 
contrasts(data$org_mission.wec)

contrasts(data$org_mission.wec_b) <- contr.wec(data$org_mission.wec_b, "Other") #Other is omitted 
contrasts(data$org_mission.wec_b)

# Apply weighted effect coding to org_geographic_lvl

data$org_geographic_lvl.wec <- factor(data$org_geographic_lvl)
data$org_geographic_lvl.wec_b <- factor(data$org_geographic_lvl) #duplicate variable to see X level vs. grand mean 

contrasts(data$org_geographic_lvl.wec) <- contr.wec(data$org_geographic_lvl.wec, "National") #National is omitted 
contrasts(data$org_geographic_lvl.wec)

contrasts(data$org_geographic_lvl.wec_b) <- contr.wec(data$org_geographic_lvl.wec_b, "Regional") #Regional is omitted 
contrasts(data$org_geographic_lvl.wec_b)

# Apply weighted effect coding to org_western

data$western_vs_nonwestern.wec <- factor(data$western_vs_nonwestern)
data$western_vs_nonwestern.wec_b <- factor(data$western_vs_nonwestern) #duplicate variable to see X level vs. grand mean 

contrasts(data$western_vs_nonwestern.wec) <- contr.wec(data$western_vs_nonwestern.wec, "Western") #western is omitted
contrasts(data$western_vs_nonwestern.wec)

contrasts(data$western_vs_nonwestern.wec_b) <- contr.wec(data$western_vs_nonwestern.wec_b, "Mixed") #mixed is omitted
contrasts(data$western_vs_nonwestern.wec_b)

# Apply weighted effect coding to org_focus

data$org_focus.wec <- factor(data$org_focus)
data$org_focus.wec_b <- factor(data$org_focus) #duplicate variable to see X level vs. grand mean 

contrasts(data$org_focus.wec) <- contr.wec(data$org_focus.wec, "Corporate Campaigns") #corporate is omitted 
contrasts(data$org_focus.wec)

contrasts(data$org_focus.wec_b) <- contr.wec(data$org_focus.wec_b, "Other") #other is omitted 
contrasts(data$org_focus.wec_b)

# Logistic regression model(this is just assessing the inclusion of extraneous variables, and we correct for multiple
# terms in the model)

###Corporate Campaigns - Model 1### 

model1 <- glm(advocacy_type_corporate ~ org_size.wec + 
                animal_type_aquatic_farm + 
                animal_type_dogcat_meat + 
                animal_type_land_farm + 
                org_years.wec + 
                org_mission.wec + 
                org_geographic_lvl.wec + 
                western_vs_nonwestern.wec + 
                org_budget_usd_standardized, 
    family = binomial, data = data)

# Get a summary of the model
summary(model1)

# Extract p-values & coefficients 
p_values_1 <- summary(model1)$coefficients[, 4] 
coefficients_1 <- coef(model1)

model1_b <- glm(advocacy_type_corporate ~ org_size.wec_b + 
                animal_type_aquatic_farm + 
                animal_type_dogcat_meat + 
                animal_type_land_farm + 
                org_years.wec_b + 
                org_mission.wec_b + 
                org_geographic_lvl.wec_b + 
                western_vs_nonwestern.wec_b + 
                org_budget_usd_standardized, 
              family = binomial, data = data)

# Get a summary of the model
summary(model1_b)

# Extract p-values & coefficients 
p_values_1b <- summary(model1_b)$coefficients[, 4] 
coefficients_1b <- coef(model1_b)

## Create data frames for each model's output
preinclusion_results <- data.frame(
  Model = "model1",
  Coefficient = names(coefficients_1),
  Estimate = coefficients_1,
  P_Values = p_values_1
)

preinclusion_results_b <- data.frame(
  Model = "model1_b",
  Coefficient = names(coefficients_1b),
  Estimate = coefficients_1b,
  P_Values = p_values_1b
)

## Omit duplicate variables from model_b (just want specific contrasts from this model)
preinclusion_results_b <- preinclusion_results_b [(preinclusion_results_b$Coefficient %in% c(
  "org_size.wec_bLess than 1",
  "org_years.wec_b2 years or under",
  "org_mission.wec_bRights",
  "org_geographic_lvl.wec_bNational",
  "western_vs_nonwestern.wec_bWestern")),]

## Join both dataframes 
preinclusion_output <- bind_rows(preinclusion_results, preinclusion_results_b) 

## Correct for multiple testing using Benjamini-Hochberg correction 
preinclusion_output <- preinclusion_output %>% 
  mutate(P_Values_BH_1 = p.adjust(P_Values, method = "BH", n = length(P_Values))) %>%
  arrange(P_Values_BH_1) %>%
  print()

###Individual Diet/ Veg*an Campaigns - Model 2### 

model2 <- glm(advocacy_type_individual_diet ~ org_size.wec + 
                animal_type_aquatic_farm + 
                animal_type_dogcat_meat + 
                animal_type_land_farm + 
                org_years.wec + 
                org_mission.wec + 
                org_geographic_lvl.wec + 
                western_vs_nonwestern.wec + 
                org_budget_usd_standardized, 
              family = binomial, data = data)


# Extract p-values & coefficients 
p_values_2 <- summary(model2)$coefficients[, 4] 
coefficients_2 <- coef(model2)

model2_b <- glm(advocacy_type_individual_diet ~ org_size.wec_b + 
                  animal_type_aquatic_farm + 
                  animal_type_dogcat_meat + 
                  animal_type_land_farm + 
                  org_years.wec_b + 
                  org_mission.wec_b + 
                  org_geographic_lvl.wec_b + 
                  western_vs_nonwestern.wec_b + 
                  org_budget_usd_standardized, 
                family = binomial, data = data)

# Get a summary of the model
summary(model2_b)

# Extract p-values & coefficients 
p_values_2b <- summary(model2_b)$coefficients[, 4] 
coefficients_2b <- coef(model2_b)

## Create data frames for each model's output
preinclusion_results_2 <- data.frame(
  Model = "model2",
  Coefficient = names(coefficients_2),
  Estimate = coefficients_2,
  P_Values = p_values_2
)

preinclusion_results_2b <- data.frame(
  Model = "model2_b",
  Coefficient = names(coefficients_2b),
  Estimate = coefficients_2b,
  P_Values = p_values_2b
)

## Omit duplicate variables from model_b (just want specific contrasts from this model)
preinclusion_results_2b <- preinclusion_results_2b [(preinclusion_results_2b$Coefficient %in% c(
  "org_size.wec_bLess than 1",
  "org_years.wec_b2 years or under",
  "org_mission.wec_bRights",
  "org_geographic_lvl.wec_bNational",
  "western_vs_nonwestern.wec_bWestern")),]

## Join both dataframes 
preinclusion_output_2 <- bind_rows(preinclusion_results_2, preinclusion_results_2b) 

## Correct for multiple testing using Benjamini-Hochberg correction 
preinclusion_output_2 <- preinclusion_output_2 %>% 
  mutate(P_Values_BH_2 = p.adjust(P_Values, method = "BH", n = length(P_Values))) %>%
  arrange(P_Values_BH_2) %>%
  print()


###Direct Work Campaigns - Model 3### 

model3 <- glm(advocacy_type_direct_work ~ org_size.wec + 
                animal_type_aquatic_farm + 
                animal_type_dogcat_meat + 
                animal_type_land_farm + 
                org_years.wec + 
                org_mission.wec + 
                org_geographic_lvl.wec + 
                western_vs_nonwestern.wec + 
                org_budget_usd_standardized, 
              family = binomial, data = data)


# Extract p-values & coefficients 
p_values_3 <- summary(model3)$coefficients[, 4] 
coefficients_3 <- coef(model3)

model3_b <- glm(advocacy_type_direct_work ~ org_size.wec_b + 
                  animal_type_aquatic_farm + 
                  animal_type_dogcat_meat + 
                  animal_type_land_farm + 
                  org_years.wec_b + 
                  org_mission.wec_b + 
                  org_geographic_lvl.wec_b + 
                  western_vs_nonwestern.wec_b + 
                  org_budget_usd_standardized, 
                family = binomial, data = data)

# Get a summary of the model
summary(model3_b)

# Extract p-values & coefficients 
p_values_3b <- summary(model3_b)$coefficients[, 4] 
coefficients_3b <- coef(model3_b)

## Create data frames for each model's output
preinclusion_results_3 <- data.frame(
  Model = "model3",
  Coefficient = names(coefficients_3),
  Estimate = coefficients_3,
  P_Values = p_values_3
)

preinclusion_results_3b <- data.frame(
  Model = "model3_b",
  Coefficient = names(coefficients_3b),
  Estimate = coefficients_3b,
  P_Values = p_values_3b
)

## Omit duplicate variables from model_b (just want specific contrasts from this model)
preinclusion_results_3b <- preinclusion_results_3b [(preinclusion_results_3b$Coefficient %in% c(
  "org_size.wec_bLess than 1",
  "org_years.wec_b2 years or under",
  "org_mission.wec_bRights",
  "org_geographic_lvl.wec_bNational",
  "western_vs_nonwestern.wec_bWestern")),]

## Join both dataframes 
preinclusion_output_3 <- bind_rows(preinclusion_results_3, preinclusion_results_3b) 

## Correct for multiple testing using Benjamini-Hochberg correction 
preinclusion_output_3 <- preinclusion_output_3 %>% 
  mutate(P_Values_BH_3 = p.adjust(P_Values, method = "BH", n = length(P_Values))) %>%
  arrange(P_Values_BH_3) %>%
  print()

###Institutional Campaigns - Model 4### 

model4 <- glm(advocacy_type_institutional ~ org_size.wec + 
                animal_type_aquatic_farm + 
                animal_type_dogcat_meat + 
                animal_type_land_farm + 
                org_years.wec + 
                org_mission.wec + 
                org_geographic_lvl.wec + 
                western_vs_nonwestern.wec + 
                org_budget_usd_standardized, 
              family = binomial, data = data)

summary(model4)

# Extract p-values & coefficients 
p_values_4 <- summary(model4)$coefficients[, 4] 
coefficients_4 <- coef(model4)

model4_b <- glm(advocacy_type_institutional ~ org_size.wec_b + 
                  animal_type_aquatic_farm + 
                  animal_type_dogcat_meat + 
                  animal_type_land_farm + 
                  org_years.wec_b + 
                  org_mission.wec_b + 
                  org_geographic_lvl.wec_b + 
                  western_vs_nonwestern.wec_b + 
                  org_budget_usd_standardized, 
                family = binomial, data = data)

# Get a summary of the model
summary(model4_b)

# Extract p-values & coefficients 
p_values_4b <- summary(model4_b)$coefficients[, 4] 
coefficients_4b <- coef(model4_b)

## Create data frames for each model's output
preinclusion_results_4 <- data.frame(
  Model = "model4",
  Coefficient = names(coefficients_4),
  Estimate = coefficients_4,
  P_Values = p_values_4
)

preinclusion_results_4b <- data.frame(
  Model = "model4_b",
  Coefficient = names(coefficients_4b),
  Estimate = coefficients_4b,
  P_Values = p_values_4b
)

## Omit duplicate variables from model_b (just want specific contrasts from this model)
preinclusion_results_4b <- preinclusion_results_4b [(preinclusion_results_4b$Coefficient %in% c(
  "org_size.wec_bLess than 1",
  "org_years.wec_b2 years or under",
  "org_mission.wec_bRights",
  "org_geographic_lvl.wec_bNational",
  "western_vs_nonwestern.wec_bWestern")),]

## Join both dataframes 
preinclusion_output_4 <- bind_rows(preinclusion_results_4, preinclusion_results_4b) 

## Correct for multiple testing using Benjamini-Hochberg correction 
preinclusion_output_4 <- preinclusion_output_4 %>% 
  mutate(P_Values_BH_4 = p.adjust(P_Values, method = "BH", n = length(P_Values))) %>%
  arrange(P_Values_BH_4) %>%
  print()


###Policy Campaigns - Model 5### 


model5 <- glm(advocacy_type_policy ~ 
                org_size.wec + 
                animal_type_aquatic_farm + 
                animal_type_dogcat_meat + 
                animal_type_land_farm + 
                org_years.wec + 
                org_mission.wec + 
                org_geographic_lvl.wec + 
                western_vs_nonwestern.wec + 
                org_budget_usd_standardized, 
              family = binomial, data = data)

summary(model5)

# Extract p-values & coefficients 
p_values_5 <- summary(model5)$coefficients[, 4] 
coefficients_5 <- coef(model5)

model5_b <- glm(advocacy_type_policy ~ org_size.wec_b + 
                  animal_type_aquatic_farm + 
                  animal_type_dogcat_meat + 
                  animal_type_land_farm + 
                  org_years.wec_b + 
                  org_mission.wec_b + 
                  org_geographic_lvl.wec_b + 
                  western_vs_nonwestern.wec_b + 
                  org_budget_usd_standardized, 
                family = binomial, data = data)

# Get a summary of the model
summary(model5_b)

# Extract p-values & coefficients 
p_values_5b <- summary(model5_b)$coefficients[, 4] 
coefficients_5b <- coef(model5_b)

## Create data frames for each model's output
preinclusion_results_5 <- data.frame(
  Model = "model5",
  Coefficient = names(coefficients_5),
  Estimate = coefficients_5,
  P_Values = p_values_5
)

preinclusion_results_5b <- data.frame(
  Model = "model5_b",
  Coefficient = names(coefficients_5b),
  Estimate = coefficients_5b,
  P_Values = p_values_5b
)

## Omit duplicate variables from model_b (just want specific contrasts from this model)
preinclusion_results_5b <- preinclusion_results_5b [(preinclusion_results_5b$Coefficient %in% c(
  "org_size.wec_bLess than 1",
  "org_years.wec_b2 years or under",
  "org_mission.wec_bRights",
  "org_geographic_lvl.wec_bNational",
  "western_vs_nonwestern.wec_bWestern")),]
#

## Join both dataframes 
preinclusion_output_5 <- bind_rows(preinclusion_results_5, preinclusion_results_5b) 

## Correct for multiple testing using Benjamini-Hochberg correction 
preinclusion_output_5 <- preinclusion_output_5 %>% 
  mutate(P_Values_BH_5 = p.adjust(P_Values, method = "BH", n = length(P_Values))) %>%
  arrange(P_Values_BH_5) %>%
  print()

#Findings from all these analyses - I mention all variables close to significance, but only include in final model if it meets p = 0.05 threshold

# Model 1: Nothing is significant

# Model 2: org_mission.wecWelfare is significantly (p = 0.03809396) and negatively (Estimate: -2.424665442) associated with conducting individual diet/ vegan advocacy.
# while org_mission.wec_bRights is significantly (p = 0.01277157) and positively (Estimate: 1.130873766) associated with conducting individual diet/ vegan advocacy.      

# Model 3: Animal type is significantly associated with the advocacy approach direct work- animal_type_aquatic_farm (p = 0.002553958) is significantly associated and animal_type_dogcat_meat (p = 0.091154927)
# is significant at the 0.1 threshold. animal_type_aquatic_farm (Estimate: -2.44851416) has a negative association with direct work
# while animal_type_dogcat_meat (Estimate: 1.81156751) has a positive association with doing direct work. 

# Model 4: No significant variables (Note: Org_size is correlated with policy work, with organisations with over 6 FTE staff more likely to conduct policy work; however, this doesn't meet significance thresholds)

# Model 5: No significant variables (Note: org_size.wec_bLess than 1 is close to significance (p = 0.07195159) and negatively (-1.15668666) associated with policy)

# Next stage of the analysis, which is adding the importance variables. These are the final models where we assess our main variables of interest
# while retaining any significant extraneous variables from the previous stage, which are the level of importance of funding 
# availability, availability of talent, local context/appropriateness, impact/cost-effectiveness, and alignment with organization’s mission/values.
# We will adjust p-values using BH correction.

final_model_1 <- glm(advocacy_type_corporate ~ 
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)


summary(final_model_1)

final_model_2 <- glm(advocacy_type_individual_diet ~  
                 org_mission.wec +
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)


final_model_2b <- glm(advocacy_type_individual_diet ~  
                       org_mission.wec_b + 
                       importance_funding_availability +
                       importance_talent_availability +
                       importance_context_appropriateness +
                       importance_impact_cost_effectiveness +
                       importance_mission_alignment, 
                     family = binomial, data = data)

summary(final_model_2)
summary(final_model_2b)


# Extract p-values & coefficients 
p_values <- summary(final_model_2)$coefficients[, 4] 
coefficients <- coef(final_model_2)

# Extract p-values & coefficients 2b
p_values_b <- summary(final_model_2b)$coefficients[, 4] 
coefficients_b <- coef(final_model_2b)

## Create data frames for each model's output
final_model_output <- data.frame(
  Model = "final_model_2",
  Coefficient = names(coefficients),
  Estimate = coefficients,
  P_Values = p_values
)

final_model_output_b <- data.frame(
  Model = "final_model_2b",
  Coefficient = names(coefficients_b),
  Estimate = coefficients_b,
  P_Values = p_values_b
)

## Omit duplicate variables from model_b (just want specific contrasts from this model)
final_model_output_b <- final_model_output_b [(final_model_output_b$Coefficient %in% c(
  "org_mission.wec_bRights")),]

## Join both dataframes 
final_model_results <- bind_rows(final_model_output, final_model_output_b) 

## Correct for multiple testing using Benjamini-Hochberg correction. Note: we're doing the p-value adjustment 
# separately here, while the adjustment for the remaining models will be done via a function for simplicity. 

model_2_combined_stats <- final_model_results %>% 
  mutate(AdjustedPValue = p.adjust(P_Values, method = "BH", n = length(P_Values))) %>%
  arrange(AdjustedPValue) 

final_model_3 <- glm(advocacy_type_direct_work ~  
                 animal_type_aquatic_farm +
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)

summary(final_model_3)

final_model_4 <- glm(advocacy_type_institutional ~  
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)

summary(final_model_4)

final_model_5 <- glm(advocacy_type_policy ~  
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)

summary(final_model_5)

extract_model_stats <- function(model) {
  model_summary <- summary(model)
  
  # Extracting statistics
  coefficients <- model_summary$coefficients[, "Estimate"]
  std_error <- model_summary$coefficients[, "Std. Error"]
  z_value <- model_summary$coefficients[, "z value"]
  original_p_values <- model_summary$coefficients[, "Pr(>|z|)"]
  
  # Applying Benjamini-Hochberg correction
  adjusted_p_values <- p.adjust(original_p_values, method = "BH")
  
  # Create a data frame
  model_stats <- data.frame(
    Estimate = coefficients,
    StdError = std_error,
    ZValue = z_value,
    PValue = original_p_values,
    AdjustedPValue = adjusted_p_values
  )
  
  return(model_stats)
}

# Applying the function to models (Note: p-value adjustment has already been done separately for model_2)

model_1_stats <- extract_model_stats(final_model_1)
model_3_stats <- extract_model_stats(final_model_3)
model_4_stats <- extract_model_stats(final_model_4)
model_5_stats <- extract_model_stats(final_model_5)

# Print data frames for comparison
print(model_1_stats)
print(model_2_combined_stats)
print(model_3_stats)
print(model_4_stats)
print(model_5_stats)

#Final Model 1: No significant variables

# Final Model 2: Only org_mission welfare  (AdjustedPValue = 0.0005104452), (Estimate : -2.02024) and org_mission_rights  (p = 0.0032542726), (Estimate : 0.85694063)remains significant after adjustment- 
# organisations with a welfare-oriented mission are significantly less likely to engage in 
# individual diet interventions, while those with a rights-oriented mission are more likely to do so, 
# but this does not correlate with the importance of any given importance variable. 

# Final Model 3: Animal type remains significantly associated with model3. Organisations that engage with aquatic farmed animals are significantly less likely to engage in direct work (p = 0.01997634, Estimate: -1.1137709). 

# Final Model 4: Nothing is significant

# Final Model 5: Nothing is significant. 

############
## RQ3: How useful are different resources? 
#############

# Mann Whitney Tests for RQ3 (To understand the usefulness of different resources (RQ3), usefulness ratings will be compared between each resource type measured using
# Mann-Whitney U tests with multiple testing corrected across all tests by using the Benjamini-Hochberg correction for false discovery rate (i.e., FDR correction). )
# Our desired number of respondents was 270, and we have 186 completed responses, meaning that this analysis is slightly underpowered.


# List of variables (in order to calculate the mean usefulness rating of each resource type, again, higher value is more useful)

variables <- c("resources_usefulness_collaboration_networking",
               "resources_usefulness_financial",
               "resources_usefulness_finding_talent",
               "resources_usefulness_grant_applications",
               "resources_usefulness_professional_development",
               "resources_usefulness_professional_mentorship",
               "resources_usefulness_research_data_access",
               "resources_usefulness_staff_well_being")

# Find the means of each response ("Not at all useful" is coded as 1, "Somewhat useful" is coded as 2, 
# "Very Useful" is coded as 3)
# Initialize a dataframe to store the results

stats_df <- data.frame(Variable = character(),
                       Mean = numeric(),
                       stringsAsFactors = FALSE)

# Loop over each variable and calculate mean
for (var in variables) {
  # Convert ordered factors to numeric
  numeric_values <- as.numeric(data[[var]])
  
  # Calculate mean
  mean_value <- mean(numeric_values, na.rm = TRUE)
  
  # Append results to the dataframe
  stats_df <- rbind(stats_df, data.frame(Variable = var,
                                         Mean = mean_value))
}

# Sort the dataframe by Mean in descending order
stats_df <- stats_df[order(stats_df$Mean, decreasing = TRUE), ]

# Print the sorted results
print(stats_df)

###

## Order of results by usefulness (mean value)
# 1. Financial (2.849462)
# 2. Grant Applications (2.666667)
# 3. Collaboration Networking (2.532258)
# 4. Research and Data Access (2.526882)
# 5. Professional Mentorship (2.456989)
# 6. Professional Development (2.435484)
# 7. Finding Talent (2.344086)
# 8. Staff well-being (2.241935)

# For this section, we perform the necessary pairwise comparisons to detect whether differences are significant.

# Loop through the variables in the order of means
comparison_results <- data.frame(
  Resource_A = character(),
  Resource_B = character(),
  P_Value = numeric(),
  Significant = logical(),
  stringsAsFactors = FALSE
)

for (current_index in 1:(nrow(stats_df) - 1)) {
  resource_a <- stats_df$Variable[current_index]
  
  # Loop through each subsequent variable
  for (next_index in (current_index + 1):nrow(stats_df)) {
    resource_b <- stats_df$Variable[next_index]
    
    # Convert ordered factors to numeric
    a_numeric <- as.numeric(data[[resource_a]])
    b_numeric <- as.numeric(data[[resource_b]])
    
    # Perform Mann-Whitney U Test
    test_result <- wilcox.test(a_numeric, b_numeric)
    
    # Check if the difference is significant
    is_significant <- test_result$p.value < 0.05
    
    # Append results to the dataframe
    comparison_results <- rbind(comparison_results, data.frame(
      Resource_A = resource_a,
      Resource_B = resource_b,
      P_Value = test_result$p.value,
      Significant = is_significant
    ))
  }
}

# Apply Benjamini-Hochberg correction
adjusted_p_values <- p.adjust(comparison_results$P_Value, method = "BH")
comparison_results$Adjusted_P_Value <- adjusted_p_values
comparison_results$Adjusted_Significant <- comparison_results$Adjusted_P_Value < 0.05

# Print the results
print(comparison_results)

# Note, 19 out of 28 pairwise comparisons are statistically significant; Differences between collaboration_networking, research_data_access, professional mentorship and professional development
# are not significant. Professional mentorship and professional development are not significantly greater than finding_talent, while finding talent is not significantly greater than staff well-being. 

############
## RQ4: What resources would best support different types of groups? 
############

# For RQ4 , our goal is to detect a medium effect size (f2 = 0.15) using a two-tailed linear multivariate regression with 80% power and alpha 
# corrected for (0.05/22 contrasts = 0.0022). This requires 107 participants per model. We reached this requirement, with 186 respondents per resource type.

# The outcome variable is the level of usefulness (“not at all useful”, “somewhat useful”, and “very useful”). Independent variables will be 
# organization size, organization country (Western or non-Western), organization scope, organization animal focus, organization revenue, 
# number of years the organization has been involved in farmed animal advocacy, organization primary focus, and organization mission

# List of dependent variables
dependent_vars <- c("resources_usefulness_collaboration_networking", 
                    "resources_usefulness_financial", 
                    "resources_usefulness_grant_applications", 
                    "resources_usefulness_finding_talent", 
                    "resources_usefulness_professional_development", 
                    "resources_usefulness_professional_mentorship", 
                    "resources_usefulness_research_data_access", 
                    "resources_usefulness_staff_well_being")

# org_budget needs to be adjusted to avoid a model error
# Calculate the mean of 'org_budget_usd_standardized', excluding NA values
mean_budget <- mean(data$org_budget_usd_standardized, na.rm = TRUE)

# Replace NA in 'org_budget_usd_standardized' with the calculated mean
data$org_budget_usd_standardized[is.na(data$org_budget_usd_standardized)] <- mean_budget

# Perform a logarithmic transformation; Adding 1 to avoid taking log of zero (log(0) is undefined)
data$org_budget_usd_standardized <- log1p(data$org_budget_usd_standardized)

# Initialize a list to store results from each model
all_results <- list()
all_results_b <- list()

# Custom function to round p-values
round_p_values <- function(p_value, threshold = 0.0001, digits = 4) {
  if (p_value < threshold) {
    return("<0.0001")
  } else {
    return(as.character(round(p_value, digits)))
  }
}
str(data$org_budget_usd_standardized)

# Adding stage to remove non-significant variables. The regression fails to function with too many variables, so I use a step-wise approach
# I run an version with the maximal number of variables (excluding org_mission.wec and org_focus.wec), then remove those that do not come close to statistical significance (p = 0.2). 
# Then I add org_mission.wec and org_focus.wec. This two-step process is necessary to avoid a model error.   

# Perform ordinal logistic regression for each dependent variable 
for (var in dependent_vars) {
  if (var %in% names(data)) {
    # Define the formula for the model
    formula <- as.formula(paste("as.ordered(", var, ") ~ org_size.wec + org_budget_usd_standardized + animal_type_aquatic_farm + 
                                 animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + western_vs_nonwestern.wec + org_geographic_lvl.wec
"))
  
    # Run the ordinal logistic regression model
    model <- polr(formula, data = data, Hess = TRUE)
    model_summary <- summary(model)
    
    # Number of observations + degrees of freedom
    n <- nrow(data)
    df <- n - length(model_summary$coefficients) - 1
    
    # Calculate p-values
    p_values <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df, lower.tail = TRUE)
    
    # Extract coefficients
    coefficients <- model_summary$coefficients[, "Value"]
    
    # Store the model results in the list
    all_results[[var]] <- data.frame(Model = var,
                                     Variable = names(p_values),
                                     Coefficient = coefficients,
                                     P_Value = p_values)
  
  }
}

# Print the results
print(all_results)

# Next step, run regression again with wec_b variables:

# Perform ordinal logistic regression for each dependent variable 
for (var in dependent_vars) {
  if (var %in% names(data)) {
    # Define the formula for the model
    formula <- as.formula(paste("as.ordered(", var, ") ~ org_size.wec_b + org_budget_usd_standardized + animal_type_aquatic_farm + 
                                 animal_type_dogcat_meat + animal_type_land_farm + org_years.wec_b + western_vs_nonwestern.wec_b + org_geographic_lvl.wec_b 
"))
    
    # Run the ordinal logistic regression model
    model <- polr(formula, data = data, Hess = TRUE)
    model_summary <- summary(model)
    
    # Number of observations + degrees of freedom
    n <- nrow(data)
    df <- n - length(model_summary$coefficients) - 1
    
    # Calculate p-values
    p_values <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df, lower.tail = TRUE)
    
    # Extract coefficients
    coefficients <- model_summary$coefficients[, "Value"]
    
    # Store the model results in the list
    all_results_b[[var]] <- data.frame(Model = var,
                                     Variable = names(p_values),
                                     Coefficient = coefficients,
                                     P_Value = p_values)
    
  }
}

# Print the results
print(all_results)
print(all_results_b)

# Specify the variables to add
variables_to_add <- c("org_years.wec_b2 years or under", "org_size.wec_bLess than 1", 
                      "western_vs_nonwestern.wec_bWestern", "org_geographic_lvl.wec_bNational")

# Iterate through the dependent variables in all_results_b
for (var in names(all_results_b)) {
  # Check if the dependent variable exists in all_results
  if (var %in% names(all_results)) {
    # Extract rows for the specified variables
    rows_to_add <- all_results_b[[var]][all_results_b[[var]]$Variable %in% variables_to_add, ]
    # Add these rows to the corresponding data frame in all_results
    all_results[[var]] <- rbind(all_results[[var]], rows_to_add)
  }
}

# Print the updated all_results for inspection
print(all_results)

# Create an empty list to store all significant results
all_significant_results <- list()

# Iterate through each model in the all_results list
for (model_name in names(all_results)) {
  # Get the dataframe for the current model
  model_df <- all_results[[model_name]]
  
  # Filter the rows where P_Value is less than 0.2
  significant_rows <- model_df[model_df$P_Value < 0.2, ]
  
  # Add the filtered results to the all_significant_results list, if not empty
  if (nrow(significant_rows) > 0) {
    all_significant_results[[model_name]] <- significant_rows
  }
}

# Print the significant results without the 'Model' column
for (model_name in names(all_significant_results)) {
  cat("\n", model_name, ":\n")
  print(all_significant_results[[model_name]][, -1], row.names = FALSE) # Exclude the first column ('Model')
}

#Next step, remove irrelevant variables (I did this manually if all components had a p-value under 0.2) 

# Define list of models with variables to be excluded for each dependent variable

exclusions <- list(
  resources_usefulness_collaboration_networking = c("animal_type_aquatic_farm", "animal_type_land_farm", "org_years.wec", "org_years.wec_b", "org_size.wec", "org_size.wec_b"),
  resources_usefulness_financial = c("western_vs_nonwestern.wec", "western_vs_nonwestern.wec_b", "animal_type_aquatic_farm", "animal_type_dogcat_meat", "org_geographic_lvl.wec", "org_geographic_lvl.wec_b", "org_size.wec", "org_size.wec_b", "org_years.wec", "org_years.wec_b"),
  resources_usefulness_grant_applications = c("animal_type_aquatic_farm", "animal_type_dogcat_meat", "animal_type_land_farm", "org_budget_usd_standardized", "org_geographic_lvl.wec", "org_geographic_lvl.wec_b"),
  resources_usefulness_finding_talent = c("org_size.wec", "org_size.wec_b", "org_budget_usd_standardized", "org_geographic_lvl.wec", "org_geographic_lvl.wec_b", "animal_type_aquatic_farm", "animal_type_dogcat_meat", "animal_type_land_farm", "org_years.wec", "org_years.wec_b"),
  resources_usefulness_professional_development = c("org_size.wec", "org_years.wec", "org_geographic_lvl.wec", "org_geographic_lvl.wec_b", "org_size.wec_b", "org_years.wec_b", "org_geographic_lvl.wec_b","org_budget_usd_standardized", "animal_type_aquatic_farm", "animal_type_land_farm", "animal_type_dogcat_meat"),
  resources_usefulness_professional_mentorship = c("animal_type_dogcat_meat", "org_years.wec", "org_geographic_lvl.wec", "org_years.wec_b", "org_geographic_lvl.wec_b", "org_size.wec", "org_size.wec_b"),
  resources_usefulness_research_data_access = c("org_budget_usd_standardized", "org_years.wec", "org_years.wec_b", "animal_type_land_farm", "animal_type_aquatic_farm"),
  resources_usefulness_staff_well_being = c("org_size.wec","org_size.wec_b", "animal_type_aquatic_farm", "animal_type_dogcat_meat", "org_geographic_lvl.wec", "org_geographic_lvl.wec_b")
)

# Initialize a list to store new results
new_all_results <- list()
new_all_results_b <- list()
all_unadjusted_p_values <- list()

for (var in dependent_vars) {
  if (var %in% names(data)) {
    # Determine variables to exclude for this model
    exclude_vars <- exclusions[[var]]
    
    # Define the formula for the model, excluding the specified variables
    independent_vars <- setdiff(c("org_size.wec", "org_budget_usd_standardized", "animal_type_aquatic_farm",
                                  "animal_type_dogcat_meat", "animal_type_land_farm", "org_years.wec",
                                  "western_vs_nonwestern.wec", "org_geographic_lvl.wec", "org_focus.wec", "org_mission.wec"), exclude_vars)
    formula <- as.formula(paste("as.ordered(", var, ") ~ ", paste(independent_vars, collapse = " + ")))
    
    # Run the ordinal logistic regression model
    model <- polr(formula, data = data, Hess = TRUE)
    model_summary <- summary(model)
    
    # Calculate p-values and compute two-tailed p-values for regression coefficients
    p_values <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df = nrow(data) - length(independent_vars) - 1, lower.tail = TRUE)
    
    # Extract coefficients
    coefficients <- model_summary$coefficients[, "Value"]
    
    # Store the model results in the new list
    new_all_results[[var]] <- data.frame(Model = rep(var, length(coefficients)),
                                         Variable = rownames(model_summary$coefficients),
                                         Coefficient = coefficients,
                                         P_Value = p_values)
  }
}

# Rerun the analysis with _b variables added

for (var in dependent_vars) {
  if (var %in% names(data)) {
    # Determine variables to exclude for this model
    exclude_vars <- exclusions[[var]]
    
    # Define the formula for the model, excluding the specified variables
    independent_vars <- setdiff(c("org_size.wec_b", "org_budget_usd_standardized", "animal_type_aquatic_farm",
                                  "animal_type_dogcat_meat", "animal_type_land_farm", "org_years.wec_b",
                                  "western_vs_nonwestern.wec_b", "org_geographic_lvl.wec_b", "org_focus.wec_b", "org_mission.wec_b"), exclude_vars)
    formula <- as.formula(paste("as.ordered(", var, ") ~ ", paste(independent_vars, collapse = " + ")))
    
    # Run the ordinal logistic regression model
    model <- polr(formula, data = data, Hess = TRUE)
    model_summary <- summary(model)
    
    # Calculate p-values and compute two-tailed p-values for regression coefficients
    p_values <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df = nrow(data) - length(independent_vars) - 1, lower.tail = TRUE)
    
    # Extract coefficients
    coefficients <- model_summary$coefficients[, "Value"]
    
    # Store the model results in the new list
    new_all_results_b[[var]] <- data.frame(Model = rep(var, length(coefficients)),
                                         Variable = rownames(model_summary$coefficients),
                                         Coefficient = coefficients,
                                         P_Value = p_values)
  }
}

# Specify the variables to add
variables_to_add <- c("org_years.wec_b2 years or under", "org_size.wec_bLess than 1", 
                      "western_vs_nonwestern.wec_bWestern", "org_geographic_lvl.wec_bNational", "org_focus.wec_bCorporate Campaigns")

# Iterate through the dependent variables in all_results_b to append _b results
for (var in names(new_all_results_b)) {
  if (var %in% names(new_all_results)) {
    rows_to_add <- new_all_results_b[[var]][new_all_results_b[[var]]$Variable %in% variables_to_add, ]
    new_all_results[[var]] <- rbind(new_all_results[[var]], rows_to_add)
    all_unadjusted_p_values[[var]] <- new_all_results[[var]]$P_Value  # Store unadjusted p-values
  }
}

# Adjust p-values for each model separately
for (var in names(new_all_results)) {
  if (!is.null(all_unadjusted_p_values[[var]])) {
    # Adjust the p-values for this specific model
    adjusted_p_values <- p.adjust(all_unadjusted_p_values[[var]], method = "BH")
    
    # Replace the unadjusted p-values with adjusted ones in the results
    new_all_results[[var]]$Adjusted_P_Value <- adjusted_p_values
  }
}

# Combine all results into a single data frame
all_results_combined <- do.call(rbind, new_all_results)

# Print the results by model
for (model_name in unique(all_results_combined$Model)) {
  cat("\nResults for Model:", model_name, "\n")
  model_specific_values <- all_results_combined[all_results_combined$Model == model_name, ]
  print(model_specific_values[, c("Variable", "Coefficient", "P_Value", "Adjusted_P_Value")])
}

# Function to summarize significant variables for each model
summarize_significance <- function(model_results, alpha = 0.05) {
  significant_vars <- model_results[model_results$Adjusted_P_Value < alpha, ]
  
  # Check and concatenate if there are any significant variables
  if (nrow(significant_vars) > 0) {
    significant_var_names <- paste(significant_vars$Variable, "(Coefficient =", significant_vars$Coefficient, ", Adjusted_P_Value =", significant_vars$Adjusted_P_Value, ")", collapse = ", ")
    return(paste("Significant variables:", significant_var_names))
  } else {
    return("No significant variables")
  }
}

# Apply the summary function to each model with alpha = 0.05 and print the results
for (model_name in unique(all_results_combined$Model)) {
  cat("\nModel:", model_name, "\n")
  model_results <- all_results_combined[all_results_combined$Model == model_name, ]
  print(summarize_significance(model_results))
}

# 
# Model: resources_usefulness_collaboration_networking 
# [1] "Significant variables: animal_type_dogcat_meat (Coefficient = 1.20067407939173 , Adjusted_P_Value = 0.0392023829210096 ), western_vs_nonwestern.wecNon-Western (Coefficient = 0.432028679772135 , Adjusted_P_Value = 0.0098069102372257 ), Not at all useful|Somewhat useful (Coefficient = -2.7479173102159 , Adjusted_P_Value = 0.000000000000128556039801873 ), western_vs_nonwestern.wec_bWestern (Coefficient = -0.785244393454789 , Adjusted_P_Value = 0.0107750900352726 )"
# 
# Model: resources_usefulness_financial 
# [1] "Significant variables: animal_type_land_farm (Coefficient = 2.08043828043349 , Adjusted_P_Value = 0.029498418520597 ), Not at all useful|Somewhat useful (Coefficient = -2.56793140786229 , Adjusted_P_Value = 0.0185540715090941 )"
# 
# Model: resources_usefulness_grant_applications 
# [1] "Significant variables: Not at all useful|Somewhat useful (Coefficient = -3.06912509301975 , Adjusted_P_Value = 0.0000000000000191416503510731 ), Somewhat useful|Very useful (Coefficient = -1.16736768305559 , Adjusted_P_Value = 0.0000000849448634169445 )"
# 
# Model: resources_usefulness_finding_talent 
# [1] "Significant variables: Not at all useful|Somewhat useful (Coefficient = -1.9021444603648 , Adjusted_P_Value = 0.0000000000000165616265612924 )"
# 
# Model: resources_usefulness_professional_development 
# [1] "Significant variables: western_vs_nonwestern.wecNon-Western (Coefficient = 0.293665356505189 , Adjusted_P_Value = 0.0493828632613771 ), org_focus.wecDiet Outreach (Coefficient = 0.55900540862333 , Adjusted_P_Value = 0.0388604585559778 ), Not at all useful|Somewhat useful (Coefficient = -2.01469114796903 , Adjusted_P_Value = 0.00000000000000391184267382671 )"
# 
# Model: resources_usefulness_professional_mentorship 
# [1] "Significant variables: Not at all useful|Somewhat useful (Coefficient = -1.97621176623926 , Adjusted_P_Value = 0.0207793809119977 ), western_vs_nonwestern.wec_bWestern (Coefficient = -0.661363792883941 , Adjusted_P_Value = 0.0409765717813021 )"
# 
# Model: resources_usefulness_research_data_access 
# [1] "Significant variables: western_vs_nonwestern.wecNon-Western (Coefficient = 0.417231961127309 , Adjusted_P_Value = 0.0205308183375799 ), Not at all useful|Somewhat useful (Coefficient = -2.866083665572 , Adjusted_P_Value = 0.000000000000141329443166326 ), western_vs_nonwestern.wec_bWestern (Coefficient = -0.930343466547159 , Adjusted_P_Value = 0.00647125684270975 )"
# 
# Model: resources_usefulness_staff_well_being 
# [1] "Significant variables: western_vs_nonwestern.wecNon-Western (Coefficient = 0.452176770070049 , Adjusted_P_Value = 0.00173642488004661 ), western_vs_nonwestern.wec_bWestern (Coefficient = -0.95130012271274 , Adjusted_P_Value = 0.00173642488004661 )"