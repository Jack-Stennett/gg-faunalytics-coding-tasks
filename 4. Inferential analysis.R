
#Title: Analysis
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

## Remove all objects from the current workspace 
rm(list = ls())

#Set working directory
setwd("C:/Users/jack_/OneDrive/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

# Load required libraries
library(tidyverse)
library(fastDummies)
library(lmtest)
library(MASS)

# Read the recoded data
load("data_recoded.RData")

data <- recoded_data

glimpse(data)

# Excluding response IDs that don't include key regression variables

response_ids_to_exclude <- c(
  "R_3EllU9woEvkxURt", "R_2Y3AgCPoUsE1EM",
  "R_2CuOci78cizOpNb", "R_1QFAa3zZtRFIPUs",
  "R_2uZC1yXGZDzyPew", "R_3fqOI2RyoDaWu6LT",
  "R_3foC4oz4yrYTzjl", "R_2PtXjvWeu84DdkI"
)
data <- data[!data$response_id %in% response_ids_to_exclude, ]

# -------------------------------
# Chi-Squared Test for Western/ Non-Western Hypothesis 
# -------------------------------

# Description: To test the hypothesis that approaches taken by non-Western groups will differ significantly from Western groups, our goal is to detect a medium effect size (w = 0.30) using chi-square tests of independence with 80% power and alpha corrected for False Discovery Rate (FDR; 0.05/5 tests = 0.01 at the strict end).

# Summarize the total count for each advocacy type

table(data$advocacy_type_corporate) # 59 selected corporate
table(data$advocacy_type_direct_work) # 59 selected direct work
table(data$advocacy_type_institutional) # 85 selected institutional 
table(data$advocacy_type_policy) # 80 selected policy
table(data$advocacy_type_individual_diet) # 133 selected individual diet
table(data$advocacy_type_other) # 39 selected "other"

# Our prereg required 130 respondents per advocayc approach, therefore we are undersampled in all but individual_diet

# Filter out 'Mixed' and 'Online only' categories
subset_filtered <- data %>%
  filter(western_vs_nonwestern != "Mixed", western_vs_nonwestern != "Online only")

# Drop unused factor levels
subset_filtered$western_vs_nonwestern <- droplevels(subset_filtered$western_vs_nonwestern)

levels(subset_filtered$western_vs_nonwestern)

# Create a new contingency table for western/ non-western and advocacy types
contingency_table_corporate <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_corporate)
contingency_table_direct_work <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_direct_work)
contingency_table_policy <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_policy)
contingency_table_individual_diet <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_individual_diet)
contingency_table_institutional <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_institutional)

# Perform the chi-square test
chi_square_corporate <- chisq.test(contingency_table_corporate)
chi_square_direct <- chisq.test(contingency_table_direct_work)
chi_square_policy <- chisq.test(contingency_table_policy)
chi_square_diet <- chisq.test(contingency_table_individual_diet)
chi_square_institutional <- chisq.test(contingency_table_institutional)

# Output the result
print(chi_square_corporate)

# X-squared = 0, p-value = 1
#No significant difference between Western and Non-Western groups in corporate advocacy.

print(chi_square_direct)

#X-squared = 3.1462, p-value = 0.07611
# Possible difference between Western and Non-Western
# groups in direct work advocacy, but not statistically significant at the 0.05 level.
# 44 out of 132 (33.3%) on non-western groups conduct direct work, compared to only 12 out of 49 (24.5%) of western

print(chi_square_policy)

#No significant difference between Western and Non-Western groups in policy advocacy.
#X-squared = 0.03341, p-value = 0.855

print(chi_square_diet)

# X-squared = 0.048354, p-value = 0.826
# No significant difference between Western and Non-Western groups in diet advocacy

print(chi_square_institutional)

#X-squared = 6.616, p-value = 0.01011
# 62 out of 132 (approximately 47.0%) non-Western groups conduct institutional advocacy, while only 16 out of 61 (approximately 26.2%) Western groups conduct institutional advocacy.

# Gather all p-values from chi-square tests
p_values <- c(1, 0.07611, 0.855, 0.826, 0.01011)

# Apply the Benjamini-Hochberg correction
adjusted_p_values <- p.adjust(p_values, method = "BH")

# Output the adjusted p-values
print(adjusted_p_values)

# 1.000000 0.190275 1.000000 1.000000 0.050550
# After adjusting for multiple testing, no advocacy type differs significantly between Western and non-Western countries. 
# However, the difference in institutional advocacy is close to the 0.05 threshold after adjustment.

# -------------------------------
# Two-tailed linear multivariate regression for RQ2
# -------------------------------

# Create a subset of data excluding NA values in 'western_vs_nonwestern'
data_no_na <- data[!is.na(data$western_vs_nonwestern), ]

# Generate dummy variables using model.matrix on the subset
dummy_vars <- model.matrix(~ western_vs_nonwestern - 1, data_no_na)
data <- cbind(data_no_na, dummy_vars)

# Get the names of the columns
col_names <- names(data)

# Convert to lower case and replace spaces/hyphens with underscores
new_col_names <- tolower(gsub(" ", "_", col_names))
new_col_names <- gsub("-", "_", new_col_names)

# Update the column names in the dataframe
names(data) <- new_col_names

# Check the new names
names(data)

# Steps: To understand which factors influence groups’ selection of approaches (RQ2),
# we will run a hierarchical binary logistic regression for each animal advocacy approach separately as the outcome variable.
# To do this, we must ensure variables are coded correctly (dummy coding/ effect coding) 
# We then fit the first set of models with the first set of predictors using the lm() function
# Add the second set of predictors.

# Importance variables need to be dummy coded

data <- dummy_cols(data, select_columns = "importance_funding_availability")
data <- dummy_cols(data, select_columns = "importance_talent_availability")
data <- dummy_cols(data, select_columns = "importance_context_appropriateness")
data <- dummy_cols(data, select_columns = "importance_impact_cost_effectiveness")
data <- dummy_cols(data, select_columns = "importance_mission_alignment")


#Specific code for western vs. non-western

dummy_vars <- model.matrix(~ western_vs_nonwestern - 1, data)
data <- cbind(data, dummy_vars)
names(data) <- gsub(" ", "_", names(data))

#Function for effect coding

effect_coding <- function(factor) {
  levels <- levels(factor)
  n <- length(levels)
  coding <- matrix(0, nrow = n, ncol = n-1)
  for (i in 1:(n-1)) {
    coding[i, i] <- 1
    coding[n, i] <- -1
  }
  colnames(coding) <- levels[1:(n-1)]
  coding
}
# Updated function for automated effect coding with simpler names
apply_effect_coding <- function(data, var_name) {
  var <- data[[var_name]]
  var <- factor(var)
  coding <- effect_coding(var)
  for (i in 1:(ncol(coding) - 1)) {
    new_var_name <- paste(var_name, i, sep = "_")
    data[[new_var_name]] <- coding[var, i]
  }
  data
}

# Apply the function to each variable
data <- apply_effect_coding(data, "org_size")
data <- apply_effect_coding(data, "animal_type_aquatic_farm")
data <- apply_effect_coding(data, "animal_type_dogcat_meat")
data <- apply_effect_coding(data, "animal_type_land_farm")
data <- apply_effect_coding(data, "org_years")
data <- apply_effect_coding(data, "org_mission")

# Logistic regression model
model1 <- glm(advocacy_type_corporate ~ org_size_1 + org_size_2 + 
      org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
      animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
      org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
      western_vs_nonwesternnon_western + western_vs_nonwesternonline_only, 
    family = binomial, data = data)

# Get a summary of the model
summary(model1)

model2 <- glm(advocacy_type_individual_diet ~ org_size_1 + org_size_2 + 
                org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
                animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
                org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
                western_vs_nonwesternnon_western + western_vs_nonwesternonline_only, 
              family = binomial, data = data)

summary(model2)

model3 <- glm(advocacy_type_direct_work ~ org_size_1 + org_size_2 + 
                org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
                animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
                org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
                western_vs_nonwesternnon_western + western_vs_nonwesternonline_only, 
              family = binomial, data = data)

summary(model3)
#


model4 <- glm(advocacy_type_institutional ~ org_size_1 + org_size_2 + 
                org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
                animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
                org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
                western_vs_nonwesternnon_western + western_vs_nonwesternonline_only, 
              family = binomial, data = data)

summary(model4)
#


model5 <- glm(advocacy_type_policy ~ org_size_1 + org_size_2 + 
                org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
                animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
                org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
                western_vs_nonwesternnon_western + western_vs_nonwesternonline_only, 
              family = binomial, data = data)

summary(model5)

#

model1_summary <- summary(model1)
p_values <- model1_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

# org_size_1 and org_size_2 (less than 1 staff member, 1-5 staff members) are the only significant variables, indicating that org_size
# correlates significantly with advocacy choice

model2_summary <- summary(model2)
p_values <- model2_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Nothing remains significant before or after adjustment - individual diet interventions are therefore not significantly affected

model3_summary <- summary(model3)
p_values <- model3_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Animal type is significantly associated with model3, and animal_type_aquatic_farm_1 and animal_type_dogcat_meat_1 are negatively associated

model4_summary <- summary(model4)
p_values <- model4_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#org_size_1 is significant

model5_summary <- summary(model5)
p_values <- model5_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#org_size_1 is significant

# Next stage of the analysis, which is adding the importance variables.

model1b <- glm(advocacy_type_corporate ~ org_size_1 + org_size_2 +
                 importance_funding_availability_Very_important + 
                 importance_talent_availability_Very_important + 
                 importance_context_appropriateness_Very_important +
                 importance_impact_cost_effectiveness_Very_important +
                 importance_mission_alignment_Very_important, 
               family = binomial, data = data)

summary(model1b)

model2b <- glm(advocacy_type_individual_diet ~ 
                 importance_funding_availability_Very_important + 
                 importance_talent_availability_Very_important + 
                 importance_context_appropriateness_Very_important +
                 importance_impact_cost_effectiveness_Very_important +
                 importance_mission_alignment_Very_important, 
               family = binomial, data = data)

summary(model2b)

model3b <- glm(advocacy_type_direct_work ~ + animal_type_aquatic_farm_1 + 
                 animal_type_dogcat_meat_1 +
                 importance_funding_availability_Very_important + 
                 importance_talent_availability_Very_important + 
                 importance_context_appropriateness_Very_important +
                 importance_impact_cost_effectiveness_Very_important +
                 importance_mission_alignment_Very_important, 
               family = binomial, data = data)

summary(model3b)


model4b <- glm(advocacy_type_institutional ~ org_size_1 + 
                 importance_funding_availability_Very_important + 
                 importance_talent_availability_Very_important + 
                 importance_context_appropriateness_Very_important +
                 importance_impact_cost_effectiveness_Very_important +
                 importance_mission_alignment_Very_important, 
               family = binomial, data = data)

summary(model4b)

model5b <- glm(advocacy_type_policy ~ org_size_1 + 
                 importance_funding_availability_Very_important + 
                 importance_talent_availability_Very_important + 
                 importance_context_appropriateness_Very_important +
                 importance_impact_cost_effectiveness_Very_important +
                 importance_mission_alignment_Very_important, 
               family = binomial, data = data)

summary(model5b)

#Tests for BH correction

# Collect p-values from each model
all_p_values <- list(
  model1b = summary(model1b)$coefficients[, 4],
  model2b = summary(model2b)$coefficients[, 4],
  model3b = summary(model3b)$coefficients[, 4],
  model4b = summary(model4b)$coefficients[, 4],
  model5b = summary(model5b)$coefficients[, 4]
)

# Apply BH correction and print adjusted p-values for each model
adjusted_p_values <- lapply(all_p_values, function(p) p.adjust(p, method = "BH"))
print(adjusted_p_values)

#Model 1b: org_size_1 and org_size_2 (less than 1 staff member, 1-5 staff members) and importance talent availability are the only significant variables, indicating that org_size
# correlates significantly with advocacy choice, and that talent constraits correlate with choosing corporate advocacy.

# Model 2b: Nothing remains significant before or after adjustment - individual diet interventions are therefore not significantly affected

# Model 3b: Animal type is significantly associated with model3, and animal_type_aquatic_farm_1 and animal_type_dogcat_meat_1 are negatively associated; importance of funding availability and cost-effectiveness 
# do not meet the threshold

#Model 4b: org_size_1 is significant

# Model 5b : org_size_1 is significant

############
## RQ3: How useful are different resources? 
#############

# Mann Whitney Tests for RQ3 (To understand the usefulness of different resources (RQ3), usefulness ratings will be compared between each resource type measured using
# Mann-Whitney U tests with multiple testing corrected across all tests by using the Benjamini-Hochberg correction for false discovery rate (i.e., FDR correction). )



# List of variables
variables <- c("resources_usefulness_collaboration_networking",
               "resources_usefulness_financial",
               "resources_usefulness_finding_talent",
               "resources_usefulness_grant_applications",
               "resources_usefulness_professional_development",
               "resources_usefulness_professional_mentorship",
               "resources_usefulness_research_data_access",
               "resources_usefulness_staff_well_being")

# Find the means of each response
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

## Order of results
# 1. Financial
# 2. Grant Applications
# 3. Collaboration Networking
# 4. Research and Data Access
# 5. Professional Mentorship
# 6. Professional Development
# 7. Finding Talent
# 8. Staff well-being
# Assuming stats_df is already sorted and printed as shown above

# Initialize an empty dataframe for results
comparison_results <- data.frame(
  Resource_A = character(0),
  Resource_B = character(0),
  P_Value = numeric(0),
  Significant = logical(0),
  stringsAsFactors = FALSE
)

# Variable to keep track of the current index for Resource_A
current_index <- 1

# Loop through the variables in the order of means
while (current_index < nrow(stats_df)) {
  resource_a <- stats_df$Variable[current_index]
  resource_b <- stats_df$Variable[current_index + 1]
  
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
  
  # Advance Resource_A only if the difference is significant
  if (is_significant || current_index == nrow(stats_df) - 1) {
    current_index <- current_index + 1
  } else {
    # If not significant, keep Resource_A same and increment Resource_B in the next iteration
    stats_df <- stats_df[-(current_index + 1), ]
  }
}

# Apply Benjamini-Hochberg correction to the p-values
adjusted_p_values <- p.adjust(comparison_results$P_Value, method = "BH")

# Add the adjusted p-values to the results dataframe
comparison_results$Adjusted_P_Value <- adjusted_p_values

# Determine significance based on the adjusted p-values
# (p = 0.05)

comparison_results$Adjusted_Significant <- comparison_results$Adjusted_P_Value < 0.05

# Print the results with adjusted p-values and significance
print(comparison_results)

#RQ4: What resources would best support different types of groups?
# For RQ4 , our goal is to detect a medium effect size (f2 = 0.15) using a two-tailed linear multivariate regression 
# with 80% power and alpha corrected for (0.05/22 contrasts = 0.0022). This requires 107 participants per model. 

ord_regression_raw_pvals <- function(dependent_var, dependent_var_name, data) {
  
  # Define the model formula
  formula <- as.formula(paste("as.ordered(", dependent_var_name, ") ~ org_size_1 + org_size_2 + 
                               org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
                               animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
                               org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
                               western_vs_nonwesternnon_western + western_vs_nonwesternonline_only"))
  
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
  
  # Return a data frame with variable names, coefficients, direction of effect, and raw p-values
  data.frame(Model = rep(dependent_var_name, length(p_values)),
             Variable = names(p_values),
             Coefficient = coefficients,
             P_Value = p_values)
}

# List of dependent variables
dependent_vars <- c("resources_usefulness_collaboration_networking", 
                    "resources_usefulness_financial", 
                    "resources_usefulness_grant_applications", 
                    "resources_usefulness_finding_talent", 
                    "resources_usefulness_professional_development", 
                    "resources_usefulness_professional_mentorship", 
                    "resources_usefulness_research_data_access", 
                    "resources_usefulness_staff_well_being")

# Collect raw p-values from all models
raw_p_values_list <- lapply(dependent_vars, function(var) ord_regression_raw_pvals(data[[var]], var, data))

# Combine into a single data frame
all_p_values <- do.call(rbind, raw_p_values_list)

# Apply BH correction
all_p_values$Adjusted_P_Value <- p.adjust(all_p_values$P_Value, method = "BH")

# Print the results by model
for (model_name in unique(all_p_values$Model)) {
  cat("\nResults for Model:", model_name, "\n")
  model_specific_values <- all_p_values[all_p_values$Model == model_name, ]
  print(model_specific_values[, c("Variable", "Coefficient", "Adjusted_P_Value")])
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

# Apply the summary function to each model with alpha = 0.1 and print the results
for (model_name in unique(all_p_values$Model)) {
  cat("\nModel:", model_name, "\n")
  model_results <- all_p_values[all_p_values$Model == model_name, ]
  print(summarize_significance(model_results))
}

# 
#Model: resources_usefulness_collaboration_networking 
#[1] "Significant variables: animal_type_dogcat_meat_1 (Coefficient = -0.735440867981472 , Adjusted_P_Value = 0.0224167047069273 ), western_vs_nonwesternnon_western (Coefficient = 1.20045298872094 , Adjusted_P_Value = 0.0221539360172886 ), Not at all useful|Somewhat useful (Coefficient = -3.1612211365124 , Adjusted_P_Value = 5.69150368809397e-05 )"

#Model: resources_usefulness_financial 
#[1] "Significant variables: western_vs_nonwesternonline_only (Coefficient = 13.0958281953941 , Adjusted_P_Value = 0 ), Not at all useful|Somewhat useful (Coefficient = -3.75682441435989 , Adjusted_P_Value = 0.00230595861221305 )"

#Model: resources_usefulness_grant_applications 
#[1] "Significant variables: org_size_1 (Coefficient = 1.28139254823968 , Adjusted_P_Value = 0.0484537992444805 ), western_vs_nonwesternnon_western (Coefficient = 1.53323584405647 , Adjusted_P_Value = 0.0094879286896074 ), Not at all useful|Somewhat useful (Coefficient = -2.02145864020198 , Adjusted_P_Value = 0.0221539360172886 )"

#Model: resources_usefulness_finding_talent 
#[1] "No significant variables"

#Model: resources_usefulness_professional_development 
#[1] "Significant variables: Not at all useful|Somewhat useful (Coefficient = -1.53340701943197 , Adjusted_P_Value = 0.0224167047069273 )"

#Model: resources_usefulness_professional_mentorship 
#[1] "Significant variables: Not at all useful|Somewhat useful (Coefficient = -1.67445350244733 , Adjusted_P_Value = 0.0200345547854036 )"

#Model: resources_usefulness_research_data_access 
#[1] "Significant variables: western_vs_nonwesternnon_western (Coefficient = 1.37544019012757 , Adjusted_P_Value = 0.00675206467398131 ), Not at all useful|Somewhat useful (Coefficient = -2.49869697265079 , Adjusted_P_Value = 0.00478595915666338 )"

#Model: resources_usefulness_staff_well_being 
#[1] "Significant variables: western_vs_nonwesternnon_western (Coefficient = 1.3849355160355 , Adjusted_P_Value = 0.0035579101120737 ), Somewhat useful|Very useful (Coefficient = 1.43051440194563 , Adjusted_P_Value = 0.0347014909496168 )"
