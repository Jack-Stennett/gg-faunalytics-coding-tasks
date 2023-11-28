
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

model1b_summary <- summary(model1b)
p_values <- model1b_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

# org_size_1 and org_size_2 (less than 1 staff member, 1-5 staff members) are the only significant variables, indicating that org_size
# correlates significantly with advocacy choice

model2b_summary <- summary(model2b)
p_values <- model2b_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Nothing remains significant before or after adjustment - individual diet interventions are therefore not significantly affected

model3b_summary <- summary(model3b)
p_values <- model3b_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Animal type is significantly associated with model3, and animal_type_aquatic_farm_1 and animal_type_dogcat_meat_1 are negatively associated

model4b_summary <- summary(model4b)
p_values <- model4b_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#org_size_1 is significant

model5b_summary <- summary(model5b)
p_values <- model5b_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#org_size_1 is significant

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

# Initialize an empty dataframe to store results
results_df <- data.frame(Comparison = character(0),
                         P_Value = numeric(0),
                         Greater_Median = character(0),
                         Median_Difference = numeric(0),
                         stringsAsFactors = FALSE)

# Loop over each pair of variables and perform the test
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    # Convert ordered factors to numeric
    x_numeric <- as.numeric(data[[variables[i]]])
    y_numeric <- as.numeric(data[[variables[j]]])
    
    # Perform Mann-Whitney U Test
    result <- wilcox.test(x_numeric, y_numeric)
    
    # Extract p-value and calculate median difference
    p_value <- result$p.value
    mean_x <- mean(x_numeric, na.rm = TRUE)
    mean_y <- mean(y_numeric, na.rm = TRUE)
    mean_difference <- abs(mean_x - mean_y)
    greater_mean <- ifelse(mean_x > mean_y, variables[i], variables[j])
    
    # Append results to the dataframe
    results_df <- rbind(results_df, data.frame(Comparison = paste(variables[i], "vs", variables[j]),
                                               P_Value = p_value,
                                               Greater_Mean = greater_mean,
                                               Mean_Difference = mean_difference,
                                               stringsAsFactors = FALSE))
  }
}

# Apply Benjamini-Hochberg correction for multiple testing
adjusted_p_values <- p.adjust(results_df$P_Value, method = "BH")
results_df$Adjusted_P_Value <- adjusted_p_values

# Sort the dataframe by adjusted p-value
results_df <- results_df[order(results_df$Adjusted_P_Value), ]

# Print the sorted results with adjusted p-values
print(results_df)

# List of variables
variables <- c("resources_usefulness_collaboration_networking",
               "resources_usefulness_financial",
               "resources_usefulness_finding_talent",
               "resources_usefulness_grant_applications",
               "resources_usefulness_professional_development",
               "resources_usefulness_professional_mentorship",
               "resources_usefulness_research_data_access",
               "resources_usefulness_staff_well_being")

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

#RQ4: What resources would best support different types of groups?
# For RQ4 , our goal is to detect a medium effect size (f2 = 0.15) using a two-tailed linear multivariate regression 
# with 80% power and alpha corrected for (0.05/22 contrasts = 0.0022). This requires 107 participants per model. 

ord_regression <- function(dependent_var) {
  # Load necessary library
  library(MASS)
  
  # Define the model formula
  formula <- as.formula(paste("as.ordered(", deparse(substitute(dependent_var)), ") ~ org_size_1 + org_size_2 + 
                               org_size_3 + org_size_4 + org_size_5 + animal_type_aquatic_farm_1 + 
                               animal_type_dogcat_meat_1 + animal_type_land_farm_1 + org_years_1 + 
                               org_years_2 + org_years_3 + western_vs_nonwesternmixed + 
                               western_vs_nonwesternnon_western + western_vs_nonwesternonline_only"))
  
  # Run the ordinal logistic regression model
  model <- polr(formula, data = data, Hess = TRUE)
  model_summary <- summary(model)
  
  # Number of observations
  n <- nrow(data)
  
  # Calculate degrees of freedom
  df <- n - length(model_summary$coefficients) - 1
  
  # Calculate p-values
  p_values <- 2 * pt(-abs(model_summary$coefficients[, "t value"]), df, lower.tail = TRUE)
  
  # Apply Benjamini-Hochberg correction
  p_values_adj <- p.adjust(p_values, method = "BH")
  
  # Add corrected p-values to the summary table
  model_summary$coefficients <- cbind(model_summary$coefficients, "BH-Adjusted P-value" = p_values_adj)
  
  # Print the updated summary
  print(model_summary$coefficients)
}

ord_regression(data$resources_usefulness_collaboration_networking) 
ord_regression(data$resources_usefulness_financial) 
ord_regression(data$resources_usefulness_finding_talent)
ord_regression(resources_usefulness_grant_applications)
ord_regression(resources_usefulness_professional_development)
ord_regression(resources_usefulness_professional_mentorship)
ord_regression(resources_usefulness_research_data_access)
ord_regression(resources_usefulness_staff_well_being)
