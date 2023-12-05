
#Title: Analysis
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

# Set the scipen option to avoid scientific notation
options(scipen = 999)

## Remove all objects from the current workspace 
rm(list = ls())

#Set working directory
setwd("C:/Users/jack_/OneDrive/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

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

data$org_budget_usd_standardized <- scale(data$org_budget_usd)

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

# Advocacy Type: Corporate: 27.7% (n = 36) of Non-western groups, and 27.9% of Western groups engage in corporate advocacy 
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

# RQ2: Our objective is to determine which factors influence groups’ selection of approaches.Our desired sample
# size was 94 participants per approach. Only individual diet meets this criteria, therefore analysis of other 
#approaches will be underpowered.

# To understand which factors influence groups’ selection of approaches (RQ2),
# we will run a hierarchical binary logistic regression for each animal advocacy approach separately as the outcome variable.
# To do this, we must ensure variables are coded correctly (dummy coding/ effect coding) 
# We then fit the first set of models with the first set of predictors using the lm() function
# Add the second set of predictors.

# Importance variables need to be recoded so that "very important" is being compared to "somewhat important" and 
# "not at all important" (note, when we don't re-level, it automatically does .L and .Q for linear and
# quadratic comparisons of ordered factors; I think both make sense, but this way is closer to the prereg)

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

# Apply the relevel function to the dataset
data <- relevel_importance_variables(data, vars_to_transform)

# Check the levels of the variables
sapply(data[vars_to_transform], levels)

# Apply weighted effect coding to org_size

data$org_size.wec <- factor(data$org_size)
contrasts(data$org_size.wec) <- contr.wec(data$org_size.wec, "Less than 1")
contrasts(data$org_size.wec)

data$org_years.wec <- factor(data$org_years)
contrasts(data$org_years.wec) <- contr.wec(data$org_years.wec, "Less than 1 year")
contrasts(data$org_years.wec)

table(data$org_years)

data$org_mission.wec <- factor(data$org_mission)
contrasts(data$org_mission.wec) <- contr.wec(data$org_mission.wec, "Rights")
contrasts(data$org_mission.wec)

data$org_geographic_lvl.wec <- factor(data$org_geographic_lvl)
contrasts(data$org_geographic_lvl.wec) <- contr.wec(data$org_geographic_lvl.wec, "National")
contrasts(data$org_geographic_lvl.wec)

data$western_vs_nonwestern.wec <- factor(data$western_vs_nonwestern)
contrasts(data$western_vs_nonwestern.wec) <- contr.wec(data$western_vs_nonwestern.wec, "Western")
contrasts(data$western_vs_nonwestern.wec)

data$org_focus.wec <- factor(data$org_focus)
contrasts(data$org_focus.wec) <- contr.wec(data$org_focus.wec, "Corporate Campaigns")
contrasts(data$org_focus.wec)

levels(data$org_focus)

# Logistic regression model(this is just assessing the inclusion of extraneous variables, and we correct for multiple
# terms in the model)

model1 <- glm(advocacy_type_corporate ~ org_size.wec + animal_type_aquatic_farm + 
      animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + org_mission.wec + org_geographic_lvl.wec + western_vs_nonwestern.wec + org_budget_usd_standardized, 
    family = binomial, data = data)

# Get a summary of the model
summary(model1)

model2 <- glm(advocacy_type_individual_diet ~ org_size.wec + animal_type_aquatic_farm + 
                animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + org_mission.wec + org_geographic_lvl.wec + western_vs_nonwestern.wec + org_budget_usd_standardized, 
              family = binomial, data = data)

summary(model2)

model3 <- glm(advocacy_type_direct_work ~ org_size.wec + animal_type_aquatic_farm + 
                animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + org_mission.wec + org_geographic_lvl.wec + western_vs_nonwestern.wec + org_budget_usd_standardized, 
              family = binomial, data = data)

summary(model3)
#


model4 <- glm(advocacy_type_institutional ~ org_size.wec + animal_type_aquatic_farm + 
                animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + org_mission.wec + org_geographic_lvl.wec + western_vs_nonwestern.wec + org_budget_usd_standardized, 
              family = binomial, data = data)

summary(model4)
#


model5 <- glm(advocacy_type_policy ~ org_size.wec + animal_type_aquatic_farm + 
                animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + org_mission.wec + org_geographic_lvl.wec + western_vs_nonwestern.wec + org_budget_usd_standardized, 
              family = binomial, data = data)

summary(model5)

#

model1_summary <- summary(model1)
p_values <- model1_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

# Nothing is significant

model2_summary <- summary(model2)
p_values <- model2_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#org_mission.wecWelfare is significant and negative (Estimate: -2.58611)

model3_summary <- summary(model3)
p_values <- model3_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Animal type is significantly associated with model3, and animal_type_aquatic_farm and animal_type_dogcat_meat are associated. animal_type_aquatic_farm (Estimate: -2.6380) has a negative assocation with direct work
# while animal_type_dogcat_meat (Estimate: 2.3207) has a positive association with doing direct work. 

model4_summary <- summary(model4)
p_values <- model4_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Nothing is significant

model5_summary <- summary(model5)
p_values <- model5_summary$coefficients[, 4]
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

#Nothing is significant

# Next stage of the analysis, which is adding the importance variables. These are the final models where we assess our main variables of interest
# while retaining any significant extraneous variables from the previous stage, which are the level of importance of funding 
#availability, availability of talent, local context/appropriateness, impact/cost-effectiveness, and alignment with organization’s mission/values.
# We will adjust p-values using BH correction.
data$importance_context_appropriateness_not_at_all_important

model1b <- glm(advocacy_type_corporate ~ 
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)


summary(model1b)

model2b <- glm(advocacy_type_individual_diet ~  org_mission.wec +
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)

summary(model2b)
data$animal_type_aquatic_farm

model3b <- glm(advocacy_type_direct_work ~  animal_type_dogcat_meat + animal_type_aquatic_farm +
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)

summary(model3b)


model4b <- glm(advocacy_type_institutional ~  
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
               family = binomial, data = data)

summary(model4b)

model5b <- glm(advocacy_type_policy ~  
                 importance_funding_availability +
                 importance_talent_availability +
                 importance_context_appropriateness +
                 importance_impact_cost_effectiveness +
                 importance_mission_alignment, 
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

#Model 1b: Importance talent availability is the only significant variable, indicating that talent constraints are less of an obstacle for those organisations pursuing corporate advocacy.

# Model 2b: Only org_mission welfare remains significant after adjustment- individual diet interventions are therefore not significantly correlated with importance. 

# Model 3b: Animal type is significantly associated with model3, and animal_type_aquatic_farm (negative) and animal_type_dogcat_meat (positive) remain associated; importance of funding availability and cost-effectiveness 
# do not meet the threshold

#Model 4b: Nothing is significant

# Model 5b : Nothing is significant

############
## RQ3: How useful are different resources? 
#############

# Mann Whitney Tests for RQ3 (To understand the usefulness of different resources (RQ3), usefulness ratings will be compared between each resource type measured using
# Mann-Whitney U tests with multiple testing corrected across all tests by using the Benjamini-Hochberg correction for false discovery rate (i.e., FDR correction). )
# Our desired number of respondents was 270, and we have 186 completed responses, meaning that this analysis is slightly underpowered.

table(data$resources_usefulness_financial)
table(data$resources_usefulness_staff_well_being)

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

# Note, 19 out of 28 pairwise comparisons are statistically significant; 
    
# For this section, we perform the necessary pairwise comparisons to detect whether differences are significant

# We only consider it necessary to test pairwise comparisons if the mean  
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

#RQ4: What resources would best support different types of groups? 
# For RQ4 , our goal is to detect a medium effect size (f2 = 0.15) using a two-tailed linear multivariate regression with 80% power and alpha 
# corrected for (0.05/22 contrasts = 0.0022). This requires 107 participants per model. We reached this requirement, with 186 respondents per question.

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

# Calculate the mean of 'org_budget_usd_standardized', excluding NA values
mean_budget <- mean(data$org_budget_usd_standardized, na.rm = TRUE)

# Replace NA in 'org_budget_usd_standardized' with the calculated mean
data$org_budget_usd_standardized[is.na(data$org_budget_usd_standardized)] <- mean_budget

# Perform a logarithmic transformation; Adding 1 to avoid taking log of zero (log(0) is undefined)
data$org_budget_usd_standardized <- log1p(data$org_budget_usd_standardized)

# Initialize a list to store results from each model
all_results <- list()

# Custom function to round p-values
round_p_values <- function(p_value, threshold = 0.0001, digits = 4) {
  if (p_value < threshold) {
    return("<0.0001")
  } else {
    return(as.character(round(p_value, digits)))
  }
}
str(data$org_budget_usd_standardized)

# Perform ordinal logistic regression for each dependent variable 
for (var in dependent_vars) {
  if (var %in% names(data)) {
    # Define the formula for the model
    formula <- as.formula(paste("as.ordered(", var, ") ~ org_size.wec + org_budget_usd_standardized + animal_type_aquatic_farm + 
                                 animal_type_dogcat_meat + animal_type_land_farm + org_years.wec + western_vs_nonwestern.wec"))
    
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
    
    # Print the results
    print(all_results)
  }
}

# Combine all results into a single data frame
all_p_values <- do.call(rbind, all_results)

# Apply BH correction
all_p_values$Adjusted_P_Value <- p.adjust(all_p_values$P_Value, method = "BH")

# Remove rows with NA adjusted p-values
all_p_values <- all_p_values[!is.na(all_p_values$Adjusted_P_Value), ]

# Applying the function to data frame
all_p_values$Adjusted_P_Value <- sapply(all_p_values$Adjusted_P_Value, round_p_values)


# Print the results by model
for (model_name in unique(all_p_values$Model)) {
  cat("\nResults for Model:", model_name, "\n")
  model_specific_values <- all_p_values[all_p_values$Model == model_name, ]
  print(model_specific_values[, c("Variable", "Coefficient", "Adjusted_P_Value")])
}

# Function to summarize significant variables for each model
summarize_significance <- function(model_results, alpha = 0.1) {
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

#Model: resources_usefulness_collaboration_networking 
#[1] "Significant variables: animal_type_dogcat_meat (Coefficient = 1.40320915965341 , Adjusted_P_Value = 0.0611 ), western_vs_nonwestern.wecNon-Western (Coefficient = 0.351950468753488 , Adjusted_P_Value = 0.0898 )"

#Model: resources_usefulness_financial 
#[1] "Significant variables: Not at all useful|Somewhat useful (Coefficient = -3.5105046179214 , Adjusted_P_Value = 0.001 )"

#Model: resources_usefulness_grant_applications 
#[1] "Significant variables: western_vs_nonwestern.wecOnline only (Coefficient = 25.6050079930687 , Adjusted_P_Value = <0.0001 )"

#Model: resources_usefulness_finding_talent 
#[1] "Significant variables: western_vs_nonwestern.wecNon-Western (Coefficient = 0.330939116804957 , Adjusted_P_Value = 0.0898 ), Not at all useful|Somewhat useful (Coefficient = -1.65808285460903 , Adjusted_P_Value = 0.0904 )"

#Model: resources_usefulness_professional_development 
#[1] "No significant variables"

#Model: resources_usefulness_professional_mentorship 
#[1] "Significant variables: org_budget_usd_standardized (Coefficient = -1.84382106129815 , Adjusted_P_Value = 0.0492 )"

#Model: resources_usefulness_research_data_access #
#[1] "Significant variables: org_size.wec1-5 (Coefficient = -0.506953302933902 , Adjusted_P_Value = 0.0581 ), org_size.wec101+ (Coefficient = 14.6511635201911 , Adjusted_P_Value = <0.0001 ), western_vs_nonwestern.wecNon-Western (Coefficient = 0.431073549607728 , Adjusted_P_Value = 0.0202 ), Not at all useful|Somewhat useful (Coefficient = -2.97316807018785 , Adjusted_P_Value = 0.0007 )"

#Model: resources_usefulness_staff_well_being 
#[1] "Significant variables: western_vs_nonwestern.wecNon-Western (Coefficient = 0.421595139964375 , Adjusted_P_Value = 0.0142 )"