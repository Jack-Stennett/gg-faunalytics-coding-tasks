
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

# Read the recoded data
load("data_recoded.RData")

data <- recoded_data

glimpse(data)

# -------------------------------
# Chi-Squared Test for Western/ Non-Western Hypothesis
# -------------------------------

# Subset corporate data

summary(data$western_vs_nonwestern)

# Filter out 'Mixed' and 'Online only' categories
subset_filtered <- data %>%
  filter(western_vs_nonwestern != "Mixed", western_vs_nonwestern != "Online only")

# Drop unused factor levels
subset_filtered$western_vs_nonwestern <- droplevels(subset_filtered$western_vs_nonwestern)

# Create a new contingency table
contingency_table_corporate <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_corporate)
contingency_table_direct_work <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_direct_work)
contingency_table_policy <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_policy)
contingency_table_other <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_other)
contingency_table_individual_diet <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_individual_diet)
contingency_table_institutional <- table(subset_filtered$western_vs_nonwestern, subset_filtered$advocacy_type_institutional)

contingency_table_corporate
contingency_table_direct_work
contingency_table_policy 
contingency_table_other
contingency_table_individual_diet
contingency_table_institutional

# Perform the chi-square test
chi_square_corporate <- chisq.test(contingency_table_corporate)
chi_square_direct <- chisq.test(contingency_table_direct_work)
chi_square_policy <- chisq.test(contingency_table_policy)
chi_square_other <- chisq.test(contingency_table_other)
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

print(chi_square_other)

#X-squared = 0.33291, p-value = 0.564
# No significant difference between Western and Non-Western groups in "other" types of advocacy.

print(chi_square_diet)

# X-squared = 0.048354, p-value = 0.826
# No significant difference between Western and Non-Western groups in diet advocacy

print(chi_square_institutional)

#X-squared = 6.616, p-value = 0.01011
# Statistically significant at the 0.05 level, 62 out of 132 (approximately 47.0%) non-Western groups conduct institutional advocacy.
# While only 16 out of 61 (approximately 26.2%) Western groups conduct institutional advocacy.

# Gather all p-values from chi-square tests
p_values <- c(1, 0.07611, 0.855, 0.564, 0.826, 0.01011)

# Apply the Benjamini-Hochberg correction
adjusted_p_values <- p.adjust(p_values, method = "BH")

# Output the adjusted p-values
print(adjusted_p_values)

# 1.00000 0.22833 1.00000 1.00000 1.00000 0.06066
# After adjustment, differences in institutional advocacy fall behind the 0.05 threshold


# -------------------------------
# Two-tailed linear multivariate regression for RQ2
# -------------------------------

