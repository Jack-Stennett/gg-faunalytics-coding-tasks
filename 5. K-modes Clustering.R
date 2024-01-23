
#Title: K-Modes Clustering
#R version: 4.2.2

# -------------------------------
# Set up
# -------------------------------

# Conditions for K-Modes Clustering (from pre-registration)
# 1. We need a minimum of 60 participants. Our initial number of clusters is three and it’s recommended to have at least 20 participants per cluster.
# 2. To investigate whether groups form meaningful segments (RQ6), we will employ a K-modes clustering analysis. 
# considering various organization characteristics (factors considered, age of organization, philosophy, mission, 
# animal focus, and advocacy approach) as segmentation variables. First, we will run the K-modes clustering algorithm 
# on the data, setting the initial number of clusters to 3, max iterations to 100, and multiple starting configurations to 100.
# 3. To ascertain the optimal number of clusters, we will explore the following clustering validity indices: 
# We will iteratively adjust the number of clusters
# (centers) based on these evaluations. Upon identifying the optimal number of clusters, we will reassess the stability 
# of the clusters by rerunning the K-modes algorithm with different initial configurations.
# 4. The quality of the final clusters will be verified using within-cluster differences. 
# Once the clusters are confirmed, we will append a new variable indicating the cluster membership to the dataset.
# Then we will carry out correlation tests (Chi-Square) to examine the strength of correlation between our clusters 
# and the profiling variables. The number of clusters will be finalized based on clustering validity indices, 
# and each resulting cluster will be evaluated and profiled according to its characteristics.


## Remove all objects from the current workspace 
rm(list = ls())

#Set working directory
setwd("C:/Users/jack_/Desktop/Documents/GitHub/International-Study-Of-Strategies-And-Needs")

# Load required libraries
library(tidyverse)
library(fastDummies)
library(lmtest)
library(klaR)
library(cluster)
library(factoextra)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Read the recoded data
load("data_recoded.RData")

data <- recoded_data

# Excluding 2 response IDs that don't include key variables (org_focus and advocacy choice questions)

response_ids_to_exclude <- c(
  "R_1QFAa3zZtRFIPUs", "R_2uZC1yXGZDzyPew")

data <- data[!data$response_id %in% response_ids_to_exclude, ]

# Helper function to convert levels to binary categories. K-modes is less suited to ordered factor variables, therefore we convert to binary. 

convert_to_2p <- function(x, type = "interest") {
  
  if (type == "interest") {
    recode_vector <- c("Very uninterested" = "Uninterested or Neutral",
                       "Somewhat uninterested" = "Uninterested or Neutral",
                       "Neutral" = "Uninterested or Neutral",
                       "Somewhat interested" = "Interested",
                       "Very interested" = "Interested")
  } else if (type == "satisfaction") {
    recode_vector <- c("Very dissatisfied" = "Dissatisfied or Neutral",
                       "Somewhat dissatisfied" = "Dissatisfied or Neutral",
                       "Neither satisfied nor dissatisfied" = "Dissatisfied or Neutral",
                       "Somewhat satisfied" = "Satisfied",
                       "Very satisfied" = "Satisfied")
  }
  
  return(as.factor(recode_vector[x]))
}

interested_cols <- grep('interest_', names(data), value = TRUE)

# Convert 'interest' columns using convert_to_2p function

for (column in interested_cols) {
  new_col <- sapply(data[[column]], convert_to_2p, type = "interest")
  data[paste(column, "2p", sep = "_")] <- factor(new_col, levels = c("Uninterested or Neutral", "Interested"), ordered = FALSE)
}

# Simpler process to create binary variables for org_years and org_size

data$org_years_binary <- ifelse(data$org_years %in% c("Less than 2 years", "3-5 years"), "5 and under", "6+")
data$org_size_binary <- ifelse(data$org_size %in% c("Less than 1", "1-5"), "5 and under", "6+")

#Confirm levels

unique(data$interest_diet)
unique(data$interest_diet_2p)

# Recoding responses for simplicity (for later analysis and visualisations)

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

# Create variable to identify the number of advocacy and animal types chosen by each organisation. 

data$advocacy_types <- rowSums(data[, c("advocacy_type_corporate", "advocacy_type_policy", "advocacy_type_institutional", "advocacy_type_direct_work", "advocacy_type_individual_diet", "advocacy_type_other")] > 0)
data$animal_types <- rowSums(data[, c("animal_type_dogcat_meat", "animal_type_companion", "animal_type_other", "animal_type_wild", "animal_type_lab", "animal_type_captive", "animal_type_aquatic_farm", "animal_type_land_farm")] > 0)

# List of countries by continent - we need this variable in order to better arrange timezones for focus groups in the later stage of research 

asia_countries <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Cambodia", 
                    "China", "Cyprus", "Georgia", "Hong Kong SAR", "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan", 
                    "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Macau SAR", "Malaysia", "Maldives", "Mongolia", "Myanmar", 
                    "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi Arabia", 
                    "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", 
                    "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Viet Nam", "Yemen")

europe_countries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                      "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                      "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", 
                      "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", 
                      "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
                      "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom")

africa_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", 
                      "Central African Republic", "Chad", "Comoros", "Congo", "Djibouti", "Egypt", "Equatorial Guinea", 
                      "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                      "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", 
                      "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                      "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
                      "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

north_america_countries <- c("Canada", "United States")


latin_america_countries <- c("Cuba", "Dominica", "Dominican Republic", "Grenada", "Haiti", 
                             "Jamaica", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", 
                             "Antigua and Barbuda", "Bahamas", "Barbados", "Argentina",  "Belize", "Bolivia", "Brazil", "Chile", 
                             "Colombia", "Ecuador", "Costa Rica", "El Salvador", "Guyana", "Honduras", "Mexico", "Paraguay", 
                             "Guatemala", "Nicaragua", "Panama", "Peru", "Suriname", "Uruguay", "Venezuela")

oceania_countries <- c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", "New Zealand", 
                       "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")

# Create patterns for matching
asia_pattern <- paste(asia_countries, collapse = "|")
europe_pattern <- paste(europe_countries, collapse = "|")
africa_pattern <- paste(africa_countries, collapse = "|")
north_america_pattern <- paste(north_america_countries, collapse = "|")
latin_america_pattern <- paste(latin_america_countries, collapse = "|")
oceania_pattern <- paste(oceania_countries, collapse = "|")

# Function to determine continent
determine_continent <- function(country) {
  continents <- c(
    "Asia" = str_detect(country, asia_pattern),
    "Europe" = str_detect(country, europe_pattern),
    "Africa" = str_detect(country, africa_pattern),
    "North America" = str_detect(country, north_america_pattern),
    "South America" = str_detect(country, latin_america_pattern),
    "Oceania" = str_detect(country, oceania_pattern)
  )
  
  matched_continents <- names(continents[continents])
  
  if (length(matched_continents) > 1) {
    return("Mixed")
  } else if (length(matched_continents) == 1) {
    return(matched_continents)
  } else {
    return(NA_character_)
  }
}

# Create a new variable for continent
data <- data %>%
  mutate(continent = sapply(country_focus, determine_continent))

table(data$continent)

# Begin analysis by handling missing data in interest_ columns (interest in  a given advocacy type, interest_policy_2p" etc.) 
# 'NA' values represent active engagement in a given advocacy type, thus are recoded as 'NA_category'.
# This recoding allows for meaningful inclusion and analysis of these 'NA' values.

# Define the columns that have NAs
cols_with_na <- c("interest_policy_2p", "interest_corp_2p", "interest_inst_2p", "interest_diet_2p", "interest_direct_2p")

# Adds "NA_category" to the levels of the factors
for (column in cols_with_na) {
  data[[column]] <- addNA(data[[column]], ifany = TRUE)
  levels(data[[column]])[is.na(levels(data[[column]]))] <- "NA_category"
}

# Assigns "NA_category" to the NA values in the columns
data[cols_with_na] <- lapply(data[cols_with_na], function(x) {
  levels(x) <- c(levels(x), "NA_category") # make sure "NA_category" is a level
  x[is.na(x)] <- "NA_category" # now can assign "NA_category" to NA's
  x
})


# -------------------------------
# K-modes clustering analysis
# -----------------------------

# This section details the iterative process employed for K-modes clustering to construct statistically 
# valid clusters relevant to our analysis. The process integrates statistical methods with practical usefulness 
# concerns regarding the clusters. 

# We measure the within-cluster sum of differences (WithinDiff) metric at each stage to test validity.  
# A lower withindiff value indicates that the objects in a cluster are more similar to 
# each other, suggesting a more cohesive and well-defined cluster. However, there is no clear threshold at 
# which WithinDiff is relevant, therefore I also manually check that the sizes of the clusters are relatively even, 
# to avoid overfitting, and that there are distinct and relevant differences between the modes of each cluster. 

# In the pre-reg we suggested various organization characteristics (factors considered (importance_), 
# age of organization, philosophy/ mission, animal focus, and advocacy approaches) as segmentation variables.
# We add these examples and size of organisation (org_size) and interest in given advocacy approaches (interest..._2p).

# List of variables to include in the K-modes clustering (both profiling and segmentation)

variables_to_include <- c(
  "org_size_binary", "org_years_binary", "org_mission", "advocacy_type_corporate", "advocacy_type_policy", 
  "advocacy_type_institutional", "advocacy_type_direct_work", "advocacy_type_individual_diet", 
  "advocacy_type_other", "importance_funding_availability", "importance_talent_availability",
  "importance_context_appropriateness", "importance_impact_cost_effectiveness", "animal_type_dogcat_meat", "animal_type_land_farm", "animal_type_aquatic_farm",
  "importance_mission_alignment", "interest_policy_2p", "interest_corp_2p", "interest_inst_2p", "interest_diet_2p", "interest_direct_2p" 
)

# Remove rows with any missing values in the specified columns
cleaned_data <- na.omit(data[, variables_to_include])

# Convert cleaned_data to a standard data frame
# (Note: Converting 'cleaned_data' to a standard data frame with 'as.data.frame' seems to resolve column indexing issues.
# This step appears necessary to prevent errors related to column subsetting in subsequent analysis steps.
# It ensures that column references are correctly aligned with the reduced dataset after removing NAs.)
cleaned_data_standard <- as.data.frame(cleaned_data)

# Define the maximum number of clusters to consider (10 is a standard maximum number of clusters for a dataset of this size)
# in order to avoid clusters with too few values, see Sofyan et al 2021 (https://iopscience.iop.org/article/10.1088/1757-899X/1087/1/012085/pdf) 

max_clusters <- 10

# Initialize vectors to store the averages of each iteration
average_withindiffs <- numeric(21) #21 different alternative random seeds from 0 to 20 - this number was chosen according to recommendations
# in Colas, Sigaud and Oudeyer (2018)'s paper on choosing number of random seeds https://arxiv.org/pdf/1806.08295.pdf

# Run K-modes clustering for different seeds and collect averages
for (i in 0:20) {
  set.seed(i)
  kmodes_result <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], 3, 100, 100)# This sets the initial number of clusters to 3, max iterations to 100, and multiple starting configurations to 100.
  
  # Calculate the average withindiff for the current iteration
  average_withindiff <- mean(kmodes_result$withindiff)
  average_withindiffs[i + 1] <- average_withindiff
  
  print(paste("For seed ", i))
  print(kmodes_result)
  
  # Print the results for the current iteration
  print(paste("For seed", i, "- Average withindiff:", average_withindiff))
}

# Calculate the overall average of the withindiffs 
overall_average_withindiff <- mean(average_withindiffs)

# Print the overall averages
print(paste("Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds:", overall_average_withindiff))


# Variable Selection and Refinement

# Observing these results, we see that there is very high variance, and within cluster simple matching distance is high ( 
# "Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds: 13.23) and highly variable. 
# We noted that, for the importance variables  the modal value (visible from K-modes results) is almost always "very important", therefore
# there seems to be insufficient variation for these variables to provide value to the cluster. Because our previous regression analyses didn't find the 
# importance variables to significantly predict advocacy type, in addition to showing little variation in the clusters, 
# we're excluding them from further analyses here. Below, you can see the same algorithm applied without these importance variables.

variables_to_include <- c(
  "org_size_binary", "org_years_binary", "org_mission", "advocacy_type_corporate", "advocacy_type_policy", 
  "advocacy_type_institutional", "advocacy_type_direct_work", "advocacy_type_individual_diet", "animal_type_dogcat_meat", "animal_type_land_farm", "animal_type_aquatic_farm",
  "advocacy_type_other", "interest_policy_2p", "interest_corp_2p", "interest_inst_2p", "interest_diet_2p", "interest_direct_2p" 
)

# Remove rows with any missing values in the specified columns
cleaned_data <- na.omit(data[, variables_to_include])

# Convert cleaned_data to a standard data frame
cleaned_data_standard <- as.data.frame(cleaned_data)

# Define the maximum number of clusters to consider
max_clusters <- 10

# Initialize vectors to store the averages of each iteration
average_withindiffs <- numeric(21)

# Run K-modes clustering for different seeds and collect averages
for (i in 0:20) {
  set.seed(i)
  kmodes_result <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], 3, 100, 100)
  
  # Calculate the average withindiff for the current iteration
  average_withindiff <- mean(kmodes_result$withindiff)
  average_withindiffs[i + 1] <- average_withindiff
  
  print(paste("For seed ", i))
  print(kmodes_result)
  
  # Print the results for the current iteration
  print(paste("For seed", i, "- Average withindiff:", average_withindiff))
}

# Calculate the overall average of the withindiffs and cluster sizes
overall_average_withindiff <- mean(average_withindiffs)

# Print the overall averages
print(paste("Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds:", overall_average_withindiff))

# Following the refinement of our variable set, the clustering results still indicated variability across 
# different seeds, with a moderately high average within-cluster sum of differences (Withindiff) of 9.36. 
# A notable observation was the tendency for certain 'advocacy_type' variables to consistently cluster together. 
# To quantitatively assess the extent of correlation among these variables, and to explore their potential 
# in enhancing the depth of our clusters, we performed chi-squared tests, using a p-value of 0.01 to identify 
# less strongly correlated variables. We selected a p-value threshold of 0.01 to more effectively distinguish 
# strongly correlated variables, as those exhibiting only weak correlations are less likely to contribute 
# to the formation of consistent and meaningful clusters. This stringent criterion helps ensure 
# that the variables we identify as significantly correlated are likely to have a substantive 
# and consistent relationship, and contribute to the integrity of the clusters.

# Initialize a matrix to store p-values
p_value_matrix <- matrix(NA, nrow = length(variables_to_include), ncol = length(variables_to_include),
                         dimnames = list(variables_to_include, variables_to_include))

# Loop through each pair of variables and perform chi-squared test
for (i in 1:length(variables_to_include)) {
  for (j in 1:length(variables_to_include)) {
    if (i != j) {
      # Create a contingency table
      contingency_table <- table(data[[variables_to_include[i]]], data[[variables_to_include[j]]])
      
      # Perform the chi-squared test and store the p-value
      test_result <- try(chisq.test(contingency_table), silent = TRUE)
      if (class(test_result) != "try-error") {
        p_value_matrix[i, j] <- test_result$p.value
      } else {
        p_value_matrix[i, j] <- NA  # Assign NA if the test fails (e.g., due to zero counts)
      }
    }
  }
}

# Count the number of significant correlations for each variable
significant_correlation_counts <- apply(p_value_matrix, 1, function(row) {
  sum(row < 0.01, na.rm = TRUE)
})

# Print the count of significant correlations for each variable
significant_correlation_counts

# org_size_binary              org_years_binary                   org_mission 
# 6                             0                             3 
# advocacy_type_corporate          advocacy_type_policy   advocacy_type_institutional 
# 6                             6                             9 
# advocacy_type_direct_work advocacy_type_individual_diet       animal_type_dogcat_meat 
# 3                             6                             2 
# animal_type_land_farm      animal_type_aquatic_farm           advocacy_type_other 
# 1                             4                             6 
# interest_policy_2p              interest_corp_2p              interest_inst_2p 
# 8                             8                             9 
# interest_diet_2p            interest_direct_2p 
# 7                             6 

# We will proceed with the refined set of variables, excluding those with lower correlation counts (under n = 5). Based on this criterion
# org_years_binary, org_mission,  animal_type_dogcat_meat, animal_type_land_farm, animal_type_aquatic_farm, and advocacy_type_direct_work were excluded.
# This allows us to distinguish more and less correlated variables, and to focus on variables that 
# contribute more significantly to the clustering process, thereby improving the clarity and utility of the resulting clusters.

variables_to_include <- c(
  "org_size_binary", "advocacy_type_corporate", "advocacy_type_policy", 
  "advocacy_type_institutional", "advocacy_type_individual_diet", 
  "advocacy_type_other", "interest_policy_2p", "interest_corp_2p", "interest_inst_2p", "interest_diet_2p", "interest_direct_2p" 
)

# Remove rows with any missing values in the specified columns
cleaned_data <- na.omit(data[, variables_to_include])

# Convert cleaned_data to a standard data frame
cleaned_data_standard <- as.data.frame(cleaned_data)

# Define the maximum number of clusters to consider
max_clusters <- 10

average_withindiffs <- numeric(21)

# Run K-modes clustering for different seeds and collect averages
for (i in 0:20) {
  set.seed(i)
  kmodes_result <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], 3, 100, 100)
  
  # Calculate the average withindiff for the current iteration
  average_withindiff <- mean(kmodes_result$withindiff)
  average_withindiffs[i + 1] <- average_withindiff
  
  print(paste("For seed ", i))
  print(kmodes_result)
  
  # Print the results for the current iteration
  print(paste("For seed", i, "- Average withindiff:", average_withindiff))
}

# Calculate the overall average of the withindiffs and cluster sizes
overall_average_withindiff <- mean(average_withindiffs)

# Print the overall averages
print(paste("Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds:", overall_average_withindiff))

# This clustering is much better. Cluster sizes are generally similar, and Within cluster simple-matching distance by cluster
# has dropped to an average of 5.74. Looking at the data in more detail, a clear pattern is starting to emerge: 
# there are groups that are more likely to conduct corporate, policy and institutional advocacy, and these are
# all positively correlated (a group that conducts one of these advocacy types is more likely to conduct another)
# Among groups who don't conduct these advocacy types, there are groups that are interested or uninterested in these types of advocacy.
# This appears to be both a consistent effect across different random seeds, and aligns with the clustering needed to answer the RQs. 

# However, after manually verifying the different iterations, some variables don't show signs of being used in this process, and the modes across clusters tend to be 
# dominated by a single response potentially raising the imprecision of the clustering. For example, in interest_diet_2p, NA_category (145/210 responses) was dominant 
# across clusters, and the other two responses (Interested and Uninterested or Neutral) failed to form consistent patterns across cluster seeds.

# Therefore we remove org_size_binary, advocacy_type_other, interest_diet_2p and interest_direct_2p from the next stage of analysis. 

  variables_to_include <- c("advocacy_type_institutional", "advocacy_type_individual_diet",
    "advocacy_type_policy", "advocacy_type_corporate", "interest_policy_2p", "interest_corp_2p", "interest_inst_2p")
  
  # Add a temporary identifier to match the rows before removing NAs
  data$id <- seq_len(nrow(data))
  
  # Keep only the variables we want to include for K-modes
  cleaned_data <- data[, c(variables_to_include, "id")]
  
  # Remove rows with any missing values in the specified columns
  cleaned_data <- na.omit(cleaned_data)
  
  # Convert cleaned_data to a standard data frame
  cleaned_data_standard <- as.data.frame(cleaned_data)
  
  # Run K-modes clustering for different seeds and collect averages
  for (i in 0:20) {
    set.seed(i)
    kmodes_result <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], 3, 100, 100)
    
    # Calculate the average withindiff for the current iteration
    average_withindiff <- mean(kmodes_result$withindiff)
    average_withindiffs[i + 1] <- average_withindiff
    
    print(paste("For seed ", i))
    print(kmodes_result)
    
    # Print the results for the current iteration
    print(paste("For seed", i, "- Average withindiff:", average_withindiff))
  }
  
  # Calculate the overall average of the withindiffs and cluster sizes
  overall_average_withindiff <- mean(average_withindiffs)
  
  # Print the overall averages
  print(paste("Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds:", overall_average_withindiff))

# This now gives the most stable set of clusters. 12/21 iterations give identical clusters, and they have low average within-cluster differences.  
# Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds: 3.07114936960419"

  variables_to_include <- c("advocacy_type_institutional", "advocacy_type_individual_diet",
    "advocacy_type_policy", "advocacy_type_corporate", "interest_policy_2p", "interest_corp_2p", "interest_inst_2p")
  
  # Add a temporary identifier to match the rows before removing NAs
  data$id <- seq_len(nrow(data))
  
  # Keep only the variables we want to include for K-modes
  cleaned_data <- data[, c(variables_to_include, "id")]
  
  # Remove rows with any missing values in the specified columns
  cleaned_data <- na.omit(cleaned_data)
  
  # Convert cleaned_data to a standard data frame
  cleaned_data_standard <- as.data.frame(cleaned_data)
  
  # Run K-modes clustering for different seeds and collect averages
  for (i in 0:20) {
    set.seed(i)
    kmodes_result <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], 3, 100, 100)
    
    # Calculate the average withindiff for the current iteration
    average_withindiff <- mean(kmodes_result$withindiff)
    average_withindiffs[i + 1] <- average_withindiff
    
    print(paste("For seed ", i))
    print(kmodes_result)
    
    # Print the results for the current iteration
    print(paste("For seed", i, "- Average withindiff:", average_withindiff))
  }
  
  # Calculate the overall average of the withindiffs and cluster sizes
  overall_average_withindiff <- mean(average_withindiffs)
  
  # Print the overall averages
  print(paste("Overall Average Within-cluster Sum of Differences (Withindiff) across all seeds:", overall_average_withindiff))

# Printing result for seed 1 here (note: the results for Seed 1 are identical to seeds: 0, 2, 3, 4, 5, 6, 7, 8, 10, 15, 16, 17, 20; I choose 1 for simplicity)
#  
#  [1] "For seed  1"
#   K-modes clustering with 3 clusters of sizes 52, 86, 72
  
#  Cluster modes:
#    advocacy_type_institutional advocacy_type_individual_diet advocacy_type_policy advocacy_type_corporate      interest_policy_2p
#  1                           0                             0                    0                       0 Uninterested or Neutral
#  2                           1                             1                    1                       1             NA_category
#  3                           0                             1                    0                       0              Interested
##  interest_corp_2p        interest_inst_2p
#  1 Uninterested or Neutral Uninterested or Neutral
#  2             NA_category             NA_category
#  3              Interested              Interested
  
# 
#  Within cluster simple-matching distance by cluster:
#    [1] 1.988610 3.492529 3.127797
  
# The outcomes of this clustering are clear. There is a clustering where: 1) The majority of cluster one conducts fewer advocacy types
# and is uninterested in pursuing policy, corporate or institutional advocacy. 2) The majority of cluster 2 conducts these four types of advocacy and 
# is therefore not interested ("NA_category") in pursuing other types. 3) The majority of cluster 3 conducts fewer advocacy types, albeit is more likely to conduct individual
# diet advocacy, but is interested in pursuing other advocacy types. This will be made clearer in the tables and figures below.

# Here I set the seed to 1 and run again, storing this as the cluster variable. This is just because the seed is currently set to 20 from the previous for loop and should
# be reset to a clearer/ more legible number.  

set.seed(1)  
kmodes_result <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], 3, 100, 100)
print(paste("For seed ", 1))
print(kmodes_result)

  # Add the cluster assignments to the cleaned data
  cleaned_data$cluster <- kmodes_result$cluster
  
  # Join the cluster assignments back to the original data
  data <- left_join(data, cleaned_data[c("id", "cluster")], by = "id")
  
  # Now remove rows where cluster is NA to remove any rows that were not part of the K-modes analysis
  data <- data[!is.na(data$cluster), ]
  
  # Remove the temporary identifier
  data$id <- NULL
  
  # Print the first few rows to verify the merge
  print(head(data))
  print(kmodes_result)
  
  # Create a list to store the tables
  cluster_summary_tables <- list()
  
# Displaying cluster frequencies
  
  # Loop through each variable to create frequency tables for each cluster
  for (var in variables_to_include) {
    # Create a contingency table of counts
    table_counts <- table(data[[var]], data$cluster)
    
    # Convert counts to proportions
    table_props <- prop.table(table_counts, margin = 2)
    
    # Convert to a dataframe for easier viewing and include the variable name
    cluster_summary_tables[[var]] <- as.data.frame.matrix(table_props)
  }
  
  # To print out the tables in the console
  for (var in names(cluster_summary_tables)) {
    cat("Variable:", var, "\n")
    print(cluster_summary_tables[[var]])
    cat("\n")
  }

# Identify list columns in the dataframe
list_columns <- sapply(data, function(x) is.list(x))

# Convert list columns to a string representation (if any)
data[, list_columns] <- lapply(data[, list_columns], function(x) {
  sapply(x, toString)
})

# Define the file path for the CSV file
file_path <- "C:/Users/jack_/Desktop/clustered_data.csv"


######
# Stage 2 : Testing Cluster Validity
######

# As the Rand Index (mentioned in pre-reg) is unsuitable for the variables we have chosen in this K-modes clustering, 
# because the variables are all categorical, we can use the Within Cluster sum of differences and the silhouette score to test the validity of clusters. 
# This paper (Salem et al, 2021) illustrates how you can use silhouette scores to validate clustering https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7998089/
# The original paper on K modes clustering (Huang 1997) illustrates how we can use within cluster sum of difference to 
# validate clustering Huang, Z. (1997) A Fast Clustering Algorithm to Cluster Very Large Categorical Data Sets in Data Mining. in KDD: Techniques and Applications (H. Lu, H. Motoda and H. Luu, Eds.), pp. 21-34, World Scientific, Singapore.

# Prepare a vector to hold the total within-cluster sums of differences for each k
wsd <- numeric(max_clusters)

# Calculate total within-cluster sums of differences for k from 1 to max_clusters
for (k in 1:max_clusters) {
  
  km <- kmodes(cleaned_data_standard[, variables_to_include, drop = FALSE], k, 100, 100)
  
  # Check the structure of km to ensure it's as expected
  if(!is.null(km$withindiff)) {
    # Sum up the within-cluster differences for all clusters
    wsd[k] <- sum(km$withindiff)
  } else {
    warning(paste("No withindiff for k =", k))
  }
}

# We use this plot to visually determine the 'elbow point' where the rate of decrease sharply changes

plot(1:max_clusters, wsd, type = "b", xlab = "Number of clusters", ylab = "Total within-cluster sum of differences", main = "Elbow Method for Determining Optimal K")

# This suggests that three is the optimal number of clusters, as it forms the smallest angle on the chart

# Extract the cluster assignments from the K-modes result
clusters <- kmodes_result$cluster

# Compute the dissimilarity matrix using the 'daisy' function from the 'cluster' package
dissimilarity_matrix <- daisy(cleaned_data_standard[, variables_to_include], metric = "gower")

print(dissimilarity_matrix)

# Calculate the silhouette scores using the cluster assignments and the dissimilarity matrix
silhouette_scores <- silhouette(clusters, dissimilarity_matrix)

# Calculate the average silhouette score for the entire dataset
avg_sil_score <- mean(silhouette_scores[, "sil_width"])
print(paste("Average Silhouette Score:", avg_sil_score))

#[1] "Average Silhouette Score: 0.328840676362168"
# This indicates that clusters are weakly distinct. (Perfectly-clustered elements have a score of 1, while 
# poorly-clustered elements have a score near -1. （https://search.r-project.org/CRAN/refmans/bios2mds/html/sil.score.html） 
# A value between 0.25 and 0.65 is considered weakly distinct (Lovmar et al, 2005 : doi:10.1186/1471-2164-6-35). 

# Plot silhouette scores 
plot(silhouette_scores, col = 1:max(clusters), border = NA)

# Interpretation: （https://search.r-project.org/CRAN/refmans/bios2mds/html/sil.score.html）
# Values close to +1 indicate that the data point is well matched to its own cluster; and poorly matched to neighboring clusters. 
# Values near 0 suggest the data point is on the border or in between clusters.
# Values close to -1 suggest the data point would be better placed in a neighboring cluster.
# As almost all values are positive, the vast majority of responses are in the correct cluster.

######
# Stage 3 : Visualising and testing Profiling Variables
######


# The following section will create frequency tables and plots for each cluster with the following variables.  

# Tables

  selected_variables <- c("org_years", "org_geographic_lvl", "continent",
                          "interest_diet", "interest_corp",
                          "interest_policy", "interest_inst", "interest_direct", 
                          "satisfaction_diet_advocacy", "satisfaction_corp_advocacy", 
                          "satisfaction_policy_advocacy", "satisfaction_institutional_advocacy", 
                          "satisfaction_direct_advocacy", "importance_funding_availability", 
                          "importance_talent_availability", "importance_context_appropriateness", 
                          "importance_impact_cost_effectiveness", "importance_mission_alignment", 
                          "obstacles_legal_regulatory_barriers", "obstacles_political_legal_influence", 
                          "obstacles_public_awareness_support", "obstacles_lack_of_training_skills", 
                          "obstacles_lack_of_staff", "obstacles_lack_of_funding", 
                          "resources_usefulness_financial", "resources_usefulness_grant_applications", 
                          "resources_usefulness_professional_development", 
                          "resources_usefulness_staff_well_being", "resources_usefulness_finding_talent", 
                          "resources_usefulness_research_data_access", 
                          "resources_usefulness_collaboration_networking", 
                          "resources_usefulness_professional_mentorship", 
                          "western_vs_nonwestern", "animal_type_dogcat_meat", 
                          "animal_type_companion", "animal_type_other", "animal_type_wild", 
                          "animal_type_lab", "animal_type_captive", "animal_type_aquatic_farm", 
                          "animal_type_land_farm", "advocacy_type_corporate", "advocacy_type_policy", 
                          "advocacy_type_institutional", "advocacy_type_direct_work", 
                          "advocacy_type_individual_diet", "advocacy_types")

  # Loop through each selected variable
  for (var in selected_variables) {
    # Check if the variable exists in the dataset
    if (var %in% names(data)) {
      # Create a table with the variable and the cluster variable
      table_output <- table(data[[var]], data[["cluster"]])
      
      # Print the table
      cat("\nTable for variable:", var, "\n")
      print(table_output)
    }
  }

# Define the directory where plots will be saved
plots_dir <- "C:/Users/jack_/Desktop/Documents/GitHub/International-Study-Of-Strategies-And-Needs/plots/"

# Check if the directory exists, and create it if it doesn't
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
  print(paste("Created directory:", plots_dir))
}
    
# Loop through each variable to create frequency tables and plots for each cluster
for (var in selected_variables) {
  # Create a contingency table of counts
  table_counts <- table(data[[var]], data$cluster)
  
  # Create a data frame with the counts for plotting
  df_for_counts <- as.data.frame.matrix(table_counts)
  df_for_counts$variable <- row.names(df_for_counts)
  df_for_counts <- pivot_longer(df_for_counts, cols = c('1', '2', '3'), names_to = 'cluster', values_to = 'count')
  
  # Confirm that the dataframe is in the correct format
  print(str(df_for_counts))
  
  # Check if the dataframe is created correctly
  if (is.null(df_for_counts) || nrow(df_for_counts) == 0) {
    warning(paste("Failed to create dataframe for variable:", var))
    next # Skip this iteration if dataframe creation failed
  }
  
  # Add cluster sizes for labeling
  cluster_sizes <- colSums(table_counts)
  cluster_labels <- paste("Cluster", names(cluster_sizes), "; n =", cluster_sizes)
  
  # Create the stacked bar chart with counts
  p <- ggplot(df_for_counts, aes(x = cluster, y = count, fill = variable)) +
    geom_bar(stat = "identity", position = 'stack') +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 6) +
    scale_x_discrete(labels = cluster_labels) +
    labs(title = paste("Count of", var, "across clusters"), x = "Cluster", y = "Count") +
    scale_fill_brewer(palette = "Set1", name = var) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Define the file name
  file_name <- paste0("plots/", var, "_plot.jpeg")
  
  # Save the plot as a JPEG file
  ggsave(file_name, plot = p, device = "jpeg", width = 8, height = 4, units = "in", dpi = 300)
  
  # Print the plot to the console as well
  print(p)
}


