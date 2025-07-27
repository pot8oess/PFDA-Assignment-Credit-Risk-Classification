# Member 1: ENG HUI ERN TP078629
# Member 2: EUGENE TANG TING SIANG TP068630
# Member 3: HONG XIANG LIN TP077706
# Member 4: SWEETHA PRAMASIVAM TP078504

# Load necessary libraries
library(DataExplorer)
library(dplyr)  # For data manipulation
library(ggplot2)  # For visualization
library(corrplot)  # For correlation analysis

library(VIM) # Hot Deck Imputation
library(mice) # Predictive Mean Matching (PMM)
library(missForest) # Miss Forest

# Load the dataset (replace file path with your own file path)
fileUrl = "C:\\Users\\User\\OneDrive\\Desktop\\5. credit_risk_classification.csv"

# Import dataset and mark missing values as NA
df = read.csv(fileUrl, na.strings = c("", "NA"), stringsAsFactors = TRUE)
# Missing values count in each column
colSums(is.na(df))
# Visualize missing values in the dataset
plot_missing(df)

# View structure of the dataset
str(df)
summary(df)

################################################################################
# Histogram for all Continuous Variables
# Function to plot histograms
plot_histograms = function(df) {
  continuous_vars = df %>% select(where(is.numeric))  # Select numerical variables
  for (var in names(continuous_vars)) {
    # Handle missing values only for visualization (no modification of df)
    temp_data = df[[var]]
    temp_data[is.na(temp_data)] = mean(temp_data, na.rm = TRUE)
    
    # Plot histogram
    print(
      ggplot(data.frame(temp_data), aes(x = temp_data)) +
        geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
        labs(title = paste("Distribution of", var), x = var, y = "Frequency") +
        theme_minimal()
    )
  }
}

# Call the function
plot_histograms(df)


# Bar Charts for all Categorical Variables
# Function to plot bar charts for all categorical variables
plot_bar_charts = function(data) {
  categorical_vars = data %>% select(where(is.character) | where(is.factor))
  for (var in names(categorical_vars)) {
    print(
      ggplot(data, aes_string(x = var)) +
        geom_bar(fill = "green", alpha = 0.7) +
        labs(title = paste("Frequency of", var), x = var, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
}

# Call the function
plot_bar_charts(df)

################################################################################
# Visualization Function
visualize_column = function(data, column_name) {
  dev.new()  # Open a new plotting window for each graph
  if (is.numeric(data[[column_name]])) {
    print(
      ggplot(data, aes(x = .data[[column_name]])) +
        geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
        labs(title = paste("Distribution of", column_name, "After Imputation")) +
        theme_minimal()
    )
  } else {
    print(
      ggplot(data, aes(x = .data[[column_name]])) +
        geom_bar(fill = "green", alpha = 0.7) +
        labs(title = paste("Frequency of", column_name, "After Imputation")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
}

################################################################################
# Mode Imputation Function for Individual Variables
mode_imputation = function(data, column_name) {
  get_mode = function(v) unique(v[!is.na(v)])[which.max(tabulate(match(v, unique(v[!is.na(v)]))))]
  data[[column_name]][is.na(data[[column_name]])] = get_mode(data[[column_name]])
  return(data)
}

# Hot Deck Imputation Function for Individual Variables
hot_deck_imputation = function(data, column_name) {
  imputed = hotdeck(data, variable = column_name)
  data[[column_name]] = imputed[[column_name]]
  return(data)
}

# Predictive Mean Matching (PMM) Function for Individual Variables
pmm_imputation = function(data, column_name) {
  # Subset the data with the target column and predictors
  subset_data = data[, c(column_name, colnames(data)[!colnames(data) %in% column_name])]
  
  # Apply mice on the subset
  imputed_data = mice(subset_data, method = 'pmm', m = 5, seed = 123)
  
  # Update the original dataset with the imputed column
  data[[column_name]] = complete(imputed_data)[[column_name]]
  return(data)
}

# MissForest Imputation Function for Individual Variables
miss_forest_imputation = function(data, column_name) {
  # Check if the column is all missing values
  if (all(is.na(data[[column_name]]))) stop(paste("Column", column_name, "is entirely missing."))
  
  # Prepare a dataset with the target column and predictors
  temp_data = data %>% mutate(across(where(is.character), as.factor))  # Ensure factors
  colnames(temp_data)[colnames(temp_data) == column_name] = "target_column"
  
  # Apply MissForest for imputation
  imputed_data = missForest(temp_data)
  
  # Extract the imputed column and update the original dataset
  data[[column_name]] = imputed_data$ximp$target_column
  return(data)
}


################################################################################
# Apply Imputation for Each Variable
# Mode Imputation
for (var in c("purpose", "personal_status", "other_parties", "housing", "num_dependents", "own_telephone", "foreign_worker")) {
  df = mode_imputation(df, var)
  visualize_column(df, var)
}

# Hot Deck
for (var in c("checking_status", "savings_status", "employment")) {
  df = hot_deck_imputation(df, var)
  visualize_column(df, var)
}

# PMM
for (var in c("duration", "installment_commitment", "residence_since", "existing_credits")) {
  df = pmm_imputation(df, var)
  visualize_column(df, var)
}

# MissForest Imputation (will take some time)
for (var in c("credit_history", "credit_amount", "property_magnitude", "age", "other_payment_plans", "job")) {
  df = miss_forest_imputation(df, var)
  visualize_column(df, var)
}

################################################################################
# Combine redundant categories in 'credit_history'
# Replace "all paid" with "no credits/all paid" for simplification
table(df$credit_history)
df$credit_history = replace(df$credit_history, df$credit_history == "all paid", "no credits/all paid")
table(df$credit_history)

# Remove unused levels "all paid"
df$credit_history = droplevels(df$credit_history)
levels(df$credit_history)
table(df$credit_history)

################################################################################
# Round up Numerical Variables
# List of variables to round
variables_to_round = c("duration", "credit_amount", "installment_commitment", 
                       "residence_since", "age", "existing_credits", "num_dependents")

# Apply round() to each variable in the list
df[variables_to_round] = lapply(df[variables_to_round], round)

################################################################################
# Validate Final Dataset
# Validate Missing Values
colSums(is.na(df))  # Should return 0 for all columns
# Visualize missing values in the dataset
plot_missing(df)
# Summary Statistics
summary(df)

# Number of Rows After Imputation
nrow(df)

# Create a duplicate of df for analysis purposes
df_duplicate <- df
