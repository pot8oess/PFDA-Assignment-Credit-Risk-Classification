library(DataExplorer)
library(dplyr)
library(ggplot2)
library(ltm) # Correlation Analysis
library(ggmosaic) # Mosaic Plot
library(caTools) # Split
library(caret) # Confusion Matrix
library(party) # Decision Tree

# Please ensure that "Group AD - Data Cleaning.R" has been run before running this file

# Please run this code below before analysis
df <- df_duplicate


# Analysis 3.1.1

# Show summary statistics of duration
summary(df$duration)

# Count number of values for each credit class
table(df$class)

# Show duration statistics for each credit class
df %>% 
  group_by(class) %>% 
  summarise(mean_duration = mean(duration),
            median_duration = median(duration),
            sd_duration = sd(duration),
            count = n())

# Generate Boxplot
ggplot(df, aes(x = class, y = duration, fill = class)) +
  geom_boxplot() +
  labs(title = "Loan Duration by Credit Class", x = "Credit Class", y = "Loan Duration (Months)") +
  theme(plot.title = element_text(hjust = 0.5)) 


# Analysis 3.1.2

# Generate Histogram with overlapping Density Plot
ggplot(df, aes(x = duration, fill = class)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_density(aes(y = after_stat(count)), color = "black", adjust = 1.5, alpha = 0.7) +
  labs(title = "Distribution of Loan Duration by Credit Class", 
       x = "Loan Duration (Months)", 
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# position = "identity" makes the good and bad charts overlap instead of stacking on top of one another
# element_text(hjust = 0.5) aligns the title to center


# Analysis 3.1.3

# Conduct T-test

t_test = t.test(duration ~ class, data = df, alternative = "two.sided", var.equal = FALSE)
t_test

# Compare the means of duration grouped by class
# Two sided test: used when testing equal / not equal and has 2 rejection regions
# Variance of 2 variables are assumed to be not equal (Welch Test)


# Analysis 3.1.4

# Calculate Point-biserial Correlation
correlation = biserial.cor(df$duration, ifelse(df$class == "good", 1, 0), level = 2)
correlation

# Convert class column into binary form (good = 1, bad = 0)
# level = 2 specifies that "good" is considered our "higher" category
# we choose "good" as the higher category because it is the more desirable outcome
# if we choose "bad" instead, the correlation would be opposite


# Analysis 3.1.5

# Analyse credit class vs employment for short/long loan duration using mosaic plot

# Assume that duration that is less than or equal to first quartile (Q1 = 25%) is short loan duration
# Assume that duration that is more than third quartile (Q3 = 75%) is long loan duration

# Why do short loan durations have bad credit class? (employment)
ggplot(data = df[df$duration <= quantile(df$duration, 0.25), ]) +
  geom_mosaic(aes(x = product(class), fill = employment)) +
  labs(title = "Loan Purpose Distribution for Short Loan Durations by Credit Class",
       x = "Credit Class", y = "Employment (years)")

# Why do long loan durations have bad credit class? (employment)
ggplot(data = df[df$duration > quantile(df$duration, 0.75), ]) +
  geom_mosaic(aes(x = product(class), fill = employment)) +
  labs(title = "Loan Purpose Distribution for Long Loan Durations by Credit Class",
       x = "Credit Class", y = "Employment (years)")

# Chi-Squared Test

# First create a contingency table between employment and class
table = table(df$employment, df$class)
chisq <- chisq.test(table)
chisq


# Analysis 3.1.6

# Logistic Regression

# Replace class column with 0 and 1
df$class <- factor(df$class, levels = c("bad","good"), labels = c("0","1"))

# Set seed value for reproducibility
set.seed(123)

# Split the dataset into training set and testing set (0.8 means splitting data to 80% and 20%)
split = sample.split(df$class, SplitRatio = 0.8) # 80% training, 20% test

# Training set is to train the model on patterns of the data
training_set = subset(df, split == TRUE)

# Test set is for the model to predict on unseen data
test_set = subset(df, split == FALSE)

dim(df)
dim(training_set) # 4800 rows (80%)
dim(test_set) # 1200 rows (20%)

# Build logistic regression (only duration)
model1 = glm(class ~ duration, training_set, family = binomial)

# class ~ duration means we are only using duration as the independent variable for class
# use training set as data to train the model
# family = binomial because class only has 2 levels (good - 1, bad - 0)

summary(model1)

# Predict on test set
test_prediction_probability = predict(model1, type="response", test_set[,-21])

# test_set[,21] is to remove the dependent variable (class, column 21) from test set before making prediction

test_prediction_class = ifelse(test_prediction_probability > 0.5, 1, 0)

# 0.5 is the threshold for good / bad 
# if probability more than 0.5, classify as good

confusionMatrix(table(test_prediction_class, test_set$class))


# Using all independent variables for better prediction

model2 = glm(class ~ ., training_set, family = binomial)

# . means that we are using all independent variables

test_prediction_probability_2 = predict(model2, type="response", test_set[,-21])

test_prediction_class_2 = ifelse(test_prediction_probability_2 > 0.5, 1, 0)

confusionMatrix(table(test_prediction_class_2, test_set$class))


# Analysis 3.1.7

# Create Decision Tree using duration only
tree <- ctree(class ~ duration, df)
plot(tree)


# Please run this code below before moving on to the next analysis
df <- df_duplicate
