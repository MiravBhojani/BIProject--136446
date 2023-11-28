# File path to the dataset
file_path <- "C:/Users/HP/OneDrive - Strathmore University/Strathmore/Year 4/4.2/Business Intelligence 2/BIProject/BIProject-136446/data/batting.csv"

# Read the data
batting_data <- read.csv(file_path)

# Display column names to ensure correctness
column_names <- names(batting_data)
print(column_names)

# Measure of Frequency (Counts of Categorical Variables)
frequency_count <- table(batting_data$Out)
print(frequency_count)

# Load necessary libraries (if not already loaded)
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}
library(e1071)
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}


# Define numerical columns
numerical_columns <- c("Runs", "Balls", "Fours", "Sixes")

# Display column names to ensure correctness
cat("Column Names:", "\n")
cat(names(batting_data), "\n\n")

# Measures of Central Tendency for Numerical Columns
cat("Measures of Central Tendency for Numerical Columns:", "\n")
for (column in numerical_columns) {
  mean_value <- mean(batting_data[[column]], na.rm = TRUE)
  median_value <- median(batting_data[[column]], na.rm = TRUE)
  mode_value <- get_mode(batting_data[[column]])
  
  cat(column, ":\n")
  cat("Mean: ", mean_value, "\n")
  cat("Median: ", median_value, "\n")
  cat("Mode: ", mode_value, "\n\n")
}

# Measures of Dispersion (Standard Deviation)
cat("Measures of Dispersion (Standard Deviation) for Numerical Columns:", "\n")
for (column in numerical_columns) {
  sd_value <- sd(batting_data[[column]], na.rm = TRUE)
  
  cat(column, ":\n")
  cat("Standard Deviation: ", sd_value, "\n\n")
}

# Measures of Shape (Skewness and Kurtosis)
cat("Measures of Shape (Skewness and Kurtosis) for Numerical Columns:", "\n")
for (column in numerical_columns) {
  skewness_value <- skewness(batting_data[[column]], na.rm = TRUE)
  kurtosis_value <- kurtosis(batting_data[[column]], na.rm = TRUE)
  
  cat(column, ":\n")
  cat("Skewness: ", skewness_value, "\n")
  cat("Kurtosis: ", kurtosis_value, "\n\n")
}

# Measures of Relationship (Correlation Matrix for Numerical Columns)
cat("Measures of Relationship (Correlation Matrix) for Numerical Columns:", "\n")
correlation_matrix <- cor(batting_data[, numerical_columns], use = "pairwise.complete.obs")
print(correlation_matrix)

# Load necessary libraries (if not already loaded)
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
library(stats)

model_anova <- aov(Runs ~ Out, data = batting_data)
summary(model_anova)

# Load necessary libraries for plotting (if not already loaded)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Assuming 'Runs' is one of the numerical variables in your dataset
# Replace 'Runs' with the actual column name you want to plot
ggplot(batting_data, aes(x = Runs)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Runs", x = "Runs", y = "Frequency")

ggplot(batting_data, aes(x = Runs)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Runs", y = "Runs")

ggplot(batting_data, aes(x = Runs)) +
  geom_density(fill = "salmon", color = "black") +
  labs(title = "Density Plot of Runs", x = "Runs")

# Load necessary libraries (if not already loaded)
if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("GGally")
}
library(GGally)

# Define numerical columns for multivariate analysis
numerical_columns <- c("Runs", "Balls", "Fours", "Sixes")

# Select the numerical columns for the scatterplot matrix
selected_data <- batting_data[numerical_columns]

# Create a scatterplot matrix
ggpairs(selected_data)

# Check for missing data
missing_data_count <- colSums(is.na(batting_data))
print(missing_data_count)

# Impute missing values for numerical columns with mean
for (column in numerical_columns) {
  if (is.numeric(batting_data[[column]]) && sum(is.na(batting_data[[column]])) > 0) {
    batting_data[[column]][is.na(batting_data[[column]])] <- mean(batting_data[[column]], na.rm = TRUE)
  }
}

# Impute missing values for categorical columns with mode
categorical_columns <- setdiff(names(batting_data), numerical_columns)
for (column in categorical_columns) {
  if (sum(is.na(batting_data[[column]])) > 0) {
    batting_data[[column]][is.na(batting_data[[column]])] <- get_mode(batting_data[[column]])
  }
}
batting_data$Runs <- log(batting_data$Runs + 1) # Adding 1 to avoid log of zero

batting_data$Balls <- scale(batting_data$Balls)

batting_data <- cbind(batting_data, model.matrix(~Out - 1, data = batting_data))

# Summary of the transformed data
summary(batting_data)

# Load necessary libraries (if not already loaded)
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (e.g., 80% training, 20% testing)
train_indices <- createDataPartition(batting_data$Runs, p = 0.8, list = FALSE)
train_data <- batting_data[train_indices, ]
test_data <- batting_data[-train_indices, ]

# Check the dimensions of the resulting sets
dim(train_data)
dim(test_data)

# Load necessary libraries (if not already loaded)
if (!requireNamespace("boot", quietly = TRUE)) {
  install.packages("boot")
}
library(boot)

# Set a seed for reproducibility
set.seed(123)

# Create a function for bootstrapping with a classification model
your_classification_model <- function(data) {
}

# Create a bootstrapping function
bootstrap_func <- function(data, indices) {
  sample_data <- data[indices, ]
  return(your_classification_model(sample_data))
}

# Run the bootstrap procedure
results <- boot(data = as.matrix(batting_data), statistic = bootstrap_func, R = 1000)

# Access and print specific results (for example, the first 10)
print(results$t[1:1000, ])  # Adjust the range as needed

# Load necessary libraries (if not already loaded)
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Set the number of folds for cross-validation
num_folds <- 10  # You can adjust this number as needed

# Set up the control parameters for cross-validation
train_control <- trainControl(method = "cv", number = num_folds)

# Remove duplicated columns in the dataset
batting_data <- batting_data[ , !duplicated(names(batting_data))]

# Train the classification model using k-fold cross-validation
model <- train(
  Runs ~ .,  # Replace 'Runs' with your target variable
  data = batting_data,
  method = "rf",  # Replace "rf" with the method you're using, e.g., "glm", "svm", etc.
  trControl = train_control
)
# Access the cross-validation results
print(model)
# Load necessary libraries (if not already loaded)
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Set the number of folds for cross-validation
num_folds <- 1000

# Set up the control parameters for cross-validation
train_control <- trainControl(method = "cv", number = num_folds)

# Train the classification model using k-fold cross-validation
model <- train(
  Runs ~ .,  # Replace 'Runs' with your target variable
  data = batting_data,
  method = "rf",  # Replace "rf" with the method you're using, e.g., "glm", "svm", etc.
  trControl = train_control
)

# Access the cross-validation results
print(model)
# Load necessary libraries (if not already loaded)
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Set the number of folds for cross-validation
num_folds <- 10

# Set up the control parameters for cross-validation
train_control <- trainControl(method = "cv", number = num_folds)

# Define the search grid
grid <- expand.grid(
  mtry = seq(2, 10, by = 2),  # Example grid for 'mtry' parameter
  nodesize = seq(1, 5, by = 1)  # Example grid for 'nodesize' parameter
)

# Train multiple classification models
model1 <- train(
  Runs ~ .,  # Replace 'Runs' with your target variable
  data = batting_data,
  method = "rf",  # Replace "rf" with the method you're using, e.g., "glm", "svm", etc.
  trControl = train_control,
  tuneGrid = grid  # Apply the defined grid
)

model2 <- train(
  Runs ~ .,
  data = batting_data,
  method = "glm",  # Example of another method
  trControl = train_control
)

# Create a list of models
models_list <- list(model1, model2)

# Compare models using resamples
comparison <- resamples(models_list)

# Print the summary of model comparison
print(summary(comparison))

# Train the classification model using boosting (AdaBoost with Random Forest)
model <- train(
  formula = as.formula(paste(target_variable, "~ .")),  
  data = batting_data,
  method = "adaboost",
  trControl = train_control,
  tuneLength = 5,  # Adjust this parameter for tuning
  metric = "RMSE"  # Adjust the metric based on your evaluation criterion
)

# Access the trained model
print(model)

# Load necessary libraries
library(plumber)

# Define and run the plumber API
plumber_api <- plumb("path_to_your_script.R")  # Replace with your script path
plumber_api$run(port = 8000)  # Choose any available port