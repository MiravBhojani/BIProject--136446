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

