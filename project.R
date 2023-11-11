# Load the readr package
library(readr)

# Specify the file path with double backslashes
file_path <- "C:\\Users\\HP\\OneDrive - Strathmore University\\Strathmore\\Year 4\\4.2\\Business Intelligence 2\\BIProject\\BIProject--136446\\data\\Batting data.csv"

# Read the CSV file into a data frame
Batting_data <- read_csv(file_path)

# Display the dimensions of the original dataset
original_dim <- dim(Batting_data)
cat("Original Dataset Dimensions:", original_dim[1], "rows and", original_dim[2], "columns\n")

# Remove rows with missing values
Batting_data_no_missing <- Batting_data[complete.cases(Batting_data), ]

# Display the dimensions of the dataset without missing values
no_missing_dim <- dim(Batting_data_no_missing)
cat("Dataset Dimensions after Removing Rows with Missing Values:", no_missing_dim[1], "rows and", no_missing_dim[2], "columns\n")

# Now you can use Batting_data_no_missing for subsequent analysis

# Assuming "Righty or Lefty" is a categorical variable in your dataset
righty_lefty_frequency <- table(Batting_data_no_missing$`Righty or Lefty`)  # Corrected column name

# Create a data frame with frequency and percentage
frequency_table <- cbind(frequency = righty_lefty_frequency,
                         percentage = prop.table(righty_lefty_frequency) * 100)

# Print the frequency table
print(frequency_table)

# Mean
mean_value <- mean(Batting_data_no_missing$Runs)

# Median
median_value <- median(Batting_data_no_missing$Runs)

# Mode
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

mode_value <- get_mode(Batting_data_no_missing$Runs)

# Display the results
cat("Mean: ", mean_value, "\n")
cat("Median: ", median_value, "\n")
cat("Mode: ", mode_value, "\n")
summary(Batting_data_no_missing)

# Standard deviation for columns 3 to 12
sapply(Batting_data_no_missing[, 3:12], sd)
# Install and load the e1071 package
if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

# Assuming "Runs" is a numeric variable in your Batting data
# Adjust the column index accordingly if needed
kurtosis_runs <- kurtosis(Batting_data_no_missing$Runs, type = 2)

# Display the kurtosis for the "Runs" variable
print("Kurtosis for Runs:")
print(kurtosis_runs)

skewness_runs <- skewness(Batting_data_no_missing$Runs)

# Display the skewness for the "Runs" variable
print("Skewness for Runs:")
print(skewness_runs)
# Adjust the column index or names based on your dataset structure
numeric_vars <- Batting_data_no_missing[, sapply(Batting_data_no_missing, is.numeric)]

# Exclude the categorical variable "Righty or Lefty" from the numeric variables
numeric_vars <- numeric_vars[, !colnames(numeric_vars) %in% c("Righty or Lefty")]

# Calculate covariance matrix
covariance_matrix <- cov(numeric_vars)

# Display the covariance matrix
print("Covariance Matrix:")
print(covariance_matrix)

numeric_vars <- Batting_data_no_missing[, sapply(Batting_data_no_missing, is.numeric)]

# Exclude the categorical variable "Righty or Lefty" from the numeric variables
numeric_vars <- numeric_vars[, !colnames(numeric_vars) %in% c("Righty or Lefty")]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_vars)

# Display the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

model <- aov(Runs ~ `Righty or Lefty`, data = Batting_data_no_missing)

# Display the ANOVA table
print("ANOVA Table:")
print(summary(model))