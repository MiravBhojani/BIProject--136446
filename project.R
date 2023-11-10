# Install and load the mlbench package
if (!requireNamespace("mlbench", quietly = TRUE)) {
  install.packages("mlbench")
}
library(mlbench)

# Load the readr package
library(readr)

# Specify the file path with double backslashes
file_path <- "C:\\Users\\HP\\OneDrive - Strathmore University\\Strathmore\\Year 4\\4.2\\Business Intelligence 2\\BIProject\\BIProject--136446\\data\\Batting data.csv"

# Read the CSV file into a data frame
Batting_data <- read_csv(file_path)

# Load the built-in dataset "Batting data" from mlbench
data("Batting data")

# Display the dimensions of the dataset
dim(`Batting data`)
