# Load the readr package
library(readr)

# Specify the file path with double backslashes
file_path <- "C:\\Users\\HP\\OneDrive - Strathmore University\\Strathmore\\Year 4\\4.2\\Business Intelligence 2\\BIProject\\BIProject--136446\\data\\Batting data.csv"

# Read the CSV file into a data frame
Batting_data <- read_csv(file_path)

# Install renv:
if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")
# Select option 1 to restore the project from the lockfile
renv::init()