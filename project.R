# Load the readr package
library(readr)

# Specify the file path with double backslashes
file_path <- "C:\\Users\\HP\\OneDrive - Strathmore University\\Strathmore\\Year 4\\4.2\\Business Intelligence 2\\BIProject\\BIProject--136446\\data\\Batting data.csv"

# Read the CSV file into a data frame
Batting_data <- read_csv(file_path)

# Display the dimensions of the dataset
dim(Batting_data)
sapply(Batting_data, class)
# Assuming "Righty/Lefty" is a categorical variable in your dataset
righty_lefty_frequency <- table(Batting_data$`Righty or Lefty`)

# Create a data frame with frequency and percentage
frequency_table <- cbind(frequency = righty_lefty_frequency,
                         percentage = prop.table(righty_lefty_frequency) * 100)

# Print the frequency table
print(frequency_table)

# Mean
mean_value <- mean(Batting_data$Runs)

# Median
median_value <- median(Batting_data$Runs)

# Mode
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

mode_value <- get_mode(Batting_data$Runs)

# Display the results
cat("Mean: ", mean_value, "\n")
cat("Median: ", median_value, "\n")
cat("Mode: ", mode_value, "\n")
