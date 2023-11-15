# Load necessary libraries
library(readr)
library(e1071)
library(corrplot)

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

# Assuming "Righty or Lefty" is a categorical variable in your dataset
righty_lefty_frequency <- table(Batting_data_no_missing$`Righty or Lefty`)  # Corrected column name

# Create a data frame with frequency and percentage
frequency_table <- cbind(frequency = righty_lefty_frequency,
                         percentage = prop.table(righty_lefty_frequency) * 100)

# Print the frequency table
print(frequency_table)

# Mean, Median, Mode
mean_value <- mean(Batting_data_no_missing$Runs)
median_value <- median(Batting_data_no_missing$Runs)
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}
mode_value <- get_mode(Batting_data_no_missing$Runs)

# Display the results
cat("Mean: ", mean_value, "\n")
cat("Median: ", median_value, "\n")
cat("Mode: ", mode_value, "\n")

# Standard deviation for columns 3 to 12
sapply(Batting_data_no_missing[, 3:12], sd)

# Kurtosis and skewness for Runs
kurtosis_runs <- kurtosis(Batting_data_no_missing$Runs, type = 2)
skewness_runs <- skewness(Batting_data_no_missing$Runs)

# Display kurtosis and skewness for the "Runs" variable
cat("Kurtosis for Runs:", kurtosis_runs, "\n")
cat("Skewness for Runs:", skewness_runs, "\n")

# Calculate covariance matrix
numeric_vars <- Batting_data_no_missing[, sapply(Batting_data_no_missing, is.numeric)]
numeric_vars <- numeric_vars[, !colnames(numeric_vars) %in% c("Righty or Lefty")]
covariance_matrix <- cov(numeric_vars)

# Display the covariance matrix
print("Covariance Matrix:")
print(covariance_matrix)

# Calculate correlation matrix
correlation_matrix <- cor(numeric_vars)

# Display the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# ANOVA
model <- aov(Runs ~ `Righty or Lefty`, data = Batting_data_no_missing)
print("ANOVA Table:")
print(summary(model))

# Check for homogeneity of variances
bartlett_test <- bartlett.test(Runs ~ `Righty or Lefty`, data = Batting_data_no_missing)
print("Bartlett Test for Homogeneity of Variances:")
print(bartlett_test)

# If Bartlett's test is significant, consider Welch's ANOVA
welch_model <- aov(Runs ~ `Righty or Lefty`, data = Batting_data_no_missing, var.equal = FALSE)
print("Welch's ANOVA Table:")
print(summary(welch_model))

# Univariate Visualizations
hist(Batting_data_no_missing$Runs, main = "Histogram of Runs", xlab = "Runs", col = "skyblue", border = "black")

boxplot(Batting_data_no_missing$Runs, main = "Boxplot of Runs", ylab = "Runs", col = "lightgreen", border = "black")

plot(density(Batting_data_no_missing$Runs), main = "Density Plot of Runs", xlab = "Runs", col = "salmon", lwd = 2)

# Scatterplot matrix for numeric variables
pairs(numeric_vars)

# Correlation matrix plot
corrplot(correlation_matrix, method = "circle", tl.cex = 0.7, title = "Correlation Matrix")

# Load necessary libraries (if not already loaded)
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE)
}
library(ggplot2)

# Scatterplot matrix for numeric variables using ggplot2
scatterplot_matrix <- ggplot(Batting_data_no_missing, aes(x = Runs, y = `Performance Runs 5`)) +
  geom_point() +
  labs(title = "Scatterplot Matrix", x = "Runs", y = "Performance Runs 5")

print(scatterplot_matrix)

# Heatmap for the correlation matrix
corr_matrix <- cor(numeric_vars)
heatmap_plot <- ggplot(data = reshape2::melt(corr_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

print(heatmap_plot)

# Confirmation of the presence of missing values
missing_values <- any(is.na(Batting_data))
cat("Missing Values Present: ", missing_values, "\n")

# Remove rows with missing values
Batting_data_no_missing <- Batting_data[complete.cases(Batting_data), ]

# Display the dimensions of the dataset without missing values
no_missing_dim <- dim(Batting_data_no_missing)
cat("Dataset Dimensions after Removing Rows with Missing Values:", no_missing_dim[1], "rows and", no_missing_dim[2], "columns\n")

# Impute missing values in the "Runs" column with the mean of non-missing values
mean_runs <- mean(Batting_data_no_missing$Runs, na.rm = TRUE)
Batting_data_no_missing$Runs[is.na(Batting_data_no_missing$Runs)] <- mean_runs

# Scale numeric variables (you can choose a different transformation method if needed)
numeric_vars <- Batting_data_no_missing[, sapply(Batting_data_no_missing, is.numeric)]
scaled_data <- as.data.frame(scale(numeric_vars))

# Univariate plot (histogram for Runs variable)
ggplot(Batting_data_no_missing, aes(x = Runs)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Runs", x = "Runs", y = "Frequency")

# Multivariate plot (scatter plot for Runs and Bowls variables)
ggplot(Batting_data_no_missing, aes(x = Runs, y = Bowls)) +
  geom_point(color = "green") +
  labs(title = "Scatter Plot of Runs and Bowls", x = "Runs", y = "Bowls")

# Install and load the caret package
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Set the seed for reproducibility
set.seed(123)

# Create an index for data partitioning
index <- createDataPartition(Batting_data_no_missing$Runs, p = 0.8, list = FALSE)

# Split the data into training and testing sets
train_data <- Batting_data_no_missing[index, ]
test_data <- Batting_data_no_missing[-index, ]


# Bootstrapping
set.seed(123)  # Set seed for reproducibility

# Number of bootstrap samples
num_bootstraps <- 1000

# Create an empty vector to store bootstrap sample means
bootstrap_means <- numeric(num_bootstraps)

# Perform bootstrapping
for (i in 1:num_bootstraps) {
  # Sample with replacement from the training data
  bootstrap_sample <- sample(train_data$Runs, replace = TRUE)
  
  # Calculate the mean of the bootstrap sample
  bootstrap_means[i] <- mean(bootstrap_sample)
}

# Display bootstrap results
cat("Bootstrap Sample Means:", bootstrap_means, "\n")

# Plot the distribution of bootstrap sample means
hist(bootstrap_means, main = "Distribution of Bootstrap Sample Means", xlab = "Mean", col = "lightblue", border = "black")

# Calculate confidence interval for the mean
confidence_interval <- quantile(bootstrap_means, c(0.025, 0.975))
cat("95% Confidence Interval for the Mean:", confidence_interval, "\n")


