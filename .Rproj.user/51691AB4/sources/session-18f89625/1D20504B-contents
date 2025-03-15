# Load the dataset
View(healthcare_dataset)
data <- healthcare_dataset
# Install necessary packages
install.packages(c("tidyverse", "janitor", "lubridate"))
# Load the packages
library(tidyverse)  # For data manipulation and visualization
library(janitor)    # For cleaning column names and data
library(lubridate)  # For date manipulation
# View the first few rows of the dataset
head(data)
#Remove Missing Values
# Remove rows with missing values
data_clean <- na.omit(data)
# Install and load the stringr package
install.packages("stringr")
library(stringr)
# Convert names to title case
data_clean$Name <- str_to_title(str_to_lower(data_clean$Name))
# Load the dplyr package
library(dplyr)
# Remove duplicate rows
data_clean <- distinct(data_clean)
# Clean the Hospital column in the existing data frame
data_clean$Hospital <- data_clean$Hospital %>%
  # Remove "and" (case insensitive) from the hospital names
  gsub("\\band\\b", "", .) %>%
  # Convert to proper title case
  tools::toTitleCase(.) %>%
  # Trim leading/trailing spaces
  trimws() %>%
  # Replace multiple spaces with a single space
  gsub("\\s+", " ", .)
print(data_clean$Hospital)
data_clean$billing.amount.rounded <- round(data_clean$Billing.Amount, 2)
# Convert 'Age' to numeric
data_clean$Age <- as.numeric(data_clean$Age)
# Filter out unrealistic ages
data_clean <- data_clean %>% filter(Age >= 0 & Age <= 120)
# Define age groups
data_clean$AgeGroup <- cut(data_clean$Age,
                           breaks = c(0, 18, 35, 50, 65, Inf),
                           labels = c("0-17", "18-34", "35-49", "50-64", "65+"),
                           right = FALSE)
View(data)
# Summarize data by age group and medical condition
condition_counts <- data_clean %>%
  group_by(AgeGroup, Medical.Condition) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(AgeGroup, desc(Count))
# Display the summarized data
print(condition_counts)
# Visualization
# Load the ggplot2 package
library(ggplot2)
# Create the plot
ggplot(condition_counts, aes(x = AgeGroup, y = Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of Medical Conditions Across Age Groups",
       x = "Age Group",
       y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
View(data_clean)
head(data_clean)
