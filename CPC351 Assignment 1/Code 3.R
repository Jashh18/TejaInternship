#1st question
install.packages("readr")

library(dplyr)
library(readr)

# List all CSV files in the folder
files <- list.files(path = "C:/Users/tunte/Desktop/Amazon_Products", pattern = "*.csv", full.names = TRUE)

# Import and combine all datasets
all_data <- do.call(rbind, lapply(files, read_csv))

# Ensure that `all_data` is a data frame
all_data <- as.data.frame(all_data)

# Save combined dataset
write_csv(all_data, "Amazon_Products_All.csv")

# Count total samples
total_samples <- nrow(all_data)
cat("Total number of samples:", total_samples, "\n") #Total num = 1102938


#2nd question
install.packages("stringr")
library(stringr)

# Extract manufacturer from 'name' column
all_data$manufacturer <- str_extract(all_data$name, "^[^ ]+")

# Reorder columns to place 'manufacturer' after 'name'
all_data <- all_data %>%
  relocate(manufacturer, .after = name)


#3rd question
# Convert columns to appropriate data types, handling non-numeric symbols carefully
all_data <- all_data %>%
  mutate(
    actual_price = as.numeric(gsub("[₹,]", "", actual_price)),   # Remove '₹' and commas
    discount_price = as.numeric(gsub("[₹,]", "", discount_price)), # Remove '₹' and commas
    no_of_ratings = as.integer(gsub("[^0-9]", "", no_of_ratings)), # Remove non-numeric values
    ratings = as.numeric(ratings)  # Convert ratings directly
  )

# Remove Non-Numeric Characters: Use gsub() to strip out non-numeric characters (like currency symbols or commas) to ensure clean numeric data.
# Convert Data Types: Use as.numeric() for decimal values (like actual_price, discount_price, and ratings) and as.integer() for integer values (like no_of_ratings).
# Handle Missing Values (NA): Any non-numeric text that cannot be coerced will become NA, so check your data after conversion.


#4th question
all_data <- all_data %>%
  mutate(discount_percentage = ((actual_price - discount_price) / actual_price) * 100)


#5th question
# Group by manufacturer and find the highest discount percentage
highest_discount <- all_data %>%
  group_by(manufacturer) %>%
  summarize(max_discount = max(discount_percentage)) %>%
  arrange(desc(max_discount)) %>%
  head(1)


#6th question
categorize_manufacturer_ratings <- function(data) {
  # Filter the ratings to include only values between 0.0 and 5.0
  data <- data %>%
    filter(ratings >= 0 & ratings <= 5)
  
  # Categorize manufacturers based on rating values
  data <- data %>%
    mutate(rating_category = case_when(
      ratings > 3 ~ "High",
      ratings >= 1.5 & ratings <= 3 ~ "Medium",
      ratings < 1 ~ "Low",
      TRUE ~ "Unrated"
    ))
  
  return(data)
}
all_data <- categorize_manufacturer_ratings(all_data)


#7th question
# Scatter plot to analyze relationship
library(ggplot2)

# Load required library
library(ggplot2)

# Identify the highest and lowest priced products
highest_price_product <- all_data %>% filter(actual_price == max(actual_price, na.rm = TRUE))
lowest_price_product <- all_data %>% filter(actual_price == min(actual_price, na.rm = TRUE))

# Display results for highest and lowest priced products
cat("Highest Priced Product:\n")
print(highest_price_product)
cat("\nLowest Priced Product:\n")
print(lowest_price_product)

# Identify and display the manufacturer of the highest priced product
highest_price_manufacturer <- highest_price_product$manufacturer
cat("\nManufacturer with the highest priced product:", highest_price_manufacturer, "\n")

# Plot the relationship between Discount Percentage and Number of Ratings
ggplot(all_data, aes(x = discount_percentage, y = no_of_ratings)) +
  geom_point(color = "blue", alpha = 0.5, na.rm = TRUE) + # Only plots non-NA values
  labs(
    title = "Relationship between Discount Percentage and Number of Ratings",
    x = "Discount Percentage (%)",
    y = "Number of Ratings"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # Adjust x-axis for discount percentage
  scale_y_continuous(labels = scales::comma)            # Format y-axis with commas



#8th question
# Add a 'sales' column to calculate approximate sales
all_data <- all_data %>%
  mutate(sales = discount_price * no_of_ratings)

# Identify the product with the highest sales
highest_sales_product <- all_data %>% 
  arrange(desc(sales)) %>% 
  slice(1)

# Display the product with the highest sales
cat("Product with the Highest Sales:\n")
print(highest_sales_product)

# Display sorted data by sales in descending order
sorted_sales_data <- all_data %>% 
  arrange(desc(sales))

# View sorted sales data (optional)
head(sorted_sales_data)

# Conclusions based on findings
cat("\nConclusions:\n")
cat("1. The product with the highest sales has a substantial number of ratings, indicating high popularity.\n")
cat("2. High sales may also indicate effective pricing strategy, customer satisfaction, or strong brand reputation.\n")
cat("3. Analyzing other high-sales products could reveal trends in discount effectiveness and rating influence on sales.\n")
