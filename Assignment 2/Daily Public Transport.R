# Load the dataset
ridership_data <- read.csv("C:/Users/tunte/Desktop/ridership_headline.csv")

# View the first few rows to check the data
head(ridership_data)

# Check for missing values in the dataset
colSums(is.na(ridership_data))  # Shows number of NAs in each column

# Replace all NAs with 0
ridership_data[is.na(ridership_data)] <- 0


#question 6
# Convert the 'date' column to Date type
ridership_data$date <- as.Date(ridership_data$date)

# Extract month and year from the 'date' column
ridership_data$Month <- format(ridership_data$date, "%B")  # Full month names
ridership_data$Year <- format(ridership_data$date, "%Y")

# Load the dplyr package
library(dplyr)

# Reshape the data from wide to long format
long_data <- ridership_data %>%
  gather(key = "Service", value = "Trips", bus_rkl:rail_komuter) %>%
  group_by(Year, Month, Service) %>%
  summarise(Total_Trips = sum(Trips, na.rm = TRUE)) %>%
  ungroup()

# Ensure the months are ordered correctly (January to December)
long_data$Month <- factor(long_data$Month, levels = month.name)

# Load ggplot2 for plotting
library(ggplot2)

# Create the stacked bar plot
ggplot(long_data, aes(x = Month, y = Total_Trips, fill = Service)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Trips Aggregated by Month for Each Public Transport Service",
       x = "Month", y = "Total Number of Trips") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Title in the center
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.title.x = element_text(size = 12),  # Increase x-axis label size
    axis.title.y = element_text(size = 12),  # Increase y-axis label size
    axis.text.y = element_text(size = 10)  # Increase y-axis label size
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis in normal range


#question 7
# Convert the 'date' column to Date type
ridership_data$date <- as.Date(ridership_data$date, format = "%Y/%m/%d")

# Filter the data for the year 2023
bus_rkl_2023 <- ridership_data %>%
  filter(format(date, "%Y") == "2023") %>%
  select(date, bus_rkl)

# Check the filtered data
head(bus_rkl_2023)

# Sort the data for highest ridership days
top_10_highest_ridership <- bus_rkl_2023 %>%
  arrange(desc(bus_rkl)) %>%
  head(10)

# Sort the data for lowest ridership days
top_10_lowest_ridership <- bus_rkl_2023 %>%
  arrange(bus_rkl) %>%
  head(10)

# Print the top 10 highest ridership days
cat("Top 10 Days with Highest Ridership for Rapid Bus (KL) in 2023:\n")
print(top_10_highest_ridership)

# Print the top 10 lowest ridership days
cat("Top 10 Days with Lowest Ridership for Rapid Bus (KL) in 2023:\n")
print(top_10_lowest_ridership)



#question 8
# Convert the 'date' column to Date type
ridership_data$date <- as.Date(ridership_data$date, format = "%Y/%m/%dY")

# Filter the data for January and February 2022
bus_rpn_2022 <- ridership_data %>%
  filter(format(date, "%Y") == "2022" & format(date, "%m") %in% c("01", "02")) %>%
  select(date, bus_rpn)

# Check the filtered data
head(bus_rpn_2022)

# Add a new column to classify weekdays and weekends
bus_rpn_2022$day_type <- ifelse(weekdays(bus_rpn_2022$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Check the data with the new 'day_type' column
head(bus_rpn_2022)

# Aggregate the ridership data by 'day_type' (Weekday vs Weekend) and month
agg_ridership <- bus_rpn_2022 %>%
  mutate(month = format(date, "%B")) %>%
  group_by(month, day_type) %>%
  summarise(total_ridership = sum(bus_rpn, na.rm = TRUE))

# Check the aggregated data
agg_ridership

# Load the necessary library for plotting
library(ggplot2)
library(scales)

# Reorder the 'month' column so that January comes first
agg_ridership$month <- factor(agg_ridership$month, levels = c("January", "February"))

# Create the bar plot with the adjusted x-axis and normal y-axis range
ggplot(agg_ridership, aes(x = month, y = total_ridership, fill = day_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Ridership for Rapid Bus (Penang) - January and February 2022",
       x = "Month",
       y = "Total Ridership",
       fill = "Day Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_y_continuous(labels = comma)



#question 9
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Convert the date column to Date format
ridership_data$date <- as.Date(ridership_data$date, format = "%Y-%m-%d")

# Define the festival dates for each year (assuming first day of each festival)
festivals <- data.frame(
  festival = c("Hari Raya Aidilfitri", "Chinese New Year", "Deepavali", "Christmas"),
  date = as.Date(c("2019-06-05", "2020-01-25", "2020-11-14", "2019-12-25"))  # Adjust the festival dates for each year
)

# Function to filter data for 7 days before and after the festival
get_festival_data <- function(festival_date, data) {
  start_date <- festival_date - 7
  end_date <- festival_date + 7
  filtered_data <- data %>%
    filter(date >= start_date & date <= end_date)
  return(filtered_data)
}

# Empty list to store the festival data
festival_data_list <- list()

# Loop through each festival to get data
for (i in 1:nrow(festivals)) {
  festival <- festivals[i, ]
  
  # Filter the data for the current festival
  festival_data <- get_festival_data(festival$date, ridership_data)
  
  # Aggregate the trips for each transport service
  aggregated_data <- festival_data %>%
    select(date, rail_lrt_ampang, rail_mrt_kajang, rail_lrt_kj, rail_monorail) %>%
    group_by(date) %>%
    summarise(
      total_lrt_ampang = sum(rail_lrt_ampang, na.rm = TRUE),
      total_mrt_kajang = sum(rail_mrt_kajang, na.rm = TRUE),
      total_lrt_kj = sum(rail_lrt_kj, na.rm = TRUE),
      total_monorail = sum(rail_monorail, na.rm = TRUE)
    )
  
  # Store the aggregated data into the list with festival name as the key
  festival_data_list[[festival$festival]] <- aggregated_data
}

# Combine the festival data into one data frame
final_data <- bind_rows(festival_data_list, .id = "festival")

# Plot the data for all festivals with y-axis adjusted to normal range
ggplot(final_data, aes(x = date)) +
  geom_line(aes(y = total_lrt_ampang, color = "LRT Ampang"), size = 1) +
  geom_line(aes(y = total_mrt_kajang, color = "MRT Kajang"), size = 1) +
  geom_line(aes(y = total_lrt_kj, color = "LRT Kelana Jaya"), size = 1) +
  geom_line(aes(y = total_monorail, color = "Monorail"), size = 1) +
  facet_wrap(~festival, scales = "free_x") +
  labs(title = "Ridership Trends during Festive Seasons (2019-2023)",
       x = "Date",
       y = "Total Ridership",
       color = "Transport Service") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)  # Ensures y-axis labels are in normal range (comma-separated values)


#question 10
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Convert the date column to Date format (yyyy-mm-dd)
ridership_data$date <- as.Date(ridership_data$date, format = "%Y-%m-%d")

# Filter data for relevant transport lines: LRT Ampang, LRT Kelana Jaya, and Monorail
ridership_data <- ridership_data %>%
  select(date, rail_lrt_ampang, rail_lrt_kj, rail_monorail) %>%
  filter(!is.na(rail_lrt_ampang) & !is.na(rail_lrt_kj) & !is.na(rail_monorail))

# Define the MCO period (March 18, 2020)
mco_start_date <- as.Date("2020-03-18")

# Create two subsets: before and after MCO
before_mco <- ridership_data %>% filter(date < mco_start_date)
after_mco <- ridership_data %>% filter(date >= mco_start_date)

# Aggregate the data by month (for each period: before and after MCO)
before_mco_monthly <- before_mco %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    lrt_ampang_total = sum(rail_lrt_ampang, na.rm = TRUE),
    lrt_kj_total = sum(rail_lrt_kj, na.rm = TRUE),
    monorail_total = sum(rail_monorail, na.rm = TRUE)
  )

after_mco_monthly <- after_mco %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    lrt_ampang_total = sum(rail_lrt_ampang, na.rm = TRUE),
    lrt_kj_total = sum(rail_lrt_kj, na.rm = TRUE),
    monorail_total = sum(rail_monorail, na.rm = TRUE)
  )

# Combine the data from both periods for plotting
combined_data <- bind_rows(
  before_mco_monthly %>% mutate(period = "Before MCO"),
  after_mco_monthly %>% mutate(period = "After MCO")
)

# Reorder the factor levels for the "period" column to ensure "Before MCO" comes first
combined_data$period <- factor(combined_data$period, levels = c("Before MCO", "After MCO"))

# Create a sequence of months from 2019-01-01 to 2024-12-01
all_months <- seq(from = as.Date("2019-01-01"), to = as.Date("2024-12-01"), by = "month")

# Ensure combined_data includes all months from 2019 to 2024 for proper plotting
combined_data <- combined_data %>%
  complete(month = all_months, fill = list(lrt_ampang_total = 0, lrt_kj_total = 0, monorail_total = 0))

# Remove rows with NA in the 'period' column
combined_data <- combined_data %>% filter(!is.na(period))

# Plot the data with ggplot2
ggplot(combined_data, aes(x = month)) +
  geom_line(aes(y = lrt_ampang_total, color = "LRT Ampang"), size = 1) +
  geom_line(aes(y = lrt_kj_total, color = "LRT Kelana Jaya"), size = 1) +
  geom_line(aes(y = monorail_total, color = "Monorail"), size = 1) +
  facet_wrap(~period, scales = "free_x") +
  labs(
    title = "Trends in Ridership for LRT Ampang, LRT Kelana Jaya, and Monorail (Before and After MCO)",
    x = "Month",
    y = "Total Ridership",
    color = "Transport Service"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma) +  # Ensures y-axis labels are in normal range (comma-separated values)
  scale_x_date(
    breaks = seq(from = as.Date("2019-01-01"), to = as.Date("2024-12-01"), by = "1 year"),
    labels = scales::date_format("%Y")  # Format the x-axis labels to display years
  )



