# Load each dataset individually
vehicles_2019 <- read.csv("C:/Users/tunte/Desktop/cars_2019.csv")
vehicles_2020 <- read.csv("C:/Users/tunte/Desktop/cars_2020.csv")
vehicles_2021 <- read.csv("C:/Users/tunte/Desktop/cars_2021.csv")
vehicles_2022 <- read.csv("C:/Users/tunte/Desktop/cars_2022.csv")
vehicles_2023 <- read.csv("C:/Users/tunte/Desktop/cars_2023.csv")
vehicles_2024 <- read.csv("C:/Users/tunte/Desktop/cars_2024.csv")


#question 1
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)  # For handling date operations

# Step 2: Add a "Year" column to each dataset based on the `date_reg`
add_year_column <- function(data) {
  data %>%
    mutate(
      Year = year(ymd(date_reg))  # Extract year from `date_reg`
    )
}

vehicles_2019 <- add_year_column(vehicles_2019)
vehicles_2020 <- add_year_column(vehicles_2020)
vehicles_2021 <- add_year_column(vehicles_2021)
vehicles_2022 <- add_year_column(vehicles_2022)
vehicles_2023 <- add_year_column(vehicles_2023)
vehicles_2024 <- add_year_column(vehicles_2024)

# Step 3: Combine the datasets for the years 2019–2024
data_list <- list(vehicles_2019, vehicles_2020, vehicles_2021, vehicles_2022, vehicles_2023, vehicles_2024)
combined_data <- bind_rows(data_list)

# Create the visualization with modifications
ggplot(aggregated_data, aes(x = Year, y = Vehicles_Registered, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Number of Vehicles Registered by State JPJ Office (2019-2024)",
    x = "Year",
    y = "Number of Vehicles Registered",
    fill = "State"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(
    breaks = 2019:2024  # Ensure all years from 2019 to 2024 are shown on the x-axis
  ) +
  scale_y_continuous(
    labels = scales::comma  # Format y-axis labels in a normal range with commas
  )


#question 2
# Filter data for Pulau Pinang and years 2019, 2020, 2021
filtered_data <- combined_data %>%
  filter(state == "Pulau Pinang" & Year %in% c(2019, 2020, 2021))

# Summarize the data to get total registrations by brand and year
brand_data <- filtered_data %>%
  group_by(maker, Year) %>%
  summarise(Vehicles_Registered = n(), .groups = "drop")

# Find the top five brands for each year
top_brands <- brand_data %>%
  group_by(Year) %>%
  slice_max(order_by = Vehicles_Registered, n = 5)

# Create a visualization
ggplot(top_brands, aes(x = reorder(maker, -Vehicles_Registered), y = Vehicles_Registered, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Top Five Brands of Registered Vehicles in Pulau Pinang (2019-2021)",
    x = "Brand",
    y = "Number of Vehicles Registered",
    fill = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#question 3
# Define the regions
northern_region <- c("Perlis", "Kedah", "Pulau Pinang", "Perak")
central_region <- c("Selangor", "Kuala Lumpur", "Putrajaya", "Negeri Sembilan")
east_coast_region <- c("Pahang", "Terengganu", "Kelantan")
southern_region <- c("Melaka", "Johor")
east_malaysia <- c("Sarawak", "Sabah", "Labuan")

# Filter and summarize data for each region
favorite_color <- function(region) {
  filtered_data <- combined_data %>%
    filter(state %in% region) %>%
    group_by(colour) %>%
    summarise(Vehicles_Registered = n(), .groups = "drop") %>%
    arrange(desc(Vehicles_Registered)) %>%
    slice(1)  # Get the most frequent color
  
  return(filtered_data)
}

# Apply function to each region
northern_color <- favorite_color(northern_region)
central_color <- favorite_color(central_region)
east_coast_color <- favorite_color(east_coast_region)
southern_color <- favorite_color(southern_region)
east_malaysia_color <- favorite_color(east_malaysia)

# Print the results
northern_color
central_color
east_coast_color
southern_color
east_malaysia_color



#question 4
# Load necessary package
library(scales)

# Filter data for the years 2019 to 2024
fuel_data <- combined_data %>%
  filter(Year %in% 2019:2024) %>%
  group_by(Year, fuel) %>%
  summarise(Vehicles_Registered = n(), .groups = "drop")

# Create the visualization with adjusted x-axis labels and formatted y-axis
ggplot(fuel_data, aes(x = as.factor(Year), y = Vehicles_Registered, fill = fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(breaks = 2019:2024) +  # Ensure all years from 2019 to 2024 appear on the x-axis
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas for readability
  theme_minimal() +
  labs(
    title = "Trends in Vehicle Fuel Types (2019-2024)",
    x = "Year",
    y = "Number of Vehicles Registered",
    fill = "Fuel Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text()  # Ensure the y-axis labels are in a readable format
  )



#question 5
# Load necessary package
library(scales)

# Filter the data for the years 2019 to 2024
pre_mco_data <- combined_data %>% filter(Year == 2019)  # Pre-pandemic (2019)
post_mco_data <- combined_data %>% filter(Year >= 2020)  # Post-pandemic (2020-2024)

# Combine both data sets into a single dataset for easier comparison
combined_data_with_mco <- bind_rows(
  mutate(pre_mco_data, Period = "Pre-pandemic"),
  mutate(post_mco_data, Period = "Post-pandemic")
)

# Group by Year and Period and summarize vehicle registrations
vehicle_trends <- combined_data_with_mco %>%
  group_by(Year, Period) %>%
  summarise(Vehicles_Registered = n(), .groups = "drop")

# Create a line plot to visualize the trend
ggplot(vehicle_trends, aes(x = Year, y = Vehicles_Registered, color = Period, group = Period)) +
  geom_line(linewidth = 1.5) +  # Use 'linewidth' instead of 'size'
  geom_point(size = 3) +
  scale_color_manual(values = c("Pre-pandemic" = "blue", "Post-pandemic" = "red")) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  labs(
    title = "Vehicle Registration Trends Before and After the COVID-19 Pandemic",
    x = "Year",
    y = "Number of Vehicles Registered",
    color = "Period"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )









