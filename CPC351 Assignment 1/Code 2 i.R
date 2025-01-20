install.packages("ggplot2")

# Load necessary library
library(ggplot2)

# Define the data
year <- c(2021, 2021, 2022, 2022)
company <- c("Amazon", "Walmart", "Amazon", "Walmart")
revenue <- c(469.8, 567.7, 1539, 382)

# Combine into a data frame
data <- data.frame(year, company, revenue)

# Create the grouped bar chart using ggplot2
ggplot(data, aes(x = factor(year), y = revenue, fill = company)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.4) + # Thinner bars
  scale_y_continuous(limits = c(0, 1600), breaks = seq(0, 1600, by = 200)) + # Custom Y-axis
  labs(y = "Revenue (in billions)", x = "Year", title = "Amazon vs Walmart Revenue in 2021 and 2022") +
  scale_fill_manual(values = c("Amazon" = "blue", "Walmart" = "orange"), name = "Company = ") + # Custom colors and legend label
  theme_minimal() + # Clean theme
  theme(
    legend.position = "top",
    legend.position.inside = c(0.9,0.9)
  ) # Position legend
