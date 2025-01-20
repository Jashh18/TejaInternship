# Given values
x <- 10500         # Number of stores
y <- 230000000     # Total weekly customers

# Calculate average customers per store per week
avg_customers_per_store <- y / x

# Classify based on average weekly customers per store
if (avg_customers_per_store > 30000) {
  category <- "High"
} else if (avg_customers_per_store >= 20000 && avg_customers_per_store <= 30000) {
  category <- "Moderate"
} else {
  category <- "Low"
}

# Print the result
message <- paste("The average weekly customers per store is:", round(avg_customers_per_store), 
                 "- Category:", category)
print(message)
