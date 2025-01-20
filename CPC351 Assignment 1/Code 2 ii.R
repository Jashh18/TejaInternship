
# Assigned each factors a score on the scale of 1-10
margins <- 8
price_sensitivity <- 7
delivery_preferences <- 6

# Calculate the total score
total_score <- margins + price_sensitivity + delivery_preferences

# Classify profitability
if (total_score > 25) {
  profitability <- "High Profitability"
} else if (total_score >= 15 && total_score <= 25) {
  profitability <- "Moderate Profitability"
} else {
  profitability <- "Low Profitability"
}

# Print the result
print(profitability)