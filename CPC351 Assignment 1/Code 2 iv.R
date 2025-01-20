# Function to calculate the estimated number of Amazon Prime members
estimate_prime_members <- function(target_year) {
  # Known data points
  year1 <- 2011
  members1 <- 4e6   # 4 million members in 2011
  year2 <- 2021
  members2 <- 200e6 # 200 million members in 2021
  
  # Calculate the annual growth rate
  growth_rate <- (members2 - members1) / (year2 - year1)
  
  # Estimate the number of members for the target year
  estimated_members <- members1 + (target_year - year1) * growth_rate
  return(estimated_members)
}

# Test the function with 2018 as the target year
target_year <- 2018
estimated_members_2018 <- estimate_prime_members(target_year)
cat("Estimated Amazon Prime members in", target_year, ":", round(estimated_members_2018), "members\n")
