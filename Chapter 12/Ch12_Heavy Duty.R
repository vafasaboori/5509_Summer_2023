# Clearing the Environment ----
rm(list=ls())

# Define the breakdown cost and replacement cost
breakdown_cost <- 11000
replacement_cost <- 6000

# Function to calculate the average cost per day for a given option
calculate_average_cost_per_day <- function(option) {
  num_cycles <- 10000  # Number of replacement cycles to simulate
  
  total_cost <- 0
  total_days <- 0
  
  for (i in 1:num_cycles) {
    days_since_breakdown <- 0
    
    # Simulate days until breakdown
    days_until_breakdown <- sample(c(4, 5, 6), size = 1, prob = c(0.25, 0.75, 1))
    
    # Add days until breakdown to the total days
    total_days <- total_days + days_until_breakdown
    
    # Check if preventive maintenance is required based on option
    if (option == "No Preventive Maintenance") {
      # No preventive maintenance, wait for breakdown
      total_cost <- total_cost + breakdown_cost
    } else if (option == "Replace After 4 Days") {
      # Replace motor after 4 days
      if (days_until_breakdown <= 4) {
        total_cost <- total_cost + replacement_cost
      } else {
        total_cost <- total_cost + breakdown_cost
      }
    } else if (option == "Replace After 5 Days") {
      # Replace motor after 5 days
      if (days_until_breakdown <= 5) {
        total_cost <- total_cost + replacement_cost
      } else {
        total_cost <- total_cost + breakdown_cost + replacement_cost
      }
    }
  }
  
  average_cost_per_day <- total_cost / total_days
  return(average_cost_per_day)
}


# Calculate the average cost per day for each option
no_preventive_maintenance <- calculate_average_cost_per_day("No Preventive Maintenance")
replace_after_4_days <- calculate_average_cost_per_day("Replace After 4 Days")
replace_after_5_days <- calculate_average_cost_per_day("Replace After 5 Days")

# Compare the average cost per day for each option
comparison <- data.frame(
  Option = c("No Preventive Maintenance", "Replace After 4 Days", "Replace After 5 Days"),
  Average_Cost_Per_Day = c(no_preventive_maintenance, replace_after_4_days, replace_after_5_days)
)
print(comparison)

