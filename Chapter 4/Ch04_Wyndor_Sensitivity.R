# Clearing the Environment ----
rm(list=ls())

# Import lpSolve package ----
library(lpSolve)

# Original Problem ----
# Algebraic Model
# Max P = 300D + 500W (Objective Function)
# Subject to (Constraints)
# 1D + 0W <= 4
# 0D + 2W <= 12
# 3D + 2W <= 18
# D >= 0
# W >= 0

# Set coefficients of the objective function
f.obj <- c(300, 500)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(1, 0,
                  0, 2,
                  3, 2), nrow = 3, byrow = TRUE)

# nrow: the desired number of rows.
# byrow: If FALSE the matrix is filled by columns, otherwise the filled by rows.

# Set inequality signs
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side parameters
f.par <- c(4,
           12,
           18)

# Final value (p)
lp_result <- lp("max", f.obj, f.con, f.dir, f.par)

# Final value (p)
lp_result

# Variables final values
lp_result$solution

# Q1: What if the unit profit of one of Wyndor’s new products is inaccurate? ----
  
# Sensitivity Analysis (Objective Function Coefficients)
sensitivity_analysis <- lp("max", f.obj, f.con, f.dir, f.par, compute.sens = TRUE)

sensitivity_analysis$sens.coef.from
sensitivity_analysis$sens.coef.to

# disabling scientific notation: scientific penalty unless wider than 999 digits.
options(scipen = 999)

# Q2: What if the unit profits of both of Wyndor’s new products are inaccurate? ----
  # Assignment: Implement 100% rule in R (try to automate the process)

# Q3: What if available hours (Constraint RHS) changes in one of the plants? ----

# Sensitivity Analysis (Shadow Prices, constraints first, variables next)
sensitivity_analysis$duals

# Sensitivity Analysis (Allowable RHS, constraints first, variables next)
sensitivity_analysis$duals.from
sensitivity_analysis$duals.to

# Q4: What if available hours (Constraint RHS) changes in one of the plants? ----
# Assignment: Implement 100% rule in R (try to automate the process)

# Q5: What if the production rates of doors and windows at plant 3 are uncertain? ----
  # range of uncertainty for hours required per door is uniform (2.5–3.5)
  # range of uncertainty for the hours required per window is uniform (1.5–2.5)

# Number of simulations
num_simulations <- 1000

# Initialize vectors to store results
results <- vector("list", num_simulations)
  # vector initialized as a list, i.e. each element ocan be any type of object
  # This vector store the solutions from each simulation run of LP.

p_values <- numeric(num_simulations)
  # vector initialized with all elements set to 0.
  # This vector store the obj fun values (profit) from each simulation run of LP.

# Run simulations
for (i in 1:num_simulations) {
  # Simulate uncertain parameters
  hours_door <- runif(1, min = 2.5, max = 3.5)
  hours_window <- runif(1, min = 1.5, max = 2.5)
  
  # Update the constraint matrix with the simulated parameters
  f.con[3, 1] <- hours_door
  f.con[3, 2] <- hours_window
  
  # Solve the linear program
  lp_result <- lp("max", f.obj, f.con, f.dir, f.par)
  
  # Store the results
  results[[i]] <- lp_result$solution
  # double brackets [[ ]] for extracting or assigning values to a element in a list
  
  p_values[i] <- lp_result$objval
  # single brackets [ ] for extracting or assigning values to subsets of a list.
}

# Find the optimal solution based on the simulated results
best_solution <- results[[which.max(p_values)]]
# which.max() returns the index of the maximum value in the vector p_values
# It finds the position of the element with the highest value.

best_p <- max(p_values)

# Print the optimal solution
cat("Optimal Solution:\n")
cat("x1 =", best_solution[1], "\n")
cat("x2 =", best_solution[2], "\n")
cat("z =", best_p, "\n")

# Assignment: Normal Distribution
# hours required per door is normal (mean=3, sd=0.5)
# hours required per window is uniform (mean=2, sd=0.5)

