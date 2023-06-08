# Clearing the Environment ----
rm(list=ls())

# Import lp function from lpSolve package ----
library(lpSolve)

# Algebraic Model ----
# X1: Factory in LA
# X2: Factory in SF
# X3: Warehouse in LA
# X4: Warehouse in SF
# Max NPV = 8X1 + 5X2 + 6X3 + 4X4 in Millions (Objective Function)
# Subject to (Constraints)
# 6X1 + 3X2 + 5X3 + 2X4 <= 10 (Capital Available in Millions)
#              X3 +  X4 <= 1  (Mutually Exclusive: Only One Warehouse)
# -X1       +  X3       <= 0 (Contingent: X3 <= X1 Warehouse if Factory in LA)
#       -X2       +  X4 <= 0 (Contingent: X4 <= X2 Warehouse if Factory in SF)
# X1, X2, X3, X4 = Binary Integer

# Solving BIP model ----
# Set coefficients of the objective function
f.obj <- c(8, 5, 6, 4)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negativity constraint; it is automatically assumed
f.con <- matrix(c(6,  3,  5,  2,
                  0,  0,  1,  1,
                 -1,  0,  1,  0,
                  0, -1,  0,  1), nrow = 4, byrow = TRUE)

# Set inequality signs
f.dir <- rep("<=",4)

# Set right-hand side parameters
f.par <- c(10, 1, 0, 0)

# Solve the optimization problem and store the result
result <- lp("max", f.obj, f.con, f.dir, f.par, int.vec = 1:4, all.bin = TRUE)

# Variables final values
solution <- result$solution

# Objective function value
objective_value <- result$objval

# Print the final result
cat("Optimized solution:\n")
print(solution)

cat("\n Objective function value:", objective_value, "Millions")

# Sensitivity Analysis ----

# Initial capital available
capital_available <- 5

# Increment for capital available
increment <- 1

# Maximum capital available
max_capital_available <- 15

# Loop for sensitivity analysis
while (capital_available <= max_capital_available) { 
  # while loop: as long as on specified condition is true
  # while loop: continue as long as acapital_available <= max_capital_available.
  # Similar to for loop, but for used when we know exact number of iterations
  
  # Set right-hand side parameters
  f.par <- c(capital_available,
             1,
             0,
             0)
  
  # Solve the optimization problem and store the result
  result <- lp("max", f.obj, f.con, f.dir, f.par, int.vec = 1:4, all.bin = TRUE)
  
  # Variables final values
  solution <- result$solution
  
  # Objective function value
  objective_value <- result$objval
  
  # Print the result for the current capital available
  cat("Capital Available:", capital_available, "Millions\n")
  cat("Optimized solution:\n")
  print(solution)
  cat("Objective function value:", objective_value, "Millions\n\n")
  
  # Increment capital available
  capital_available <- capital_available + increment
}
