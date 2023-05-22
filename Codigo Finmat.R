# Install and load necessary libraries
library(readxl)
library(writexl)
library(matrixStats)

# Read data from the Excel file
input_parameters <- read_excel("~/Finanzas Matematicas/Proyecto Final.xlsx", sheet = "Input Parameters")

# Assign each input parameter to a variable
option_type <- as.character(input_parameters$`option type`)
option_direction <- as.character(input_parameters$`option direction`)
strike_price <- as.numeric(input_parameters$`strike price`)
barrier_level <- as.numeric(input_parameters$`Barrier  level`)
volatility <- as.numeric(input_parameters$volatility)
interest_rate <- as.numeric(input_parameters$`Interest rate`)
dividend_yield <- as.numeric(input_parameters$`dividend yield`)
time_to_expiration <- as.numeric(input_parameters$`time of expiration`)
simulation_number <- as.numeric(input_parameters$simulation_number)
time_steps <- as.numeric(input_parameters$time_steps)

# Monte Carlo simulation function for exotic options
exotic_option_monte_carlo <- function(option_type, option_direction, strike_price, barrier_level, volatility, 
                                      interest_rate, dividend_yield, time_to_expiration, simulation_number, 
                                      time_steps, S0) {
  # Initialize the matrix for storing the paths
  paths <- matrix(ncol = time_steps, nrow = simulation_number)
  
  # Initial asset price at time 0
  S0 <- 100 # You can adjust this value according to your needs
  
  # Calculate the drift
  drift <- (interest_rate - dividend_yield - 0.5 * volatility^2) * (time_to_expiration / time_steps)
  
  # Calculate the volatility factor
  vol_factor <- volatility * sqrt(time_to_expiration / time_steps)
  
  # Simulate the paths
  for (i in 1:simulation_number) {
    # Initialize the path
    path <- rep(0, time_steps)
    path[1] <- S0
    
    for (j in 2:time_steps) {
      # Generate a random standard normal number
      Z <- rnorm(1)
      
      # Calculate the asset price at the next time step
      path[j] <- path[j-1] * exp(drift + vol_factor * Z)
    }
    
    # Store the path
    paths[i,] <- path
  }
  
  # Pricing the exotic option based on the simulated paths
  if (option_type == "knock in") {
    if (option_direction == "call") {
      # Knock-in call option
      payoff <- pmax(pmax(paths, na.rm = TRUE) - barrier_level, 0) * (paths[, time_steps] > strike_price)
    } else {
      # Knock-in put option
      payoff <- pmax(barrier_level - pmin(paths, na.rm = TRUE), 0) * (paths[, time_steps] < strike_price)
    }
  } else if (option_type == "knock out") {
    if (option_direction == "call") {
      # Knock-out call option
      payoff <- (pmax(paths[, time_steps] - strike_price, 0)) * (pmax(paths, na.rm = TRUE) < barrier_level)
    } else {
      # Knock-out put option
      payoff <- (pmax(strike_price - paths[, time_steps], 0)) * (pmin(paths, na.rm = TRUE) > barrier_level)
    }
  }
  
  # Discount the payoff back to the present value
  option_price <- exp(-interest_rate * time_to_expiration) * mean(payoff)
  
  return(option_price)
}

# Run the Monte Carlo simulation
option_price <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, volatility, 
                                          interest_rate, dividend_yield, time_to_expiration, simulation_number, 
                                          time_steps)

# Print the option price
print(option_price)

# Create a vector of different volatility values
volatilities <- seq(0.1, 0.5, by = 0.01)

# Initialize a vector to store the option prices
option_prices <- numeric(length(volatilities))

# Run the Monte Carlo simulation for each volatility value
for (i in seq_along(volatilities)) {
  option_prices[i] <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                volatilities[i], interest_rate, dividend_yield, 
                                                time_to_expiration, simulation_number, time_steps)
}

# Plot option prices vs volatility
plot(volatilities, option_prices, type = "l", xlab = "Volatility", ylab = "Option Price", 
     main = "Sensitivity Analysis: Option Price vs Volatility")

# Function to calculate Delta
calc_delta <- function(S) {
  dS = 0.01 * S  # small change in stock price
  option_price_plus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                 volatility, interest_rate, dividend_yield, time_to_expiration, 
                                                 simulation_number, time_steps, S + dS)
  option_price_minus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                  volatility, interest_rate, dividend_yield, time_to_expiration, 
                                                  simulation_number, time_steps, S - dS)
  delta <- (option_price_plus - option_price_minus) / (2 * dS)
  return(delta)
}

# Function to calculate Gamma
calc_gamma <- function(S) {
  dS = 0.01 * S
  delta_plus <- calc_delta(S + dS)
  delta_minus <- calc_delta(S - dS)
  gamma <- (delta_plus - delta_minus) / (2 * dS)
  return(gamma)
}

# Function to calculate Vega
calc_vega <- function(S) {
  dvol = 0.01  # small change in volatility
  option_price_plus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                 volatility + dvol, interest_rate, dividend_yield, 
                                                 time_to_expiration, simulation_number, time_steps, S)
  option_price_minus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                  volatility - dvol, interest_rate, dividend_yield, 
                                                  time_to_expiration, simulation_number, time_steps, S)
  vega <- (option_price_plus - option_price_minus) / (2 * dvol)
  return(vega)
}

# Function to calculate Theta
calc_theta <- function(S) {
  dt = 0.01  # small change in time
  option_price_minus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                  volatility, interest_rate, dividend_yield, 
                                                  time_to_expiration - dt, simulation_number, time_steps, S)
  option_price <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                            volatility, interest_rate, dividend_yield, 
                                            time_to_expiration, simulation_number, time_steps, S)
  theta <- (option_price_minus - option_price) / dt
  return(theta)
}

# Function to calculate Rho
calc_rho <- function(S) {
  dr = 0.01  # small change in interest rate
  option_price_plus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                 volatility, interest_rate + dr, dividend_yield, 
                                                 time_to_expiration, simulation_number, time_steps, S)
  option_price_minus <- exotic_option_monte_carlo(option_type, option_direction, strike_price, barrier_level, 
                                                  volatility, interest_rate - dr, dividend_yield, 
                                                  time_to_expiration, simulation_number, time_steps, S)
  rho <- (option_price_plus - option_price_minus) / (2 * dr)
  return(rho)
}

library(ggplot2)
S <- seq(91, 121, by = 1)

# Plot Delta
ggplot(data.frame(Stock_Price = S), aes(x = Stock_Price, y = calc_delta(Stock_Price))) +
  geom_point() +
  labs(title = "Delta vs. Stock Price",
       x = "Stock Price",
       y = "Delta")

# Plot Gamma
ggplot(data.frame(Stock_Price = S), aes(x = Stock_Price, y = calc_gamma(Stock_Price))) +
  geom_point() +
  labs(title = "Gamma vs. Stock Price",
       x = "Stock Price",
       y = "Gamma")

# Plot Vega
ggplot(data.frame(Stock_Price = S), aes(x = Stock_Price, y = calc_vega(Stock_Price))) +
  geom_point() +
  labs(title = "Vega vs. Stock Price",
       x = "Stock Price",
       y = "Vega")

# Plot Rho
ggplot(data.frame(Stock_Price = S), aes(x = Stock_Price, y = calc_rho(Stock_Price))) +
  geom_point() +
  labs(title = "Rho vs. Stock Price",
       x = "Stock Price",
       y = "Rho")

# Plot Theta
ggplot(data.frame(Stock_Price = S), aes(x = Stock_Price, y = calc_theta(Stock_Price))) +
  geom_point() +
  labs(title = "Theta vs. Stock Price",
       x = "Stock Price",
       y = "Theta")

# Calculate and print the Greeks
S0 <- 100  # initial asset price

# Calculate Delta
delta <- calc_delta(S0)
cat("Delta: ", delta, "\n")

# Calculate Gamma
gamma <- calc_gamma(S0)
cat("Gamma: ", gamma, "\n")

# Calculate Vega
vega <- calc_vega(S0)
cat("Vega: ", vega, "\n")

# Calculate Theta
theta <- calc_theta(S0)
cat("Theta: ", theta, "\n")

# Calculate Rho
rho <- calc_rho(S0)
cat("Rho: ", rho, "\n")

greeks <- data.frame(delta,gamma,vega,rho,theta)
greeks



