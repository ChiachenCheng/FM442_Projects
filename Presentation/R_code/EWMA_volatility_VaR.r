library(reshape2)
library(lubridate)
library(tseries)
library(MASS,stats)
library(car)
library(fGarch)
library(rugarch)

data <- read.csv("SP500.csv")
head(data)

Ret <- data$sprtrn
y  <- as.matrix(Ret)
dates <- ymd(data$caldt)

n <- dim(y)[1]
# Initializing the EWMA matrix
EWMA <- matrix(NA, nrow = n, ncol = 3)
# Determine lambda
lambda <- 0.94

S <- cov(y)
S

# Fill the initial row of EWMA with the sample covariances
EWMA[1,] <- S[upper.tri(S, diag = TRUE)]

# Manually computing EWMA elements for t = 2

# Apply the formula for EWMA
S_2 <- lambda * S + (1-lambda) * y[1,] %*% t(y[1,])
# Get the variances and covariances
S_2[upper.tri(S_2, diag = TRUE)]

# Populating the EWMA matrix

# Create a loop for rows 2 to n
for (i in 2:n) {
    # Update S with the new weighted moving average
    S <- lambda * S + (1-lambda) * y[i-1,] %*% t(y[i-1,])
    
    # Fill the following EWMA row with the covariances
    EWMA[i,] <- S[upper.tri(S, diag = TRUE)]
}

head(EWMA)

# Plotting estimated variances and covariances
matplot(dates, EWMA, type = "l", main = "EWMA of volatility^2", lty = 1)

# Implementing EWMA VaR

# Specifying the parameters for VaR
p <- 0.01
portfolio_value <- 1000

# Specifying the parameters for EWMA
lambda <- 0.94
n <- length(y)
BurnTime <- 30

# Vector to store estimated conditional volatility
EWMA_Variance <- rep(NA, length = n)

# First value is the sample variance
EWMA_Variance[1] <- var(y)

# See the vector
head(EWMA_Variance)

# Run the EWMA model using a for loop
for (i in 2:n) {
    EWMA_Variance[i] <- lambda * EWMA_Variance[i-1] + (1-lambda) * y[i-1]^2
}

# Replacing the data in the estimation window to NA
EWMA_Variance[1:BurnTime] <- NA

# Plot estimation for conditional volatility
EWMA_cond_volatility <- sqrt(EWMA_Variance)
plot(EWMA_cond_volatility, type = "l", main = "EWMA Conditional Volatility",
    las = 1, col = "red", xlab = "Date", ylab = "EWMA Cond. Volatility")

# Implementing the VaR forecast
EWMA_VaR <- -qnorm(p) * EWMA_cond_volatility * portfolio_value

# Plotting it
plot(dates, EWMA_VaR, type = "l", main = "EWMA VaR",
    las = 1, col = "red", xlab = "Date", ylab = "USD")

print(EWMA_VaR)

print(sqrt(var(Ret)))

data <- read.csv("SP500.csv")
head(data)

Ret <- data$sprtrn
y  <- as.matrix(Ret)
dates <- ymd(data$caldt)

# Parameters
portfolio_value = 1000
p = 0.01 #probability for VaR
probability = p

# Steps inside the function:
# Take as argument
# GARCH spec, here the default
# Probability, here 0.05
# Portfolio value, here 1000
# Estimation window, here 1000
spec <- ugarchspec()
p <- 0.01
portfolio_value <- 1000
WE <- 1000

# Determine number of observations
n <- length(y)

# Initialize empty VaR vector
    
VaR <- rep(NA, n)
    
# Do a loop for the forecast
for (i in 1:(n-WE)){
        
    # Subset the dataset to the estimation window
    window <- y[i:(i+WE-1)]
        
    # Fit the GARCH
    res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
        
    # Save coefficients
    omega <- coef(res)['omega']
    alpha <- coef(res)['alpha1']
    beta <- coef(res)['beta1']
        
    # Estimate sigma2 using the last observation of window
    sigma2 <- omega + alpha*tail(window,1)^2 + beta*tail(res@fit$var,1)
        
    # Allocate the VaR forecast in the vector
    VaR[i+WE] <- -sqrt(sigma2) * qnorm(probability) * portfolio_value
    }

# Function that creates a GARCH forecast

DoGARCH <- function(y, spec, probability = p, portfolio_value = 1, WE = 1000){
    # GARCH function that takes as argument:
    # y: A vector of returns, ordered by date
    # spec: The ugarchspec object with the GARCH specification
    # probability: The probability to be used for VaR - Default 5%
    # portfolio_value: The portfolio value - Default 1
    # WE: Estimation window for the forecast - Default 1000 days
    
    # To calculate elapsed time, first get the current time
    old <- Sys.time()
    
    # Print message
    cat("Doing GARCH VaR forecast", "\n",
       "Estimation window:", WE, "\n",
       "Number of observations:", length(y), "\n",
       "VaR probability:", probability, "\n",
       "Portfolio value:", portfolio_value)
    
    # Number of observations
    n <- length(y)
    
    # Initialize empty VaR vector
    VaR <- rep(NA, n)
    
    # Do a loop for the forecast
    for (i in 1:(n-WE)){
        
        # Subset the dataset to the estimation window
        window <- y[i:(i+WE-1)]
        
        # Fit the GARCH
        res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
        
        # Save coefficients
        omega <- coef(res)['omega']
        alpha <- coef(res)['alpha1']
        beta <- coef(res)['beta1']
        
        # Estimate sigma2 using the last observation of window
        sigma2 <- omega + alpha*tail(window,1)^2 + beta*tail(res@fit$var,1)
        
        # Allocate the VaR forecast in the vector
        VaR[i+WE] <- -sqrt(sigma2) * qnorm(probability) * portfolio_value
    }
    
    # Get the new time and print the elapsed time
    time <- difftime(Sys.time(), old, units = "secs")
    cat("\n", "Elapsed time:", round(time,4), "seconds")
    
    # Return the VaR vector
    return(VaR)
}

# Create specification
spec <- ugarchspec(
  variance.model = list(garchOrder= c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean=FALSE)
)

# GARCH VaR for 300 days
GARCH300 <- DoGARCH(y, spec = spec, probability = p, portfolio_value = 1000, WE = 300)

# GARCH VaR for 100 days
GARCH100 <- DoGARCH(y, spec = spec, probability = p, portfolio_value = 1000, WE = 100)

# Bind into a matrix
GARCH_VaR <- cbind(GARCH300, GARCH100)

# Plot and modify axis to include dates
matplot(dates, GARCH_VaR, type = "l", lty = 1, col = 1:2, xaxt = "n", main = "GARCH VaR", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topright", legend = c("WE: 300", "WE: 100"), lty = 1, col = 1:2)

VaR <- cbind(EWMA_VaR)

# Combining all VaR forecasts
# VaR is load from .RData file includes VaR for HS300, HS1000, HS2000 and EWMA
VaR <- cbind(VaR, GARCH300, GARCH100)

# Plot all
matplot(dates, VaR, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(VaR), lty = 1, col = 1:6)

# Backtesting and Violation Ratios

# Let's transform VaR to a data.frame
VaR <- as.data.frame(VaR)

# Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
Violations <- VaR
Violations[] <- NA

dim(Violations)

start <- 1
end <- length(dates)

# Backtesting and Violation Ratios

# Let's transform VaR to a data.frame
VaR <- as.data.frame(VaR)

# Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
Violations <- VaR
Violations[] <- NA

dim(Violations)

# Populating the Violations matrix

# For every model (columns in VaR) restricted to largest estimation window
for(i in 1:dim(VaR)[2]){
    
    # Fill the column in Violations with TRUE/FALSE
    # TRUE if the realized return is lower than VaR
    # FALSE otherwise
    Violations[,i] <- (y*portfolio_value < -VaR[,i])
}

# Restrict to largest estimation window
Violations[1:(start-1),] <- NA

# Find where violations happened
dates[which(Violations$EWMA_VaR)]

# Plotting the violations
plot(dates[start:end], VaR$EWMA_VaR[start:end], type = "l", main = "EWMA VaR with violations",)

# Add points where the violations happened
points(dates[Violations$EWMA_VaR], VaR$EWMA_VaR[Violations$EWMA_VaR], pch = 16, col = "red")

# Check dates where all models have a violation
# restrict to largest estimation window
w <- apply(Violations, 1, all)

# Days where all models have a violation
# na.rm =TRUE means we will firstly remove all NA elements
sum(w, na.rm = TRUE)

# Plotting the returns and adding the days where all models had a violation
plot(dates, y, main = "Microsoft returns", type = "l", lwd = 2, las = 1,
    xlab = "Date", ylab = "Returns")
points(dates[w], y[w], pch = 16, col = "red")

# Counting Violations by model
colSums(Violations, na.rm = TRUE)

# Creating a Violation Ratio object

# Remove the rows with NA
Violations <- Violations[!is.na(Violations[,2]),]

# Get the column sums
V <- colSums(Violations)

# Calculate expected violations
EV <- dim(Violations)[1]*p

# Violation Ratios
VR <- V/EV

# Call object, rounding to 3 decimals
round(VR,3)

# We can write a function that uses our rule of thumb to assess the model
model_assessment <- function(VR) {
    if (VR > 0.8 & VR < 1.2) {
        paste0(names(VR), "Model is good")
    } else if ((VR > 0.5 & VR <= 0.8) | (VR > 1.2 & VR <= 1.5)) {
        paste0(names(VR), "Model is acceptable")
    } else if ((VR > 0.3 & VR <= 0.5) | (VR > 1.5 & VR <= 2)) {
        paste0(names(VR), "Model is bad")
    } else {
        paste0(names(VR), "Model is useless")
    }
}

# We can use sapply(), the vector version of apply()
sapply(VR, model_assessment)

# Best performing - VR closest to 1
sort(round(abs(VR-1),3))




