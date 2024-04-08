library(reshape2)
library(lubridate)
library(tseries)
library(MASS,stats)
library(car)

data <- read.csv(file = "D:/Desktop/FM442/Presentation/R_code/SP500.csv")
head(data)

Ret <- data$sprtrn
y  <- as.matrix(Ret)
dates <- ymd(data$caldt)

# Specify the probability p
p <- 0.05
# Assume we have a portfolio value of 1000 USD
portfolio <- 1000
# Sort the values in y using sort()
ys <- sort(y)
# Plot them
plot(ys, type = "l", main = "Sorted returns for SP500 in 2006-2009", las = 1)

# Number of observations
n <- length(y)
# Get the 5% quantile by multiplying the length times p
n * p
# Round up
quant <- ceiling(n*p)
quant
# Find the 407th element in ys
ys[quant]
# Visually finding the 5% quantile
plot(ys, type = "l", main = "Sorted returns for SP500 in 2006-2009 ", las = 1)
# Adding the segments
segments(x0 = quant, y0 = -0.5, y1 = ys[quant], lty = "dashed", lwd = 2, col ="red")
segments(x0 = -1000, y0 = ys[quant], x1 = quant, lty = "dashed", lwd = 2, col ="red")
# Adding ticks
axis(1, at = quant, label = quant)
axis(2, at = ys[quant], label = round(ys[quant],3), las = 1)

# Use it to calculate VaR, scaling for the portfolio value
VaR <- -ys[quant] * portfolio
VaR
# For ES, we get the mean of observations up to the 5th quantile
ES <- -mean(ys[1:quant]) * portfolio
ES
# Report our findings:
cat("We have", length(y), "observations on SP500", "\n",
    p , "VaR is", VaR, "and ES is", ES, "on portfolio worth $", portfolio, "\n")

# Using four different estimation window sizes
windows <- c(50, 100,150,200,250)

# Create an empty data frame to fill with the forecasts
HS <- data.frame(HS50 = numeric(),
                 HS100 = numeric(),
                 HS150 = numeric(),
                 HS200 = numeric(),
                 HS250 = numeric())

# Do a loop for every element of windows
for (window in windows) {
  
  # Perform a daily HS VaR
  for (i in 1:(n-window)) {
    
    # Sort the returns for the respective estimation window
    ys <- sort(y[i:(i+window)])
    
    # Get the quantile
    quant <- ceiling(p*length(ys))
    
    # Allocate the result to the corresponding column of HS
    # Use which() to find the index of the window and allocate to that column
    column <- which(windows == window)
    HS[i+window, column] <- -ys[quant]*portfolio
  }
}

# Plotting the forecasts
plot(HS$HS50, main = "HS with different estimation windows", ylab = "VaR in USD",
     xlab = "Date", type = "l", col = "red", lwd = 2)
lines(HS$HS100, col = "blue", lwd = 2)
lines(HS$HS150, col = "green", lwd = 2)
lines(HS$HS200, col = "black", lwd = 2)
lines(HS$HS250, col = "purple", lwd = 2)

legend("topleft", legend = names(HS), lty = 1, col = c("red", "blue", "green", "black","purple"))

# Specifying the parameters for VaR
p <- 0.05
portfolio <- 1000

# Specifying the parameters for EWMA
lambda <- 0.94
n <- length(y)
BurnTime <- 5

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
plot(dates, EWMA_cond_volatility, type = "l", main = "EWMA Conditional Volatility",
     las = 1, col = "red", xlab = "Date", ylab = "EWMA Cond. Volatility")

# Implementing the VaR forecast
EWMA_VaR <- -qnorm(p) * EWMA_cond_volatility * portfolio

# Plotting it
plot(dates, EWMA_VaR, type = "l", main = "EWMA VaR",
     las = 1, col = "red", xlab = "Date", ylab = "USD")

# Storing the data
VaRdata <- cbind(HS, EWMA_VaR)
VaRdata <- as.data.frame(VaRdata)
save(VaRdata, file = "VaRdata.RData")


library(lubridate)
library(rugarch)

load("VaRdata.RData")
# Steps inside the function:
# Take as argument
# GARCH spec, here the default
# Probability, here 0.05
# Portfolio value, here 1000
# Estimation window, here 1000
spec <- ugarchspec()
probability <- 0.05
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

DoGARCH <- function(y, spec, probability = 0.05, portfolio_value = 1, WE = 1000){
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
# GARCH VaR for 50 days
GARCH50 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 50)
# Saving the output
save(GARCH50, file = "GARCH50.RData")
# GARCH VaR for 100 days
GARCH100 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 100)
# Saving the output
save(GARCH100, file = "GARCH100.RData")
# GARCH VaR for 150 days
GARCH150 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 150)
# Saving the output
save(GARCH150, file = "GARCH150.RData")
# GARCH VaR for 200 days
GARCH200 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 200)
# Saving the output
save(GARCH200, file = "GARCH200.RData")
# GARCH VaR for 200 days
GARCH250 <- DoGARCH(y, spec = spec, probability = 0.05, portfolio_value = 1000, WE = 250)
# Saving the output
save(GARCH250, file = "GARCH250.RData")
# If we have it already saved, we can load it by:
load("GARCH50.RData")
load("GARCH100.RData")
load("GARCH150.RData")
load("GARCH200.RData")
load("GARCH250.RData")
# Bind into a matrix
GARCH_VaR <- cbind(GARCH50,GARCH100,GARCH150,GARCH200, GARCH250)
typeof(GARCH_VaR)

# Plot and modify axis to include dates
matplot(dates, GARCH_VaR, type = "l", lty = 1, col = 1:5, xaxt = "n", main = "GARCH VaR", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("topleft", legend = c("WE: 50","WE: 100","WE: 150","WE: 200", "WE: 250"), lty = 1, col = 1:5)

# Combining all VaR forecasts
VaRdata <- cbind(VaRdata, GARCH_VaR)
VaRdata <- as.data.frame(VaRdata)
save(VaRdata, file = "VaRdata.RData")
# Means for each forecast
round(colMeans(VaRdata, na.rm = TRUE),3)
# Standard deviations - We
round(apply(VaRdata, MARGIN = 2, sd, na.rm = TRUE))

typeof(VaRdata)


# Find maximum estimation window
windows <- colSums(is.na(VaRdata))

#restrict to largest estimation window
start <- max(windows) + 1
end <- length(dates)

# Plot all
matplot(dates[start:end], VaRdata[start:end,], type = "l", lty = 1, col = 1:11, xaxt = "n",
        main = "VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(dates[max(windows)], max(dates), by = "years"))

# Legend
legend("topleft", legend = colnames(VaRdata), lty = 1, col = 1:11,cex=0.5)


# Backtesting and Violation Ratios

# Let's transform VaR to a data.frame
VaRdata <- as.data.frame(VaRdata)

# Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
Violations <- VaRdata
Violations[] <- NA

dim(Violations)

# Populating the Violations matrix

# For every model (columns in VaR) restricted to largest estimation window
for(i in 1:dim(VaRdata)[2]){
  
  # Fill the column in Violations with TRUE/FALSE
  # TRUE if the realized return is lower than VaR
  # FALSE otherwise
  Violations[,i] <- (y*portfolio_value < -VaRdata[,i])
}
# Restrict to largest estimation window
Violations[1:(start-1),] <- NA

# Find where violations happened
dates[which(Violations$EWMA_VaR)]
# Get a random day where EWMA VaR is violated using sample()
# sample() returns specified size of elements from input
random_day <- sample(dates[which(Violations$HS200)],1)
# See that row in Violations
paste0("Violation for HS200 on ",random_day)
Violations[day_index,]

# Find the index in dates using which()# Plotting the violations
plot(dates[start:end], VaRdata$EWMA_VaR[start:end], type = "l", main = "EWMA VaR with violations",xlab = "Date", ylab = "EWMA_VaR")

# Add points where the violations happened
points(dates[Violations$EWMA_VaR], VaRdata$EWMA_VaR[Violations$EWMA_VaR], pch = 20, col = "red",cex=0.5)
day_index <- which(dates == random_day)

# Find the index in dates using which()# Plotting the violations
plot(dates[start:end], VaRdata$GARCH250[start:end], type = "l", main = "Garch_250 VaR with violations",xlab = "Date", ylab = "Garch250_VaR")

# Add points where the violations happened
points(dates[Violations$GARCH250], VaRdata$GARCH250[Violations$GARCH250], pch = 20, col = "red",cex=0.5)
day_index <- which(dates == random_day)




# Check dates where all models have a violation
# restrict to largest estimation window
w <- apply(Violations, 1, all)
# Days where all models have a violation
# na.rm =TRUE means we will firstly remove all NA elements
sum(w, na.rm = TRUE)
# Plotting the returns and adding the days where all models had a violation
plot(dates, y, main = "SP500 returns", type = "l", lwd = 2, las = 1,
     xlab = "Date", ylab = "Returns")
points(dates[w], y[w], pch = 20, col = "red",cex = 0.5)

# Counting Violations by model
colSums(Violations, na.rm = TRUE)

# Creating a Violation Ratio object

# Remove the rows with NA
Violations <- Violations[!is.na(Violations[,1]),]

# Get the column sums
V <- colSums(Violations)

# Calculate expected violations
EV <- dim(Violations)[1]*p
EV

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




