library(reshape2)
library(lubridate)
library(tseries)
library(MASS,stats)
library(car)
#library(fGarch)

data <- read.csv(file = "D:/Desktop/FM442/Presentation/R_code/SP500.csv")
head(data)

Ret <- data$sprtrn
y  <- as.matrix(Ret)
dates <- ymd(data$caldt)

# Specify the probability p
p <- 0.01
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



