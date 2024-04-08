library(reshape2)
library(lubridate)

data <- read.csv("./x0vytqxbkgstowoq.csv")
head(data)
class(data)

# Keeping the unadjusted prices in a new column
data$Unadjusted_PRC <- data$PRC
# Modifying the PRC column
data$Adjusted_PRC <- data$PRC / data$CFACPR
head(data)

QCOM <- data[data$PERMNO==77178, c("date", "Adjusted_PRC")]
names(QCOM)[2] <- "QCOM"
NVDA <- data[data$PERMNO==86580, c("date", "Adjusted_PRC")]
names(NVDA)[2] <- "NVDA"

# Create a new data frame 
PRC <- dcast(data, date ~ PERMNO, value.var = "Adjusted_PRC")
names(PRC) <- c("date", "QCOM", "NVDA")
head(PRC)
RET <- dcast(data, date ~ PERMNO, value.var = "RET")
names(RET) <- c("date", "QCOM", "NVDA")
head(RET)

# We choose all the columns except the first one 
# And transform them into a new Y data frame
Y <- log(1 + RET[,2:3])
Y$date <- RET$date
head(Y)

# Save the original int type date to int_date
Y$int_date <- Y$date
# Use the function ymd() to transform the column into Dates
Y$date <- ymd(Y$date)
# Check if it worked
class(Y$date)

# Lets do the same for PRC
PRC$int_date <- PRC$date
PRC$date <- ymd(PRC$date)

# Saving the data frame of returns
save(Y, file = "Y.RData")
# Saving the data frame of prices
save(PRC, file = "PRC.RData")

# Remove the existing data frame of returns
rm(Y)
# Load the saved file
load("Y.RData")
head(Y)

write.csv(Y, file = "Y.csv")

# By default, plot() uses points, we can plot a line with the option "type", "l" denotes line
plot(Y$date, Y$QCOM, type = "l", 
     main = "Compound returns for Qualcomm", 
     ylab = "Returns", xlab = "Observation", col = "red")

par(mfrow = c(1,1))
# By default, plot() uses points, we can plot a line with the option "type", "l" denotes line
plot(Y$date, Y$NVDA, type = "l", 
     main = "Compound returns for NVIDIA", 
     ylab = "Returns", xlab = "Observation", col = "blue")

# First we plot the returns of QCOM
plot(PRC$date, PRC$QCOM, type = "l", main = "Prices for Qualcomm and NVIDIA", 
     ylab = "Price", xlab = "Date", col = "red", ylim = c(0, 400))
# Then we add the returns of NVDA
lines(PRC$date, PRC$NVDA, col = "blue")
# And we create a legend
legend("topleft",legend = c("QCOM", "NVDA"), col = c("red", "blue"), lty=1)

par(mfrow = c(2,1))
for (i in 2:3) { 
  plot(PRC$date, PRC[,i], type = "l", ylab = "Returns", xlab = "Date",
       main = paste("Prices for", names(PRC)[i]))
}

summary(PRC$QCOM)
summary(PRC$NVDA)
summary(Y$QCOM)
summary(Y$NVDA)
sd(Y$QCOM)
sd(Y$NVDA)

library(tseries)
library(car)

# Loading the data
load("PRC.RData")
load("Y.RData")

# Subset the PRC data frame
QCOM <- subset(PRC, select = "QCOM")
# Rename the column as price
names(QCOM) <- "price"
# Plot
QCOM$date <- PRC$date
QCOM$returns <- Y$QCOM

# Get the mean and sd of returns
qcom_mean <- mean(QCOM$returns)
qcom_sd <- sd(QCOM$returns)
paste0("Mean: ", round(qcom_mean,3))
paste0("Standard Deviation: ", round(qcom_sd,3))
# Create the histogram
hist(QCOM$returns, freq = FALSE, main = "Returns of QCOM", col = "lightgrey", breaks = 50)
# Add the normal distribution
x <- seq(-3,3,0.001)
lines(x, dnorm(x, mean = qcom_mean, sd = qcom_sd), lwd = 3, col = "red")

# qqPlot of the normal distribution
qqPlot(QCOM$returns, distribution = "norm", envelope = FALSE)
# 3 degrees of freedom
qqPlot(QCOM$returns, distribution = "t", df = 3, envelope = FALSE,
       main = "QQ Plot of QCOM and Student-t with 3 Degrees of Freedom")
# 4 degrees of freedom
qqPlot(QCOM$returns, distribution = "t", df = 4, envelope = FALSE,
       main = "4 Degrees of Freedom")

# Subset the PRC data frame
NVDA <- subset(PRC, select = "NVDA")
# Rename the column as price
names(NVDA) <- "price"
# Plot
NVDA$date <- PRC$date
NVDA$returns <- Y$NVDA

# Get the mean and sd of returns
nvda_mean <- mean(NVDA$returns)
nvda_sd <- sd(NVDA$returns)
paste0("Mean: ", round(nvda_mean,3))
paste0("Standard Deviation: ", round(nvda_sd,3))
# Create the histogram
hist(NVDA$returns, freq = FALSE, main = "Returns of NVDA", col = "lightgrey", breaks = 50)
# Add the normal distribution
x <- seq(-3,3,0.001)
lines(x, dnorm(x, mean = nvda_mean, sd = nvda_sd), lwd = 3, col = "red")

# qqPlot of the normal distribution
qqPlot(NVDA$returns, distribution = "norm", envelope = FALSE)
# 3 degrees of freedom
qqPlot(NVDA$returns, distribution = "t", df = 3, envelope = FALSE,
       main = "QQ Plot of NVDA and Student-t with 3 Degrees of Freedom")
# 4 degrees of freedom
qqPlot(NVDA$returns, distribution = "t", df = 4, envelope = FALSE,
       main = "4 Degrees of Freedom")
