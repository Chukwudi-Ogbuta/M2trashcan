
# *****************************************************************************
#                                 Bass Model
# *****************************************************************************

# *****************************************************************************
# Estimation of the Bass model for digital cameras 
# using data from 1999-2008 
# *****************************************************************************

# Activation of necessary packages
library("readxl")
library("rootSolve")

# Initialization (clearing all objects from memory)
rm(list=ls())

# Loading the data
data <- read_excel("MDA_data.xlsx", sheet = "Camera")

# Creating the column for cumulative sales
# Starting with 0 and then cumulative sum
# Implementing a shift: concatenation of "0" + variable without the last observation
data$C_Sales <- c(0, cumsum(data$Sales[-nrow(data)]))
data$C_Sales_SQ <- data$C_Sales^2

for (year in c(2020, 2012:2005)) {
  # Estimating coefficients beta0, beta1, beta2
  reg <- lm(Sales ~ C_Sales + C_Sales_SQ, data = data[data$Year <= year,])
  summary(reg)
  
  # Definition of the system of equations
  model <- function(x) {
    F1 = x[1] * x[3] - reg$coefficients[1]
    F2 = x[2] - x[1] - reg$coefficients[2]
    F3 = x[2] / x[3] + reg$coefficients[3]
    c(F1,F2,F3)
  }
  
  # Solution Search
  x <- multiroot(f = model, start = c(1, 1, 1))
  print(c(year, x$root))
}

full_data <- data
data <- data[data$Year <= 2007,]
reg <- lm(Sales ~ C_Sales + C_Sales_SQ, data = data)


# if the sales prediction of the new is significantly bigger than 0 (here >1)
while (data$Sales[nrow(data)] > 1) {
  # New line in the data table with Previous Year + 1
  Year <- tail(data$Year, n = 1) + 1
  # rbind to add a row to the table "data"
  data <- rbind(data, c(Year, 0, 0, 0))
  # Calculation of the cumulative sum
  data$C_Sales <- c(0, cumsum(data$Sales[-nrow(data)]))
  # Calculation of the cumulative sum squared
  data$C_Sales_SQ <- data$C_Sales^2
  # Calculation of Sales in the last row
  data$Sales[nrow(data)] <- tail(predict(reg, newdata = data), n = 1)
}

# Add a column with the observed values (when data are available)
data$Sales_act <- 0
data[1:nrow(full_data), ]$Sales_act <- full_data$Sales

# Creating graphical representations
plot(Sales_act ~ Year, data)
# Bass Model estimations
lines(data$Year, data$Sales, col = "red")
# Observations to evaluate the ability of the model to make good predictions
lines(data$Year, data$Sales_act, col = "blue")



