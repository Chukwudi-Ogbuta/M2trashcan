# *****************************************************************************
#                                 Conjoint Analysis
# *****************************************************************************

# Required packages
library(conjoint)  # Load the 'conjoint' package for conjoint analysis
library(readxl)    # Load the 'readxl' package for reading Excel files

# Clear the current workspace to remove any existing objects
remove(list = ls())

# Read data from an Excel file named "MDA_data.xlsx" in the "Laptop" sheet
data <- read_excel("MDA_data.xlsx", sheet = "Laptop")

# Extract the ranks data from columns 12 to 47 in the Excel sheet
Ranks <- data[12:47]

# *****************************************************************************
#                                 Simplified Approach
# *****************************************************************************

# Transform rank values into points obtained (higher rank = fewer points)
Pts <- 13 - Ranks

# Calculate the mean points for each configuration
meansPts <- rowMeans(Pts)
print(meansPts)

# Perform linear regression to estimate the specific contributions of features
reg <- lm(meansPts ~ screen_17 + HD_2To + price_500 + price_600, data = data)
summary(reg)

# *****************************************************************************
#                                 Traditionnal Approach
# *****************************************************************************


Ranks<-t(Ranks)
Pts<-caRankToScore(Ranks)
Pts

# Extract the relevant columns for the conjoint analysis experiment
experiment <- data[, 2:4]
experiment

# Create a orthogonal design for the experiment (less)
design <- caFactorialDesign(data = experiment, type="orthogonal")
design

# Create a full factorial design for the experiment
design <- caFactorialDesign(data = experiment, type = "full")
design

# Encode the design into a format suitable for conjoint analysis
code <- caEncodedDesign(design)
code

# Define the levels (categories) for the attributes
levels <- as.factor(c("screen_15", "screen_17", "HD_1To", "HD_2To", "price_500", "price_600", "price_700"))
levels

# Perform conjoint analysis using the 'Conjoint' function
conjoint_result <- Conjoint(Pts, code, levels, "score")
caTotalUtilities(Pts,code)
