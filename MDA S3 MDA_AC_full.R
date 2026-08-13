# Required packages
library(conjoint)  # Load the 'conjoint' package for conjoint analysis
library(readxl)    # Load the 'readxl' package for reading Excel files

# Clear the current workspace to remove any existing objects
remove(list = ls())

# Read data from an Excel file named "MDA_data.xlsx" in the "Laptop" sheet
data <- read_excel("MDA_data.xlsx", sheet = "Laptop")

# Extract the ranks data from columns 12 to 47 in the Excel sheet
Ranks <- data[12:47]


# For convenience, transposing the data table (rows become columns and vice versa)
Ranks <- t(Ranks)
# Transforming ranks into points obtained
Pts <- 13 - Ranks

# Calculate the mean points for each configuration
meansPts <- colMeans(Pts)
print(meansPts)

# Perform linear regression to estimate the specific contributions of features
reg <- lm(meansPts ~ screen_17 + HD_2To + price_500 + price_600, data = data)
summary(reg)

# Why segment? A visual insight
# Displaying the judgments of 3 individuals
plot(data$Ind21, type = "o", col = "red", ylab = "Utility", xlab = "Configuration")
lines(data$Ind8, type = "o", col = "blue")
lines(data$Ind17, type = "o", col = "green")

# To avoid modifying the rest of the program, assigning points
# to the table of variables used in hierarchical clustering
hac_vars <- Pts
# Calculating distances between different data points
hac.dist <- dist(hac_vars, method = "euclidean")^2 / (2 * nrow(hac_vars))

# Implementing agglomerative hierarchical clustering 
# based on the distance matrix
hac.res <- hclust(hac.dist, method = "ward.D")
# Displaying the dendrogram
plot(hac.res)
# Displaying the within-cluster loss of inertia
# in numeric form
print(hac.res$height)
# Displaying the within-cluster loss of inertia
# as a barplot
barplot(hac.res$height)
# Dividing groups based on the evolution of the "height" parameter
# (a measure akin to within-cluster loss of inertia)
# and the dendrogram into 2 segments.
seg <- cutree(hac.res, k = 2)
# Calculating the number of observations in each segment
# Here, it represents the number of individuals in each segment
table(seg)

# Creating a table adding the "seg" variable (segment membership of each individual)
# cbind: "c" for column and "bind" for binding.
# You can look at Points before and after.
Pts <- cbind(Pts, seg)

# Creating a results table that aggregates the estimated coefficients
# Results obtained with all individuals
# By specifying nrow=1, we impose one column per coefficient.
coeff <- matrix(reg$coefficients, nrow = 1)

# Estimation made with only individuals in group 1
meanAll1 <- colMeans(Pts[seg == 1, 1:12])
print(meanAll1)
# Estimation made with only individuals in group 1
reg <- lm(meanAll1 ~ screen_17 + HD_2To + price_500 + price_600, data = data)
summary(reg)
# Results obtained with individuals in group 1
coeff <- rbind(coeff, reg$coefficients)

# Estimation made with only individuals in group 2
meanAll2 <- colMeans(Pts[seg == 2, 1:12])
print(meanAll2)
# Estimation made with only individuals in group 2
reg <- lm(meanAll2 ~ screen_17 + HD_2To + price_500 + price_600, data = data)
summary(reg)
# Results obtained with individuals in group 2
coeff <- rbind(coeff, reg$coefficients)
row.names(coeff) <- c("All", "Seg_1", "Seg_2")
# Presentation of all results (All individuals and then by segment)
# in numeric form
print(coeff)
# in graphical form
barplot(coeff, col = colors()[c(640, 26, 552)], border = "white", font.axis = 2, beside = TRUE,
        legend = rownames(coeff), xlab = "Features", ylab = "Utility", font.lab = 2)
