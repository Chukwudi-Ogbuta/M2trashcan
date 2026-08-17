# ******************************************************************************
# Principal Component Analysis with FactoMineR
# ******************************************************************************

# Activating the necessary packages
library(FactoMineR)

# Initialization (clearing all elements from memory)
rm(list=ls())

# Loading the data
data_acp <- read.csv("4P.csv", row.names = 1)

# Displaying the data
print(data_acp)
cor(data_acp)

# Implementation of Principal Component Analysis
res_pca <- PCA(data_acp, scale.unit = TRUE, graph = TRUE, axes = c(1, 2))


# Displaying eigenvalues
print(res_pca$eig)

# Scree plot of eigenvalues
plot(res_pca$eig[,1], type = "b")
abline(h = 1, col = "red")

# Displaying correlations between original variables and new variables
print(res_pca$var$cor)

# Displaying individual coordinates
print(res_pca$ind$coord)

# Displaying individual contributions to the principal components
print(res_pca$ind$contrib)

# Displaying squared cosines to evaluate the representation of individuals
print(res_pca$ind$cos2)

# Biplot variables and objects
biplot(res_pca$ind$coord[,1:2], res_pca$var$coord[,1:2])
abline(h=0, v=0)
