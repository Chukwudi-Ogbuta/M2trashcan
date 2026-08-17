# ********************************************************************************************
#                                Principal Component Analysis with FactoMineR
# ********************************************************************************************

# Activation of necessary packages
library(FactoMineR)

# Initialization (clearing all elements from memory)
rm(list=ls())

# Loading data
# Row names in the first column
data_pca <- read.csv("cars.csv", row.names = 1)

# Displaying the data
print(data_pca)
cor(data_pca)

# Implementation of Principal Component Analysis
res_pca <- PCA(data_pca, scale.unit = TRUE, graph = TRUE)

# Displaying eigenvalues
print(res_pca$eig)
# Scree plot of eigenvalues
plot(res_pca$eig[,1], type="b")
abline(1, 0)

# Displaying correlations between original variables and new variables
print(res_pca$var$cor)
# Displaying coordinates of individuals
print(res_pca$ind$coord)
# Displaying contributions of different objects to the emergence of new variables
print(res_pca$ind$contrib)
# Displaying squared cosines to assess the good representation of individuals
print(res_pca$ind$cos2)

# Biplot Variables and Objects
biplot(res_pca$ind$coord[,1:2], res_pca$var$coord[,1:2])
abline(h=0, v=0)


# Implementation of varimax rotation for the first 2 axes
sol_varimax <- varimax(res_pca$var$cor[,1:2])


# Synthesis ability of the first 2 components before 
print(res_pca$var$cor[,1:2])
print(res_pca$eig[1:2,])

# and after rotation
print(sol_varimax$loadings)

# Biplot before and after rotation
biplot(res_pca$ind$coord[,1:2], res_pca$var$coord[,1:2])
abline(h=0, v=0)

biplot(res_pca$ind$coord[,1:2] %*% sol_varimax$rotmat, sol_varimax$loadings)
abline(h=0, v=0)

