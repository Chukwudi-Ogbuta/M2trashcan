# ******************************************************************************
#                            G20 Model Case Study
# ******************************************************************************
library("FactoMineR")
library("factoextra")
# Initialization (clearing all elements from memory)
rm(list=ls())
#setwd("~/Documents/Cours/Ressources/Textes/Etudes de Cas/Cas G20/")

# Loading data
# The 1st variable becomes the row name
data_g20 <- read.csv("g20.csv", dec = ",", row.names = 1)


# Creating the data table containing
# only consumer preferences
data_hac <- as.data.frame(t(data_g20[, 16:90]))
# Calculating distances between different points
hac.dist <- dist(data_hac, method = "euclidean")^2 / (2 * nrow(data_hac))

# Implementing hierarchical agglomerative clustering
# based on the distance matrix
hac.res <- hclust(hac.dist, method = "ward.D")

# Evolution of inter-class inertia losses
# in numerical form
print(hac.res$height)

# as a histogram
barplot(
  hac.res$height,
  ylab = "HEIGHT Measure (relative to Inter-Class Inertia Loss)",
  xlab = "Number of groups",
  names.arg = c(74:1)
)

# Displaying the dendrogram
plot(hac.res)

grp <- cutree(hac.res, k = 3)

results <- colMeans(data_hac)
results <- rbind(results, colMeans(data_hac[grp == 1, ]))
results <- rbind(results, colMeans(data_hac[grp == 2, ]))
results <- rbind(results, colMeans(data_hac[grp == 3, ]))

rownames(results) <- c('All', 'Seg1', 'Seg2', 'Seg3')

data_acp <- cbind(data_g20, t(results))

# PCA with additional variables (segments)
res_pca <- PCA(
  data_acp[c(1:15, 91:94)],
  scale.unit = TRUE,
  quanti.sup = c(16:19),
  graph = TRUE
)
print(res_pca$ind)

# Biplot Variables and Objects
biplot(res_pca$ind$coord[, 1:2], res_pca$var$coord[, 1:2])
abline(h = 0, v = 0)

# Biplot Variables and Objects
# and additional variables to be represented only
fviz_pca_biplot(res_pca)

