# *****************************************************************************
#       Agglomerative Hierarchical Clustering: Introductory Example
# *****************************************************************************
# Loading the necessary packages
library("readxl")
# Initialization (clearing the entire workspace)
rm(list=ls())
# Loading the data
data_hac <- read_excel("MDA_data.xlsx", sheet = "HAC")
# Converting data to a data frame and setting row names
data_hac <- data.frame(data_hac, row.names = 1)
# Calculating distances between different data points
hac.dist <- dist(data_hac, method = "euclidean")^2 / (2 * nrow(data_hac))
print(hac.dist)
# Implementing agglomerative hierarchical clustering 
# based on the distance matrix
hac.res <- hclust(hac.dist, method = "ward.D")
# Displaying the within-cluster loss of inertia
# in numeric form
print(hac.res$height)
# Displaying the within-cluster loss of inertia
# as a barplot
barplot(hac.res$height)
# Displaying the dendrogram
plot(hac.res)
# History of cluster merging
hac.res$merge
# Cutting the dendrogram into 'k' clusters (in this case, k=3)
seg <- cutree(hac.res, k = 3)
# Adding cluster membership to the original data
data_hac <- cbind(data_hac, seg)
# Calculating the number of observations in each cluster
# Here, it represents the number of individuals in each segment
table(seg)





