# **************************************************************************************************
#                                 The PDA Case
# **************************************************************************************************

# Clear the current workspace to remove any existing objects
remove(list = ls())

# Read data from a CSV file
pda_data <- read.csv("pda.csv")

# Read the data
head(pda_data)

# Segmentation is performed considering only the data related to 
# consumers' "needs"
data_hac <- pda_data[, 2:12]
summary(data_hac)

# Handling missing data in the "Monthly" variable
# Replace with the mean
# data_hac$Monthly[is.na(data_hac$Monthly)] <- mean(data_hac$Monthly, na.rm = TRUE)
# Replace with the value 0 
# Here, the absence of response may indicate a preference not to pay 
# for a monthly subscription... So we replace it with 0
data_hac$Monthly[is.na(data_hac$Monthly)] <- 0
# Standardize the data due to heterogeneity of units of measure
data_cluster <- scale(data_hac)

# Calculate distances between different data points
hac.dist <- dist(data_cluster, method = "euclidean")^2 / (2 * nrow(data_cluster))

# Implement hierarchical agglomerative clustering 
# using the distance matrix
hac_res <- hclust(hac.dist, method = "ward.D")
# Display the dendrogram
plot(hac_res)
# Display the history of associations
hac_res$merge

# Plot the inter-cluster inertia losses
# in numerical form
print(hac_res$height)

# in histogram form
barplot(hac_res$height)




# Number of clusters chosen
nb_clusters <- 3

# Cut groups based on the evolution of the parameter 
# height (similar to loss of inter-class inertia)
# and the dendrogram into 3 segments.
hac_seg <- cutree(hac_res, k = nb_clusters)
print(hac_seg)

# Calculate the count for each group
# here, Number of individuals per segment
table(hac_seg)

# Create a table with the variable "seg" (segment membership of each individual) added
# cbind: "c" for column and "bind" for bind.
pda_data <- cbind(pda_data, hac_seg)

# In order to save a file for another software
# write.csv(pda_data, file='output_HAC.csv')

# Segment summary by calculating the mean for each variable
data_seg <- aggregate(. ~ hac_seg
                      , data = pda_data[, c(-1, -26)], FUN = mean)

# Graphical summary of different expectations for each segment
barplot(as.matrix(data_seg[2:12]), col = colors()[c(50, 30, 142)], 
        border = "white", font.axis = 2, beside = T,
        legend = rownames(data_seg), xlab = "Questions X", font.lab = 2)

# Graphical summary of different profiles for each segment
barplot(as.matrix(data_seg[c(13:25)]), col = colors()[c(50, 30, 142)], 
        border = "white", font.axis = 2, beside = T,
        legend = rownames(data_seg), xlab = "Questions Z", font.lab = 2)

# Initial data is centered and scaled to obtain synthetic and readable plots
pda_data[, 2:25] = scale(pda_data[, 2:25])

data_seg <- aggregate(. ~ hac_seg
                      , data = pda_data[, c(-1, -26)], FUN = mean)

# Graphical summary of different expectations for each segment
barplot(as.matrix(data_seg[2:12]), col = colors()[c(50, 30, 142)], 
        border = "white", font.axis = 2, beside = T,
        legend = rownames(data_seg), xlab = "Questions X", font.lab = 2)

# Graphical summary of different profiles for each segment
barplot(as.matrix(data_seg[c(13:25)]), col = colors()[c(50, 30, 142)], 
        border = "white", font.axis = 2, beside = T,
        legend = rownames(data_seg), xlab = "Questions Z", font.lab = 2)
