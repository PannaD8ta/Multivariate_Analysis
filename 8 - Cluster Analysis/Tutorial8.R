# Install / Load the factoextra package (this is used to generate cluster plots)
install.packages("factoextra")
library(factoextra)

# Install / Load the cluster package (this is used to determine K)
install.packages("cluster")
library(cluster)

# Load the data (this is a base data set for R)
data("USArrests")

# Show the data
View(USArrests)

# Define (I like to use df) and Standardize the data
df <- scale(USArrests)

# Show the first 10 rows (round to 2 decimal places)
round(head(df, n = 10),2)

# Set a seed so that we can reproduce the same results
set.seed(123)

# Compute the gap statistic
## This function will conduct a k-means cluster analysis using the gap statistic method 
## (with 25 random starting points) using 500 bootstrap samples
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 500)

# Visualise the gap statistic plot
fviz_gap_stat(gap_stat)

# Compute k-means with K = 4
km.res <- kmeans(df, 4, nstart = 25)

# Print the k-means results
print(km.res)

# View the cluster means by the original data
round(aggregate(USArrests, by=list(cluster=km.res$cluster), mean),2)

# View which US state was assigned to which cluster
dd <- cbind(USArrests, cluster = km.res$cluster)
show(dd)

# Visualise the clustering
fviz_cluster(km.res, data = df)

# Visualise the clustering (enhanced)
fviz_cluster(km.res, data = df, palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
