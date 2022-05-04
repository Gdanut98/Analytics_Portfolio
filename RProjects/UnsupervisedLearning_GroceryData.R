## Unsupervised Learning
# Association Rules
library(arules)
data("Groceries")

# run summary report
summary(Groceries)

x <- Groceries[size(Groceries) > 25]
inspect(x)

# Checking which items are important in the dataset with 10% frequency
itemFrequencyPlot(Groceries, support = 0.1, cex.names = 0.8)

# run the apriori algorithm ith a minimum support of 0.3% and a confidence of 0.5.
basket_rules <- apriori(Groceries, parameter = list(sup = 0.003, conf = 0.5, target = "rules"))

summary(basket_rules)

# check the generated rules using inspect()
inspect(head(sort(basket_rules, by = "lift")))

# basket rules of size greater than 4
inspect(subset(basket_rules, size(basket_rules) > 4))

# Find the subset of rules with lift greater than 5
inspect(subset(basket_rules, lift > 5))

# Find the subset of rules that has yogurt in the consequent lift to be greater than 3.
yogurt.rhs <- subset(basket_rules, subset = rhs %in% "yogurt" & lift > 3)

inspect(yogurt.rhs)

# Find the subset rules that have meat in the antecedent
meat.lhs <- subset(basket_rules, subset = lhs %in% "meat" & lift > 1.5)
inspect(meat.lhs)

# Clustering
seed <- read.table("seeds_dataset.txt")
seed <- seed[, 1:7]
colnames(seed) <- c("area", "perimeter", "compactness", "length", "width", "asymmetry", "groovelength")

seed <- scale(seed)

# k-means cluster analysis
set.seed(1)
fit <- kmeans(seed, 5) # 5 cluster solution

# display the number of kernels in each cluster
table(fit$cluster)

# Plot the clusters
install.packages("fpc")
library(fpc)
plotcluster(seed, fit$cluster)

# Look at the first cluster
seed[fit$cluster == 1, ]
# Look at the center of each group
fit$centers

# Determine the number of clusters
wss <- rep(0, 12)
for (i in 1:12) {
  wss[i] <- sum(kmeans(seed, centers = i)$withinss)
}
plot(1:12, wss, type = "b", xlab = "Number of Clusters", ylab = "Within group sum of squares")

# Choosing the optimal number of clusters using the "elbow method" - in this case it's 3
set.seed(1)
fit1 <- kmeans(seed, 3) # 3 cluster solution

# display the number of kernels in each cluster
table(fit1$cluster)

plotcluster(seed, fit1$cluster)

fit1$centers

# Hierarchical Clustering
# calculate the distance matrix
seed.dist <- dist(seed)

# obtain clusters
seed.hclust <- hclust(seed.dist)
plot(seed.hclust, ann = FALSE)

# cut dendrogram at the three clusters level and obtain cluster membership
seed.3clust <- cutree(seed.hclust, k=3)
rect.hclust(seed.hclust, k=3)

# Check items in the 3rd group
seed[seed.3clust == 3, ]
# Find the center
aggregate(seed, by = list(seed.3clust), FUN = mean)

# Draw the clusters on a 2-D graph
library(fpc)
plotcluster(seed, seed.3clust)



