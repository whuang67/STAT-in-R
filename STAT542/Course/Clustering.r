# STAT / CES 542, Fall 2016
# This is the R code for lecture note Unsupervised


library(datasets)

# the famous iris data 

head(iris)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

# k mean clustering

iris.kmean <- kmeans(iris[, 3:4], centers = 3, nstart = 20, trace = TRUE) 

# the center of each class
iris.kmean$centers

# the within cluster variation 
iris.kmean$withinss

# the between cluster variation 
iris.kmean$betweenss

# plot the fitted clusters 

iris.kmean$cluster <- as.factor(iris.kmean$cluster)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
		geom_point(alpha = 0.4, size = 3.5) + # true cluster 
		geom_point(col = iris.kmean$cluster) + # fitted cluster 
		scale_color_manual(values = c('blue', 'red', 'green'))
		
		
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris.kmean$cluster)) + geom_point()


# hierarchical clustering
# the default is a complete link
# the dist() function computes pairwise distance using euclidean norm

iris.hclust <- hclust(dist(iris[, 3:4]), method = "complete")
plot(iris.hclust)

# choose a cutoff for the tree

hclust.cut <- cutree(iris.hclust, 3)
table(hclust.cut , iris$Species)

# use average link

iris.hclust <- hclust(dist(iris[, 3:4]), method = 'average')
plot(iris.hclust)

hclust.cut <- cutree(iris.hclust, 3)
table(hclust.cut , iris$Species)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
		geom_point(alpha = 0.4, size = 3.5) + # true cluster 
		geom_point(col = hclust.cut) + # fitted cluster 
		scale_color_manual(values = c('blue', 'red', 'green'))




