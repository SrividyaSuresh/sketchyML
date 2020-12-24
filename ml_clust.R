#############################################################
# Machine Learning Workshop, St. Joseph's College, 01-03-2018
# K-Means Clustering Analysis on Iris
#############################################################

# Install and Load packages
# install.packages("ggplot2")
# install.packages("animation")
library(ggplot2)
library(animation)

# Exploring the dataset
help(iris)
dim(iris)
str(iris)
summary(iris)
attach(iris)
plot(Sepal.Length, Petal.Length, col = "red", pch = 19, 
     main = "Iris", xlab = "Sepal Length", ylab = "Petal Length")

# Creating dataset for cluster analysis
IData <- iris 
IData$Species <- NULL 
str(IData)
summary(IData)

# K-Means
kfit <- kmeans(IData, 3) 
kfit
kfit$cluster <- as.factor(kfit$cluster)
(t <- table(iris$Species,kfit$cluster))  # let's cross-verify with the answer
plot(t, col = c("pink", "lightblue", "lightgreen"))
ggplot(iris, aes(Sepal.Length, Petal.Length, color=kfit$cluster)) + geom_point() # not-boring proof
ggplot(iris, aes(Sepal.Length, Petal.Length, color=iris$Species)) + geom_point()
ggplot(iris, aes(Sepal.Length, Petal.Width, color=kfit$cluster)) + geom_point()
ggplot(iris, aes(Sepal.Length, Petal.Width, color=iris$Species)) + geom_point()

# Let's Animate it
ani <- iris[,c(1,3)]
kmeans.ani(ani,centers=3)
