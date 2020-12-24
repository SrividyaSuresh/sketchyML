###############################################################
# Machine Learning Workshop, St. Joseph's College, 01-03-2018
# Principal Component Analysis on Iris
###############################################################

# Install and Load Packages
# install.packages("ggfortify")
# install.packages("pca3d")
library(pca3d)
library(ggfortify)
library(ggplot2)
library(Rcpp)
library(digest)

# Exploring the dataset
help(iris)
dim(iris)
str(iris)
summary(iris)
attach(iris)

# Convert and visualize the dataset
IData <- iris 
IData$Species <- NULL 
pairs(IData, pch = 19, col = as.numeric(iris$Species))

# Perform Principal Component Analysis
irisPCA <- prcomp(IData, center = TRUE, scale. = TRUE)
summary(irisPCA)
irisPCA

# Projection on Comp.1 and Comp.2
autoplot(irisPCA, col = as.numeric(iris$Species),
         loadings = TRUE, loadings.label = TRUE)

# 3-D Visualisation
pca3d(irisPCA, group = iris$Species)
pca2d(irisPCA, group=iris$Species)
