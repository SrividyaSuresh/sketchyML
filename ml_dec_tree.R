#############################################################
# Machine Learning Workshop, St. Joseph's College, 01-03-2018
# Decision Tree on Advertising
#############################################################

# Install and Load Packages
# install.packages("tree")
library("tree")

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)

# Basic exploration
dim(advData)
names(advData)
str(advData)
head(advData)
summary(advData)

# Build a Regression Tree
treeFit <- tree(Sales ~ TV + Radio + Newspaper, data = advData)

# Visualize the tree
plot(treeFit)
text(treeFit, pretty = FALSE)

# Check the output
summary(treeFit)
treeFit