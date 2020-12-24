#############################################################
# Machine Learning Workshop, St. Joseph's College, 01-03-2018
# Linear Regression on Advertising
#############################################################

# Install and Load Packages
# install.packages("corrplot")
# install.packages("scatterplot3d")
library(corrplot)
library(scatterplot3d)

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)

# Basic exploration
dim(advData)
names(advData)
str(advData)
head(advData)
summary(advData)

# Let us consider Sales to be our Dependent variable.
# TV, Radio and Newspaper are the independent variables.

# Exploring Sales
boxplot(advData$Sales, horizontal=TRUE, 
        col = "lightblue", main = "Boxplot of Sales")
hist(advData$Sales, prob = TRUE, 
     col = "lightblue", main = "Histogram of Sales")
lines(density(advData$Sales), col = "red", lwd = 3)

# Exploring each Feature
boxplot(advData$TV, advData$Radio, advData$Newspaper, horizontal=TRUE, 
        col = "lightblue", main = "Boxplot")

# Sales vs TV Advertisement
plot(advData$TV, advData$Sales, pch = 19, col = "red",
     main = "TV vs Sales", xlab = "TV Advertisement", ylab = "Sales")

# Sales vs Radio Advertisement
plot(advData$Radio, advData$Sales, pch = 19, col = "red",
      main = "Radio vs Sales", xlab = "Radio Advertisement", ylab = "Sales")
     
# Sales vs Newspaper Advertisement
plot(advData$Newspaper, advData$Sales, pch = 19, col = "red",
     main = "Newspaper vs Sales", xlab = "Newspaper Advertisement", ylab = "Sales")
     
# Pairwise Plots
pairs(advData, pch = 19, col = "red")

# Correlation Plots
corrplot.mixed(cor(advData))
# We observe the condensed correlation plots 
# with their corresponding correlation coefficients.

# Simple linear regression

# Sales vs TV Advertisement
plot(advData$TV, advData$Sales, pch = 19, col = "blue",
     main = "TV vs Sales", xlab = "TV Advertisement", ylab = "Sales")
model1 <- lm(Sales ~ TV, data = advData)
summary(model1)
model1$coefficients                                   # alpha and beta
abline(model1$coefficients, col="black", lwd=3)       # line of y = alpha + (beta*x)

# Sales vs Radio Advertisement
plot(advData$Radio, advData$Sales, pch = 19, col = "blue",
     main = "Radio vs Sales", xlab = "Radio Advertisement", ylab = "Sales")
model2 <- lm(Sales ~ Radio, data = advData)
summary(model2)
model2$coefficients                                   
abline(model2$coefficients, col="black", lwd=3)      

# Sales vs Newspaper Advertisement
plot(advData$Newspaper, advData$Sales, pch = 19, col = "blue",
     main = "Newspaper vs Sales", xlab = "Newspaper Advertisement", ylab = "Sales")
model3 <- lm(Sales ~ Newspaper, data = advData)
summary(model3)
model3$coefficients                                   
abline(model3$coefficients, col="black", lwd=3)    

# Multiple Linear Regression

# Sales vs TV and Radio Advertisement
scatterplot3d(advData$TV, advData$Radio, advData$Sales, angle = 45,
              pch = 19, highlight.3d = T, box = T,
              xlab = "TV", ylab = "Radio", zlab = "Sales")

lmFit1 <- lm(Sales ~ TV + Radio, data = advData)
summary(lmFit1)
lmFit1$coefficients
plot3d <- scatterplot3d(advData$TV, advData$Radio, advData$Sales, angle = 45,
                        pch = 19, highlight.3d = TRUE, box = TRUE,
                        xlab = "TV", ylab = "Radio", zlab = "Sales")
plot3d$plane3d(lmFit1$coefficients)

# Multiple linear regression with all features
lmFit2 <- lm(Sales ~ TV + Radio + Newspaper, data = advData)
summary(lmFit2)

# Insights from the model:

################################################################################
# About the dataset
# Dataset taken from An Introduction to Statistical Learning, with applications in R, by T. Hastie and R. Tibshirani
