#load libraries using pacman
#install.packages("pacman")
pacman::p_load(tidyverse, skimr, corrplot, ggplot2, GGally)

train <- read.csv("data/patient-data-train.csv") #read train data

str(train) #first glimpse on variables
skim(train) #simple statistics

#explore correlations
#plot the data to check for unusual features (particularly outliers and skewness)
pairs(train[,c(6:10)], pch = 20, lower.panel = NULL) #focus on scores
#scatter plots did not reveal any obvious outliers 
#there seem to be some linear relationships between pairs of variables

#lets refine the graph
#Coloring plots by Class helps identify potential outliers
ggpairs(train[, c(6:10)], ggplot2::aes(colour = factor(train$Class))) #using ggplot2 and GGally
#using standard pairs function
pairs(train[,c(6:10)], #main = "Title",
      pch = 21, bg = c("red", "blue")[factor(train$Class)],
      lower.panel = NULL)

#identify possible outliers 
#plot(train$Nscore, train$Escore, xlab = "Nscore", ylab = "Cscore",
#     pch = 21, bg = c("red", "blue")[factor(train$Class)])
#identify outliers
#outlier <- identify(cbind(train$Nscore, train$Cscore))
#probably do this with box plots

#looking into correlations
m <- cor(train[,c(2:13)]);corrplot(m, method = "number", type = "upper", diag = F) #calculate correlation matrix and plot correlogram
#strong correlation (0.63) between Impvulsive and SS
#other correlations in the weak and moderate range
#this suggest that there is some degree of redundancy in the information between the variables
#and therefore, some dimension reduction might be possible

#looking into variances
round(diag(var(train[, c(2:13)])),2) #variances
#variances are fairly similar across all variables
#if we were to perform some dimensional reduction (PCA), we could proceed using the variances

#That is because PCA can be sensitive to scale differences in the variables, 
#e.g. variables with high variance compared to the others, will be over-represented in the PCA 

#Therefore, when the variables are measured in different units or the variances are very
#different over different variables, it is wise to perform PCA on the correlation matrix rather than the covariance matrix.
#This is equivalent to standardizing the variables.

#In this case all variables use the same unit (standardized) - need to double check in the original paper?
