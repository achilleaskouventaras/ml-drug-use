
#Classification trees are a type of nonparametric classification and partitioning method. 
#They are are intuitive concepts for making decisions. 
#They work by splitting the observations into a number of regions, 
#and predictions are made based on the mean or mode of the outcome variable
#on the training observations in that region.

#Tree-based modeling is a technique for:
#1. Devising prediction rules that are easily interpretable (particularly by non-data scientists).
#2. Screening variables.
#3. Summarising large data sets.

#Tree-based classification methods have several advantages since they:
#• make no distributional assumptions;
#• allow for a flexible form of the input variable 
#(i.e. allow for interaction effects and monotonic transformations automatically);
#• automatically deal with missing data.

#Trees can be presented as diagrams or as pseudo-code via text

#load libraries using pacman
#install.packages("pacman")
pacman::p_load(gmodels, ggplot2, ggridges, rpart, rpart.plot)


#read the data
train <- read.csv("data/patient-data-train.csv") #read train data
head(train, 5) #variable X can be dropped
train <- train[,c(2:13)] #create a new data frame to work with (X is dropped)

#library(gmodels)
CrossTable(train$Class, train$Impulsive, digits=2,
           prop.r=F, prop.t=F, prop.chisq=F)
#check CrossTable documentation to understand the package and use in the future
# https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html


#library(rpart);library(rpart.plot) #load the rpart package
first_tree <- rpart(Class ~ .,
                    data = train, method = "class") #use method="class" for factors
#i.e. for a classification tree rather than a regression tree
rpart.plot(first_tree, type = 2, extra = 4)




