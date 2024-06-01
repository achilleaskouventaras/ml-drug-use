
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
CrossTable(train$Class, train$Impulsive, digits = 2,
           prop.r = F, prop.t = F, prop.chisq = F)
#check CrossTable documentation to understand the package and use in the future
# https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html


#library(rpart);library(rpart.plot) #load the rpart package
first_tree <- rpart(Class ~ .,
                    data = train, method = "class") #use method = "class" for factors 
                    #gini index as default for splitting

#i.e. for a classification tree rather than a regression tree
rpart.plot(first_tree, type = 2, extra = 4)

#sanity check
#we double check the 24% misclassification rate for Oscore with the following code:
table(train[train$Oscore < -0.52, "Class"])/sum(train$Oscore < -0.52)


#### Number of observations in a node and splitting criteria ####

#In general, we could have used more variables and rpart could still have shown the same tree, as before, as an output.
# One of the reason why this may occur is because rpart has some default parameters 
#that may prevent the tree from growing. These are:
  # minsplit (the minimum number of observations that must exist in a node in order for a split to be attempted)
  # minbucket (the minimum number of observations in any terminal node).

#By default minsplit is equal to 20 and
#minbucket is equal to the rounded value of one third of the value for minsplit.

#Furthermore, rpart uses the Gini impurity measure to select splits when performing classification. 
#If you want, you can use information gain instead by specifying it in the parms argument.

second_tree <- rpart(Class ~ .,
                     data = train, method = "class",
                     parms = list(split = "information"))
rpart.plot(second_tree,type=2,extra=4)

#Entropy is a measure of how mixed up the data is. 
#In other words, it tells you how uncertain you are about the value 
#of the target variable for any given data point.
#The higher the entropy, the more mixed up the data is.

#Information gain is a measure of how much a question splits the data up.  
#When you ask a question about the data, it splits the data into groups that (hopefully) 
#are more pure than the original data. 
    #We iterate through each feature in the data set and split the data based on its different values.
    #Entropy After Split: For each split, we calculate the entropy (measure of uncertainty) of the resulting subsets.
    #Information Gain Calculation: We subtract the combined entropy of the subsets from the initial entropy of the entire data set. 
    #This difference represents the information gain achieved by splitting on that particular feature.
    #Feature Selection: The feature with the highest information gain is chosen as the root node because
    #it leads to the most significant reduction in uncertainty about the target variable.
#In other words, information gain measures how much the entropy of the data goes down after you ask a question. 
#The higher the information gain, the more the question helps you figure out the target variable.
#In tree classification, the feature with the highest information gain #is chosen as the root node of the decision tree.

#Entropy and information gain aim to minimize uncertainty, 
#while the Gini Index aims to maximize the purity of the splits.

#choosing the minsplit parameter
third_tree <- rpart(Class ~ .,
                    data = train, method = "class", minsplit = 100)
rpart.plot(third_tree, type = 2, extra = 4)
#It makes sense that this is a smaller tree, compared to the first_tree, 
#since we only allow splits to happen when there are at least 100 observations in that node.
#The default minsplit in rpart is 20 observations in a node
