
#lets explore the potential for dimension reduction

train <- read.csv("data/patient-data-train.csv") #read train data

#firstly, we can perform some basic variable selection
head(train, 5) #variable X can be dropped
df <- train[,c(2:13)] #create a new data frame to work with (X is dropped)
summary(df) #summary statistics

#Principal Component Analysis (PCA) is a technique for continuous data 
#that takes p different linear combinations of the p original variables 
#to create p new variables, called components. 
#These components are constructed to be uncorrelated with one another.

#We then select the number of components that describe the data best. 
#The golden rule when using PCA to reduce dimensionality is:
#Variation = Information

#Our goal, therefore, is to reduce the dimensionality of the data 
#while still retaining most of the information contained in the original data.


#From EDA we saw moderate to strong correlation between some variables.
#This suggest that some dimension reduction might be possible.
#We also saw that variances are reasonably similar across all variables.
#Therefore we will proceed with PCA using the covariance matrix.
#If high variances show high differences, 
#we use the correlation matrix to standardize the data.

df.pca <- princomp(df, cor = F)
df.pca #Since we had 13 variables to start with, we get 13 components. 
#There are three methods we wish to use to decide on how many components to retain.


#### Method 1: Proportion of variance ####

#Let’s say we wanted to decide on this based on proportion of variance and we wanted to have at least 90% of the original variability explained. 
#We need to see the cumulative proportions of variance for all the components:
summary(df.pca)
#We can see here that if we wanted at least 90%, the smallest number of components that will give us at least 90% is 8 (as 7 only explain around 87%).


#### Method 2: Cattell's Method ####

#To use this method, we need to 
#produce a scree plot of the proportion of variance versus component number. 
#What we are looking for in the plot is a bend, where the rate of decrease suddenly reduces. 
#The component number at the bend is the number we look to retain. 

#This method uses plot assessment, and it is quite subjective and not unusual for different people to see different answers. 

plot(df.pca) #there are a couple of different transitions that we could say
#represent a change in rate of decrease: 
#between components 3 and 4, or 6 and 7, or 8 and 9.
#If we wanted to really reduce the dimensionality we could argue the first one 
#and retain only 3 components.


#### Method 3: Kaiser's Method ####

#To use this method, we need to 
#find the average eigenvalue to discover which set of components
#have variation above it that we will retain. 

sd.pca <- df.pca$sdev #Extract the component standard deviations
#to find the average variance, take the mean after squaring them
ave.var <- mean(sd.pca^2) 
ave.var #0.752

#To which components that have higher than average variance (TRUE)
sd.pca^2 > ave.var #We would retain the first 3 PCs.


#### Loadings ####

#Lets stick with Kaiser's method (keep first 4 components).

#Lets see if there is any interpretation that can be attached to the loadings 
#of the PCA for our patient data. 
#To do that, we extract the loadings (or rotation) matrix from the fitted object.
df.pca$loadings[, 1:4]

#Interpreting these can be something of an art. 
#We look at each column in turn, look for variables with reasonably large positive values, 
#group them together and then look for variables with reasonably large negative values and group them together. 
#The PC is interpreted as being the contrast between these groups of variables. 
#If all variables of reasonable size loading have the same sign 
#then that PC is interpreted as the (weighted) average or overall score of these variables.
#Βlanks indicate zeros, i.e. variables with no weight in that particular component. 

#For Component 1,  We can see that there are two sets of variables with reasonable size weights here, 
#ones with pluses and ones with minuses so, 
#we would interpret this component as the difference between 
#the average of Age, Education, Country, Ascore, and Cscore
#and the average of Nscore, Oscore, Impulsive, SS, and Class
#We can do the same with all other components that we have decided to retain.

#We are usually interested in examining what the data look like in the new reduced space.
#The scores for the PCs on the original data are automatically produced by princomp. 
#So we can look at the pairs plot of the PCs we decided to retain.
scores.train <- df.pca$scores[, 1:4]
pairs(scores.train, pch = 20, lower.panel = NULL)

#In the new space our data look like a cloud of points that no longer 
#follow a line but just look like a random scatter of points centered at
#the origin (0,0) in the new space. PCs are uncorrelated (scattered points don't concentrate along the diagonals)
#For the most part, it Looks like the data describe a single homogeneous population.
#There may also be a few of outliers. 
#We could iterate this process by removing the outliers from the data and running PCA again. 
#Often PCA is an iterative process, rather than a one and done kind of deal.


