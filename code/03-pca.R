#lets explore the potential for dimension reduction

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
