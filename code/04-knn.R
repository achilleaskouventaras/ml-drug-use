
#load libraries using pacman
#install.packages("pacman")
pacman::p_load(class)


#read the data
train <- read.csv("data/patient-data-train.csv") #read train data
head(train, 5) #variable X can be dropped
train <- train[,c(2:13)] #create a new data frame to work with (X is dropped)

valid <- read.csv("data/patient-data-valid.csv") #read train data
valid <- valid[,c(2:13)] #create a new data frame to work with (X is dropped)

test <- read.csv("data/patient-data-test.csv") #read train data
test <- test[,c(2:13)] #create a new data frame to work with (X is dropped)


# fit k-nearest neighbours models for a variety of k
corr.class.rate <- numeric(200) #values of k

for(k in 1:200)
{
  pred.class <- knn(train[, -12], valid[, -12], train[, 12], k = k)
  corr.class.rate[k] <- sum((pred.class == valid$Class))/length(pred.class)
}

plot(c(1:200), corr.class.rate, type = "l",
     main = "Correct Classification Rates for the validation data for a range of k",
     xlab = "k", ylab = "Correct Classification Rate", cex.main = 0.7)

#evaluate  performance on the validation data to select the optimal value of k
corr.class.rate #correct classification rate
which.max(corr.class.rate) #identify max correct classification rate 
#107


#### Prediction ####

pred <- knn(train[,-12], test[,-12], train[,12], k = 107)
#estimate of the future performance for a model with the optimal k using the test data
sum((pred == test$Class))/length(pred) #0.788 correct classification rate
