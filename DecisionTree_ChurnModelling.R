getwd()
setwd("C:/Users/Canbek/Desktop/Week5")

#installing the essential packages and libraries
install.packages("party")
library (tree)

#reading the data file
df <- read.csv("Churn_Modelling.csv", header = TRUE)
head(df)

#drop the unnecessary columns for the analysis
df <-df [, -c(1:3)]
head(df)

#if the exited column equals to 0,it means the customer is not likely to churn
churn = ifelse(df$Exited ==0,"No","Yes")

#adding our dependent variable to dataframe
df = data.frame(df,churn)
head(df)

#calculating how many rows we have
n_rows <-nrow(df)

##################################################################################

#developing the prediction model

#setting the seed number
set.seed(123)

#getting a sample of data rows, %80 for training dataset, %20 for test dataset
t_rows = sample(1:10000, 8000)
train_df = df[t_rows,]
test_df = df[-t_rows,]

#constructing the model using training dataset. Excluded Exited and churn columns because they are dependent variables of the classification model
tree_model = tree(formula = churn ~ .-Exited-churn, data = train_df)

#misclassification error calculated as 0.147 which equals to 1176 misclassified predictions from 8000 rows which is pretty good
summary(tree_model)

#plot of the classification tree 
plot(tree_model)
text(tree_model,pretty = 0)

#using the classification model to predict on testing data
testpredictions = predict(tree_model,test_df,type = "class")
#1730 "No" and 270 "Yes" prediction from 2000 rows
summary(testpredictions)

#constructing the confusion matrix and calculating the accuracy score of our model for testing data.
cm <- table(testpredictions,test_df$churn)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#Accuracy score calculated as 0.8565 with 1520 true "No" prediction and 193 true "Yes" Prediction from 2000 rows.
print(accuracy)

##################################################################################

# k-fold cross validation: i chose the k number as 10 

#implementing a vector to store all 10 accuracy score
acc <- rep(0,10) 

#for loop to construct the tree model 10 times 
for (i in 1:10) {
  #take 10 different sample from data
  t_rows_cv = sample(1:10000, 8000)
  
  #divide the data 10 times to training and test datasets
  train_cv = df[t_rows_cv,]
  test_cv = df[-t_rows_cv,]
  
  #construct the model 10 times with different training datasets
  tree_model_cv = tree(formula = churn ~ .-Exited-churn, data = train_cv)
  
  #use the model 10 times to predict 10 different testing datasets
  testpredictions_cv = predict(tree_model_cv,test_cv,type = "class")
  
  #construct the confusion matrix 
  cm_cv <- table(testpredictions_cv,test_cv$churn)
  
  #calculate the accuracy score 10 times for all indices of vector
  acc[i] = (cm_cv[1,1] + cm_cv[2,2]) / (cm_cv[1,1] + cm_cv[2,2] + cm_cv[1,2] + cm_cv[2,1])
  
}

#print the vector indices which are the different accuracy scores we calculated from cross validation which are ranged between 0.8300 and 0.8535
print(acc)

#calculated the mean of these accuracy scores as 0.8435.
mean(acc)

#As a result of mean(acc) we could validate that our model is reliable. With k-fold cross validation we tested the effectiveness of our model.
