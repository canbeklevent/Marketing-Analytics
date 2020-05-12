############################### DATA PREPROCESSING ###########################################

getwd()
setwd("C:/Users/Dell/Desktop/Student Dataset")

md <- read.csv("student-mat.csv", header = T, sep = ";")
pd <- read.csv("student-por.csv", header = T, sep = ";")

#there are several students that belong to both datasets 
#These students can be identified by searching for identical attributes shown below in the merge function 
#that characterize each student
merged=merge(md,pd,by=c("school","sex","age","address","famsize","Pstatus",
                        "Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#head
head(md)
head(pd)
head(merged)

#to see the count of different values of columns in summary function, i used formatting to character typed columns
#integer values are excluded from factorization because they have more wide ranges
for (i in c(1,2,4:6,9:12,16:23)) {
  md[, i] <- as.factor(md[, i])
}

for (i in c(1,2,4:6,9:12,16:23)) {
  pd[, i] <- as.factor(pd[, i])
}

#dimensions
dim(md) #395 students with 33 attributes
dim(pd) #649 students 33 attributes
dim(merged) #382 students 53 attributes


#structure of data
str(md)
str(pd)
str(merged)


#Checking for NA values
any(is.na(md))
any(is.na(pd))
#checking for NA based on columns
sapply(md,function(x) sum(is.na(x)))
sapply(pd,function(x) sum(is.na(x)))

#summary of analytics
summary(md)
summary(pd)


####################################### DATA EXPLORATION ##############################################

#math students age plot for different genders
ggplot(aes(x=age, fill=sex), data=md)+
  geom_histogram(binwidth = 0.50)+
  ggtitle("Age of students for Math Course")

#Portuguese students age plot for different genders
ggplot(aes(x=age, fill=sex), data=pd)+
  geom_histogram(binwidth = 0.50)+
  ggtitle("Age of students for Portuguese Course")

#Math Course final grade(G3) histogram with mode and median values
hist(md$G3, main = "Histogram of Math Course Performance in G3", xlab = "Performance in G3", col = "darkgrey")
abline(v=mean(md$G3), col = "red")
abline(v=median(md$G3), col = "blue")
#labeling 
text(9, 15, "mean", col = "red")
text(9, 9, round(mean(md$G3), 2) , col = "red")
text(13, 15, "median", col = "blue")
text(13, 9, round(median(md$G3), 2) , col = "blue")

#Portuguese Course final grade(G3) histogram with mode and median values
hist(pd$G3, main = "Histogram of Portuguese Course Performance in G3", xlab = "Performance in G3", col = "darkgrey")
abline(v=mean(pd$G3), col = "red")
abline(v=median(pd$G3), col = "blue")
#labeling 
text(9, 15, "mean", col = "red")
text(9, 9, round(mean(pd$G3), 2) , col = "red")
text(13, 15, "median", col = "blue")
text(13, 9, round(median(pd$G3), 2) , col = "blue")

#Effect of parents education on children's final math score
md$Fedu=factor(md$Fedu)
mfedu <- ggplot(md, aes(x = Fedu, y = G3)) + 
  geom_boxplot(aes(fill = Fedu),alpha = .6,size = 1) + 
  scale_fill_brewer(palette = "Set2") + 
  stat_summary(fun = "mean", geom = "point", shape= 23, size= 3, fill= "white") +
  ggtitle("Grade distribution by father's education") + 
  theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank())

md$Medu=factor(md$Medu)
mmedu <- ggplot(md, aes(x = Medu, y = G3)) + 
  geom_boxplot(aes(fill = Medu),alpha = .6,size = 1) + 
  scale_fill_brewer(palette = "Set2") + 
  stat_summary(fun = "mean", geom = "point", shape= 23, size= 3, fill= "white") +
  ggtitle("Grade distribution by mother's education") + 
  theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank())

multiplot(mfedu, mmedu, cols =1)

#final grade(G3) histogram for Math and Portuguese courses distinction
library(tidyr)
merged %>%
  gather(`G3.x`, `G3.y`, key="course", value="grade") %>%
  ggplot() +
  geom_bar(aes(x=grade, fill=course), position="dodge") + 
  ggtitle("Distribution of final grades in Math and Portuguese courses") +
  scale_fill_discrete(name = "Course", labels = c("Math", "Portuguese"))

#School performance comparison for student's math and porteguese scores
library(ggplot2)
library(multiplot)
install.packages('Rmisc', dependencies = TRUE)

p1<-ggplot(data=merged, aes(x=school, y=G3.x))+
  geom_point(aes(color=school))+
  geom_boxplot(aes(color=school))+
  xlab('School')+
  ylab("Final Grade of Math")+
  geom_hline(yintercept=9,colour='grey20',size=0.5,linetype=2)+
  geom_hline(yintercept=11,colour='grey20',size=0.5,linetype=2)+
  geom_hline(yintercept=13,colour='grey20',size=0.5,linetype=2)+
  geom_hline(yintercept=15,colour='grey20',size=0.5,linetype=2)+
  annotate('text', x=2.5, y=c(8.5,10.5,12.5,14.5, 17), 
           label=c('F', 'D', 'C', 'B', 'A'), colour='red')+
  ggtitle("G3 in Math vs School")+
  theme_bw()+
  theme(legend.position="none")


p2<-ggplot(data=merged, aes(x=school, y=G3.y))+
  geom_point(aes(color=school))+
  geom_boxplot(aes(color=school))+
  scale_x_discrete(name='School')+
  scale_y_continuous(name="Final Grade of Portuguese")+
  geom_hline(yintercept=9,colour='grey20',size=0.5,linetype=2)+
  geom_hline(yintercept=11,colour='grey20',size=0.5,linetype=2)+
  geom_hline(yintercept=13,colour='grey20',size=0.5,linetype=2)+
  geom_hline(yintercept=15,colour='grey20',size=0.5,linetype=2)+
  annotate('text', x=2.5, y=c(8.5,10.5,12.5,14.5, 17), 
           label=c('F', 'D', 'C', 'B', 'A'), colour='red')+
  ggtitle("G3 in Portuguese vs School")+
  theme_bw()+
  theme(legend.position="none")

multiplot(p1, p2, cols =1)

install.packages("dplyr")

#as a passing criteria, we use median of final score
mdpass <- md %>% mutate(pass = ifelse(G3 > median(G3), 1, 0))%>%
  filter(sex=="F"|sex=="M")%>%
  group_by(sex)%>%
  summarise(Pass=sum(pass))

str(mdpass) #74 females, 88 males are passed

#in histogram
mdpass%>%
  ggplot(aes(x=sex,y=Pass))+
  geom_bar(stat="identity",  fill="steelblue")

#histogram of current performance Vs future goals of the student based on gender of the student
md%>%
  ggplot(aes(x=higher, y=G3))+
  geom_boxplot()+
  facet_grid(~sex)

#graphic of the relationship between health and attendance in class and the gender of the student
md%>%
  group_by(sex)%>%
  ggplot(aes(x=factor(health), y=absences, color=sex))+
  geom_smooth(aes(group=sex), method="lm", se=FALSE)

#Correlation matrix for numeric columns
mdnumcols <- sapply(md, is.numeric)
mdcordata <- cor(md[,mdnumcols])
corrplot(mdcordata, method="number")

pdnumcols <- sapply(pd, is.numeric)
pdcordata <- cor(pd[,pdnumcols])
corrplot(pdcordata, method="number")

################## LINEAR REGRESSION ###################
#I want to see the relationship between the absence of a student (between 0 days and 93 days) and other variables

library(caTools)
library(MASS)

set.seed(101)
sample <- sample.split(md$absences, SplitRatio = 0.7)

train <- subset(md, sample == T) #%70 of the df is in training data set
test <- subset(md, sample == F) #%30 of the df is in training data set

lin_model <- lm(absences ~., data = md) 
summary(lin_model) #adjusted r squared is 0.14

step1 <- stepAIC(lin_model, direction="both")
#according to stepwise selection, independent variables that significantly effect absences are
#school + sex + age + address + Pstatus + Medu + reason + 
#studytime + internet + romantic + Walc + G2 + G3

#use selected variables in linear model to see the effect on training data
lin_model2 <- lm(absences ~ school + sex + age + address + Pstatus + Medu + reason + studytime + internet + romantic + Walc + G2 + G3, data = train) 

summary(lin_model2) #adjusted r squared is 0.15 which is very low

res <- residuals(lin_model2)
summary(res)
res <- as.data.frame(res)
pl <- ggplot(res, aes(x = res)) + geom_histogram(fill = 'blue', alpha = 0.5)
print(pl)

abs.predictions <- predict(lin_model2, test)

results <- cbind(abs.predictions, test$absences)

colnames(results) <- c('predictions','correct values')

results <- as.data.frame(results)

#checking the performance of the model: mean squared error (instead of adjusted R square in Lm)
mse <- mean((results$`correct values` - results$predictions) ^ 2)
print(mse) #23.72

################## LOGISTIC REGRESSION ###################

#our dependent variable is 'pass' which indicates if the student passes the course
pass = ifelse((md$G3 + md$G2 + md$G1) / 3 > 10,1,0)
mdp = data.frame(md,pass)
mdp <- mdp[,-31:-33]

str(mdp)

log_model1 <- glm(pass ~ failures, data=mdp, family = "binomial")
summary(log_model1) #AIC 499.82

mdp$Walc <- factor(mdp$Walc)

log_model2 <- glm(pass ~ failures + Walc, data=mdp, family = "binomial")
summary(log_model2) #AIC 500.21

log_model3 <- glm(pass ~ failures + age, data=mdp, family = "binomial")
summary(log_model3) #AIC 501.8

log_model4 <- glm(pass ~ failures + school, data=mdp, family = "binomial")
summary(log_model4) #AIC 501.77 

log_model5 <- glm(pass ~ failures + school + sex, data=mdp, family = "binomial")
summary(log_model5) #AIC 496.98

log_model6 <- glm(pass ~ failures + school + sex + Walc, data=mdp, family = "binomial")
summary(log_model6) #AIC 494.74

log_model7 <- glm(pass ~ failures + school + sex + Walc + Fedu, data=mdp, family = "binomial")
summary(log_model7) #AIC 492.59

log_model8 <- glm(pass ~ failures + school + sex + Walc + goout, data=mdp, family = "binomial")
summary(log_model8) #AIC 492.32 which is the most optimal one


#individual odd ratios
round(exp(coef(log_model8)), 2) 

#Find pass value for failure=1, school = GP, sex=M, Walc=5, goout=4
passvalue <- coef(log_model8)[[1]]+coef(log_model8)[[2]]*1+coef(log_model8)[[3]]*0+coef(log_model8)[[4]]*1+coef(log_model8)[[5]]*5+coef(log_model8)[[6]]*4

probofpass<-round((exp(passvalue)/(exp(passvalue)+1)), 2) #32%

#Find pass value for failure=1, school = GP, sex=F, Walc=5, goout=4
passvalue2 <- coef(log_model8)[[3]]+coef(log_model8)[[2]]*1+coef(log_model8)[[3]]*0+coef(log_model8)[[4]]*0+coef(log_model8)[[5]]*5+coef(log_model8)[[6]]*4

probofpass2<-round((exp(passvalue2)/(exp(passvalue2)+1)), 2) #7%

#Find pass value for failure=0, school = GP, sex=M, Walc=5, goout=4
passvalue3 <- coef(log_model8)[[3]]+coef(log_model8)[[2]]*0+coef(log_model8)[[3]]*0+coef(log_model8)[[4]]*0+coef(log_model8)[[5]]*5+coef(log_model8)[[6]]*4

probofpass3<-round((exp(passvalue3)/(exp(passvalue3)+1)), 2) #57%

#Find pass value for failure=0, school = GP, sex=F, Walc=5, Fedu=1, goout=4
passvalue4 <- coef(log_model8)[[3]]+coef(log_model8)[[2]]*0+coef(log_model8)[[3]]*0+coef(log_model8)[[4]]*0+coef(log_model8)[[5]]*5+coef(log_model8)[[6]]*5+coef(log_model8)[[7]]*4

probofpass4<-round((exp(passvalue4)/(exp(passvalue4)+1)), 2) #41%


########################## K MEANS CLUSTERING ################################
getwd()
setwd("C:/Users/Dell/Desktop/Student Dataset")

math <- read.csv("student-mat.csv", header = T, sep = ";")
por <- read.csv("student-por.csv", header = T, sep = ";")

#there are several students that belong to both datasets 
#These students can be identified by searching for identical attributes shown below in the merge function that characterize each student
merge_df=merge(math,por,by=c("school","sex","age","address","famsize","Pstatus",
                             "Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

str(merge_df)

#Clusters will be implemented for the students that belong to both datasets that takes both math and portuguese lessons
#before doing k-means clustering, i need to normalize the variables that i want to include to the clustering algorithm

set.seed(123)
options(digits = 2)


#chosen columns on the analysis are the ones that do not change between math and portuguese data sets for the same student that also has type numeric
mydata <- merge_df[,c("age","Medu","Fedu","traveltime.x","famrel.x","freetime.x","goout.x","Dalc.x","Walc.x","health.x")]
str(mydata)

#scaling the data to represent them as z-scores

mydata$age<-scale(merge_df$age)
mydata$Medu<-scale(merge_df$Medu)
mydata$Fedu<-scale(merge_df$Fedu)
mydata$traveltime.x<-scale(merge_df$traveltime.x)
mydata$famrel.x<-scale(merge_df$famrel.x)
mydata$freetime.x<-scale(merge_df$freetime.x)
mydata$goout.x<-scale(merge_df$goout.x)
mydata$Dalc.x<-scale(merge_df$Dalc.x)
mydata$Walc.x<-scale(merge_df$Walc.x)
mydata$health.x<-scale(merge_df$health.x)


#As a first step, let's choose the cluster number as 4:
mydata_cluster<-kmeans(mydata, 4)

#Let's see how many students the clusters have 
mydata_cluster$size # size of clusters

mydata_cluster$centers #center of clusters
#1st cluster: 0.48 st deviation below from the mean of age, 0.8 st deviation below from the mean of medu etc..


#Now let's use the elbow criteria to find a better number of clusters to represent the data
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss) 
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}
wssplot(mydata, nc=10) #elbow point looks like 3 clusters

#Then applied 3 clusters to my data
optimal_cluster<-kmeans(mydata, 3)

summary(optimal_cluster)

attributes(optimal_cluster)

optimal_cluster$size

optimal_cluster$cluster #shows that which student belongs to which cluster among 382 students 

# Here i added cluster column of optimal_cluster as a new column to mydata to easily see which student belongs to which cluster
mydata<-cbind(mydata,Cluster=optimal_cluster$cluster)


install.packages("cluster")
library(cluster)

# plot of 3 clusters.
clusplot(mydata, optimal_cluster$cluster, main='Graph of the Student Clusters',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
#These two components explain 42.09% of the point variability

optimal_cluster$centers

########################## DECISION TREE ################################
getwd()
setwd("C:/Users/Dell/Desktop/Student Dataset")

df <- read.csv("student-por.csv", header = T, sep = ";")

for (i in c(1,2,4:6,9:12,16:23)) {
  df[, i] <- as.factor(df[, i])
}

str(df)


numeric <- sapply(df, is.numeric)
df_pass <- df[,numeric]

#i decided on a passing criteria which is taking the average of all 3 scores as passing score which needs to be above 10
df_pass$final <- NULL
df_pass$final <- factor(ifelse((df_pass$G3 + df_pass$G2 + df_pass$G1) / 3 > 10 , 1, 0),
                        labels = c("fail", "pass"))


#In order to construct the decision tree, we need to normalize the data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

df_pass$age <- normalise(df_pass$age)
df_pass$Medu <- normalise(df_pass$Medu)
df_pass$Fedu <- normalise(df_pass$Fedu)
df_pass$traveltime <- normalise(df_pass$traveltime)
df_pass$studytime <- normalise(df_pass$studytime)
df_pass$failures <- normalise(df_pass$failures)
df_pass$famrel <- normalise(df_pass$famrel)
df_pass$freetime <- normalise(df_pass$freetime)
df_pass$goout <- normalise(df_pass$goout)
df_pass$Dalc <- normalise(df_pass$Dalc) 
df_pass$Walc <- normalise(df_pass$Walc)
df_pass$health <- normalise(df_pass$health)
df_pass$absences <- normalise(df_pass$absences)

n_rows <-nrow(df_pass) #we will split 500 of them as training data and 149 as test data

#setting the seed number
set.seed(123)

t_rows = sample(1:649, 500)
train_df = df_pass[t_rows,]
test_df = df_pass[-t_rows,]

#constructing the model using training dataset. Excluded final because it is the dependent variable of the classification model and G1,G2,G3 because they specify passing criteria
tree_model = tree(formula = final ~ .-final-G1-G2-G3, data = train_df)

#misclassification error calculated as 0.184 which equals to 92 misclassified predictions from 500 rows which is good
summary(tree_model)

#plot of the classification tree 
plot(tree_model)
text(tree_model,pretty = 0)

#constructing the model using training dataset. Excluded final because it is the dependent variable of the classification model and G1,G2,G3 because they specify passing criteria
tree_model = tree(formula = final ~ .-final-G1-G2-G3-failures, data = train_df)

#misclassification error calculated as 0.184 which equals to 92 misclassified predictions from 500 rows which is good
summary(tree_model)

#plot of the classification tree 
plot(tree_model)
text(tree_model,pretty = 0)

#using the classification model to predict on testing data
testpredictions = predict(tree_model,test_df,type = "class")
#23 "fail" and 126 "pass" prediction from 149 rows
summary(testpredictions)

#constructing the confusion matrix and calculating the accuracy score of our model for testing data.
cm <- table(testpredictions,test_df$final)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#Accuracy score calculated as 0.72 with 21 true "fail" prediction and 87 true "pass" Prediction from 149 rows.
print(accuracy)
print(cm)

##################################################################################

# k-fold cross validation: i chose the k number as 10 

#implementing a vector to store all 10 accuracy score
acc <- rep(0,10) 

#for loop to construct the tree model 10 times 
for (i in 1:10) {
  #take 10 different sample from data
  t_rows_cv = sample(1:649, 500)
  
  #divide the data 10 times to training and test datasets
  train_cv = df_pass[t_rows_cv,]
  test_cv = df_pass[-t_rows_cv,]
  
  #construct the model 10 times with different training datasets
  tree_model_cv = tree(formula = final ~ .-final-G1-G2-G3-failures, data = train_cv)
  
  #use the model 10 times to predict 10 different testing datasets
  testpredictions_cv = predict(tree_model_cv,test_cv,type = "class")
  
  #construct the confusion matrix 
  cm_cv <- table(testpredictions_cv,test_cv$final)
  
  #calculate the accuracy score 10 times for all indices of vector
  acc[i] = (cm_cv[1,1] + cm_cv[2,2]) / (cm_cv[1,1] + cm_cv[2,2] + cm_cv[1,2] + cm_cv[2,1])
  
}

#print the vector indices which are the different accuracy scores we calculated from cross validation which are ranged between 0.8300 and 0.8535
print(acc)

#calculated the mean of these accuracy scores as 0.8435.
mean(acc)

#As a result of mean(acc) we could validate that our model is reliable. With k-fold cross validation we tested the effectiveness of our model.












