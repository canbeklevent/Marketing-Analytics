install.packages("caTools")
install.packages("plyr")

library(caTools)
library(plyr)


getwd()
setwd("C:/Users/Canbek/Desktop/BAU/Spring Semester/Marketing Analytics/Hands-On Exercises/Week3")

d <- read.csv("adult_income.csv")

#Firstly, when i checked the data there were dirty data such as "?", i turned them into NA value then deleted them.
#After that i assigned it to d1 to compare with d. Thats how i could check i did right.

d[d == " ?"] <- NA
d1 <- na.omit(d)

attributes(d1)

#I used factor to make these rows categorical value.
d1$race <- factor(d1$race)
d1$workclass <- factor(d1$workclass)
d1$marital_status <- factor(d1$marital_status)
d1$sex <- factor(d1$sex)
d1$occupation <- factor(d1$occupation)
d1$native_country <- factor(d1$native_country)

# After that i seperated data as d1_train and d1_test and i used %75 of data for train dataset.
# Here, i used set.seed() function because of prevent to produce same sample again and again.
set.seed(123)

rate <- sample.split(d1, SplitRatio = 0.75)
d1_train <- subset(d1, rate == TRUE)
d1_test <- subset(d1, rate == FALSE)

# After that i created 3 model. From m1 to m3 i kept adding values which is called forward selection to see how my models react. 

m1 <- glm(income_high ~ age, data=d1_train, family = "binomial")
summary(m1)

m2 <- glm(income_high ~ age + education_num, data=d1_train, family = "binomial")
summary(m2)

m3 <- glm(income_high ~ age + education_num + hours_per_week, data=d1_train, family = "binomial")
summary(m3)

# When i checked the AIC value of all these models.We can clearly see that m3 is better model because AIC value is lower that the others. Thats why i chose this model to predict. After that i trained my data.
result_of_prediction <- predict(m3,d1_test, type = "response")

# to see result of my prediction i used table function.
gr = ifelse(result_of_prediction > 0.5,"Yes","No")
table(gr)
