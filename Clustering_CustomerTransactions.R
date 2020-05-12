getwd()
setwd("C:/Users/Canbek/Desktop/Week4")

ct <- read.csv("CustomerTransactions.csv", header = T)

head(ct)

attributes(ct)

#There were a lot of independent variales so result of plot seems meaningless. 
plot(ct)

#Elbow method function to find the optimum number of clusters.
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

# I didn't include first two rows because those rows are not the subjects for segmentation.
# According to result of wssplot function. Found out that there must be 7 clusters because within cluster sum of squared errors becomes first starts to dimminish.
wssplot(ct[-1:-2], nc=10)

# I chose kmeans instead of hierarchical clustering because my dataset is complex with 32 product choice and 100 customer so kmeans is computional faster.
#  with k = 7 value i used kmeans function and assigned it as ct_model.
ct_model <- kmeans(ct[-1:-2], 7) # k = 7

# Indicates that: size 7 which means number of cluster. 
summary(ct_model)

attributes(ct_model)

ct_model$cluster

# Here i added cluster column of ct_model as column to ct_2 to easily see which person belong which cluster.
ct_2<-cbind(ct,Cluster=ct_model$cluster)

head(ct_2)

install.packages("cluster")
library(cluster)

# plot of 7 clusters.
clusplot(ct_2[-1:-2], ct_model$cluster, main='Graph of the Customer Clusters',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# table of customer names and corresponding clusters.
head(table(ct_2[,2],ct_model$cluster))

# Final observation: The dataset have too many independent variable to segment customers which makes harder to find obvious number of cluster. I tried to find the best number of cluster. 
