getwd()

#reading the data file
df <-read.csv("Forddata.csv",header = T)

#dropping the unnecessary "bought" column and after comma values that i added to amount column(fixed in excel)
df <-df[,-c(3:4)]

head(df)

#there are 635 rows and 3 columns
dim(df)

#changing the "date bought" type as date
df[,1] <- as.Date(as.character(df[,1]),"%d.%m.%Y")

#ordering the data frame as decreasing w.r.t date 
newdf <- df[order(df[,1],decreasing = TRUE),]

#to find how many customers we have, removed the duplicate customer ids
newdf <- newdf[!duplicated(newdf[,"Customer"]),]
#there are 117 customers
dim(newdf)
head(newdf)

#because there is only 117 customers, kept all the rows to not to loose data and accepted start and end date as min and max values from df
startDate_df <- min(newdf$Date.bought)
endDate_df <- max(newdf$Date.bought)

#to find recency of customers, calculated the days count between last shopping date and maximum value of dates bought which is 2012-04-05
Recency_df<-as.numeric(difftime(endDate_df,newdf[,1],units="days"))

#adding recency of all customers to dataframe
newdf <-cbind(newdf,Recency_df)

#ordering w.r.t customer ids
newdf <- newdf[order(newdf[,2]),]

#to find car buying frequencies of customers, calculated how many rows a customer id have in df
freq_df <- as.data.frame(table(df[,2]))
head(freq_df)

#adding frequency of all customers to dataframe
Frequency_df <- freq_df[,2]
newdf <- cbind(newdf,Frequency_df)

#to find monetary values of customers, took the sum of amounts spent for each customer id
mon_df <- as.data.frame(tapply(df[,3],df[,2],sum))
Monetary_df <- mon_df[,1]/Frequency_df

#adding monetary value of all customers to dataframe
newdf <- cbind(newdf,Monetary_df)


#scoring for r scores
rnum_df<-round((max(newdf$Recency_df)-min(newdf$Recency_df))/5)

#R scores for recency intervals
newdf$R_Score = 1
newdf$R_Score[which(newdf$Recency_df <= rnum_df*4 & newdf$Recency_df > rnum_df*3)] = 2
newdf$R_Score[which(newdf$Recency_df <= rnum_df*3 & newdf$Recency_df > rnum_df*2)] = 3
newdf$R_Score[which(newdf$Recency_df <= rnum_df*2 & newdf$Recency_df > rnum_df*1)] = 4
newdf$R_Score[which(newdf$Recency_df < rnum_df*1)] = 5

#scoring for f scores
fnum_df<-round((max(newdf$Frequency_df)-min(newdf$Frequency_df))/5)

#F scores for frequency intervals
newdf$F_Score = 5
newdf$F_Score[which(newdf$Frequency_df <= fnum_df*4 & newdf$Frequency_df > fnum_df*3)] = 4
newdf$F_Score[which(newdf$Frequency_df <= fnum_df*3 & newdf$Frequency_df > fnum_df*2)] = 3
newdf$F_Score[which(newdf$Frequency_df <= fnum_df*2 & newdf$Frequency_df > fnum_df*1)] = 2
newdf$F_Score[which(newdf$Frequency_df < fnum_df*1)] = 1


#scoring for m scores
mnum_df<-round((max(newdf$Monetary_df)-min(newdf$Monetary_df))/5)

#M scores for monetary value intervals
newdf$M_Score = 5
newdf$M_Score[which(newdf$Monetary_df <= mnum_df*4 & newdf$Monetary_df > mnum_df*3)] = 4
newdf$M_Score[which(newdf$Monetary_df <= mnum_df*3 & newdf$Monetary_df > mnum_df*2)] = 3
newdf$M_Score[which(newdf$Monetary_df <= mnum_df*2 & newdf$Monetary_df > mnum_df*1)] = 2
newdf$M_Score[which(newdf$Monetary_df < mnum_df*1)] = 1

# calculating the total RFM score
#Because this is an automobile company, as a strategy, i think f score must get the most weight in the formula with 100 and recency with 10 and mv with 1
Total_Score <- c(10*newdf$R_Score + 100*newdf$F_Score+newdf$M_Score)

#adding the total scores for each customer to data frame
newdf <- cbind(newdf,Total_Score)

head(newdf)

#following histograms show that RFM scores range approximately between 400 and 550 with no observations in the score range 430-500
par(mfrow = c(1,4))
hist(newdf$Recency_df)
hist(newdf$Frequency_df)
hist(newdf$Monetary_df)
hist(newdf$Total_Score)


#targeting strategy: because there is no customer with an RFM score between 430 and 500, i set the total score limit to 530 for targeting the customers according to their values. 
target_df <- newdf[newdf$Total_Score>=530,]

#there are 67 customers out of 117 which has RFM score higher than 529.
dim(target_df)
head(target_df)

#As a result, i focused on Frequency_df score because i would like to target people who are frequently change their car and reliable customer for car companies. That's why i multiply F score with 100.

