# Capstone
Capstone
#Capstone assignment 

install.packages("twitteR", dependencies=T)
install.packages("plyr")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("caTools")
install.packages("tm")

library(caTools)
library(ROAuth)
library(twitteR)
library(plyr)
library(base64enc)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)

#STEP 1 - DOWNLOADING DATA FROM TWITTER API
#Part A- connect to the Twitter API
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "5DYzbfgWnSgiKekorWhJbbmCh"
consumerSecret <- "7QUcO2ySrFddt5vUL1zWso1994KABsoSpVwaLDbcpH4MYmjpi2"
accessToken <- "316615384-D9xpi9xfxlCDNGWwqlrYCyz7IFfPy2tJvRGejhMq"
accessTokenSecret <- "0pIUjlE2McnQnupdmUsQiDOV94lxVeL9PH0wLJbfVd1Vo"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
#Part B - download tweets by politician last name 
harper.tweets = searchTwitter('Harper', n=10000, since = '2015-10-01', until = '2015-10-13')
mulcair.tweets = searchTwitter('Mulcair', n=10000, since = '2015-10-01', until = '2015-10-13')
trudeau.tweets = searchTwitter('Trudeau', n=10000, since = '2015-10-01', until = '2015-10-13')

#save files to a dataframe
harper.tweets.df<-do.call(rbind,lapply(harper.tweets,as.data.frame))
mulcair.tweets.df<-do.call(rbind,lapply(mulcair.tweets,as.data.frame))
trudeau.tweets.df<-do.call(rbind,lapply(trudeau.tweets,as.data.frame))

#Combine datasets
total <- rbind(harper.tweets.df, mulcair.tweets.df)
all.df <- rbind(total,trudeau.tweets.df)
save(all.df,file="all_df")

# Data Cleaning
#Remove duplicate tweets
texts=all.df$text
duplicated<-duplicated(texts)
all.df$duplicated<-duplicated
all_df_unq<-subset(all.df,duplicated==FALSE)
#DATA EXPLORATION, PREPARATION AND CLUSTERING


#TEXT ANALYSIS
install.packages("tm")
library("tm")
CorpusDescription = Corpus(VectorSource(all_df_unq_merged$text))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms- 99% which means that a word will appear if it is in 1% of the documents, but remember that you can try different numbers!
install.packages("SnowballC")
library(SnowballC)
dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.99) 


#Find freq terms 
##note the freq.terms variable is a character
##the sparse variable is a document term matrix 
(freq.terms <- findFreqTerms(sparse, lowfreq = 50))
class(sparse)

#Create a word cloud
library(wordcloud)
m <- as.matrix(sparse)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(colSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

#Make a chart of the frequent terms
term.freq <- colSums(m)
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + ggtitle("Word Frequency") + coord_flip() 


#In the Word Frequency chart above, I only have a list of the top 26 words. As you will note, most of these words are related to the party name or the leader name- so not much in the way of suggesting sentiment.  
#Now create clusters of words.
# Cluster terms using Kmeans 
# the reason I am choosing kmeans instead of hierarchical clustering is because my R memory cannot handle a vector of size 934 MB
# Note that the result from this clustering is not very helpful.  So in the next iteration, I will try a different type of topic modelling.

#CLUSTERING

library("caTools")
load("DescriptionWords")
wordsVector = as.vector(DescriptionWords)
k = 4
set.seed(1)
KMC = kmeans(wordsVector, centers = k, iter.max = 1000)
wordClusters = KMC$cluster
KMC$centers[2]
KMC$centers
round(KMC$centers, digits = 3)
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(KMC$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}
#Now create unique dataframes for each cluster
#Note- I don't really understand how the KMC clusters dataframe is organized.  I think it is that for each row is classified into one cluster(from 1 to 4 in this case)
cluster1 <- subset(wordsVector, KMC$cluster == 1)
cluster2 <- subset(wordsVector, KMC$cluster == 2)
cluster3 <- subset(wordsVector, KMC$cluster == 3)
cluster4 <- subset(wordsVector, KMC$cluster == 4)

save(cluster1,file="cluster1")
save(cluster2,file="cluster2")
save(cluster3,file="cluster3")
save(cluster4,file="cluster4")
save(KMC,file="KMC")

load("cluster1")
load("cluster2")
load("cluster3")
load("cluster4")

#As seen in the clusters, these are not very meaningful.  One issue that I am still having with this text topic analysis is that the clusters are not meaningful.  As such, I would like to try some new approaches- perhaps TFIDF or using ngrams.

#Now merge your document term matrix with your original matrix
all_df_unq_merged<-cbind(all_df_unq,DescriptionWords)
save(all_df_unq_merged, file="all_df_unq_merged")
write.csv(all_df_unq_merged, file="all_df_unq_merged.csv")
str(all_df_unq_merged)


#Add in a time stamp extraction
#convert date - create new vectors for hour and minute
xlt <- as.POSIXlt(all_df_unq$created)
all_df_unq$hour<-xlt$hour
all_df_unq$minute<-xlt$min

#Set the baseline
table(all_df_unq$retweetCount)

#From this table, I see that the most frequent retweet count is 0 at 8371, followed by 1 at 2293, and 2 at 1153
#If I always predict that the tweet will NOT be retweeted, then I am accurate 53.5% of the time.
8371/nrow(all_df_unq)
##I will bin the data in order to make this a classification problem.  
#The bins will be as follows : 
# Bin 1: 0
# Bin 2: 1-10
# Bin 3: 11-50
# Bin 4: 50+
bin<-numeric(15648)
all_df_unq$bin<-ifelse(all_df_unq$retweetCount==0, 1,     
                       ifelse(all_df_unq$retweetCount>=1 & all_df_unq$retweetCount<=10,2,
                              ifelse(all_df_unq$retweetCount>=11 & all_df_unq$retweetCount<=50,3,4)))4)))
#ifelse(all_df_unq$retweetCount>50 & all_df_unq$retweetCount<,3,4)))4)))

#Create a bar chart to show the distribution of retweets
# Simple Bar Plot 
counts <- table(all_df_unq$bin)
barplot(counts, main="Retweet Frequency", 
        xlab="Bins (Bin 1: 0, Bin 2: 1-10, Bin 3: 11-50, Bin 4: 50+)")

table(all_df_unq$bin)

#Conclusion from this round of data exploration is that I will need to use some type of a classification or multinomial logistics regression equation in order to predict bin number for a given tweet.

#TFIDF

## convert to a Corpus
dtm2list <- apply(sparse, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})

myCorp <- VCorpus(VectorSource(dtm2list))

dtm4 <-DocumentTermMatrix(myCorp,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) 

DescriptionWords = as.data.frame(as.matrix(dtm4)) 

# Let's make sure our variable names are okay for R:
colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
save(all_df_unq_merged, file="all_df_unq_merged")



#Now merge your document term matrix with your original matrix

all_df_unq_merged<-cbind(all_df_unqtest,DescriptionWords)

#Make a train set and a test set
install.packages("caTools")
library("caTools")
set.seed(100)

split = sample.split(all_df_unq_merged$bin, SplitRatio = 0.75) 
Train = subset(all_df_unq_merged, split==TRUE)
Test = subset(all_df_unq_merged, split==FALSE)

#Remove unnecessary columns 
Train$replyToSN=NULL
Train$replyToSID=NULL
Train$created=NULL
Train$statusSource=NULL
Train$longitude=NULL
Train$latitude=NULL
Train$duplicated=NULL
Train$retweetCount=NULL
Test$retweetCount=NULL


save(Train,file="Train.df")
save(Test,file="Test.df")

Train2<-Train
write.csv(Train2, file="Train2.csv")




#PREDICTION 1:  USE A DATASET WITH NO TEXT FEATURES FOR NAIVE BAYES AND MULTINOMIAL PREDICTION

TrainNoText<-Train
TestNoText<-Test

#Adjust Train set
TrainNoText<-TrainNoText[,1:8]


#Adjust Test set
TestNoText$text=NULL
TestNoText$id=NULL
TestNoText$replyToUID=NULL
TestNoText$screenName=NULL
TestNoText$replyToSN=NULL
TestNoText$replyToSID=NULL
TestNoText$created=NULL
TestNoText$statusSource=NULL
TestNoText$longitude=NULL
TestNoText$latitude=NULL
TestNoText$duplicated=NULL
TestNoText$retweetCount=NULL
TestNoText<-TestNoText[,1:8]

save(TestNoText, file="TestNoText")
save(TrainNoText,file="TrainNoText")


#Naive Bayes
TrainNoText_NB= TrainNoText
TestNoText_NB=TestNoText


#For NaiveBayes you must factorize relevant variables.  It cannot understand logical vectors. (True-False)

for (i in 1:8){
  TrainNoText_NB[,i]=as.factor(TrainNoText_NB[,i])
}

for (i in 1:8){
  TestNoText_NB[,i]=as.factor(TestNoText_NB[,i])
}


naiveBayesNoText_m1 <- naiveBayes(bin~., data = TrainNoText_NB)


#Naive Bayes Prediction evaluation

naiveBayesprediction<- predict(naiveBayesNoText_m1, newdata=TestNoText_NB)
summary(naiveBayesprediction)
table(TestNoText_NB$bin,naiveBayesprediction)

#Create a multinomial logistic regression prediction
install.packages(nnet)
library(nnet)
TrainNoText_Log<-Train
TestNoText_Log<-Test


#adjust Test set
TestNoText_Log$text=NULL
TestNoText_Log$id=NULL
TestNoText_Log$replyToUID=NULL
TestNoText_Log$screenName=NULL
TestNoText_Log$replyToSN=NULL
TestNoText_Log$replyToSID=NULL
TestNoText_Log$created=NULL
TestNoText_Log$statusSource=NULL
TestNoText_Log$longitude=NULL
TestNoText_Log$latitude=NULL
TestNoText_Log$duplicated=NULL
TestNoText_Log$retweetCount=NULL
TestNoText_Log$bin<-as.factor(TestNoText$bin)

TrainNoText_Log<-TrainNoText_Log[,1:8]
TestNoText_Log<-TestNoText_Log[,1:8]

logreg_m1 <- multinom(bin ~ ., data=TrainNoText_Log)
logregprediction<- predict(logreg_m1, newdata=TestNoText_Log)
#summary
(logreg_m1 <- multinom(bin ~ ., data=TrainNoText_Log))

table(TestNoText_Log$bin,logregprediction)

#Save
save(TestNoText_Log, file="TestNoText_Log")
save(TrainNoText_Log,file="TrainNoText_Log")


#PREDICTION 2: USE A DATASET WITH CLUSTER FEATURES FOR NAIVE BAYES AND MULTINOMIAL PREDICTION

all_df_unq_merged$cluster<-KMC$cluster

#Make a train set and a test set
install.packages("caTools")
library("caTools")
set.seed(100)

split = sample.split(all_df_unq_merged$bin, SplitRatio = 0.75) 
TrainCluster= subset(all_df_unq_merged, split==TRUE)
TestCluster = subset(all_df_unq_merged, split==FALSE)

#remove relevant variables in the train and test set

#Train adjustment

TrainCluster$text=NULL
TrainCluster$id=NULL
TrainCluster$replyToUID=NULL
TrainCluster$screenName=NULL
TrainCluster$replyToSN=NULL
TrainCluster$replyToSID=NULL
TrainCluster$created=NULL
TrainCluster$statusSource=NULL
TrainCluster$longitude=NULL
TrainCluster$latitude=NULL
TrainCluster$duplicated=NULL
TrainCluster$retweetCount=NULL
TrainCluster$bin<-as.factor(TrainCluster$bin)

#Test adjustment
TestCluster$text=NULL
TestCluster$id=NULL
TestCluster$replyToUID=NULL
TestCluster$screenName=NULL
TestCluster$replyToSN=NULL
TestCluster$replyToSID=NULL
TestCluster$created=NULL
TestCluster$statusSource=NULL
TestCluster$longitude=NULL
TestCluster$latitude=NULL
TestCluster$duplicated=NULL
TestCluster$retweetCount=NULL
TestCluster$bin<-as.factor(TestCluster$bin)

#Keep only relevant variables
#Train
TrainClusterx<-TrainCluster[,1:8]
TrainClusterx$cluster<-TrainCluster$cluster

#Test
TestClusterx<-TestCluster[,1:8]
TestClusterx$cluster<-TestCluster$cluster

save(TrainClusterx,file="TrainClusterx")
save(TestClusterx,file="TestClusterx")

#Naive Bayes
#For NaiveBayes you must factorize relevant variables.  It cannot understand logical vectors. (True-False)
TrainClusterx_NB<-TrainClusterx
TestClusterx_NB<-TestClusterx

for (i in 1:9){
  TrainClusterx_NB[,i]=as.factor(TrainClusterx_NB[,i])
}

for (i in 1:8){
  TestClusterx_NB[,i]=as.factor(TestClusterx_NB[,i])
}


naiveBayes_m1 <- naiveBayes(bin~., data = TrainClusterx_NB)


#Naive Bayes Prediction evaluation

naiveBayesprediction<- predict(naiveBayes_m1, newdata=TestClusterx_NB)
summary(naiveBayesprediction)
table(TestClusterx_NB$bin,naiveBayesprediction)




#Multinomial Logistic Regression

logreg_m1 <- multinom(bin ~ ., data=TrainClusterx)
logregprediction<- predict(logreg_m1, newdata=TestClusterx)
#summary
(logreg_m1 <- multinom(bin ~ ., data=TrainClusterx))

table(TestClusterx$bin,logregprediction)



#PREDICTION 3: USE A DATASET WITH TFIDF FEATURES FOR NAIVE BAYES AND MULTINOMIAL PREDICTION

#Naive Bayes Model

install.packages("e1071")
library("e1071")


TrainNB= Train
TestNB=Test


#For NaiveBayes you must factorize relevant variables.  It cannot understand logical vectors. (True-False)
TestNB$bin<- as.factor(TestNB$bin)

for (i in 1:6){
  TrainNB[,i]=as.factor(TrainNB[,i])
}

for (i in 1:6){
  TestNB[,i]=as.factor(TestNB[,i])
}

save(TrainNB,file="TrainNB")
save(TestNB,file="TestNB")

naiveBayes_m1 <- naiveBayes(bin~., data = TrainNB)


#Naive Bayes Prediction evaluation

naiveBayesprediction<- predict(naiveBayes_m1, newdata=TestNB)
summary(naiveBayesprediction)
table(TestNB$bin,naiveBayesprediction)

#Create a multinomial logistic regression prediction
install.packages(nnet)
library(nnet)
Train$bin<-as.factor(Train$bin)

#remove additional variables from Train due to errors
Train$text=NULL
Train$id=NULL
Train$screenName=NULL
Train$replyToUID=NULL

#remove additional variables from Test due to errors
Test$text=NULL
Test$id=NULL
Test$screenName=NULL
Test$replyToUID=NULL

logreg_m1 <- multinom(bin ~ ., data=Train)
logregprediction<- predict(logreg_m1, newdata=Test)
#summary
(logreg_m1 <- multinom(bin ~ ., data=Train))

#get p values- does not work
#z <- summary(logreg_m1)$coefficients/summary(logreg_m1)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
#p <- (1 - pnorm(abs(logreg_m1), 0, 1)) * 2
#p

#get p values- does not work
install.packages(AER)
library(AER)
coeftest(logreg_m1)
Likelihood ratio tests you can get using

install.packages(afex)
library(afex)
set_sum_contrasts()
library(car)
Anova(logreg_m1,type="III")


table(Test$bin,logregprediction)

save(Train,file="Train")
save(Train,file="Test")

#Save my training model: 
save(Train2,file="Train2")
Train<-Train2



