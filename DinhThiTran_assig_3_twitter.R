
rm(list=ls())
dev.off()
library(tm)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)

library(stringr)
library(wordcloud)
library(tm) 
library(SnowballC)
library(tidytext)



#read the tweets data
dt <- read_csv("data_science.csv")


#data exploration
nrow(dt)
names(dt)
str(dt)

#check valure of retweet 
unique(dt$retweet)


#none of them was retweeted so remove duplicate tweets
dt <-dt %>% distinct(tweet,.keep_all = TRUE)





#count the number of tweets each username posted and sort by descending
username_count <- dt %>% count(username) %>% arrange(desc(n))
nrow(username_count)

#see the top 10 users
top_user <-head(username_count,10)
ggplot(data=top_user)+aes(x=username, y=n)+geom_point()

sum(top_user$n)
sum(top_user$n)/nrow(dt)
nrow(username_count)
 71528/nrow(dt)
 
 #top 100 users
 top_user100 <-head(username_count,100)
 sum(top_user100$n)/nrow(dt)

#-------------------------#part1: ANALYSING HASHTAGS-----------------------------------------------#


##----------------data preperation-----------------------------------------

#filter out tweets without hashtags, choose only date, username, hashtags columns
hash <- dt %>% filter(hashtags != "[]")
hash <- hash %>%  select(date, username, hashtags)

# group by date & username
hash <- hash %>% group_by(date, username) %>%  summarise(hashtags= paste(hashtags, collapse = ", "))

#replace characters which are not letters, digits, convert to lower cases
hash$hashtags <- str_replace_all(hash$hashtags, "[^A-Za-z0-9, ]" , "" )
hash$hashtags <- tolower(hash$hashtags)


#replace similar tags for some important tags

hash$hashtags <- str_replace_all(hash$hashtags, "machinelearning" , "ml" )
hash$hashtags <- str_replace_all(hash$hashtags, "artificialintelligence" , "ai" )
hash$hashtags <- str_replace_all(hash$hashtags, "deeplearning" , "dl" )


nrow(hash)



#check popular users who used hashtags

hash$count <-1
username_hash_count <-hash %>%  group_by(username) %>% summarise(count=sum(count)) %>% arrange(desc(count))
head(username_hash_count)


#remove duplicate hashtags in each row
hash$hashtags <-sapply(hash$hashtags, function(x) toString(paste(unique(unlist(str_split(x,", "))), collapse = ",  ")))

hash <- hash %>% filter(hashtags !="")


hashStopwords <- c("datascience", "data", "scientist", "scientists","datascientists","datascientist", "analytics", "dataanalytics","dataliteracy", "science", "coding", "statistics", "mathenatics", "maths", "stats", "algorithms", "algorithm", "tech", "job", "jobs", "internet", "knowledge", "download")


## ------------------------ANALYSING THE WHOLE PERIOD-----------------------------------------------
# changing to corpus
docshash<-Corpus(VectorSource(hash$hashtags))


#Remove punctuation - replace punctuation marks with " "
docshash <- tm_map(docshash, removePunctuation)
#Transform to lower case
docshash <- tm_map(docshash,content_transformer(tolower))
#Strip digits
#docs <- tm_map(docs, removeNumbers)
#Remove stopwords from standard stopword list 
docshash <- tm_map(docshash, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
docshash <- tm_map(docshash, stripWhitespace)
docshash <- tm_map(docshash, removeWords, hashStopwords)


#inspect output
writeLines(as.character(docshash[[30]]))
#Stem document

#Create document-term matrix
dtmhash <-DocumentTermMatrix(docshash, control=list(wordLengths=c(2, Inf),
                                                        bounds = list(global = c(100,Inf))))

#summary
dtmhash
dim(dtmhash)
#inspect segment of document term matrix

freqhash <- colSums(as.matrix(dtmhash))
#length should be total number of terms
length(freqhash)
#create sort order (asc)
ordhash <- order(freqhash,decreasing=TRUE)
#inspect most frequently occurring terms
freqhash[head(ordhash)]
#write to disk and inspect file
write.csv(file="freqhash.csv",freqhash[ordhash])
#inspect least frequently occurring terms
freqhash[tail(ordhash)]
#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtmhash,lowfreq=500)


#correlations
findAssocs(dtmhash,c("ml", "ai", "bigdata", "iot", "dl"),0.4)


#histogram
library(ggplot2)
wf=data.frame(term=names(freqhash),occurrences=freqhash)
p <- ggplot(subset(wf, occurrences>1000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>1000), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity", fill="white", colour="blue")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))+labs(xlab="term",title="Plot of hashtags with ocurrences higher than 1000 over 2010-2021")
p
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
wordcloud(names(freqhash),freqhash,max.words=50,colors=brewer.pal(6,"Dark2"), random.order = FALSE)



#correlations
findAssocs(dtmhash,c( "bigdata","ml", "ai"),0.4)
findAssocs(dtmhash,c("dl", "iot"),0.5)

findAssocs(dtmhash,c("dl", "iot"),0.53)







 ##--------------------------#ANALYSING BY YEAR#---------------------------------



#finding year range
max_year <- year(max(hash$date))
min_year <- year(min(hash$date))
max_year
min_year



# getting hash data for each year
ydt_hash <- list()
for (year in c(min_year: max_year)){
  temp_hash <- hash %>% filter(year(date)==year)
  ydt_hash[[year]] <- temp_hash
}

# getting document term matrix for each year
ydtm_hash <-list()
for (year in c(min_year: max_year)){
  temp_docs <- Corpus(VectorSource(ydt_hash[[year]]$hashtags))
  
  #cleaning
  temp_docs<- tm_map(temp_docs, removePunctuation)
  temp_docs <- tm_map(temp_docs, removeWords, stopwords("english"))
  temp_docs <- tm_map(temp_docs, stripWhitespace)
  temp_docs <- tm_map(temp_docs, removeWords, hashStopwords)
  

  
  
  #Create document-term matrix
  if (year < 2015){
    dtm_temp <-DocumentTermMatrix(temp_docs, control=list(wordLengths=c(2, Inf),
                                                          bounds = list(global = c(1,Inf))))
  } else {
    dtm_temp <-DocumentTermMatrix(temp_docs, control=list(wordLengths=c(2, Inf),
                                                          bounds = list(global = c(100,Inf))))
  }
  ydtm_hash[[year]]<-dtm_temp
}



# getting frequency
yfreqhash <- list()
for (year in c(min_year: max_year)){
  temp_freq<- colSums(as.matrix(ydtm_hash[[year]]))
  ord <- order(temp_freq,decreasing=TRUE)
  yfreqhash[[year]]<-temp_freq
}


#wordcloud for each year using for loop
par(mfrow=c(2,3))
for (year in c(2010: 2015)){
  set.seed(42)
  tempwcl<-wordcloud(names(yfreqhash[[year]]),yfreqhash[[year]],max.words=20,colors=brewer.pal(8,"Dark2"), main=paste("Hashtags wordcloud in ", year, sep=" "),scale=c(3,1), random.order = FALSE)
}

par(mfrow=c(2,3))
for (year in c(2016: 2021)){
  set.seed(42)
  tempwcl<-wordcloud(names(yfreqhash[[year]]),yfreqhash[[year]],max.words=20,colors=brewer.pal(8,"Dark2"), main=paste("Hashtags wordcloud in ", year, sep=" "),scale=c(3,1), random.order = FALSE)
}








#--------------------------part2 ANALYSING TWEETS----------------------------


#Check languages used in the data
unique(dt$language)


# filter data with English language only
dt <- dt %>%  filter(language=="en")
nrow(dt)
names(dt)
head(dt$tweet)

 # tweets <- dt %>% select(date, username, tweet) %>% group_by(date, username) %>%  summarise(tweet= paste(tweet, collapse = ", "))
 

#group tweets by date
 tweets <- dt %>% select(date,  tweet) %>% group_by(date) %>%  summarise(tweet= paste(tweet, collapse = ", "))
nrow(tweets)


#replace url, metions with empty character
tweets$tweet <- gsub("(http|https)://[^ ]*","",tweets$tweet)
tweets$tweet <- gsub("#\\S+","",tweets$tweet)
tweets$tweet <- gsub("RT @\\S+","",tweets$tweet)
tweets$tweet <- gsub("@\\S+","",tweets$tweet)
tweets$tweet <- gsub("[^A-Za-z0-9,  ]", "", tweets$tweet)

tweets$tweet <- tolower(tweets$tweet)


tweets$tweet  <- str_replace_all(tweets$tweet , "machine learning" , "ml" )
tweets$tweet  <- str_replace_all(tweets$tweet , "artificial intelligence" , "ai" )
tweets$tweet  <- str_replace_all(tweets$tweet , "deep learning" , "dl" )
tweets$tweet  <- str_replace_all(tweets$tweet , "big data" , "bigdata" )
tweets$tweet  <- str_replace_all(tweets$tweet , "open data" , "opendata" )

tweets$tweet  <- str_replace_all(tweets$tweet , "predicitve analysis" , "predictiveanalysis" )




tweetstopwords <- c(stopwords("english"),
                    "datascience",  "datascientists","datascientist", 
                    "science","sciences","scientist","scientists","scienc","data",
                    "use", "read", "collect",
                    "algorithms", "algorithm", "tech", "job", "jobs", 
                    "internet", "knowledge", "download", 
                    "can", "say","one","way","use", "also",
                   "tell","will", "much","need","take","tend","like", "want","love","know","need",
                    "particular","rather", 
                   "get","make","ask","come","read", "find", "say", "have", "join",
                   "start', end","check","well",
                    "first","two","help","may",  "might","see","someth","thing","point",
                    "post","right","think","'ve ",  "'re ","anoth","put","set",
                    "new","good","great","best","better", 
                   "interest", "interets", "intersted", "interesting",
                   "sure","kind","large","yes,","day","etc", 
                    "quit","sinc","attempt","lack","seen","awar",
                    "littl","ever","moreov","though","found","abl",
                    "enough","far","earli","away","achiev","draw",
                    "last","never","brief","bit","entir","brief",
                    "lot","man","say","just", 
                   "today", "time","year","week","latest","now","yesterday", "tomorrow",
                    "world","talk","show", "look","share","us", 
                   "work", "book", "team", 
                   "the", "via","here",
                 "however","but","often", "what", "where", "next","news","even",
                 "thank", "people", "dont", "follow", "live", "meet", "register",
                 "skill", "using","person",
                "small", "top", "big", "go"
                    )


docs <- Corpus(VectorSource(tweets$tweet))

#cleaning

docs1<- tm_map(docs, removePunctuation)
docs2 <- tm_map(docs1, content_transformer(tolower))
docs3 <- tm_map(docs2 , removeNumbers)

docs4 <- tm_map(docs3, stripWhitespace)
docs5 <- tm_map(docs4, removeWords, tweetstopwords)
docs6 <- tm_map(docs5,stemDocument)
docs7 <- tm_map(docs6,removeWords, tweetstopwords)


## Document term matrix with tfidf
dtm <-DocumentTermMatrix(docs7,
                              control=list(wordLengths=c(2, Inf),
                                           weighting = weightTfIdf,
                                           bounds = list(global = c(500,Inf))
                              ))

dim(dtm)
freq <- colSums(as.matrix(dtm))

ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]

library(ggplot2)
wf=data.frame(term=names(freq),occurrences=freq)
p <- ggplot(subset(wf, occurrences>20 & occurrences<40), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>=20 & occurrences<=40), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity",fill="white", colour="blue")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))+labs(xlab="term",title="Plot of terms with ocurrences between 20 and 40")
p


#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
wordcloud(names(freq),freq,max.words=50,scale=c(3,0.25), random.order = FALSE,colors=brewer.pal(6,"Dark2"))

findAssocs(dtm,c("predict"),0.1)


#trying bigram
BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# 
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtmbi <- DocumentTermMatrix(docs7, control = list(tokenize = BigramTokenizer))

dtmbi <-removeSparseTerms(dtmbi, 0.9)
freqbi <- colSums(as.matrix(dtmbi))
#length should be total number of terms
length(freqbi)

dim(dtmbi)
#create sort order (asc)
ordbi <- order(freqbi,decreasing=TRUE)
#inspect most frequently occurring terms
freqbi[head(ordbi,20)]


wf=data.frame(term=names(freqbi),occurrences=freqbi)
p <- ggplot(subset(wf, occurrences>3000 & occurrences<4000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>3000 & occurrences<4000), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

par(mfrow=c(1,1))

set.seed(1982)
#...add color
wordcloud(names(freqbi),freqbi,max.words=50, min.freqbi=1000,colors=brewer.pal(6,"Dark2"),scale=c(3,0.5), random.order = FALSE)


findAssocs(dtmbi, "predict",0.35)





# ---------------------clustering and topic modeling for data from 2016 to 2021----------------------#

#---------------filter for dt from 2016 to 2021
tweets_a16 <- tweets %>% filter(year(tweets$date)>=2016)
#extract year and month
tweets_a16$ym <-format(tweets_a16$date, '%Y-%m')
head(tweets_a16$ym)

tweets_a16<-tweets_a16 %>% group_by(ym) %>%  summarise(tweet= paste(tweet, collapse = ", "))
nrow(tweets_a16)


docsa16 <- Corpus(VectorSource(tweets_a16$tweet))

#cleaning

docsa16<- tm_map(docsa16 , removePunctuation)
docsa16  <- tm_map(docsa16 , content_transformer(tolower))
docsa16 <- tm_map(docsa16  , removeNumbers)

docsa16  <- tm_map(docsa16 , stripWhitespace)
docsa16 <- tm_map(docsa16 , removeWords, tweetstopwords)
docsa16  <- tm_map(docsa16 ,stemDocument)
docsa16  <- tm_map(docsa16 ,removeWords, tweetstopwords)






#--------------without bigram---------------------


dtma16 <-DocumentTermMatrix(docsa16 ,
                             control=list(wordLengths=c(2, Inf)))
                           
dtma16 <-removeSparseTerms(dtma16, 0.9)


freqa16 <- colSums(as.matrix(dtma16))
ord <- order(freqa16,decreasing=TRUE)
#inspect most frequently occurring terms
freqa16[head(ord,10)]



set.seed(42)
#wordcloud(names(freq),freq, max.words=50)
wordcloud(names(freqa16 ),freqa16 ,max.words=50,scale=c(3,0.25), random.order = FALSE,colors=brewer.pal(6,"Dark2"))

wf=data.frame(term=names(freqa16),occurrences=freqa16)
p <- ggplot(subset(wf, occurrences>3000 & occurrences<10000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>3000 & occurrences<10000), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


m<-as.matrix(dtma16)
#write as csv file
write.csv(m,file="dtmAsMatrix16.csv")

d <- dist(m)
groups <- hclust(d,method="ward.D")
#plot, use hang to ensure that labels fall below tree
par(mfrow=c(1,1))
plot(groups, hang=-1)
#cut into 2 subtrees. Try 3,4,5,6 cuts; comment on your results
rect.hclust(groups,12)
hclusters <- cutree(groups,6)
write.csv(hclusters,"hclusters16.csv")
hclusters 

#plot cuttree
plot(hclusters)





#---- clustering with kmeans-----------


d <- dist(m)


#kmeans - run with nstart=25 and k=2,3,5 to compare results with hclust

# try different k
for (k in c(2:12)){
  print(kmeans(d, 6, nstart=25))
  print(k)
  print("--------------------")
}


# choose k=6
kfit <- kmeans(d, 6, nstart=25)

#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups6.csv")


kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", xlim=c(2,(length(docsa16)-1))) 


#----try different n -

#kmeans - n=30, d=6 ----- figures given in the report-----------
kfit <- kmeans(d, 6, nstart=30)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster

## plot cluster trees-----
plot(kfit$cluster)
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups6.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", xlim=c(2,(length(docsa16)-1)))


#----try different k, n -

#kmeans - n=30
kfit <- kmeans(d, 10, nstart=30)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups6.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", xlim=c(2,(length(docsa16)-1)))


#_________________________cosine distance-----------------------
#rerun using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

kfit <- kmeans(cd, 6, nstart=25)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster

#plot cluster "tree"
plot(kfit$cluster)
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docsa16)-1), wss[2:(length(docsa16)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


kfit <- kmeans(cd, 6, nstart=30)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docsa16)-1), wss[2:(length(docsa16)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


kfit <- kmeans(cd, 18, nstart=30)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docsa16)-1), wss[2:(length(docsa16)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


kfit <- kmeans(cd, 24, nstart=30)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsa16)-1)
for (i in 2:(length(docsa16)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docsa16)-1), wss[2:(length(docsa16)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#---------------topic modelling for data from 2016--------------------------------


library(topicmodels)

dtm <-DocumentTermMatrix(docsa16)
dim(dtm)

dtm<-removeSparseTerms(dtm, .80)

dim(dtm)

burnin <- 1000
# and perform 2000 iterations (after burn-in)...
iter <- 2000
#..taking every 500th one for further use. This "thinning" is done to ensure that
# samples are not correlated.
thin <- 500
#We'll use 5 different, randomly chosen starting points
nstart <- 5
#using random integers as seed. Feel free to change these
seed <- list(2022,9,11,1982,42)
#...and take the best run (the one with the highest probability) as the result
best <- TRUE

#Number of topics (try different numbers from, say 4 to 8 and see which one returns
# the best results)
k <- 6
#Patience, this WILL take a while....
#................
#.............
#..........
#......
#....
#..
ldaOut <- LDA(dtm,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopicsa16.csv"))
terms(ldaOut,10)
ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTermsa16.csv"))
#Find probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilitiesa16.csv"))



# Shows the probability of a word being associated to a topic
beta_topics <- tidy(ldaOut, matrix = "beta") # create the beta model
beta_topics # shows all the information in the beta_topics

#Grouping the terms by topic
beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Display the grouped terms on the charts
beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()+labs(title="Grouping Terms by topic for tweets from 2016 to 2021")

# plot months in topics
plot(ldaOut.topics)





# ---------------------clustering and topic modeling for data from 2010 to 2015----------------------#

#---------------filter for dt from 2010 to 2015
tweets_b16 <- tweets %>% filter(year(tweets$date) < 2016)
#extract year and month
tweets_b16$ym <-format(tweets_b16$date, '%Y-%m')
head(tweets_b16$ym)

tweets_b16<-tweets_b16 %>% group_by(ym) %>%  summarise(tweet= paste(tweet, collapse = ", "))
nrow(tweets_b16)


docsb16 <- Corpus(VectorSource(tweets_b16$tweet))

#cleaning

docsb16<- tm_map(docsb16 , removePunctuation)
docsb16  <- tm_map(docsb16 , content_transformer(tolower))
docsb16 <- tm_map(docsb16  , removeNumbers)

docsb16  <- tm_map(docsb16 , stripWhitespace)
docsb16 <- tm_map(docsb16 , removeWords, tweetstopwords)
docsb16  <- tm_map(docsb16 ,stemDocument)
docsb16  <- tm_map(docsb16 ,removeWords, tweetstopwords)



#--------------without bigram---------------------


dtmb16 <-DocumentTermMatrix(docsb16 ,
                            control=list(wordLengths=c(2, Inf)))

dtmb16 <-removeSparseTerms(dtmb16, 0.9)


freqb16 <- colSums(as.matrix(dtmb16))
ordb16 <- order(freqb16,decreasing=TRUE)
#inspect most frequently occurring terms
freqb16[head(ordb16,10)]



set.seed(42)
#wordcloud(names(freq),freq, max.words=50)
wordcloud(names(freqb16 ),freqb16 ,max.words=50,scale=c(3,0.25), random.order = FALSE,colors=brewer.pal(6,"Dark2"))

wf=data.frame(term=names(freqb16),occurrences=freqb16)
p <- ggplot(subset(wf, occurrences>500), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>500), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


m<-as.matrix(dtmb16)
#write as csv file
write.csv(m,file="dtmAsMatrix19.csv")

d <- dist(m)
groups <- hclust(d,method="ward.D")
#plot, use hang to ensure that labels fall below tree
par(mfrow=c(1,1))
plot(groups, hang=-1)
#cut into 2 subtrees. Try 3,4,5,6 cuts; comment on your results
rect.hclust(groups,6)
hclusters <- cutree(groups,6)
write.csv(hclusters,"hclusters19.csv")

#plot of cuttree
plot(hclusters)




#---- clustering with kmeans-----------


d <- dist(m)


#kmeans - run with nstart=100 and k=2,3,5 to compare results with hclust
kfit <- kmeans(d, 6, nstart=30)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
#plot cluster trees
plot(kfit$cluster)

write.csv(kfit$cluster,file="KMClustGroups6.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsb16)-1)
for (i in 2:(length(docsb16)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", xlim=c(2,(length(docsb16)-1))) 





#kmeans - run with k=12, n=30-----
kfit <- kmeans(d, 12, nstart=30)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
#plot cluster trees
plot(kfit$cluster)

write.csv(kfit$cluster,file="KMClustGroups6.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsb16)-1)
for (i in 2:(length(docsb16)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", xlim=c(2,(length(docsb16)-1)))

#rerun using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

kfit <- kmeans(cd, 6, nstart=100)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docsb16)-1)
for (i in 2:(length(docsb16)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docsb16)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 





####-------------- Topic modelling before 2016---------------------------------

library(topicmodels)

dtm <-DocumentTermMatrix(docsb16)
dim(dtm)

dtm<-removeSparseTerms(dtm, .9)

dim(dtm)

burnin <- 1000
# and perform 2000 iterations (after burn-in)...
iter <- 2000
#..taking every 500th one for further use. This "thinning" is done to ensure that
# samples are not correlated.
thin <- 500
#We'll use 5 different, randomly chosen starting points
nstart <- 5
#using random integers as seed. Feel free to change these
seed <- list(2022,9,11,1982,42)
#...and take the best run (the one with the highest probability) as the result
best <- TRUE

#Number of topics (try different numbers from, say 4 to 8 and see which one returns
# the best results)
k <- 6
#Patience, this WILL take a while....
#................
#.............
#..........
#......
#....
#..
ldaOutb16 <- LDA(dtm,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
topics(ldaOutb16)
ldaOutb16.topics <-as.matrix(topics(ldaOutb16))
write.csv(ldaOutb16.topics,file=paste("LDAGibbs",k,"DocsToTopicsb16.csv"))
terms(ldaOutb16,10)
ldaOutb16.terms <- as.matrix(terms(ldaOutb16,10))
write.csv(ldaOutb16.terms,file=paste("LDAGibbs",k,"TopicsToTermsb16.csv"))
#Find probabilities associated with each topic assignment
topicProbabilitiesb16 <- as.data.frame(ldaOutb16@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilitiesb16.csv"))



# Shows the probability of a word being associated to a topic
beta_topicsb16 <- tidy(ldaOutb16, matrix = "beta") # create the beta model
beta_topicsb16 # shows all the information in the beta_topics

#Grouping the terms by topic
beta_top_termsb16 <- beta_topicsb16 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Display the grouped terms on the charts
beta_top_termsb16 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()+labs(title="Terms by topic for tweets from 2010 to 2015")

# plot months in topics
plot(ldaOutb16.topics)



########---------------------------------------------------------------------------------

