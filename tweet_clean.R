
library(tm)
library(SnowballC)
library(caTools)
#install.packages("e1071")
library(e1071)
library(caret)
library(gmodels)
library(pROC)
library(ROCR)
library(randomForest)
#install.packages("wordcloud")
library(wordcloud)


#set.seed(96857) #78.91 and 80.83 w/o 10 k fold 
#set.seed(77)#79.13
#set.seed(55564) #77.95
set.seed(6521) #
#Reading data
tweet<-read.csv("tweet.csv",header=TRUE,stringsAsFactors = FALSE)

tweet<-tweet[-1]

str(tweet)

str(tweet)
#shuffling rows of data frame 
tweet <- tweet[sample(nrow(tweet)),]

#col names to data frame
colnames(tweet)<-c("label","text")

#changing label from char to factor form
tweet$label<-factor(tweet$label)

#top row of data frame
head(tweet)

### splitting big data file to 2/3 of iths size
ind<-createDataPartition(tweet$label,p=2/3,list = FALSE)
tweet1<-tweet[ind,]

str(tweet1)

#Remove non english words
tweet1 <- tweet[which(!grepl("[^\x01-\x7F]+", tweet$text)),]


str(tweet1)


#create corpus (data structure for test mining and manupilation)
tweet_corpus<-VCorpus(VectorSource(tweet1$text))
print(tweet_corpus)
# to lower+ remove number+ remove stopwords + 
#remove punctuation + trim whitespaces + stem document +
#remove url+ only letters 

NumPunct<- function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeURL<-function(x) gsub("http[^[:space:]]*","",x)
tweet_clean<-tm_map(tweet_corpus,content_transformer(removeURL))
tweet_clean<-tm_map(tweet_clean,content_transformer(NumPunct))
tweet_clean<-tm_map(tweet_clean,removeNumbers)
tweet_clean<-tm_map(tweet_clean,removeWords,stopwords())
tweet_clean<-tm_map(tweet_clean,removePunctuation)
tweet_clean<-tm_map(tweet_clean,stemDocument)
tweet_clean<-tm_map(tweet_clean,stripWhitespace)
tweet_clean<-tm_map(tweet_clean,removeWords,letters)
tweet_clean<-tm_map(tweet_clean,content_transformer(tolower))



as.character(tweet_clean[[9]])

#SPlitting text doxuments into words 
#corpus into MATRIX
tweet_dtm<-DocumentTermMatrix(tweet_clean)
inspect(tweet_dtm)
# frequent words
freq_words<-findFreqTerms(tweet_dtm_train,5)

str(freq_words)
head(freq_words)
freq_words[[1]]
inspect(freq_words)
#filter DTM 

S_DTM<-tweet_dtm[,freq_words]
#tweet_dtm_freq_test<- tweet_dtm_test[,freq_words]
#inspect(tweet_dtm_freq_train)
S_DTM <- removeSparseTerms(tweet_dtm, 0.995)
S_DTM
inspect(S_DTM)

##changing document term matrix explicitly into data frame

tweetsS <- as.data.frame(as.matrix(S_DTM))

colnames(tweetsS) <- make.names(colnames(tweetsS))

##adding label in DTM data frame as one of the column
tweetsS$label <- tweet1$label

head(tweetsS$label)

nrow(tweetsS)


