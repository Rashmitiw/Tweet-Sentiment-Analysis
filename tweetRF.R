
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




####Partition in train and test

ind<-createDataPartition(tweetsS$label,p=0.86,list = FALSE)
tweet_dtm_train<-tweetsS[ind,]
tweet_dtm_test<-tweetsS[-ind,]
str(tweet_dtm_train)



####Cross validation control
Control<-trainControl(method="cv",number=10)

#####Random Forest
#is.na(tweet_dtm_train$label)
#train model with 10 k fold cross validation
tweet_RF_Model<-train(label~.,data=tweet_dtm_train,method="rf", parms = list(split = "information"),trControl=Control,na.action = na.roughfix)

####train model with out 10 k fold cross validation

#TRAIN model 
tweet_RF_Model <- randomForest(label~ . , data = tweet_dtm_train,na.action = na.roughfix)

#Predict result using model

predict_tweet_RF <- predict(tweet_RF_Model, newdata = tweet_dtm_test)

#Cross table
CrossTable(predict_tweet_RF,tweet_dtm_test$label,prop.chisq = FALSE,prop.t = FALSE,prop.r=FALSE,dnn=c('predicted','actual'))

#predicting result in probability for cal AUC

PredictionRF<-predict(tweet_RF_Model,tweet_dtm_test,type = "prob")
head(PredictionRF)

###AUC score

auc<-auc(tweet_dtm_test$label,PredictionRF[,2])
auc

####Confusion Matrix

confusionMatrix(predict_tweet_RF,tweet_dtm_test$label)

