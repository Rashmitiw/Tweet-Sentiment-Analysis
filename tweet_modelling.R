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




####Partition in train and test

ind<-createDataPartition(tweetsS$label,p=0.86,list = FALSE)
tweet_dtm_train<-tweetsS[ind,]
tweet_dtm_test<-tweetsS[-ind,]
str(tweet_dtm_train)


####train model with out 10 k fold cross validation

######################
####Random Forest

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






##############################################################################
#########Decision Tree 


tweet_DT_model <- rpart(label~ . , data = tweet_dtm_train, method = "class")

#Predict result using model

predict_tweet_DT <- predict(tweet_RF_Model, newdata = tweet_dtm_test)

#Cross table
CrossTable(predict_tweet_DT,tweet_dtm_test$label,prop.chisq = FALSE,prop.t = FALSE,prop.r=FALSE,dnn=c('predicted','actual'))

#predicting result in probability for cal AUC

PredictionDT<-predict(tweet_DT_Model,tweet_dtm_test,type = "prob")
head(PredictionRF)

###AUC score

auc<-auc(tweet_dtm_test$label,PredictionDT[,2])
auc

####Confusion Matrix

confusionMatrix(predict_tweet_DT,tweet_dtm_test$label)


##############################################################
##########Logistic Regression

tweet_LR_model <- glm(label~ . , data = tweet_dtm_train,family = "binomial")



#Predict result using model

predict_tweet_LR <- predict(tweet_LR_Model, newdata = tweet_dtm_test)

#Cross table
CrossTable(predict_tweet_LR,tweet_dtm_test$label,prop.chisq = FALSE,prop.t = FALSE,prop.r=FALSE,dnn=c('predicted','actual'))

#predicting result in probability for cal AUC

PredictionLR<-predict(tweet_LR_Model,tweet_dtm_test,type = "raw")
head(PredictionLR)

###AUC score

auc<-auc(tweet_dtm_test$label,PredictionLR[,2])
auc

####Confusion Matrix

confusionMatrix(predict_tweet_LR,tweet_dtm_test$label)




