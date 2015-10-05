emails = read.csv("emails.csv", stringsAsFactors=FALSE) # created data set
str(emails) # determine number of emails
table(emails$spam) # how many emails are spam

#which word appears at the beginning of every email, as lower case word and punct removed
emails$text[1]
emails$text[1000] #compare the two, but every one begins the same

#ncar function counts the number of characters in a piece of text
# detetmine the amount of characters in the longest email
max(nchar(emails$text))
#which row contains shortest email

which(nchar(emails$text)==13)

#or
which.min(nchar(emails$text))

#PreProcess the corpus
library("tm")
library("rpart")
library("rpart.plot")
library("randomForest")
library("caTools")

corpus=Corpus(VectorSource(emails$text))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,stemDocument)
dtm=DocumentTermMatrix(corpus)
dtm

#remove sparse terms
spdtm=(removeSparseTerms(dtm,0.95))
spdtm

#build a dataframe from spdtm and use the make.names function to make the variable names valid

emailsSparse=as.data.frame(as.matrix(spdtm))
colnames(emailsSparse)=make.names(colnames(emailsSparse))

#find the word stem that returns the most common word
sort(colSums(emailsSparse))
#or
which.max(colSums(emailsSparse))

#add a variable called spam to emails Sparse
emailsSparse$spam=emails$spam

#count the number of wordstems that appears at least 5000 times(not counting variable just added)
#shows the list from small to big
sort(colSums(subset(emailsSparse,spam ==0)))

#how many word stems appear at least 1000 times, spam is the dependent var
sort(colSums(subset(emailsSparse,spam ==1)))

#convert the dependent variable to a facor, set random seet to 123, and do a 70/30 split for train and test

emailsSparse$spam=as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
spl=sample.split(emailsSparse$spam,0.7)
train=subset(emailsSparse,spl==TRUE)
test=subset(emailsSparse,spl==FALSE)

#create a logistic regression model called spamLog
#create a CART model called spamCART
#create a random forest model called spamRF

spamLog=glm(spam~.,data=train,family="binomial")
spamCART=rpart(spam~.,data=train,method="class")
set.seed(123)
spamRF=randomForest(spam~.,data=train,method="class")

#for each model, obtain the predicted spam probabilities for the training set, and #be careful to obtain probabilities instead of predicted classes, as the values will #be used to compute AUC values. Remember that you an obtain probabilities from a #random forest by adding the argument type="prob". For cart and random forest, you #need #to select the second column of the output from predict() function, #corresponding to the probability of a message being spam


predTrainLog=predict(spamLog,type="response")
predTrainCART=predict(spamCART)[2]
predTrainRF=predict(spamRF,type="prob")[2]

#messages like "algorithm did not converge" and "fitted probabilities numerically 0 or 1 occurred" indicate overfitting and the first indicates severe overfitting, #often to the point that training sets are fit perfectly by the model

#how many training set probs form spamlog are less than 0.00001
#how many of the training set probs from spamlog are greater than 0.99999
#how many of the training set probs are between 0.00001 and 0.99999

table(predTrainLog<0.00001)
table(predTrainLog>0.999999)
table(predTrainLog>=0.00001  & predTrainLog <= 0.99999)

#how many variables are labeled as significant(at the p=0.0 level) in the logistic
#regression summary output?

summary(spamLog)

#how many word stems "enron", "hou", "vinc" and "kaminski" appear in cart tree
prp(spamCART)

#training set accuracy of spamlog
table(train$spam,predTrainLog >=0/5)
#accuracy is 3052+954/nrow(train) 

#training set auc of spam log
predictionTrainLog=prediction(predTrainLog,train$spam)
as.numeric(performance(predictionTrainLog,"auc")@y.values)

#training set accuracy of spamCART, using 0.5 threshold
table(train$spam,predTrainCART >= 0.5)
# then the accuracy is (2885+894)/nrow(train)

#determine the training set auc of spam cart
predictionTrainCART=prediction(predTrainCART,train$spam)
as.numeric(performance(predictionTrainCART,"auc")@y.values)

#determine the training set accuracy of spamRF, 0.5 threshold
table(train$spam,predTrainRF >=0.5)
# the accuracy is (3013+914)/nrow(train)

#determine the training set auc of spamRF
predictionTrainRF=prediction(predTrainRF,train$spam)
as.numeric(performance(predictionTrainRF,"auc")@y.values

#obtain predicted probabilities for the testing set of each model
predTestLog=predict(spamLog,newdata=test,type="response")
predTestCART=predict(spamCART,newdata=test)[,2]
predTestRF=predict(spamRF,newdata=test,type="prob")[,2]

#determine the testing set accuracy of spamLog, using 0.5 threshold
table(test$spam,predTestLog>=0.5)
#then the accuracy is (1257+376)/nrow(test)

#determine the testing set auc of spamLog
predictionTestLog=prediction(predTestLog,test$spam)
as.numeric(performance(predictionTestLog,"auc")@y.values)

#testing set accuracy of spam cart, using 0.5 threshold
table(test$spam,predTestCART >=0.5)
#then the accuracy is (1228+386)/nrow(test)

#determine testing set auc of sparmCART
predictionTestCART=prediction(predTestCART,test$spam)
as.numeric(performance(predictionTestCART,"auc")@y.values)

#determine testing set accuracy of spamRF
table(test$spam,predTestRF>=0.5)
# then the accuracy is (1290+386)/nrow(test)

#determine testing set auc of spamRF
predictionTestRF=prediction(predTestRF,test$spam)
as.numeric(performance(predictionTestRF,"auc")@y.values)


## End Homework Part 1 of Separating Spam from Ham

## Start Homework Part 2 of Separating Spam from Ham
