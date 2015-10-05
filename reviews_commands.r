trials=read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
max(nchar(trials$abstract)) #find the maximum article length
table(nchar(trials$abstract)==0) #find the number of missing abstracts
which.min(nchar(trials$title)) #which title is shortest

library("tm")

#preprocess the title data
corpusTitle=Corpus(VectorSource(trials$title))
corpusTitle=tm_map(corpusTitle,tolower)
corpusTitle=tm_map(corpusTitle,removePunctuation)
corpusTitle=tm_map(corpusTitle,removeWords,stopwords("english"))
corpusTitle=tm_map(corpusTitle,stemDocument)
dtmTitle=DocumentTermMatrix(corpusTitle)
dtmTitle=removeSparseTerms(dtmTitle,0.95)
dtmTitle=(as.data.frame(as.matrix(dtmTitle)

#preprocess the abstract data
corpusAbstract=corpus(VectorSource(trials$abstract))
corpusAbstract=tm_map(corpusAbstract,tolower)
corpusAbstract=tm_map(corpusAbstract,removePunctuation)
corpusAbstract=tm_map(corpusAbstract,removeWords,stopwords("english"))
corpusAbstract=tm_map(corpusAbstract,stemDocument)
dtmAbstract=DocumentTermMatrix(corpusAbstract)
dtmAbstract=removeSparseTerms(dtmAbstract,0.95)
dtmTAbstract=(as.data.frame(as.matrix(dtmAbstract)

csAbstract=colSums(dtmAbstract)#compute the column sums
which.max(csAbstract) #find the most common one(word stem)

colnames(dtmTitle)=paste0("T",colnames # Adds a letter T to each column name for dtmTitle, which are variable names
colnames(dtmAstract)=paste0("A",colnames(dtmTitle)) #Adds a letter A to each column

dtm(cbind(dtmTitle,dtmAbstract) #combines dataframes dtmTitle and dtmAbstract
dtm$trial = trials$trial #add trials variable to new dtm

#split data into train and test sets, with 70% going to training
set.seed(144)
spl=sample.split(dtm$trial, 0.7)
train = subset(dtm,spl ==TRUE)
test=subset(dtm,spl==FALSE)


#create a CART model
trialCART=rpart(trial~.,data=train,method="class")
prp(trialCART)

# finding training set predictions, keeping only second column of predict output
predTrain=predict(trialCart),[2]
summary(predTrain)

#create confusion matrix to determine accuracy, sensitivity, and specificity
table(train$trial, predTrain >= .05)

# testing set predictions and confusion matrix
predTest=predict(trialCART, newData=test),[2]
table(test$trial,predTest >=0.5)

#determine what is the testing set AUC of the prediciton model
library(ROCR)
pred=prediction(predTest, test$trial)
as.numeric(performance(pred,"auc")@y.values



