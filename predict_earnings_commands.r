census = read.csv("census.csv")
library(caTools)
set.seed 2000

#Split the dataset based on earnings over 50k

splitData = sample.split(census$over50k,SplitRatio = 0.6)
train = subset(census, splitData ==TRUE)
test = subset(census, splitData ==FALSE)

#create the model, using over50k as the dependent variable, all others independent

censusglm = glm(over50k ~.,family = "binomial", data = train)

summary(censusglm)

#Determine the accuracy, using a threshold of 0.5

predictTest = predict(censusglm, newdata = test, type="response")

table(test$over50k, predictTest >=0.5)

#Obtain the accuracy by dividing the sum of the top left and bottom right numbers over the sum of all numbers

(9009+1855)/(9009+704+1223+1855)
#[1] 0.8493472

#This says the accuracy is about 84%
#To find the baseline accuracy model, first determine the most frequent outcome in the training set

table(train$over50k)

#The most frequent is <=50k. 

table(test$over50k)

#Then, using the test data, determine the accuracy by dividing the sum of the number in the top left and bottom right #over the sum of all numbers

(9713)/(9713+3078)
#[1] 0.7593621

#We can tell the logistic regression model is more accuracte

#Determine the area under curve(AUC)

library(ROCR)

ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCpred,"auc")@y.values

#Build a classification tree for the model to determine which variables are statistically significant

library(rpart)

censustree=rpart(over50k~.,method="class",data="train")

prp(censustree)

#Generate predictions on the test set using the CART tree

predictTest = predict(censustree, newdata = test, type = "class")
table(test$over50k, predictTest)


#Find the AUC of the CART model

predictTest = predict(censustree, newdata = "train")
predictTest = predictTest[,2] # take second column

ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values

#Create a random forest, with a smaller sample for memory's sake

set.sed(1)
trainSmall = train[sample(nrow(train),2000),]

#Trying to run this will throw an error regarding categorical predictiors
#R can't handle a factor variable with more than 32 possible values, and nativecountry is the offender

censusRF = randomForest(over50k~., data = trainSmall)
str(census)

#Now build the random forest without the offending variable

set.sedd(1)
censusRF = randomForest(over50k ~., -nativecountry, data = trainSmall)

#Make predictions on the data
predictTest = predict(censusRF, newdata=test)

#Compute the accuracy
table(test$over50k, predictTest)

#one of the issues with Random Forest is the large collection of trees, and the loss of some interpretability that comes with seeing how predictions are made and what variables are important. However, metrics can still be gathered to gain insight

#One metric that can be seen is the number of times, aggregated over all the trees, that a certain variable is used for a split

varU = varUsed(censusRF, count = TRUE)
varUsorted = sort(varU, decreasing = FALSE, index.return = TRUE)
dotchart(varUsorted$x, names(censusRF$forest$xlevels[varUsorted$ix]))

#This code produces a chart for that for each variable measures the number of times a variable was chosen for splitting (value on X axis). Which was the most important? Age of course!


#Another metric that can be seen is the "impurity", which measures how homogenous each bucket or leaf the tree is. Whenever a variable is selected, and a split is formed, the impurity decreases. Therefore, one way to measure the importance of te variable is to average the reduction in impurity, taken over alll the times that a variable is selected for splitting in all the trees in the forest.

varlmpPlot(censusRF)

#The most important variable in terms of mean reduction in impurity is occupation

#Now lets look at how CART behaves with different choices of its parameters. We will use k-fold cross validation, wit k = 10 folds. The P values will be set from 0.002 to 0.1 in 0.002 increments

#This might take some time to run, so please be patient

library(caret)

set.seed(2)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grad(.cp = seq(0.002, 0.1, 0.002))
train(over50k~., data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

#The final outcome should display "Accuracy was used to select the optimal mode using the largest value. The final value used for the model was cp = 0.002

#In other words, the best value was 0.002, corresponding to the lowest cp value. The accuracy shows that it is decreasing steadily as the cp value increases. The cp value needs to become quite low before the accuracy starts to degrade

#Fit a CART model ot the training data using the new cp value

censustree = rpart(over50k ~., data = train, cp = 0.002)
predictTest = predict(censustree, newdata = test, type = "class")

table(test$over50k, predictTest)
#should see higher accuracy


prp(censustree)

#should see 18 splits
