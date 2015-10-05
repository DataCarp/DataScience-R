pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# find average reading score of males
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

# find average reading score of females
tapply(pisaTrain$readingScore,pisaTrain$female,mean)

#find which variables are missing data
summary(pisaTrain)

#removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#determine the number of rows in the data set
str(pisaTrain)
str(pisaTest)

#factor variables are variables that take on a discrete set of values, like a "region" value. This is an unordered factor because there i no underlying order to it. A ordered factor has natural order like small/med/large

#to include unordered factors in a linear regression model, we define one level as the reference level, and add a binary variable for each of the remaining levels.The reference level is typically seleced to be the most frequently occurring level in the dataset.However, by default R takes the first level alphabetically, instead of the most common.

#set the reference value
pisaTrain$raceeth=rlevel(pisaTrain$raceeth,"White")pisaTest$raceeth=rlevel(pisaTest$raceeth,"White")

#build a linear regression model

lmScore=lm(readingScore~.,data=pisaTrain)

#read the R^2 value by reading summary(lmScore)
#the R squared will be lower, but its not because the model is of low quality. For this case, it is because the prediction problem of predicting a student's test score based on demographic and school related variables is more difficult than other prediction problems like predicting a team's number of wins from runs scored/allowed

#find the root means squared error
SSE=sum(lmScore$residuals^2)
RMSE = sqrt(SEE/nrow(pisaTrain))

#or
sqrt(mean(lmScore$residuals^2))

#predict test scores

predTest = predict(lmSCore,newDAta=pisaTest)

#find the sum of square errors on the test set

sum((predTest-pisaTest$readingScore)^2)

# find the root mean squared error of lmScore on test ste
sqrt(mean((predTest-pisaTest$readingScore)^2))

#what is the predicted test score used in the baseline
baseline = mean(pisaTrain$readingScore)

#what is the sse of the baseline model of the test set

baseline=mean(pisaTrain$readingScor)

#what is SSE of baseline

sum((baseline-pisaTest$readingScore)^2)

#what is the test set r quare?

1-SSE/SST, where SSE is sum of square error on the test set and sst is sum of square errors on baseline.

1-5762082/7802354