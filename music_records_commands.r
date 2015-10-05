songs = read.csv("songs.csv")
# num of songs in 2010
table(songs$year)

#how many songs include michael jackson
MichaelJackson = subset(songs,artistname == "Michael Jackson")
str(MichaelJackson)

#which songs made top 10
MichaelJackson[c("songtitle","Top10")]

#what are values of time signature?
#which values occur most often?
table(songs$timesignature)

#which song has the highest tempo?
which.max(songs$tempo)
songs$songtitle[6206]

#predict whether or not a song will make top 10

#first create a subset of songs, <= 2009, another =2010
SongsTrain=subset(songs,year<=2009)
SongsTest=subset(songs,year==2010)
nrow(SongsTrain)
#or
str(SongsTrain)

# the outcome variable is top 10, so whether or not a song makes it is a binary outcome (true/false, 1/0)
# all song attributes will be independent variables, used in model1
# only using variables that are numeric
# adds all variables
SongsLog = glm(Top10~.,data=SongsTrain,family=binomial)

# excludes variables you dont want to use, first create a vector to contain them
nonvars=c("year","songtitle","artistname","songID","artistID")

# remove the variables
SongsTrain = SongsTrain[,!(names(SongsTrain)%in% nonvars)]
SongsTest = SongsTest[,!(names(SongsTest)%in% nonvars)]

# look at the summary of the model. What is the Akaike Information Criterion (AIC)?
SongsLog1=glm(Top10~.,data=SongsTrain,family=binomial)
summary(SongsLog1)

# look at the variables related to the confidence about time signature, key, and temp
# timesignature_confidence, key_confidence, tempo_confidence
# the model indicates that the confidence variables are significant. what does this suggest?
# ** The higher the confidence about sig, key, and temp, the more likely top 10 **
# if you look at the output of summary, the coefficient estimates for the confidence variables are positive

# if the confidence is low, then the song is likely to be complex, which means people like less complex songs
