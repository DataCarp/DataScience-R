library(tm)
tweets=read.csv("tweets.csv",stringsAsFactors=FALSE)
corpus=Corpus(VectorSource(tweets$Tweet)
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpuse,PlainTextDocument)
corpus=tm_map(corpuse,removePunctuation)
corpus=tm_map(corpuse,removeWords,stopwords("english"))
frequencies=DocumentTermMatrix(corpus)
allTweets=as.data.frame(as.matrix(frequencies))

#read total amount of words across tweets
frequencies
#or
str(allTweets)
# or
ncol(allTweets)

library("wordcloud")

#get the names of all the columns of all tweets
colnames(allTweets)

#access the sums of each column in all tweets
colSums(allTweets)

#create a scale. 4 for the most frequent words, to 0.5 for less

scale=c(2,0.25)

#output the wordcloud
wordcloud(colnames(allTweets),colSums(allTweets)


# Create a new word cloud and remove the most frequent word
#replace allTweets with the documentTermMatrix

tweets=read.csv("tweets.csv",stringsAsFactors=FALSE)
corpus=Corpus(VectorSource(tweets$Tweet)
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpuse,PlainTextDocument)
corpus=tm_map(corpuse,removePunctuation)
corpus=tm_map(corpuse,removeWords,c("apple",stopwords("english")))
frequencies=DocumentTermMatrix(corpus)
allTweets=as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25)

#iphone should now be the most common, instead of apple

#obtain a word cloud that is limited to a subset of the tweets with most common negative words

negativeTweets=subset(allTweets,tweets$avg<=-1)
wordcloud(colnames(negativeTweets),colSums(negativeTweets))


#rot.per= 0.5 - means rotate 50% of words
# load colors for word clouds

install.package("RColoBrewer")
library(RColorBrewer)

#color patterns Accent and Set 2 are qualitative palettes, which means color changes dony imply a change in magnitude. can also verify with output of display.brewer.all()

#the palette YIOrRd is a sequential palette, with earlier colors being lighter and later colors being darker. This is good for indicating low vs high freq. words

#colors=brewer.pal(9,"Blues") returns a sequential palette of blues with 9 colors

# to remove the first four, type either:
brewer.pal(9,"blues")[c(-1,-2,-3,-4)]
#or
brewer.pal(9,"Blues")[c(5,6,7,8,9)]
or [-1:-4] or [5:9]









