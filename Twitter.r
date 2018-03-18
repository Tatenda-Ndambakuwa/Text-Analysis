install.packages("TwitteR")
library(stringr)
library(twitteR)
library(xlsx)
library(plyr)
api_key<- "insert consumer key here"
api_secret <- "insert consumer secret here"
access_token <- "insert access token here"
access_token_secret <- "insert access token secret here
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
setwd("C:/Users/Tatenda/Documents")
neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")
neg = c(neg, 'screw you')
score.sentiment = function(tweets, pos.words, neg.words)
 
{
 
require(plyr)
require(stringr)

scores = laply(tweets, function(tweet, pos.words, neg.words) {



tweet = gsub('https://','',tweet) # removes https://
tweet = gsub('http://','',tweet) # removes http://
tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
       #like emoticons 
tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
tweet = gsub('\\d+', '', tweet) # removes numbers
tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 

tweet = tolower(tweet) # makes all letters lowercase

word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
 
words = unlist(word.list) # turns the list into vector
 
pos.matches = match(words, pos.words) ## returns matching 
          #values for words from list 
neg.matches = match(words, neg.words)
 
pos.matches = !is.na(pos.matches) ## converts matching values to true of false
neg.matches = !is.na(neg.matches)
 
score = sum(pos.matches) - sum(neg.matches) # true and false are 
                #treated as 1 and 0 so they can be added
 
return(score)
 
}, pos.words, neg.words )
 
scores.df = data.frame(score=scores, text=tweets)
 
return(scores.df)
 
}
tweets = searchTwitter('RIP Save',n=2500)
Tweets.text = laply(tweets,function(t)t$getText()) # gets text from Tweets

analysis = score.sentiment(Tweets.text, pos, neg) # calls sentiment function
hist(analysis$score)
write.xlsx(analysis, "myResults.xlsx")
