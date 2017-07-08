setwd("C:/Users/Ethan/Documents/Spring 2017/DS4100/Extra Credit")
library(RCurl)
library(ROAuth)
library(streamR)
library(twitteR)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

outFile <- "tweets_sample.json"

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "oH2X1aadnRpITwhQ8jkshXm9y"                                                                                   
consumerSecret <- "lk1Gx2155YyDa0IkmnHxm2QzEqKQxMfiMnj8TPNfMemAVPIxMw"                                                                                   
accessToken <- "983977105-ullvXjXYXLEucIssAcCWxHbsjL16l5RJzla8eThq"
accessTokenSecret <- "cAmQFwk900TD9iC6YHqOGyTxe0kumRTmcXIObX51JNZjq"

my_oauth <- OAuthFactory$new( consumerKey=consumerKey,
                              consumerSecret=consumerSecret,
                              requestURL=requestURL,
                              accessURL=accessURL, 
                              authURL=authURL)
my_oauth$handshake(cainfo="cacert.pem")

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

sampleStream( file=outFile, oauth=my_oauth, tweets=100 )

follow <- ""  
track <- "Boston,RedSoxs"  
location <- c(23.786699, 60.878590, 37.097000, 77.840813)  
filterStream( file.name=outFile, follow=follow, track=track, 	locations=location, oauth=my_oauth, timeout=5)
