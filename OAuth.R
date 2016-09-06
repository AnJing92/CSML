setwd("/Users/Fangzhou/Desktop/project/R")
# Install and Activate Packages
install.packages("streamR")
install.packages("RCurl")
install.packages("ROAuth")
install.packages("RJSONIO")
install.packages("stringr")
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)

# PART 1: Declare Twitter API Credentials & Create Handshake
library(ROAuth)
request_URL <- "https://api.twitter.com/oauth/request_token"
access_URL <- "https://api.twitter.com/oauth/access_token"
auth_URL <- "https://api.twitter.com/oauth/authorize"
consumer_key <- "6pBZrviQqveIkqIY6RZNaVaTw" 
consumer_secret <- "6psKscbNB4SNutfTWqMKBDjGALq7RTHUmGIbanSIX5ALyOt5i5" 

oauth_01 <- OAuthFactory$new(consumerKey = consumer_key,
                             consumerSecret = consumer_secret,
                             requestURL = request_URL,
                             accessURL = access_URL,
                             authURL = auth_URL)

oauth_01$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

### STOP HERE!!! ###

# PART 2: Save the my_oauth data to an .Rdata file
save(oauth_01, file = "oauth_01.Rdata")