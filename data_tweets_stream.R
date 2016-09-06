setwd("/Users/Fangzhou/Desktop/project/R")
install.packages("streamR")
install.packages("RCurl")
install.packages("ROAuth")
install.packages("RJSONIO")
install.packages("stringr")
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
load("oauth_01.Rdata")


filterStream(file.name = "tweets_data.json", # Save tweets in a json file
             #track = c("Affordable Care Act", "ACA", "Obamacare"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
             timeout = 25200, # Keep connection alive for 60 seconds
             location = c(-0.489,51.28,0.236,51.686), # latitude/longitude pairs providing southwest and northeast corners of the bounding box.
             oauth = oauth_01) # Use my_oauth file as the OAuth credentials

tweets_data_full <- parseTweets("tweets_data.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.

tweets_selected<-cbind(tweets_data_full$id_str,
                       tweets_data_full$user_id_str,
                       tweets_data_full$name,
                       tweets_data_full$retweeted,
                       tweets_data_full$created_at,
                       tweets_data_full$location,
                       tweets_data_full$place_lat,
                       tweets_data_full$place_lon,
                       tweets_data_full$place_id,
                       tweets_data_full$lat,
                       tweets_data_full$lon,
                       tweets_data_full$text
                        )
colnames(tweets_selected) <- c("id_str","user_id_str","name",
                                "retweeted","created_at","location",
                                "place_lat","place_lon","place_id",
                                "lat","lon","text"
                                 )

tweets_selected<-as.data.frame(tweets_selected)

write.csv(tweets_selected, file = "data_08_01_a_time.csv",
          row.names = FALSE)
          #col.names = TRUE )

############write.csv(tweets_selected, file = "small_data_07_30_.csv")
#================================================================



tweets_ordered <- tweets_selected[order(tweets_selected$user_id_str),]
tweets_ordered <- as.data.frame(tweets_ordered )

write.csv(tweets_ordered, file = "data_08_01_a_id.csv",
          row.names = FALSE)
          #col.names = TRUE )
