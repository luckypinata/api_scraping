

library(rtweet)
library(maps)
library(jsonlite)
library(tidyverse)


twitter_stream <- function(iterations, timeout, app) {

	load("myauthentication.rda")

	twitter_token <- create_token(app = app, 
                              consumer_key = authentication$consumer_key,
                              consumer_secret = authentication$consumer_secret,
                              access_token = authentication$access_token,
                              access_secret = authentication$access_token_secret)


	if (nchar(twitter_token$credentials[1]) == 0 | nchar(twitter_token$credentials[2]) == 0 ) {break}


	if (iterations <= 0) {break}


	for (i in 0:iterations) {


		file_name <- paste0('streamed_geo_tweets', i, '.json')


		stream_tweets(q = c(-125, 26, -65, 49), timeout = timeout, parse = FALSE, file_name = file_name)


		Sys.sleep(1)

	}


}

twitter_stream(10, 600, 'sentiment_covid_my472')