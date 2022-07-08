
require(stringr)
require(stringi)
require(httr)
require(jsonlite)
require(plyr)

twitter_scraper <- function(iterations, output_name = '') {
  
  if (iterations <= 0) {
    
    print('No iterations specified')
    
    break
    
  }
  
  bearer_token <- Sys.getenv("BEARER_TOKEN")
  
  if (nchar(bearer_token) <= 1) {
    
    print('No Bearer Token Found: Exiting Loop')
    
    break
    
  }
  
  if (nchar(output_name) <= 1) {
    
    output_name <- 'twitter_dat.json'
    
  }
  
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))
  
  params = list(
    `query` = '#covid OR covid OR Coronavirus OR coronavirus OR #Coronavirus OR #coronavirus',
    `expansions` = 'geo.place_id',
    `max_results` = '10',
    `tweet.fields` = 'author_id,created_at,lang,conversation_id,entities,referenced_tweets',
    `user.fields` = 'id,name',
    `place.fields` = 'contained_within,country,country_code,full_name,geo,id,name,place_type'
  )
  
  dat <- list()
  
  initial_request <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                               httr::add_headers(.headers=headers), query = params) %>% 
    httr::content(as = "text") %>% 
    fromJSON(flatten = TRUE) %>% 
    as.data.frame()
  
  i <- 1
  
  if (i == 1) {
    
    print(paste0('Starting iteration loop, total iterations: ', iterations))
    
    print(paste0('Current iteration: ', i))
    
    dat[[1]] <- initial_request
    
  }
  
  Sys.sleep(2)
  
  repeat {
    
    if (is.null(dat[[i]]$meta.next_token[1]) == TRUE) {break}
    
    if (i > iterations) {break}
    
    next_token <- dat[[i]]$meta.next_token[1]
    
    i <- i + 1
    
    print(paste0('Current iteration: ', i))
    
    params = list(
      `query` = '#covid OR covid OR Coronavirus OR coronavirus OR #Coronavirus OR #coronavirus',
      `expansions` = 'geo.place_id',
      `max_results` = '10',
      `tweet.fields` = 'author_id,created_at,lang,conversation_id,entities,referenced_tweets',
      `user.fields` = 'id,name',
      `place.fields` = 'contained_within,country,country_code,full_name,geo,id,name,place_type',
      `next_token` = next_token
    )
    
    dat[[i]] <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                          httr::add_headers(.headers=headers), query = params) %>% 
      httr::content(as = "text") %>% 
      fromJSON(flatten = TRUE) %>% 
      as.data.frame()
    
    n_obs <- sapply(dat, nrow) %>% sum()
    
    print(paste0('Number of observations collected: ', n_obs))
    
    Sys.sleep(2)
    
    print('Sleeping for 2 seconds')
    
  }
  
  data_combined <- rbind.fill(dat)
  
  output_name_end <- stri_sub(output_name, -5, -1)
  
  if (output_name_end != '.json') {
    
    output_file <- paste0(output_name, '.json')
    
  } else {
    
    output_name <- gsub('.{5}$', '', output_name)
    
    output_file <- paste0(output_name, '.json')
  }
  
  if (file.exists(output_file) == FALSE) {
    
    print(paste0('Writing file: ', output_file))
    
    write_json(data_combined, output_file)
    
  } else {
    
    print('File already exists, exiting.')
    
    break
    
  }
  
}

twitter_scraper(100, 'twitter_cov_dat')