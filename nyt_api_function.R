# NYT API Script
# 2021/12/28

apikey <- ''

nyt_scraper <- function(query, start_date, end_date, output_name) {
  
  base_url <- paste0('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=', query,
                     '&begin_date=', start_date,'&end_date=', end_date,
                     '&facet_filter=true&api-key=', apikey)
  
  initial_query <- fromJSON(base_url)
  
  max_pages <- round((initial_query$response$meta$hits[1] / 10) - 1) 
  
  pages <- list()
  
  time_start <- Sys.time()
  
  estimated_time <- round((max_pages * 10) / 60)

  print(paste0('Entering loop: ', max_pages, ' total iterations'))
  
  print(paste0('Started at time: ', time_start))
  
  print(paste0('Estimated total time: ', estimated_time, ' minutes'))
  
  for(i in 0:max_pages) {
    
    result <- fromJSON(paste0(base_url, '&page=', i), flatten = TRUE) %>% data.frame() 
    
    msg <- paste0('Scraping page number: ', i, '\n', 'From link: ', base_url, '&page=', i, '\n')
    
    message(msg)
    
    pages[[i+1]] <- result
    
    Sys.sleep(7) 
    
  }
  
  print('Finished loop, building file.\n')
  
  data_combined <- rbind_pages(pages) %>% 
    select(-c(response.docs.multimedia, response.docs.keywords, response.docs.byline.person)) %>%
    as_tibble()
  
  output_name_end <- stri_sub(output_name, -4, -1)
  
  if (output_name_end != '.csv') {
    
    output_file <- paste0(output_name, '.csv')
    
  }
  
  if (file.exists(output_file) == FALSE) {
    
    write.csv(data_combined, output_file)
    
  }
  
  time_end <- Sys.time()
  
  time_taken <- time_end - time_start
  
  print(paste0('\nLoop finished in: ', time_taken, ' minutes'))
  
}










