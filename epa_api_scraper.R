

epa_scraper <- function(table_names) {

	for (i in 1:length(table_names)) {
	  
	  table <- table_names[i]
	  
	  url_nrow <- paste0('https://data.epa.gov/efservice/', table, '/COUNT')
	  
	  max_rows <- xml_find_all(read_xml(url_nrow), xpath = '/Envirofacts/RequestRecordCount', flatten = TRUE) %>% 
	    str_extract(pattern = "\\d+")
	  
	  i <- as.numeric(max_rows)
	  
	  print(paste0('Max rows of table ', table, ' is: ', i))
	  
	  if (i < 9999) {
	    
	    url <- paste0('https://data.epa.gov/efservice/', table)
	    
	    print(paste0('URL is: ', url))
	    
	    file_name <- paste0('data/', table, '.xml')
	    
	    download_xml(url, file = file_name)
	    
	    Sys.sleep(1)
	    
	  } else {
	    
	    message('Starting pagination')
	    
	    while (i > 0) {
	      
	      max <- i - 1
	      
	      if (i - 9999 > 0) {
	        
	        min <- i - 10000
	        
	        i <- i - 10000
	        
	      } else {
	        
	        min <- 1
	        
	        range <- paste(as.character(min), as.character(max), sep = ":")
	        
	        url <- paste0('https://data.epa.gov/efservice/', table, '/', range)
	        
	        print(paste0('URL is: ', url))
	        
	        file_name <- paste0('data/', table, '_' ,range ,'.xml')
	        
	        print(paste0('Downloading file: ', file_name))
	        
	        download_xml(url, file = file_name)
	        
	        Sys.sleep(1)
	        
	        break
	        
	      }
	      
	      range <- paste(as.character(min), as.character(max), sep = ":")
	      
	      url <- paste0('https://data.epa.gov/efservice/', table, '/', range)
	      
	      print(paste0('URL is: ', url))
	      
	      file_name <- paste0('data/', table, '_' ,range ,'.xml')
	      
	      print(paste0('Downloading file: ', file_name))
	      
	      download_xml(url, file = file_name)
	      
	      Sys.sleep(1)
	      
	    }
	    
	  }
	  
	}

}

epa_scraper(c('pub_dim_facility', 'pub_facts_sector_ghg_emission', 'pub_dim_ghg'))
