library(httr)
library(plyr)

get_recent_tracks <- function(user = '', 
                              api_key = '', 
                              from = '', 
                              to= '') {
  
  #convert dates to numeric
  
  if(from != "") {from = as.numeric(as.POSIXct(from))}
  if(to != "") {from = as.numeric(as.POSIXct(to))}
  
  #create placeholder for tracks data
  recent_tracks <- data.frame()
  
  #test request, get number of pages
  
  request <- GET(url = "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks", 
                 query = list(user = user, 
                              from = from,
                              to = to,
                              limit = 200,
                              api_key = API_KEY, 
                              format = "json"))
  
  if(status_code(request) == 200) {
    
    response <- content(request)
    response_metadata <- response$recenttracks$'@attr'
    
    pagenum <- response_metadata$totalPages
    
  } else {
      
    return(status_code(request))
    
    }
  
  #request data
  
  for(page in 1:pagenum) {
    
    message(paste('requesting page number', page, sep = ' '))
    
    request <- GET(url = "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks", 
                   query = list(user = user, 
                                from = from,
                                to = to,
                                limit = 200,
                                page = page,
                                api_key = API_KEY, 
                                format = "json"))
    
    if(status_code(request) == 200) {
      
      response <- content(request)
      response_metadata <- response$recenttracks$'@attr'
      
      tracks <- ldply(response$recenttracks$track, data.frame)
      
      recent_tracks <- rbind.data.frame(recent_tracks, tracks)
      
      Sys.sleep(1)
      
      
    } else {
      
      return(status_code(request))
      
    }
    
  }

  return(recent_tracks)
  
}