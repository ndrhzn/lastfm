library(dplyr)

classify_tracks <- function(data = '') {
  
  listened <- vector(mode = 'character')
  
  data$listened <- NA
  
  for(i in 1:nrow(data)) {
    
    if(data$url[i] %in% listened) {
      
      data$listened[i] = -1
      
    } else {
      
      data$listened[i] = 1
      
    }
    
    listened <- append(x = listened, values = as.character(data$url[i]))
  }
  
  data <- data %>% 
    group_by(session_num) %>% 
    mutate(track_position = cumsum(listened),
           session_status = ifelse(sum(track_position) > 0, 'discovery', 'repeating'))
  
  return(data)
  
}