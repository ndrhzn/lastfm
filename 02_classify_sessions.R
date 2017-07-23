library(dplyr)
library(tidyr)

classify_sessions <- function(data = ''){
  
  data <- data %>% 
    select(artist..text, name, album..text, url, date.uts) %>% 
    mutate(datetime = as.POSIXct(as.numeric(as.character(date.uts)), origin = '1970-01-01')) %>%
    arrange(datetime) %>% 
    mutate(timediff =  as.numeric(datetime - lag(datetime))) %>% 
    replace_na(list(timediff = 0))
  
  data$session_num <- NA
  
  session = 1
  
  for(i in 1:nrow(data)) {
    
    if(data$timediff[i] <= 900) {
      
      data$session_num[i] = session
    
    } else {
      
      session = session + 1
      
      data$session_num[i] = session
      
    }
  }
  
  data <- data %>% 
    group_by(session_num) %>% 
    mutate(track_num = row_number())
  
  return(data)
  
}