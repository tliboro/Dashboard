libs <- c('data.table', 'dplyr', 'magrittr', 'ggplot2', 'plyr', 'readr', 'plotly', 'crosstalk', 'DT')
lapply(libs, require, character.only = T)

dir <- "/Users/tylerliboro/Desktop/MyData/"

files <- list.files(path = dir, pattern = "*.csv", all.files = T, full.names = T)   

data <- do.call(rbind, lapply(files, function(fn) read.csv(file = fn, header = T)) ) %>% as.data.table

#Exploration
head(data)  
summary(data)
names(data)

dt <- data[, secPlayed := format( as.POSIXct(Sys.Date())+`msPlayed`/1000, "%M:%S")] %>%
  .[, minPlayed := as.numeric(substring(text = secPlayed, 
                                      first = 2, 
                                      last = 2) )] %>%
  .[, hrPlayed := round(x = minPlayed/60, digits = 2)] %>%
  .[, endtime_Day := substr(x = endTime, start = 9, stop = 10)] %>%
  .[, endtime_Month := substr(x = endTime, start = 6, stop = 7)] %>%
  .[, endtime_Year := substr(x = endTime, start = 1, stop = 4)] 
  

shared_dt <- SharedData$new(dt)

topartists_2019 <- dt[endtime_Year %in% 2019]$artistName %>% table %>%
  sort(x = ., decreasing = T) %>% as.data.table %>% setnames(., old = '.', new = 'Artist') %>%
  .[1:10]

newDF <- dt[artistName %in% topartists_2019$Artist]
newDF <- as.data.frame(newDF)
newDF$artistName <- factor(newDF$artistName)

p <- plot_ly(data = newDF, x = ~endtime_Month, 
        color = ~artistName, type = 'box',
        showlegend = F) %>%
  config(displayModeBar = F) #%>%
  #layout(title = 'Top Artists in 2019')

songs <- dt$trackName %>% table %>% 
  sort(x = ., decreasing = T) %>% as.data.table %>%
  setnames(., old = '.', new = 'Song') %>%
  .[1:10] %>%
  .[ , Artist := dt[trackName %in% Song]$artistName, by = c("Song")] %>%
  .[, c('Artist', 'Song', 'N')]
