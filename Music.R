libs <- c('data.table', 'dplyr', 'magrittr', 
          'ggplot2', 'plyr', 'readr', 'plotly', 
          'crosstalk', 'DT', 'lubridate')
lapply(libs, require, character.only = T)

dir <- "/Users/tylerliboro/Desktop/MyData/"
files <- list.files(path = dir, pattern = "*.csv", all.files = T, full.names = T)   
data <- do.call(rbind, lapply(files, function(fn) read.csv(file = fn, header = T)) ) %>% as.data.table

#Exploration
head(data)  
summary(data)
names(data)

returnDay <- function(day) {
  list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
            "Friday", "Saturday")
  list[day]
}

dt <- data[, secPlayed := format( as.POSIXct(Sys.Date())+`msPlayed`/1000, "%M:%S")] %>%
  .[, minPlayed := as.numeric(substring(text = secPlayed, 
                                      first = 2, 
                                      last = 2) )] %>%
  .[, hrPlayed := round(x = minPlayed/60, digits = 2)] %>%
  .[, endtime_Day := substr(x = endTime, start = 9, stop = 10)] %>%
  .[, Date := as.Date(endTime)] %>% 
  .[, endtime_Month := substr(x = endTime, start = 6, stop = 7)] %>%
  .[, endtime_Year := substr(x = endTime, start = 1, stop = 4)] %>%
  .[, Day := wday(endTime)] %>%
  .[, Day := returnDay(Day)] %>%
  .[, hour_listened := substr(x = endTime, start = 12, stop = 13) ] %>%
  .[hour_listened %in% '00', hour_listened := '24'] #Formatting
  
# Exploration of Metrics
years <- c(2019, 2020)

#'The following lapply functions will find the metrics of our interest in different years
#'(1) Total Minutes is below.
topArtist <- do.call(rbind, lapply(years, function(year) {
  dt[endtime_Year %in% 2019]$artistName %>% table %>%
    sort(x = ., decreasing = T) %>% as.data.table %>% setnames(., old = '.', new = 'Artist') %>%
    .[1:10] %>% .[, Year := year]
}) ) %>% as.data.table 


#'Crosstalk data table that is connnected to the sidebar
#shared_dt <- SharedData$new(data = dt, key = dt$endtime_Year, group = 'Year')
#shared_dt <- SharedData$new(data = topArtist, key = topArtist$Year, group = 'Year')
# shared_dt <- SharedData$new(data = topArtist, 
#                             key = topArtist$Year,
#                             group = 'tab1')


# 
# p <- plot_ly(data = topArtist, x = ~Artist,
#         y = ~N,
#         name = 'Artist',
#         type = 'bar') %>%
#   config(displayModeBar = F) #%>%
#   #layout(title = 'Top Artists in 2019')
# p
# 
# 
# songs <- dt$trackName %>% table %>%
#   sort(x = ., decreasing = T) %>% as.data.table %>%
#   setnames(., old = '.', new = 'Song') %>%
#   #.[1:10] %>%
#   .[ , Artist := dt[trackName %in% Song]$artistName, by = c("Song")] %>%
#   .[, c('Artist', 'Song', 'N')]
  
#shared_songs <- SharedData$new(data = songs, key = songs$)

get_ordered <- function(column, name) {
  column %>% table %>%
    sort(., decreasing = T) %>% as.data.table(.) %>% 
    setnames(., old = '.', new = name) #%>%
  #  .[1, name] %>% as.character(.)
  
}
returnCharacter <- function(object, name) {
  return(object[1, ..name] %>% as.character(.) )
}



#'Last Metric Should be the total amount of new songs that were added 
#'within the past month to actually record my findinds and maybe do an extra analyis
Sys.time()
latest.date <- Sys.Date() %>% as.Date





#'The following lapply functions will find the metrics of our interest in different years
#'(2) Total Minutes is below.
totalMinutes <- do.call(cbind, lapply(years, function(year) {
  time <- dt[endtime_Year %in% year]$minPlayed %>%
    sum %>% prettyNum(., big.mark = ',', scintific = F)
  output <-(c(year, time)) 
}) ) %>% as.data.table %>% setnames(., as.character(.[1,])) %>% .[-1]


#'(3) Order Day
weeklyMin <- do.call(rbind, lapply(years, function(year) {
  get_ordered(dt[endtime_Year %in% year]$`Day`, 'Day') %>%
    .[, Year := year]
}) )




yearlyHour <- do.call(rbind, lapply(years, function(year) {
        get_ordered(dt[endtime_Year %in% year]$`hour_listened`, 'hour_listened') %>% as.data.table %>%
          .[, Year := year]
      }) )

#'Crosstalk data table that is connnected to the sidebar
#shared_dt <- SharedData$new(data = dt, key = dt$endtime_Year, group = 'Year')

#MASTER_TABLE <- as.data.table()










temp_music <- dt[, Listened_Songs := 1] %>% .[year(Date) %in% '2019'] %>% 
  dcast(data = ., formula = Date ~ ., fun.aggregate = sum, value.var = c('Listened_Songs', 'minPlayed') ) 

fig2 <- plot_ly(data = temp_music,
        x = ~Date, y = ~minPlayed,
        mode = 'lines', line = list(color = 'rgb(205,12,24)'),
        text = ~paste('</br> Minutes Played: ', minPlayed, 
                      '</br> Songs Listened: ', Listened_Songs)) %>%
  display_seasons(., 'Listening habits by Season', max(temp_music$minPlayed))
