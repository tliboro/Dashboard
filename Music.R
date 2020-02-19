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
  .[, endtime_Month := substr(x = endTime, start = 6, stop = 7)] %>%
  .[, endtime_Year := substr(x = endTime, start = 1, stop = 4)] %>%
  .[, Day := wday(endTime)] %>%
  .[, Day := returnDay(Day)] %>%
  .[, hour_listened := substr(x = endTime, start = 12, stop = 13) ] %>%
  .[hour_listened %in% '00', hour_listened := '24'] #Formatting
  



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
  #.[1:10] %>%
  .[ , Artist := dt[trackName %in% Song]$artistName, by = c("Song")] %>%
  .[, c('Artist', 'Song', 'N')] 
  



#'Crosstalk data table that is connnected to the sidebar
shared_dt <- SharedData$new(data = dt, key = dt$endtime_Year, group = 'Year')


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

#'Can also try to get what hours of the day i'll listen to the most music

t <- dt$hour_listened %>% table %>%
  sort(., decreasing = T) %>% as.data.table %>%
  setnames(., old = '.', new = 'Time')
t


p <- dt[endtime_Year %in% '2020'] %>% .[, c('hour_listened')] %>% table %>%
  sort(., decreasing = T) %>% as.data.table %>%
  setnames(., old = '.', new = 'Time')
p


#If SharedData causes errors the just use the 



#2019 Metrics
total_min2019 <- dt[endtime_Year %in% 2019]$minPlayed %>% 
  sum %>% 
  prettyNum(., big.mark = ',', scientific = F)

orderDay_2019 <- get_ordered(dt[endtime_Year %in% '2019']$`Day`, 'Day')
topDay_2019 <- returnCharacter(orderDay_2019, 'Day')
orderHour_2019 <- get_ordered(dt[endtime_Year %in% '2019']$`hour_listened`, 'hour_listened')
topHour_2019 <- returnCharacter(orderHour_2019, 'hour_listened')

#2020 Metrics
total_min2020 <- dt[endtime_Year %in% 2020]$minPlayed %>% 
  sum %>% 
  prettyNum(., big.mark = ',', scientific = F)

orderDay_2020 <- get_ordered(dt[endtime_Year %in% '2020']$`Day`, 'Day')
topDay_2020 <- returnCharacter(orderDay_2020, 'Day')
orderHour_2020 <- get_ordered(dt[endtime_Year %in% '2020']$`hour_listened`, 'hour_listened')
topHour_2020 <- returnCharacter(orderHour_2020, 'hour_listened')

years <- c(2019, 2020)

#'The following lapply functions will find the metrics of our interest in different years
#'(1) Total Minutes is below.
list_totalMinutes <- lapply(years, function(year) {
  time <- dt[endtime_Year %in% year]$minPlayed %>%
    sum %>% prettyNum(., big.mark = ',', scintific = F)
  output <- c(year, time)
})
totalMinutes <- cbind(as.data.table(list_totalMinutes[1]), 
      as.data.table(list_totalMinutes[2]) ) %>% 
  setnames(., as.character(.[1,]) ) %>% .[-1,]


#'(2) Order Day
list_weeklyMin <- lapply(years, function(year) {
  get_ordered(dt[endtime_Year %in% year]$`Day`, 'Day') %>%
    .[, Year := year]
})
weeklyMin <- rbind(as.data.table(list_weeklyMin[1]), 
                   as.data.table(list_weeklyMin[2]) )


list_hour <- lapply(years, function(year) {
  get_ordered(dt[endtime_Year %in% year]$`hour_listened`, 'hour_listened') %>% as.data.table %>%
    .[, Year := year]
})
yearly_hour <- rbind(as.data.table(list_hour[1]), as.data.table(list_hour[2]) )



#'(3) Top Day
list_TopDays <- lapply(years, function(year) {
  topHour_2020 <- returnCharacter(dt[Year %in% year], 'hour_listened')
})



getTopDay <- function(year) {
  setMax <- weeklyMin
  wk <- weeklyMin[Year %in% year]
  topday <- wk[N %in% max(wk$N)]$Day
  return(topday)
}

topHour_2020 <- returnCharacter(orderHour_2020, 'hour_listened')

#'Crosstalk data table that is connnected to the sidebar
#shared_dt <- SharedData$new(data = dt, key = dt$endtime_Year, group = 'Year')

#MASTER_TABLE <- as.data.table()


