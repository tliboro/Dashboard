path <- paste0('/Users/tylerliboro/Desktop/PersonalProjects/transactions.csv')
library(plotly)

##### Data Cleaning ####
bank <- read.csv(file = path, stringsAsFactors = F) %>% as.data.table %>%
  .[, Date := as.Date(Date, format = '%m/%d/%Y')] %>%
  .[, Year := year(Date)] %>%
  .[, State := ifelse(grepl(pattern = 'VA', x = Original.Description), 'VA', 'Other')] %>%
  .[, State := ifelse(grepl(pattern = 'DC', x = Original.Description), 'DC', State)] %>%
  .[Description %in% 'Manna Cafe Arliington', Description := 'MANNA CAFE ARLIINGTON VA'] %>% 
  .[Description %in% 'Colonial Parking, Inc.', Description := 'Colonial Parking Inc.'] %>% 
  .[Description %in% 'Metro Fare Autoload', Description := 'METRO FARE AUTOLOAD 20010WASHINGTON DC'] %>%
  .[, Order := 1:.N] %>%
  .[!Description %in% c('Online', 'DMV')] %>%
  .[, Month := as.factor(month(Date))] %>%
  .[, Weekday := as.factor(weekdays(Date))]


DT_AMEX <- bank[Account.Name %in% 'American Express'] %>% .[,Purchases := 1]
ordered_weekly <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
                    'Thursday', 'Friday', 'Saturday')
DT_AMEX$Weekday <- ordered(x = DT_AMEX$Weekday, c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
                                            'Thursday', 'Friday', 'Saturday'))

#MMerge and standardization of the time series (from DT_BANK & DT_MUSIC)
x <-  min(data[, Date])  ; y <- min(data[, Date])
days <- seq(from =x, to = y , by = 'days') %>%
  as.data.table %>% setnames(., '.', 'days') ; rm(x, y)

#Combining Data Sets (BANK )
DT_MUSIC_BANK <- dcast(data = DT_AMEX, formula = Date ~ ., fun.aggregate = sum, 
                        value.var = c('Amount', 'Purchases') ) %>%
  .[year(Date) %in% 2019] %>% #Only interested in the yearly data. 
        merge(x = ., y = DT_MUSIC, 
            by.x = 'Date', by.y = 'days', all.y = T)
DT_MUSIC_BANK[is.na(DT_MUSIC_BANK)] <- 0




# Creating Seasonal Data
MUSIC_SPRING <- DT_MUSIC_BANK[Date > seasons$Spring[1] & Date < seasons$Spring[2]]
summary(MUSIC_SPRING)

seasonal_averages <- do.call(rbind, lapply(names(seasons), function(season) {
          beginning <- seasons[[season]][1]
          end <- seasons[[season]][2]
          
          avg <- DT_MUSIC_BANK[beginning < Date & Date < end]$minPlayed %>% mean %>% round(., 2)
          return(c(season, avg) )
        }) 
      )  %>% as.data.table(.) %>% 
  setnames(., old = c('V1','V2'),  new = c('Season','Avg') )


#'Based on the information below there does not seem to be any significant 
#'correlation between the 'total.money.spent'/day and the 'total.music.listened'/day.
#'
#'Slightly unfortunate, but I think there still may be something more to uncover.
#'
#'Could potentially just look at a single purchase (or type of) which could be the next step.
#'However, I do not believe that the effort should be explored based on the initial results.
cor(x = DT_MUSIC_BANK$Purchases, y = DT_MUSIC_BANK$minPlayed, method = 'pearson')

#'Lag of zero represents the correlation against itself (no lag); from comparing both graphs
#'(on the dashboard) I expected a lag of around 3~days. Based on the output below we can see that
#'there is some significance. CROSS CORRELATION
ccf(x=DT_MUSIC_BANK$Purchases, y= DT_MUSIC_BANK$minPlayed, lag.max = 10, type = 'correlation', plot = TRUE)

#'Proof that the Cross Correlation provided real results. Therefore it is not worth exploring this area any further.
#'UNLESS we also try to consider the unique types of purchases s.a. - Food, Small Purchases, or even on the music 
#'side where only listening in the mornings against the nights (or where one is greater than the other).
lagged_DT_MUSIC_BANK <- cbind(DT_MUSIC_BANK[1:(NROW(DT_MUSIC_BANK)-2)]$Purchases, 
                              DT_MUSIC_BANK$minPlayed[3:NROW(DT_MUSIC_BANK)]) %>% as.data.table
cor(x = lagged_DT_MUSIC_BANK$V1, y = lagged_DT_MUSIC_BANK$V2, method = 'pearson') ; rm(lagged_DT_MUSIC_BANK)



