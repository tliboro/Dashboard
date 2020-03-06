path <- paste0('/Users/tylerliboro/Desktop/PersonalProjects/transactions.csv')

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


AMEX <- bank[Account.Name %in% 'American Express']
ordered_weekly <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
                    'Thursday', 'Friday', 'Saturday')
AMEX$Weekday <- ordered(x = AMEX$Weekday, c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
                                            'Thursday', 'Friday', 'Saturday'))



library(plotly)



days <- seq(from = as.Date('2019-02-03'), to = as.Date('2019-12-31'), by = 'days') %>%
  as.data.table %>% setnames(., '.', 'days')

temp <- AMEX[, Purchases := 1]
daily_payments <- dcast(data = temp, formula = Date ~ ., fun.aggregate = sum, value.var = c('Amount', 'Purchases') )
daily_payments <- daily_payments[year(Date) %in% 2019]

daily_payments <- merge(x = days, y = daily_payments, by.x = 'days', by.y = 'Date', all.x = T)

#Need to make sure that there are placeholders for zeros
PAYMENTS <- merge(x = daily_payments, y = temp_music, by = 'days')
PAYMENTS[is.na(PAYMENTS)] <- 0

summary(PAYMENTS)

MUSIC_SPRING <- PAYMENTS[Date > seasons$Spring[1] & Date < seasons$Spring[2]]
summary(MUSIC_SPRING)

seasonal_averages <- do.call(rbind, lapply(names(seasons), function(season) {
          beginning <- seasons[[season]][1]
          end <- seasons[[season]][2]
          
          avg <- PAYMENTS[beginning < Date & Date < end]$minPlayed %>% mean %>% round(., 2)
          return(c(season, avg) )
                  }) 
      )  %>% 
  as.data.table()




#'Based on the information below there does not seem to be any significant 
#'correlation between the 'total.money.spent'/day and the 'total.music.listened'/day.
#'
#'Slightly unfortunate, but I think there still may be something more to uncover.
#'
#'Could potentially just look at a single purchase (or type of) which could be the next step.
#'However, I do not believe that the effort should be explored based on the initial results.
cor(x = PAYMENTS$Purchases, y = PAYMENTS$minPlayed, method = 'pearson')

#'Lag of zero represents the correlation against itself (no lag); from comparing both graphs
#'(on the dashboard) I expected a lag of around 3~days. Based on the output below we can see that
#'there is some significance. CROSS CORRELATION
ccf(x=PAYMENTS$Purchases, y= PAYMENTS$minPlayed, lag.max = 10, type = 'correlation', plot = TRUE)





#source(paste0(getwd(), '/Desktop/PersonalProjects/plotly_shortcuts.R') )
# fig <- plot_ly(data = daily_payments, 
#                x = ~Date, y = ~Amount, mode = 'lines', text = ~paste('</br> Date: ', Date,
#                                                                     '</br> Total Amount: ', Amount, 
#                                                                      '</br> Total Purchases: ', Purchases), 
#                hoverinfo = 'text') %>% display_seasons(., 'Spending Habits by Season', max(daily_payments$Amount))
# fig
# 


