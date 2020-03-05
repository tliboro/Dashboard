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
temp <- AMEX[, Purchases := 1]
daily_payments <- dcast(data = temp, formula = Date ~ ., fun.aggregate = sum, value.var = c('Amount', 'Purchases') )
daily_payments <- daily_payments[year(Date) %in% 2019]
max.height <- daily_payments$Amount %>% max



#source(paste0(getwd(), '/Desktop/PersonalProjects/plotly_shortcuts.R') )
fig <- plot_ly(data = daily_payments, 
               x = ~Date, y = ~Amount, mode = 'lines', text = ~paste('</br> Date: ', Date,
                                                                    '</br> Total Amount: ', Amount, 
                                                                     '</br> Total Purchases: ', Purchases), 
               hoverinfo = 'text') %>% display_seasons(., 'Spending Habits by Season', max(daily_payments$Amount))
fig



