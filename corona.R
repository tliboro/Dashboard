# Corona Virus
# https://aws.amazon.com/marketplace/pp/Global-Coronavirus-COVID-19-Data/prodview-rmk3gahdzo3tg#overview
libs <- c('data.table', 'dplyr', 'magrittr', 
          'ggplot2', 'plyr', 'readr', 'plotly', 
          'lubridate') ; lapply(libs, require, character.only = T)

#' Data Upload and slight cleaning  
dir <- paste0(getwd(),'/Documents/github/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/')
files <- list.files(path = dir, pattern = '03-', all.files = T, full.names = T) #'*.csv
data <- do.call(rbind, lapply(files[-1], function(fn) read.csv(file = fn))) %>%
  as.data.table %>% .[, Date := as.Date(Last.Update, format = '%Y-%m-%d')]


library(leaflet)
USA <- data[`Country.Region` %in% 'US']
loc <- leaflet(data) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~Deaths)
loc <- loc %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = ~Confirmed/100000, color = '#FF0000')
loc



#' Reports that analysis will be conducted on 
libs <- c('ggpubr', 'jpeg') ; lapply(libs, require, character.only = T)
report <- as.Date('2020-03-11') #not confident in most recent reports, until fully updated
latestReport <- data[Date %in% report]


#' COVID-19 Confirmed Cases per Country
topconfirmed <- dcast(data = latestReport, formula = Country.Region ~ ., fun = sum, value.var = c('Confirmed', 'Deaths', 'Recovered')) %>%
  as.data.table %>% setorder(., -Confirmed) %>% .[1:10]
topconfirmed$Country.Region <- factor(topconfirmed$Country.Region, levels = topconfirmed$Country.Region)

image <- readJPEG(source = paste0(getwd(), '/Desktop/PersonalProjects/corona_world.jpeg'))
ggplot(data = topconfirmed, aes(x = Country.Region, y = Confirmed, fill = Country.Region)) +   
  background_image(image) + 
  geom_bar(stat = 'identity') + geom_text(aes(label=Confirmed), size = 3, position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Confirmed Cases per Country', subtitle = paste0('Data as of: ', latestReport$Date[1]) )


#' COVID-19 Descriptive Table
#' @description Returning the table that we will be visualizing
libs <- c('grid', 'gridExtra', 'ggthemes') ; lapply(libs, require, character.only = T)
top.rates <- topconfirmed %>% .[, Mortality.Rate := round(100*Deaths/Confirmed, 2)] %>% 
  .[, Recovery.Rate := round(100*Recovered/Confirmed, 2)] %>%
  setcolorder(., c('Country.Region', 'Recovery.Rate', 'Mortality.Rate') ) %>%
  setorder(., -Recovery.Rate)

g <- tableGrob(top.rates)

find_cell <- function(table, row, col, name = 'core-fg') {
  l <- table$layout
  which(l$t==row &l$l==col & l$name==name )
}
ind <- find_cell(g, 2, 3, 'core-bg') # Recovery CHINA
ind2 <- find_cell(g, 3,3, 'core-bg') # Recovery IRAN
g$grobs[ind][[1]][['gp']] <- gpar(fill='darkolivegreen1', col = 'darkolivegreen4', lwd = 5)
g$grobs[ind2][[1]][['gp']] <- gpar(fill='darkolivegreen1', col = 'darkolivegreen4', lwd = 5)
grid.newpage(); grid.draw(g)

#' COVID-19 Death and Recovery Rates
#' @name Mortality.Rate  (# of Deaths / # of Confirmed)
#' @name Recovery.Rate (# of Recovered / # of Confirmed)
top.rates.plot <- melt(data= top.rates, id.vars = 'Country.Region', measure.vars = c('Mortality.Rate', 'Recovery.Rate')) %>%
  as.data.table() %>% setnames(., old = 'variable', 'Rate') %>% .[Rate %in% 'Mortality.Rate', Rate := 'Mortality  Rate'] %>%
  .[Rate %in% 'Recovery.Rate', Rate := 'Recovery Rate']
ggplot(data = top.rates.plot, aes(x = Country.Region, y = value)) +   
  theme_economist() + 
  geom_bar(aes(fill = Rate), position = 'dodge', stat = 'identity') + 
  labs(title = 'COVID-19 Death & Recovery Rates', 
       subtitle = paste0('Data as of: ', latestReport$Date[1]),
       x = 'Country', y = 'Rates') 


#' IDEA : Understand the spread rate by looking at the incremental changes for 
#' Confirmed Cases, Deaths, Recoveries on chronological reports.
#' ----- ISSUE -----> Historical reports don't seem to add up to one anonther. Will pause this section
#' SOLUTION ---> USE DIFFERENT DATA SET THAT WAS INCLUDED

ts_data <- fread(file = paste0(getwd(), '/Documents/github/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'), stringsAsFactors = F, data.table = T)         
ts_data_melted <- melt(data = ts_data, id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'), variable.name = 'Date', value.name = 'Confirmed') %>%
  as.data.table() %>% .[, Date := as.Date(Date, format = '%m/%d/%y')] 

library(shiny) ; library(leaflet) ; library(htmltools)

ui <- fluidPage(
  titlePanel('COVID-19'),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(inputId = 'selected_Date',
                  label = 'Date: ',
                  min = as.Date("2020-01-22"),
                  max = as.Date("2020-03-13"),
                  value = as.Date("2020-01-25"),
                  timeFormat = '%Y-%m-%d')
    )
  ,
  
  
  mainPanel(leafletOutput('mymap'))
)
)
###################

server <- function(input, output) {
  
  reactiveDT <- reactive({
    day <- input$selected_Date
    print(day)
    reactiveDT <- ts_data_melted[Date %in% day]
    
  })


  output$mymap <- renderLeaflet({
    leaflet(reactiveDT()) %>%
      setView(lng = -99, lat = 45, zoom = 2) %>% 
      addTiles() %>%
      addCircles(lng = ~ reactiveDT()$Long, lat = ~ reactiveDT()$Lat, popup = ~htmlEscape(Confirmed), radius = ~Confirmed, weight = ~Confirmed)
    
      })
}
#shinyApp(ui, server)


#' USE THIS DATA SET TO CREATE THE INCREMENTAT
