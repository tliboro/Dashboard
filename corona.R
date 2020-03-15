# Corona Virus
# https://aws.amazon.com/marketplace/pp/Global-Coronavirus-COVID-19-Data/prodview-rmk3gahdzo3tg#overview
libs <- c('data.table', 'dplyr', 'magrittr', 
          'ggplot2', 'plyr', 'readr', 'plotly', 
          'lubridate')
lapply(libs, require, character.only = T)

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


libs <- c('ggpubr', 'jpeg') ; lapply(libs, require, character.only = T)
img.file <- file.path(paste0(getwd(), '/Desktop/PersonalProjects/corona_world.jpeg'),
                        package = "ggpubr")
image <- readJPEG(source = paste0(getwd(), '/Desktop/PersonalProjects/corona_world.jpeg'))

report <- as.Date('2020-03-11')
latestReport <- data[Date %in% report]

topconfirmed <- dcast(data = latestReport, formula = Country.Region ~ ., fun = sum, value.var = c('Confirmed', 'Deaths', 'Recovered')) %>%
  as.data.table %>% setorder(., -Confirmed) %>% .[1:10]
topconfirmed$Country.Region <- factor(topconfirmed$Country.Region, levels = topconfirmed$Country.Region)

ggplot(data = topconfirmed, aes(x = Country.Region, y = Confirmed, fill = Country.Region)) +   background_image(image) + 
  geom_bar(stat = 'identity') + geom_text(aes(label=Confirmed), size = 3, position=position_dodge(width=0.9), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Confirmed Cases per Country', subtitle = paste0('Data as of: ', latestReport$Date[1]) )

libs <- c('grid', 'gridExtra', 'ggthemes') ; lapply(libs, require, character.only = T)
top.rates <- topconfirmed %>% .[, Death.Rate := round(Deaths/Confirmed, 2)] %>% 
  .[, Recovery.Rate := round(Recovered/Confirmed, 2)]

grid.newpage(); grid.table(top.rates)


top.rates.plot <- melt(data= top.rates, id.vars = 'Country.Region', measure.vars = c('Death.Rate', 'Recovery.Rate')) %>%
  as.data.table() %>% setnames(., old = 'variable', 'Rate') %>% .[Rate %in% 'Death.Rate', Rate := 'Death Rate'] %>%
  .[Rate %in% 'Recovery.Rate', Rate := 'Recovery Rate']
ggplot(data = top.rates.plot, aes(x = Country.Region, y = value)) +   
  theme_economist() + 
  geom_bar(aes(fill = Rate), position = 'dodge', stat = 'identity') + 
  labs(title = 'COVID-19 Death & Recovery Rates', 
       subtitle = paste0('Data as of: ', latestReport$Date[1]),
       x = 'Country', y = 'Rates') 


#I am curious to find the rate of spread
ROS <- dcast(data = data, formula = Country.Region + Date ~ ., fun = sum, value.var = c('Confirmed', 'Deaths'))

library(fmsb) #Creating Radar Charts


## MAKE CONTOUR LINES
## Note, bandwidth choice is based on MASS::bandwidth.nrd()
kde <- bkde2D(USA[ , list(Longitude, Latitude)],
              bandwidth=c(.0045, .0045), gridsize = c(500,500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

library(sp)
library(rgdal)
library(KernSmooth)
