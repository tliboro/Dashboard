removeButtons = list(
  'sendDataToCloud',
  'autoScale2d',
  'toggleSpikelines',
  'hoverClosestCartesian',
  'hoverCompareCartesian',
  'zoom2d',
  'pan2d',
  'select2d',
  'lasso2d'
) 

display_seasons <- function (fig, title, max.height) {
  output <- layout(fig, title = title, 
                   shapes = list(
                     
                     #WINTER MONTHS
                     list(type = 'rect', fillcolor = "blue", line = list(color = "black"), opacity = 0.3,
                          x0 = "2019-01-01", x1 = "2019-03-18", xref = "x",
                          y0 = 0, y1 = max.height, yref = "y"),
                     list(type = "rect",
                          fillcolor = "blue", line = list(color = "black"), opacity = 0.3,
                          x0 = "2019-12-01", x1 = "2019-12-31", xref = "x",
                          y0 = 0, y1 = max.height, yref = "y"),
                     
                     #SPRING MONTHS
                     list(type = "rect",
                          fillcolor = "green", line = list(color = "black"), opacity = 0.3,
                          x0 = "2019-03-19", x1 = "2019-06-20", xref = "x",
                          y0 = 0, y1 = max.height, yref = "y"),
                     
                     #SUMMER MONTHS
                     list(type = "rect",
                          fillcolor = "yellow", line = list(color = "black"), opacity = 0.3,
                          x0 = "2019-06-21", x1 = "2019-09-22", xref = "x",
                          y0 = 0, y1 = max.height, yref = "y"),
                     
                     #FALL MONTHS
                     list(type = "rect",
                          fillcolor = "brown", line = list(color = "black"), opacity = 0.3,
                          x0 = "2019-09-22", x1 = "2019-12-01", xref = "x",
                          y0 = 0, y1 = max.height, yref = "y")
                     
                   )
  ) %>%
    config(modeBarButtonsToRemove = removeButtons)
  return(output)
}