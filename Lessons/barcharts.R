x <- c('Product<br>Revenue', 'Services<br>Revenue', 'Total<br>Revenue', 'Fixed<br>Costs', 'Variable<br>Costs', 'Total<br>Costs', 'Total')
y <- c(400, 660, 660, 590, 400, 400, 340)
base <- c(0, 430, 0, 570, 370, 370, 0)
revenue <- c(430, 260, 690, 0, 0, 0, 0)
costs <- c(0, 0, 0, 120, 200, 320, 0)
profit <- c(0, 0, 0, 0, 0, 0, 370)
text <- c('$430K', '$260K', '$690K', '$-120K', '$-200K', '$-320K', '$370K')
data <- data.frame(x, base, revenue, costs, profit, text)

#The default order will be alphabetized unless specified as below:
data$x <- factor(data$x, levels = data[["x"]])

p <- plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)')) %>%
  add_trace(y = ~revenue, marker = list(color = 'rgba(55, 128, 191, 0.7)',
                                        line = list(color = 'rgba(55, 128, 191, 0.7)',
                                                    width = 2))) %>%
  add_trace(y = ~costs, marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                      line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                  width = 2))) %>%
  add_trace(y = ~profit, marker = list(color = 'rgba(50, 171, 96, 0.7)',
                                      line = list(color = 'rgba(50, 171, 96, 1.0)',
                                                  width = 2))) %>%
  layout(title = 'Annual Profit - 2015',
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         barmode = 'stack',
         paper_bgcolor = 'rgba(245, 246, 249, 1)',
         plot_bgcolor = 'rgba(245, 246, 249, 1)',
         showlegend = FALSE) %>%
  add_annotations(text = text,
                  x = x,
                  y = y,
                  xref = "x",
                  yref = "y",
                  font = list(family = 'Arial',
                              size = 14,
                              color = 'rgba(245, 246, 249, 1)'),
                  showarrow = FALSE)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="bar/waterfall")
chart_link
