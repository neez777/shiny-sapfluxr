library(plotly)
library(htmlwidgets)

csv_btn <- list(
  name = "Download CSV",
  icon = list(
    path = "M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z",
    transform = "matrix(0.8 0 0 0.8 2.4 2.4)"
  ),
  click = htmlwidgets::JS("function(gd) { alert('CSV download clicked!'); }")
)

p <- plot_ly(x = 1:10, y = rnorm(10), type = 'scatter', mode = 'lines') %>%
  config(
    displayModeBar = TRUE,
    displaylogo = FALSE,
    toImageButtonOptions = list(format = "png", filename = "test"),
    modeBarButtons = list(
      list("toImage", csv_btn),
      list("zoom2d", "pan2d"),
      list("zoomIn2d", "zoomOut2d", "resetScale2d")
    )
  )

htmltools::save_html(p, "test_plotly.html")