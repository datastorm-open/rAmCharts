library(shiny)
library(rAmCharts)
library(shinydashboard)

shinydashboard::dashboardPage(
  
  shinydashboard::dashboardHeader(title="rAmCharts"),
  
  source("./src/menu/menu.R", local = TRUE, encoding = "UTF-8")$value,
  
  shinydashboard::dashboardBody(
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    shinydashboard::tabItems(
      source("./src/Information/information_ui.R", local = TRUE)$value,
      
      source("./src/pie/pie_ui.R", local = TRUE)$value,
      source("./src/pie/ampie_ui.R", local = TRUE)$value,
      
      source("./src/radar/radar_ui.R", local = TRUE)$value,
      source("./src/radar/amradar_ui.R", local = TRUE)$value,
      
      source("./src/lines/lines_ui.R", local = TRUE)$value,
      source("./src/lines/amlines_ui.R", local = TRUE)$value,
      
      source("./src/bar/bar_ui.R", local = TRUE)$value,
      source("./src/bar/ambar_ui.R", local = TRUE)$value,

      source("./src/xy/xy_ui.R", local = TRUE)$value,
      source("./src/xy/amxy_ui.R", local = TRUE)$value,
      
      source("./src/stock/stock_ui.R", local = TRUE)$value,
      source("./src/stock/amstock_ui.R", local = TRUE)$value,
      
      source("./src/funnel/funnel_ui.R", local = TRUE)$value,
      source("./src/funnel/amfunnel_ui.R", local = TRUE)$value,
      
      source("./src/gauge/gauge_ui.R", local = TRUE)$value,
      source("./src/gauge/amgauge_ui.R", local = TRUE)$value,
      
      source("./src/candlestick/candlestick_ui.R", local = TRUE)$value,
      source("./src/candlestick/amcandlestick_ui.R", local = TRUE)$value,
      
      source("./src/bullet/bullet_ui.R", local = TRUE)$value,
      source("./src/bullet/ambullet_ui.R", local = TRUE)$value,
      
      source("./src/mekko/mekko_ui.R", local = TRUE)$value,
      source("./src/mekko/ammekko_ui.R", local = TRUE)$value
    )
  )
)


