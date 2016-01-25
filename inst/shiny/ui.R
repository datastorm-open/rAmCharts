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
      source("./src/pie/amPie_ui.R", local = TRUE)$value,
      
      source("./src/radar/radar_ui.R", local = TRUE)$value,
      source("./src/radar/amRadar_ui.R", local = TRUE)$value,
      
      source("./src/lines/lines_ui.R", local = TRUE)$value,
      source("./src/lines/amLines_ui.R", local = TRUE)$value,
      
      source("./src/bar/bar_ui.R", local = TRUE)$value,
      source("./src/bar/amBar_ui.R", local = TRUE)$value,

      source("./src/xy/xy_ui.R", local = TRUE)$value,
      source("./src/xy/amXY_ui.R", local = TRUE)$value,
      
      source("./src/stock/stock_ui.R", local = TRUE)$value,
      source("./src/stock/amStock_ui.R", local = TRUE)$value,
      
      source("./src/funnel/funnel_ui.R", local = TRUE)$value,
      source("./src/funnel/amFunnel_ui.R", local = TRUE)$value,
      
      source("./src/gauge/gauge_ui.R", local = TRUE)$value,
      source("./src/gauge/amGauge_ui.R", local = TRUE)$value,
      
      source("./src/candlestick/candlestick_ui.R", local = TRUE)$value,
      source("./src/candlestick/amCandlestick_ui.R", local = TRUE)$value,
      
      source("./src/bullet/bullet_ui.R", local = TRUE)$value,
      source("./src/bullet/amBullet_ui.R", local = TRUE)$value,
      
      source("./src/mekko/mekko_ui.R", local = TRUE)$value,
      source("./src/mekko/amMekko_ui.R", local = TRUE)$value
    )
  )
)


