shinydashboard::dashboardSidebar(
shinydashboard::sidebarMenu(id = "tabs",
            menuItem(text ="Information",tabName = "information",icon = icon("search")),
            
            ##Pie
            menuItem(text ="Pie",icon = icon("pie-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amPie"),
                     menuSubItem(text = "Complexe Function", tabName = "pie")),
            
            
            ##Radar
            menuItem(text ="Radar",icon = icon("circle"),
                     menuSubItem(text = "Simple Function", tabName = "amRadar"),
                     menuSubItem(text = "Complexe Function", tabName = "radar")),
            
            ##Bar plot
            menuItem(text ="Bar",icon = icon("bar-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amBar"),
                     menuSubItem(text = "Complexe Function", tabName = "bar")),
            
            ##Lines
            menuItem(text ="Lines",icon = icon("line-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amLines"),
                     menuSubItem(text = "Complexe Function", tabName = "lines")),
            
            
            
            
            menuItem("XY",tabName = "XY",icon = icon("line-chart")),
            menuItem("Stock",tabName = "stock",icon = icon("sliders")),
            menuItem(text = "More",
                     menuSubItem(text = "Gauge", tabName = "gauge"),
                     menuSubItem(text = "Funnel", tabName = "funnel"),
                     menuSubItem(text = "Drilldown", tabName = "drilldown")
            ),
            tags$li(  selectInput("theme", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")))
)
)