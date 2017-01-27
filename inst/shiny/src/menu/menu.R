shinydashboard::dashboardSidebar(
shinydashboard::sidebarMenu(id = "tabs",
            menuItem(text ="Information",tabName = "information",icon = icon("search")),
            
            
            ##Hist
            menuItem(text ="Hist",icon = icon("area-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amhist"),
                     menuSubItem(text = "Complex Function", tabName = "hist")),
            
            
            
            ##Pie
            menuItem(text ="Pie",icon = icon("pie-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amPie"),
                     menuSubItem(text = "Complex Function", tabName = "pie")),
            
            
            ##Radar
            menuItem(text ="Radar",icon = icon("circle"),
                     menuSubItem(text = "Simple Function", tabName = "amRadar"),
                     menuSubItem(text = "Complex Function", tabName = "radar")),
            
            ##Bar plot
            menuItem(text ="Bar",icon = icon("bar-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amBarplot"),
                     menuSubItem(text = "Complex Function", tabName = "bar")),
            
            ##Lines
            menuItem(text ="Lines",icon = icon("line-chart"),
                     menuSubItem(text = "Simple Function", tabName = "amLines"),
                     menuSubItem(text = "Complex Function", tabName = "lines")),
            
            
            
            
            ##Lines
            menuItem(text ="XY",icon = icon("soundcloud"),
                     menuSubItem(text = "Simple Function", tabName = "amxy"),
                     menuSubItem(text = "Complex Function", tabName = "xy")),
            
            
            
            
            ##Stock
            menuItem(text ="Stock",icon = icon("sliders"),
                     menuSubItem(text = "Simple Function", tabName = "amstock"),
                     menuSubItem(text = "Complex Function", tabName = "stock")),
            
            ##Funnel
            menuItem(text ="Funnel",icon = icon("sort-desc"),
                     menuSubItem(text = "Simple Function", tabName = "amfunnel"),
                     menuSubItem(text = "Complex Function", tabName = "funnel")),
            
            ##Gauge
            menuItem(text ="Gauge",icon = icon("repeat"),
                     menuSubItem(text = "Simple Function", tabName = "amgauge"),
                     menuSubItem(text = "Complex Function", tabName = "gauge")),
            
            ##Candlesticks
            menuItem(text ="Candlesticks",icon = icon("arrows-h"),
                     menuSubItem(text = "Simple Function", tabName = "amcandlesticks"),
                     menuSubItem(text = "Complex Function", tabName = "candlesticks")),
            
            
            ##Bullet
            menuItem(text ="Bullet",icon = icon("ticket"),
                     menuSubItem(text = "Simple Function", tabName = "amBullet"),
                     menuSubItem(text = "Complex Function", tabName = "Bullet")),
            ##Mekko
            menuItem(text ="Mekko",icon = icon("square"),
                     menuSubItem(text = "Simple Function", tabName = "ammekko"),
                     menuSubItem(text = "Complex Function", tabName = "mekko"))
)
)