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
            
            
            
            
            ##Lines
            menuItem(text ="XY",icon = icon("soundcloud"),
                     menuSubItem(text = "Simple Function", tabName = "amxy"),
                     menuSubItem(text = "Complexe Function", tabName = "xy")),
            
            
            
            
            ##Stock
            menuItem(text ="Stock",icon = icon("sliders"),
                     menuSubItem(text = "Simple Function", tabName = "amstock"),
                     menuSubItem(text = "Complexe Function", tabName = "stock")),
            
            ##Funnel
            menuItem(text ="Funnel",icon = icon("sort-desc"),
                     menuSubItem(text = "Simple Function", tabName = "amfunnel"),
                     menuSubItem(text = "Complexe Function", tabName = "funnel")),
            
            ##Gauge
            menuItem(text ="Gauge",icon = icon("repeat"),
                     menuSubItem(text = "Simple Function", tabName = "amgauge"),
                     menuSubItem(text = "Complexe Function", tabName = "gauge"))
)
)