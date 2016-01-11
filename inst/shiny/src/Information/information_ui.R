shinydashboard::tabItem(
  tabName = "information",
  h1("rAmCharts"),
  p(strong("rAmChart "),"is a graphic package. It is based on the javascript library", 
    a("amcharts",href='https://www.amcharts.com/amCharts'), "."),
  p("Development is done in two steps :"),
  
  tags$ul(
    tags$li("Integration of all amCharts library function to construct json object.
            These functions are difficult to use but they offer contruction possibilities very varied graphics."), 
    tags$li("Adaptation of complex construction way to simple R functions. They are esay to use and options
            of base R'graphics are keep up. They are not exthautives.")
  ),
  p("All wep page named 'Complexe Function' use the first method."),
  p("All wep page named 'Simple Function' use the second method.")
  

)

