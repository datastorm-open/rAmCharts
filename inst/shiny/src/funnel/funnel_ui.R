shinydashboard::tabItem(
  tabName = "funnel",
  fluidRow(
    column(
      width = 12,
     
      br(),
      rAmCharts::amChartsOutput("funnel1"),
      verbatimTextOutput("code_funnel1"),
      br(), hr(), br(),
      h2("3D Funnel chart"),
      rAmCharts::amChartsOutput("funnel2"),
      fluidRow(
        column(width = 6,
               align = "center",
               sliderInput("angle_funnel", label = "Angle", min = 0, max = 60, value = 40)),
        column(width = 6,
               align = "center",
               sliderInput("depth_funnel", label = "Depth", min = 1, max = 120, value = 100))
      ),
      verbatimTextOutput("code_funnel2"),
      br(), hr(), br(),
      h2("Pyramid chart"),
      rAmCharts::amChartsOutput("funnel3"),
      verbatimTextOutput("code_funnel3")
    )
  )
)