context("amBarplot")

library(pipeR)
data("data_bar")
data("data_gbar")

testthat::test_that("amBarplot: basic example", {
  testthat::expect_silent({
    # Test with label rotation
    amBarplot(x = "country", y = "visits", data = data_bar, labelRotation = -45)
    
    # Test with label rotation
    amBarplot(x = "country", y = "visits", data = data_bar, labelRotation = -45) 
    
    # Horizontal bar
    amBarplot(x = "country", y = "visits", data = data_bar, horiz = TRUE, labelRotation = -45)
    
    # 3D bar
    amBarplot(x = "country", y = "visits", data = data_bar, depth = 15, labelRotation = -45)
    
    # Display values
    amBarplot(x = "country", y = "visits", data = data_bar, show_values = TRUE, labelRotation = -45)
  })
})

testthat::test_that("amBarplot: grouped columns", {
  testthat::expect_silent({   
    # Grouped columns
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar)
    
    # Change groups colors
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, 
              groups_color = c("#87cefa", "#c7158"))
  })
})

testthat::test_that("amBarplot: parse date", {
  testthat::expect_silent({  
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar,
              dataDateFormat = "YYYY", minPeriod = "YYYY")
    
    # Default label: first day of each month
    amBarplot(x = "month", y = c("income", "expenses"), data = data_gbar,
              dataDateFormat = "MM/YYYY", minPeriod = "MM")
    
    amBarplot(x = "day", y = c("income", "expenses"), data = data_gbar,
              dataDateFormat = "DD/MM/YYYY")
  })
})

testthat::test_that("amBarplot: stacked bars", {
  testthat::expect_silent({  
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "regular")
    
    # 100% stacked bars
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "100")
  })
})

testthat::test_that("amBarplot: layered bars", {
  testthat::expect_silent({   
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, layered = TRUE)
    
    # Data with row names
    dataset <- data.frame(get(x = "USArrests", pos = "package:datasets"))
    amBarplot(y = c("Murder", "Assault", "UrbanPop", "Rape"), data = dataset, stack_type = "regular")
    
    
    # Round values
    amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar) %>>%
      setProperties(precision = 0)
  })
})