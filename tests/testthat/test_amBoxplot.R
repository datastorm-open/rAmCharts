testthat::context("amBoxplot")


testthat::test_that("Formula", {
  testthat::expect_silent({
    
    (obj <- amBoxplot(count ~ spray, data = InsectSprays))
    
    # Adding parameters
    amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50),
              xlab = "spray", col = c("darkblue", "gray"))
    
    # Transpose
    amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50), xlab = "spray", horiz = TRUE)
    
    # Using a custom colum to identify outliers
    InsectSprays$id <- paste0("ID : ", 1:nrow(InsectSprays))
    amBoxplot(count ~ spray, data = InsectSprays, id = "id")
    
    # Parameter for amOptions
    amBoxplot(count ~ spray, data = InsectSprays, main = "amcharts")
  })
})

testthat::test_that("data.frame", {
  testthat::expect_silent({
    don <- data.frame(a = 1:10, b = 1:5)
    amBoxplot(don, ylim = c(0,15))
    
    # Parameter for amOptions
    amBoxplot(count ~ spray, data = InsectSprays, creditsPosition = "top-right")
  })
})

testthat::test_that("matrix", {
  testthat::expect_silent({
    x <- matrix(nrow = 10, ncol = 5, rnorm(50))
    
    amBoxplot(x) # on columns
    colnames(x) <- LETTERS[1:5]
    amBoxplot(x) # with names
    amBoxplot(x, use.cols = FALSE, col = c("blue", "red"))
    
    # Parameter for amOptions
    amBoxplot(x, export = TRUE, exportFormat = "SVG")
  })
})

testthat::test_that("numeric vector", {
  testthat::expect_silent({
    amBoxplot(rnorm(100))
    
    # Parameter for amOptions
    amBoxplot(rnorm(100), zoom = TRUE)
  })
})