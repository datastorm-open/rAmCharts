require(testthat)
testthat::auto_test(code_path = getwd(), test_path = paste0(getwd(), "/tests"))