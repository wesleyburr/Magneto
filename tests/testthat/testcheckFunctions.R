context(desc = ".is_tiff.R")
test_that(".is_tiff returns the the correct bool", {
  vector <- c("test.tif", "test-@.tif", "test.tiff", "test-@.tiff", "test.png", "test-@.pdf")
  ret <- vector()
  for (i in 1:6) {
    ret[i] <- .is_tiff(vector[i])
  }
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})
