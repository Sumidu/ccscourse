context("test-cor_matrix_plot")


# Test when only an empty data-frame is available
test_that("Empty data frame", {
  data <- data.frame()
  expect_error(cor_matrix_plot(data), "Data must contain at least 2 columns.")

})


# Tests what happens when only 1 column is available
test_that("Single column data frame", {
  data <- data.frame(a=c(1,2,3))
  expect_error(cor_matrix_plot(data), "Data must contain at least 2 columns.")
})


# Submit 4x4 Matrix
test_that("Should print corplot", {
  data <- mtcars %>% select(mpg, disp, hp, drat)
  cor_matrix_plot(data) -> res
  expect_is(cor_matrix_plot(data), "matrix")
})
